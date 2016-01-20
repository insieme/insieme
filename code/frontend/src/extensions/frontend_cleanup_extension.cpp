/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#include "insieme/frontend/extensions/frontend_cleanup_extension.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/pattern/rule.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/decl_only.h"
#include "insieme/annotations/c/include.h"
#include "insieme/annotations/data_annotations.h"

#include "insieme/frontend/converter.h"
#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/tu/ir_translation_unit_io.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/assert.h"

#include <functional>

namespace insieme {
namespace frontend {
namespace extensions {

	using namespace core;
	namespace icp = pattern;
	namespace irp = pattern::irp;

	namespace {
		//////////////////////////////////////////////////////////////////////
		// Assure return statements for "Main" functions typed as int
		// ==========================================================
		//
		// In C, it's allowed for the main function to by typed as () -> int, but not actually contain a "return x".
		// Since in the backend we move that function to a "normal" one, this will generate a warning in the BE compiler.
		// We fix this by generating a "return 0" in that case.
		//
		ProgramPtr mainReturnCorrection(ProgramPtr prog) {
			auto& mgr = prog->getNodeManager();
			auto& basic = mgr.getLangBasic();
			const auto& build = IRBuilder(mgr);

			// if entry point returns int ensure that it has a return statement
			auto eplist = prog->getEntryPoints();
			// we only need to care about C programs which do not have a return because of C semantics on "main"
			if(eplist.size() == 1) {
				auto eP = prog->getEntryPoints()[0];
				if(eP.isa<LambdaExprPtr>()) {
					auto lambdaExp = eP.as<LambdaExprPtr>();
					auto funType = lambdaExp->getFunctionType();
					if(basic.isInt(funType->getReturnType())) {
						auto compound = LambdaExprAddress(lambdaExp)->getBody();

						icp::TreePattern compoundPattern =
							irp::compoundStmt(icp::empty | icp::single(!irp::returnStmt(icp::any)) | (icp::anyList << !irp::returnStmt(icp::any)));
						if(compoundPattern.match(compound)) {
							// outermost compound does not contain return, add it
							auto newRoot = core::transform::append(mgr, compound, toVector<core::StatementPtr>(build.returnStmt(build.intLit(0))))
											   .as<core::ExpressionPtr>();
							prog = Program::remEntryPoint(mgr, prog, eP);
							prog = Program::addEntryPoint(mgr, prog, newRoot);
						}
					}
				}
			}
			return prog;
		}

		//////////////////////////////////////////////////////////////////////
		// Remove calls to RecordTypeFixup (from the frontend inspire module)
		// ==================================================================
		//
		// These are generated to ensure valid semantics pre-resolver, and should be eliminated now.
		// All of these should have the same argument type as their return type (and type literal) at this point.
		//
		ProgramPtr removeRecordTypeFixup(ProgramPtr prog) {
			auto& mgr = prog->getNodeManager();
			auto& inspModule = mgr.getLangExtension<frontend::utils::FrontendInspireModule>();

			prog = irp::replaceAllAddr(irp::callExpr(inspModule.getRecordTypeFixup(), icp::anyList), prog, [&](const NodeAddress& matchingAddress) {
				auto call = matchingAddress.getAddressedNode().as<CallExprPtr>();
				auto arg0 = call->getArgument(0);
				// ensure correct typing
				//assert_pred2(core::analysis::equalTypes, call->getType(), arg0->getType()) << "Record types not correctly resolved"; 
				// TODO FIXME re-enable assertion and remove this when uniform representation of recursive types is available
				if(!core::analysis::equalTypes(call->getType(), arg0->getType())) {
					arg0 = core::transform::replaceAddress(mgr, ExpressionAddress(arg0)->getType(), call->getType()).getRootNode().as<ExpressionPtr>();
				} 
				return arg0;
			}).as<ProgramPtr>();

			return prog;
		}
		
		//////////////////////////////////////////////////////////////////////////
		// Remove superfluous bool_to_int calls (from the frontend inspire module)
		// =======================================================================
		//
		// These are generated to ensure valid C-style semantics, but can be removed if they are unnecessary in the final IR.
		//
		ProgramPtr removeSuperfluousBoolToInt(ProgramPtr prog) {
			auto& mgr = prog->getNodeManager();
			auto& inspModule = mgr.getLangExtension<frontend::utils::FrontendInspireModule>();

			prog = irp::replaceAllAddr(irp::callExpr(inspModule.getBoolToInt(), icp::anyList), prog, [&](const NodeAddress& matchingAddress) -> NodePtr {
				auto call = matchingAddress.getAddressedNode().as<CallExprPtr>();
				auto arg0 = call->getArgument(0);
				// ensure unused (currently simply check parent)
				if(matchingAddress.getDepth()>0 && matchingAddress.getParentNode().isa<CompoundStmtPtr>()) {
					core::transform::utils::migrateAnnotations(call, arg0);
					return arg0;
				}
				// else keep call
				return call;
			}).as<ProgramPtr>();

			return prog;
		}

		
		//////////////////////////////////////////////////////////////////////////
		// Find and replace zero inits of tag types (which can only occur for global inits)
		// =======================================================================
		ProgramPtr replaceZeroStructInits(ProgramPtr prog) {
			auto& mgr = prog->getNodeManager();
			core::IRBuilder builder(mgr);

			prog = irp::replaceAllAddr(irp::callExpr(mgr.getLangBasic().getZero(), icp::anyList), prog, [&](const NodeAddress& matchingAddress) -> NodePtr {
				auto call = matchingAddress.getAddressedNode().as<CallExprPtr>();
				auto arg0 = call->getArgument(0);
				auto t = arg0->getType().as<core::GenericTypePtr>()->getTypeParameter(0);
				if(t.isa<core::TagTypePtr>()) {
					return builder.getZero(t);
				}
				// else keep call
				return call;
			}).as<ProgramPtr>();

			return prog;
		}
		
	}

	boost::optional<std::string> FrontendCleanupExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// last or second-last
		auto it = setup.getExtensions().crbegin();
		if(it->get() != this) {
			std::advance(it, 1);
			if(it->get() != this) { return boost::optional<std::string>("FrontendCleanup needs to be the last or second-to-last Extension"); }
		}

		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	insieme::core::ProgramPtr FrontendCleanupExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		prog = mainReturnCorrection(prog);
		prog = removeRecordTypeFixup(prog);
		prog = removeSuperfluousBoolToInt(prog);
		prog = replaceZeroStructInits(prog);

		return prog;
	}

} // extensions
} // frontend
} // insieme

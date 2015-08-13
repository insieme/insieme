/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/extensions/insieme_pragma_extension.h"

#include <iostream>
#include <functional>

#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/loop_annotations.h"
#include "insieme/annotations/transform.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/functions/transformations.h"
#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/transformation.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace frontend {
namespace extensions {

	using namespace insieme::frontend::pragma;
	using namespace insieme::frontend::pragma::tok;
	using namespace stmtutils;

	namespace ia = insieme::annotations;

	namespace {

		core::annotations::Location getStartLocation(const core::NodePtr& node) {
			if(core::annotations::hasAttachedLocation(node)) { return core::annotations::getAttachedLocation(node); }
			return core::annotations::Location::getShared();
		}

		pragmaHandlerFunction getTransformLambda(ia::TransformationHint::Type type) {
			return [&, type](const pragma::MatchObject& object, core::NodeList nodes) {
				auto&& trg = nodes.front().as<StatementPtr>();

				vector<unsigned> intValues =
				    ::transform(object.getStrings("values"), [](const string& element) { return insieme::utils::numeric_cast<unsigned>(element); });

				if(!trg->hasAnnotation(ia::TransformAnnotation::KEY)) { trg->addAnnotation(std::make_shared<ia::TransformAnnotation>()); }

				ia::TransformAnnotation& ann = *trg->getAnnotation(ia::TransformAnnotation::KEY);
				ann.getAnnotationList().push_back(std::make_shared<ia::TransformationHint>(type, intValues));

				return nodes;
			};
		}

	} // anonymous

	core::ProgramPtr InsiemePragmaExtension::IRVisit(core::ProgramPtr& program) {
		program = insieme::utils::measureTimeFor<ProgramPtr, INFO>("Pragma.Transformer", [&]() { return this->applyTransformations(program); });
		return program;
	}

	frontend::tu::IRTranslationUnit InsiemePragmaExtension::IRVisit(frontend::tu::IRTranslationUnit& tu) {
		// if there are no previously marked entry points, there's nothing to be done
		if(entryPoints.size() < 1) { return tu; }

		// get IR for checking whether nodes are still valid
		core::ExpressionPtr&& singlenode = frontend::tu::toIR(tu.getNodeManager(), tu);
		assert_true(singlenode) << "Conversion of IRTranslationUnit to IR failed!";
		IRBuilder builder(singlenode->getNodeManager());

		// check if nodes previously marked as entry points are still valid and add them
		for(auto it = entryPoints.begin(); it != entryPoints.end(); ++it) {
			visitBreadthFirstInterruptible(singlenode, [&](const NodePtr& node) {
				ExpressionPtr expr = dynamic_pointer_cast<core::ExpressionPtr>(node);

				if(!expr) { return false; }

				if(*it != expr) { return false; }

				LambdaExprPtr lambda = dynamic_pointer_cast<core::LambdaExprPtr>(expr);
				assert_true(lambda) << "Non-LambdaExpression marked as entry point!";

				string cname = core::annotations::getAttachedName(lambda);
				assert_gt(cname.length(), 0) << "LambdaExpression marked as entry point has no cname annotation!";

				core::LiteralPtr lit = builder.literal(lambda->getType(), cname);
				assert_true(lit) << "Could not build literal!";

				tu.addEntryPoints(lit);

				return true;
			});
		}
		entryPoints.clear();

		return tu;
	}

	InsiemePragmaExtension::InsiemePragmaExtension() {
		// some utilities
		auto range = ~l_paren >> var["var"] >> ~equal >> expr["lb"] >> ~colon >> expr["ub"] >> ~r_paren;
		// range *(, range)
		auto range_list = range >> *(~comma >> range);

		// entry points
		pragmaHandlers.push_back(
		    std::make_shared<PragmaHandler>(PragmaHandler("insieme", "mark", pragma::tok::eod, [&](const pragma::MatchObject& object, core::NodeList nodes) {
			    LambdaExprPtr expr = dynamic_pointer_cast<const LambdaExpr>(nodes.front());
			    assert_true(expr) << "Insieme mark pragma can only be attached to function declarations!";

			    entryPoints.push_back(expr);

			    return nodes;
			})));

		// feature estimation
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler(
		    "insieme", "iterations", tok::numeric_constant["value"] >> pragma::tok::eod, [](const pragma::MatchObject& object, core::NodeList nodes) {

			    const size_t n = insieme::utils::numeric_cast<size_t>(object.getStrings("value").front());
			    core::NodeAnnotationPtr annot = std::make_shared<ia::LoopAnnotation>(n);
			    nodes.front().addAnnotation(annot);
			    return nodes;
			})));

		pragmaHandlers.push_back(
		    std::make_shared<PragmaHandler>(PragmaHandler("insieme", "fun_unroll", l_paren >> tok::numeric_constant["values"] >> r_paren >> pragma::tok::eod,
		                                                  getTransformLambda(ia::TransformationHint::REC_FUN_UNROLL))));
	}


	core::ProgramPtr InsiemePragmaExtension::applyTransformations(const core::ProgramPtr& program) {
		using namespace insieme::transform;

		insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;

		typedef ia::TransformationHint::ValueVect ValueVect;

		typedef std::shared_ptr<ia::TransformAnnotation> TransformAnnPtr;
		typedef std::shared_ptr<ia::TransformationHint> HintPtr;

		auto&& transformer = [&](const core::NodePtr& cur) {
			if(const TransformAnnPtr& trans = cur->getAnnotation(ia::TransformAnnotation::KEY)) {
				try {
					std::vector<TransformationPtr> tr;
					for_each(trans->getAnnotationList(), [&](const HintPtr& hint) {
						const ValueVect& values = hint->getValues();
						switch(hint->getType()) {
						case ia::TransformationHint::REC_FUN_UNROLL: {
							LOG(INFO) << "Unrolling recursive function according to transformation hint at location: [" << getStartLocation(cur) << "]";

							assert_eq(values.size(), 1) << "Function-Unrolling requires a single integer argument";

							tr.push_back(functions::makeRecFunUnrolling(values.front()));
							break;
						}

						default: LOG(WARNING) << "TransformationHint " << hint->getType() << " not handled.\n";
						}
					});

					TransformationPtr pipeline = makePipeline(tr);
					replacements.insert(std::make_pair(cur, pipeline->apply(cur)));

					// Add more transformations here
				} catch(const InvalidTargetException& e) {
					LOG(WARNING) << "Transformation hint from user at position"
					             << " [" << getStartLocation(cur) << "] could not be applied for the following reasons: \n\t" << e.what();
				}
			}
		};

		// FIXME filter all the for stmt
		core::visitDepthFirstOnce(program, core::makeLambdaVisitor(transformer));

		return static_pointer_cast<const core::ProgramPtr>(
		    core::transform::replaceAll(program->getNodeManager(), program, replacements, core::transform::globalReplacement));
	}

} // extensions
} // frontend
} // insieme

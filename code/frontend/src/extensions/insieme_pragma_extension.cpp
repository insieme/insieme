/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/extensions/frontend_plugin.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/loop_annotations.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

namespace insieme {
namespace frontend {
namespace extensions {

using namespace insieme::frontend::pragma;
using namespace insieme::frontend::pragma::tok;

namespace {
	using namespace stmtutils;
	using namespace insieme::core;

	std::function<StmtWrapper (const pragma::MatchObject&, StmtWrapper)> getDataRangeLambda() {
		return [] (pragma::MatchObject object, StmtWrapper node) {


			// TODO: add UNIT TESTS for all pragmas
			// TODO: test operator<< of MatchObject
			assert(object.getExprs("ranges").size() == (object.getVars("ranges").size() * 2) && "Mismatching number of variables and expressions in insieme datarange pragma handling");

			insieme::annotations::DataRangeAnnotation dataRanges;
			auto exprIt = object.getExprs("ranges").begin();
			for(const auto& e : object.getVars("ranges")) {
				core::VariablePtr var = static_pointer_cast<core::VariablePtr>(e);
				core::ExpressionPtr lowerBound = static_pointer_cast<core::ExpressionPtr>(*exprIt++);
				core::ExpressionPtr upperBound = static_pointer_cast<core::ExpressionPtr>(*exprIt++);
				dataRanges.addRange(insieme::annotations::Range(var, lowerBound, upperBound));
			}

			insieme::core::NodeAnnotationPtr annot = std::make_shared<insieme::annotations::DataRangeAnnotation>((dataRanges));
			node.front().addAnnotation(annot);

			return node;
		};
	}

	std::function<StmtWrapper (const pragma::MatchObject&, StmtWrapper)> getDataTransformLambda() {
		return [] (pragma::MatchObject object, StmtWrapper node) {

			assert(object.getStrings("arg").size() == 1 && "Insieme DataTransform pragma cannot have more than one argument");
			std::string datalayout = object.getStrings("arg").front();

			const unsigned tilesize = insieme::utils::numeric_cast<unsigned>(datalayout.substr(1u, datalayout.size()-2u));

			insieme::annotations::DataTransformAnnotation dataTransform(tilesize);
			insieme::core::NodeAnnotationPtr annot = std::make_shared<insieme::annotations::DataTransformAnnotation>(dataTransform);
			node.front().addAnnotation(annot);
			return node;
		};
	}

	std::function<StmtWrapper (const pragma::MatchObject&, StmtWrapper)> getLoopIterationsLambda() {
		return [] (pragma::MatchObject object, StmtWrapper node) {

			const size_t n = insieme::utils::numeric_cast<size_t>(object.getStrings("value").front());
			insieme::core::NodeAnnotationPtr annot = std::make_shared<insieme::annotations::LoopAnnotation>(n);
			node.front().addAnnotation(annot);
			return node;
		};
	}

	std::function<StmtWrapper (const pragma::MatchObject&, StmtWrapper)> getKernelFileLambda() {
		return [] (pragma::MatchObject object, StmtWrapper node) {

			assert(object.getStrings("arg").size() == 1 && "Insieme KernelFile pragma cannot have more than one argument");
//			std::cout << "################### attaching kernel file: " << object.getStrings("arg").front() << "\n";
			insieme::annotations::ocl::KernelFileAnnotation kernelFile(object.getStrings("arg").front());
			insieme::core::NodeAnnotationPtr annot = std::make_shared<insieme::annotations::ocl::KernelFileAnnotation>(kernelFile);
			auto& callExprPtr = dynamic_pointer_cast<const CallExprPtr>(node.front());
			if(callExprPtr) {
				auto& arguments = callExprPtr->getArguments();
				if(arguments.size() == 2) {
					arguments[1].addAnnotation(annot);
				} else {
					// TODO: assert? debug output?
				}
			} else {
				// TODO: assert? debug output?
			}
			return node;
		};
	}

} // anonymous

	std::function<StmtWrapper (const pragma::MatchObject&, StmtWrapper)> InsiemePragmaExtension::getMarkLambda() {
		return [&] (pragma::MatchObject object, StmtWrapper node) {
			StatementAddress addr = StatementAddress(node.front());

			LambdaExprPtr expr = dynamic_pointer_cast<const LambdaExpr>(node.front());
			assert(expr && "Insieme mark pragma can only be attached to function declarations!");

			entryPoints.push_back(expr);

			return node;
		};
	}

	insieme::frontend::tu::IRTranslationUnit InsiemePragmaExtension::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {

		// if there are no previously marked entry points, there's nothing to be done
		if(entryPoints.size() < 1)
			return tu;

		// get IR for checking whether nodes are still valid
		core::ExpressionPtr singlenode = insieme::frontend::tu::toIR(tu.getNodeManager(), tu);
		IRBuilder builder(singlenode->getNodeManager());
		assert(singlenode && "Conversion of IRTranslationUnit to IR failed!");

		// check if nodes previously marked as entry points are still valid and add them
		for(auto it = entryPoints.begin(); it != entryPoints.end(); ++it) {
			visitBreadthFirst(singlenode, [&](const NodePtr& node){
				ExpressionPtr expr = dynamic_pointer_cast<insieme::core::ExpressionPtr>(node);
				if(!expr)
					return false;
				if(*it != expr)
					return false;

				LambdaExprPtr lambda = dynamic_pointer_cast<insieme::core::LambdaExprPtr>(expr);
				assert(lambda && "Non-LambdaExpression marked as entry point!");

				string cname = insieme::core::annotations::getAttachedName(lambda);
				insieme::core::LiteralPtr lit = builder.literal(lambda->getType(), cname);
				assert(lit && "Could not build literal!");

				tu.addEntryPoints(lit);
				return true;
			});
		}
		entryPoints.clear();

		return tu;
	}

	InsiemePragmaExtension::InsiemePragmaExtension() {
		// some utilities
		auto range              = ~l_paren >> var["var"] >> ~equal >> expr["lb"] >> ~colon >> expr["ub"] >> ~r_paren;
		// range *(, range)
		auto range_list   		= range >> *(~comma >> range);

		// Insieme pragmas for OpenCL
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "mark",
				pragma::tok::eod, getMarkLambda())));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "kernelFile",
				string_literal["arg"] >> pragma::tok::eod, getKernelFileLambda())));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "datarange",
				range_list["ranges"] >> pragma::tok::eod, getDataRangeLambda())));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "transform",
				string_literal["arg"] >> pragma::tok::eod, getDataTransformLambda())));

		// Insieme pragmas for feature estimation
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "iterations",
				tok::numeric_constant["value"] >> pragma::tok::eod, getLoopIterationsLambda())));

		// Insieme pragmas for loop transformations
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "strip",
				l_paren >> (tok::numeric_constant >> ~comma >> tok::numeric_constant)["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "interchange",
				l_paren >> (tok::numeric_constant >> ~comma >> tok::numeric_constant)["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "tile",
				l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "unroll",
				l_paren >> tok::numeric_constant["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "fuse",
				l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "split",
				l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "stamp",
				l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "reschedule",
				l_paren >> tok::numeric_constant["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "parallelize",
				l_paren >> tok::numeric_constant["values"] >>r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "rstrip",
				l_paren >> tok::numeric_constant["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "fun_unroll",
				l_paren >> tok::numeric_constant["values"] >> r_paren >> pragma::tok::eod, nullptr)));

		// Insieme pragma for InfoAnnotations TODO TODO TODO
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(PragmaHandler("insieme", "info",
				kwd("id") >> colon >> tok::numeric_constant["id"] >> l_paren >> (identifier >> *(~comma >> identifier))["values"] >> r_paren >> pragma::tok::eod, nullptr)));
	}

} // extensions
} // frontend
} // insieme


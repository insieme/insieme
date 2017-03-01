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

#include "insieme/frontend/extensions/cilk_extension.h"

#include "insieme/frontend/cilk/cilk_annotation.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/core/tu/ir_translation_unit.h"

namespace insieme {
namespace frontend {
namespace extensions {


	namespace {

		using namespace insieme::core;

		// returns the pragma function lambda which will add the annotation which is passed as a template argument when called
		template <typename T>
		PragmaHandler::pragmaHandlerFunction getMarkerAttachementLambda() {
			return [](const pragma::MatchObject& object, core::NodeList nodes) {
				core::NodeList res;
				for(const NodePtr& element : nodes) {
					StatementPtr stmt = element.as<StatementPtr>();
					StatementPtr tmp;
					IRBuilder builder(stmt->getNodeManager());
					if(element->getNodeCategory() == NC_Statement) {
						tmp = builder.markerStmt(stmt);
					} else if(element->getNodeCategory() == NC_Expression) {
						tmp = builder.markerExpr(stmt.as<ExpressionPtr>());
					} else {
						assert_fail() << "Cannot annotate non statement/expression!";
					}
					tmp->attachValue<T>();
					res.push_back(tmp);
				}
				return res;
			};
		}

	} // end anonymous namespace


	CilkFrontendExtension::CilkFrontendExtension() : flagActivated(false) {
		// Define the macros which will replace the cilk keywords with inline pragmas
		macros.insert(std::make_pair("cilk=", ""));
		macros.insert(std::make_pair("spawn", "_Pragma(\"cilk spawn\")"));
		macros.insert(std::make_pair("cilk_spawn", "_Pragma(\"cilk spawn\")"));
		macros.insert(std::make_pair("sync", "_Pragma(\"cilk sync\")"));
		macros.insert(std::make_pair("cilk_sync", "_Pragma(\"cilk sync\")"));

		// Add pragma handlers which will attach cilk annotations at the correct location
		pragmaHandlers.push_back(
		    std::make_shared<PragmaHandler>(PragmaHandler("cilk", "spawn", pragma::tok::eod, getMarkerAttachementLambda<cilk::CilkSpawnMarker>())));

		pragmaHandlers.push_back(
		    std::make_shared<PragmaHandler>(PragmaHandler("cilk", "sync", pragma::tok::eod, getMarkerAttachementLambda<cilk::CilkSyncMarker>())));
	}

	core::tu::IRTranslationUnit CilkFrontendExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		// We'll let the Cilk sema do the actual work here for every TU
		return cilk::applySema(tu, tu.getNodeManager());
	}

	FrontendExtension::flagHandler CilkFrontendExtension::registerFlag(boost::program_options::options_description& options) {
		// register omp flag
		options.add_options()("fcilk", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "Cilk support");
		// create lambda
		auto lambda = [&](const ConversionJob& job) { return flagActivated; };
		return lambda;
	}


} // end namespace extensions
} // end namespace frontend
} // end namespace insieme

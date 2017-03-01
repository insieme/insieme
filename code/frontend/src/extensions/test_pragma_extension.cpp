/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/frontend/extensions/test_pragma_extension.h"

#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/annotations/loop_annotations.h"
#include "insieme/annotations/transform.h"

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace frontend {
namespace extensions {

	using namespace insieme::frontend::pragma;
	using namespace insieme::frontend::pragma::tok;
	using namespace insieme::annotations;

	#define ARG_LABEL "arg"
	
	TestPragmaExtension::TestPragmaExtension() : expected(""), dummyArguments(std::vector<string>()) {
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
		    PragmaHandler("test", "expect_ir", tok::l_paren >> cpp_string_lit[ARG_LABEL] >> *(~comma >> cpp_string_lit[ARG_LABEL]) >> tok::r_paren >> tok::eod, 
					[&](const pragma::MatchObject& object, core::NodeList nodes) {

			    assert_gt(object.getStrings(ARG_LABEL).size(), 0) << "Test expect_ir pragma expects at least one string argument!";
				assert_gt(nodes.size(), 0) << "Test expect_ir pragma needs to be attached to at least one IR node!";

				auto strings = object.getStrings(ARG_LABEL);
				string expectedString = toString(join("", strings));

				NodePtr node;
			    // if we are dealing with more than one node, construct a compound statement
			    if(nodes.size() > 1) {
				    stmtutils::StmtWrapper wrapper;
				    for(const auto& e : nodes) {
					    wrapper.push_back(e.as<core::StatementPtr>());
				    }
				    IRBuilder builder(nodes[0].getNodeManager());
				    node = stmtutils::aggregateStmts(builder, wrapper);
			    } else {
				    node = nodes[0];
			    }


			    ExpectedIRAnnotation expectedIRAnnotation(expectedString);
			    ExpectedIRAnnotationPtr annot = std::make_shared<ExpectedIRAnnotation>(expectedIRAnnotation);
			    node->addAnnotation(annot);
			    return nodes;
			})));
		
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
		    PragmaHandler("test", "expect_num_vars", tok::l_paren >> tok::numeric_constant[ARG_LABEL] >> tok::r_paren >> tok::eod, 
					[&](const pragma::MatchObject& object, core::NodeList nodes) {
				assert_eq(1, object.getStrings(ARG_LABEL).size()) << "Test expect_num_vars expects a number";
				auto numStr = object.getStrings(ARG_LABEL).back();
				auto num = insieme::utils::numeric_cast<unsigned>(numStr);
				expectNumVarsHandler(object.getConverter(), num);
			    return nodes;
			})));

		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
		    PragmaHandler("test", "dummy", string_literal[ARG_LABEL] >> tok::eod, [&](const pragma::MatchObject& object, core::NodeList nodes) {
			    assert_eq(1, object.getStrings(ARG_LABEL).size()) << "Test dummy pragma expects exactly one string argument!";
			    dummyArguments.push_back(object.getString(ARG_LABEL));
			    return nodes;
			})));
	}

	TestPragmaExtension::TestPragmaExtension(const std::function<void(conversion::Converter&, int)>& expectNumVarsHandler) : TestPragmaExtension() {
		this->expectNumVarsHandler = expectNumVarsHandler;
	}

} // extensions
} // frontend
} // insieme

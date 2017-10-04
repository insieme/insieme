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
 */

#include <gtest/gtest.h>

#include <string>
#include <vector>

#include "insieme/backend/backend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/sequential/sequential_preprocessor.h"
#include "insieme/backend/sequential/sequential_type_handler.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/backend_interception_info.h"
#include "insieme/core/checks/full_check.h"



namespace insieme {
namespace backend {

	TEST(CppSnippet, BackendInterception) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		// a Dummy backup, which also uses the BackendInterceptor preprocessor
		class InterceptingBackend : public Backend {
			virtual Converter buildConverter(core::NodeManager& manager) const {
				Converter converter(manager, "InterceptingBackend", getConfiguration());

				// set up pre-processing
				auto backendInterceptionPreprocessor = makePreProcessor<BackendInterceptor>();
				backendInterceptionPreprocessor->addBackendInterception("IMP_Foo", "whatever/foo.h");

				PreProcessorPtr preprocessor = makePreProcessor<PreProcessingSequence>(
						backendInterceptionPreprocessor,
						makePreProcessor<sequential::Sequentializer>(),
						getBasicPreProcessorSequence());
				converter.setPreProcessor(preprocessor);

				TypeManager& typeManager = converter.getTypeManager();
				typeManager.addTypeHandler(sequential::SequentialTypeHandler);
				return converter;
			}
		};

		auto backend = std::make_shared<InterceptingBackend>();

		// parse the program
		auto program = builder.parseProgram(R"(
			def struct IMP_Foo {
				//const function IMP__operator_assign_ = (other : ref<IMP_Foo,t,f,cpp_ref>) -> unit {}
				function bar = () -> unit {}
			};
			int<4> main() {
				var ref<IMP_Foo> f0;
				f0.bar();
				var ref<IMP_Foo> f1;
				f0.IMP__operator_assign_(ref_cast(f1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
				return 0;
			}
		)");

		ASSERT_TRUE(program);
		ASSERT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// add annotations, like the frontend would have done
		core::visitDepthFirstOnce(program, [](const core::NodePtr& node) {
			if(auto tagType = node.isa<core::TagTypePtr>()) {
				std::vector<std::string> args{ "int", "2" };
				core::annotations::attachBackendInterceptionInfo(tagType, { "dummy::Foo", args });
			}
			else if(auto lambda = node.isa<core::LambdaExprPtr>()) {
				auto lambdaName = lambda->getReference()->getNameAsString();
				if(lambdaName == "IMP_Foo::bar") {
					core::annotations::attachBackendInterceptionInfo(lambda, { "bar" });
				}
			}
		});

		// convert to C++ source
		auto converted = backend->convert(program);
		ASSERT_TRUE((bool)converted);
		auto code = toString(*converted);
//		std::cout << code << std::endl;

		// check for presence of desired code
		EXPECT_PRED2(containsSubString, code, "#include <whatever/foo.h>");
		EXPECT_PRED2(containsSubString, code, "dummy::Foo<int, 2 > f0;");
		EXPECT_PRED2(containsSubString, code, "f0.bar();");
		EXPECT_PRED2(containsSubString, code, "f0 = (dummy::Foo<int, 2 > const&)f1;");
	}

}
}

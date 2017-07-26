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

#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/mapping_frontend_extension.h"


namespace insieme {
namespace frontend {
namespace extensions {

	core::ExpressionPtr singleBarMapper(const detail::ClangExpressionInfo& exprInfo) {
		auto& builder = exprInfo.converter.getIRBuilder();
		auto funType = builder.functionType(builder.getLangBasic().getInt4(), builder.getLangBasic().getUnit());
		return builder.callExpr(builder.literal("singleBar", funType), exprInfo.converter.convertExpr(exprInfo.args[0]));
	}

	struct MultipleBarMapper {
		core::ExpressionPtr operator()(const detail::ClangExpressionInfo& exprInfo) {
			auto& builder = exprInfo.converter.getIRBuilder();
			core::ExpressionList args;
			core::TypeList paramTypes;
			for(const auto& arg : exprInfo.args) {
				args.push_back(exprInfo.converter.convertExpr(arg));
				paramTypes.push_back(builder.getLangBasic().getInt4());
			}
			auto funType = builder.functionType(paramTypes, builder.getLangBasic().getUnit());
			return builder.callExpr(builder.literal("multipleBar", funType), args);
		}
	};

	class DummyMappingExtension : public MappingFrontendExtension {

		std::map<std::string, std::string> getTypeMappings() override {
			return {
				{ "Simple.*", "simple" }, // simple mapping. Note that the pattern can match against the fully qualified name using a regex
				{ ".*Templates", "templates<TEMPLATE_T_1,TEMPLATE_T_0,TEMPLATE_T_3>" }, // extract template arguments
				{ "ns::ToTuple", "('TEMPLATE_T_0...)" }, // create tuple types - here from a template argument type
				{ "ns::ToTupleType", "extracted_tuple<TUPLE_TYPE_0<TUPLE_TYPE_0<('TEMPLATE_T_0...)>>>" }, // extract the element type of an IR tuple type
				{ "EnclosingType<.*>::NestedType", "nested<TUPLE_TYPE_0<('ENCLOSING_TEMPLATE_T_0...)>>" }, // extract template args types from enclosing type

				// type mappings for expr tests below
				{ "Foo", "foo" },
			};
		}

		std::vector<detail::FilterMapper> getExprMappings() override {
			auto makeFoo = [](const detail::ClangExpressionInfo& exprInfo) {
				auto& builder = exprInfo.converter.getIRBuilder();
				auto funType = builder.functionType(core::TypeList(), builder.genericType("foo"));
				return builder.callExpr(builder.literal("makeFoo", funType), core::ExpressionList());
			};

			return {
				{ "Foo::Foo", 0, makeFoo },       // default ctor
				{ "foo.*",  makeFoo },            // matches calls to foo.* - implementation in a lambda
				{ "bar.*", 1, singleBarMapper },  // matches calls to bar.* with one param - implementation in a function
				{ "bar.*", MultipleBarMapper() }, // matches calls to bar.* with pultiple params. The above entry will have priority over this one - implementation is a class
			};
		}
	};

	TEST(MappingTest, Types) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/mapping_types.cpp", [](ConversionJob& job) {
			job.registerFrontendExtension<extensions::DummyMappingExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(MappingTest, Exprs) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/mapping_exprs.cpp", [](ConversionJob& job) {
			job.registerFrontendExtension<extensions::DummyMappingExtension, extensions::TestPragmaExtension>();
		});
	}

} // extensions namespace
} // fe namespace
} // insieme namespace

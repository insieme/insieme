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
#include <gtest/gtest.h>
#include <insieme/backend/runtime/runtime_extension.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/extension.h"

#include "insieme/backend/runtime/runtime_entities.h"

namespace insieme {
namespace backend {
namespace runtime {

namespace enc = insieme::core::encoder;

core::LambdaExprPtr getDummyImpl(core::NodeManager& manager) {
	core::IRBuilder builder(manager);
	const auto& basic = manager.getLangBasic();
	const auto& ext = manager.getLangExtension<RuntimeExtension>();

	core::VariablePtr param = builder.variable(builder.refType(builder.refType(ext.getWorkItemType())), 1);
	core::StatementPtr body = builder.getNoOp();
	return builder.lambdaExpr(basic.getUnit(), toVector(param), body);
}

core::LambdaExprPtr getDummyEffort(core::NodeManager& manager) {
	core::IRBuilder builder(manager);
	const auto& basic = manager.getLangBasic();

	core::VariablePtr a = builder.variable(basic.getInt8(), 1);
	core::VariablePtr b = builder.variable(basic.getInt8(), 2);
	core::StatementPtr body = builder.returnStmt(builder.castExpr(basic.getUInt8(), builder.sub(b, a)));
	return builder.lambdaExpr(basic.getUInt8(), toVector(a, b), body);
}


TEST(RuntimeExtension, WorkItemVariant) {
	core::NodeManager manager;
	core::IRBuilder builder(manager);
	const RuntimeExtension& ext = manager.getLangExtension<RuntimeExtension>();

	// test type
	EXPECT_EQ(ext.getWorkItemVariantType(), enc::getTypeFor<WorkItemVariant>(manager));

	// test encoding
	WorkItemVariant variant(getDummyImpl(manager));
	core::ExpressionPtr encoded = enc::toIR(manager, variant);
	EXPECT_EQ("decl fun000 : (ref<irt_wi,f,f,plain>) -> unit;def fun000 = function (v1 : ref<ref<irt_wi,f,f,plain>,f,f,plain>) -> unit { };WorkItemVariant(fun000)",
	          toString(core::printer::PrettyPrinter(encoded, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE)));

	// test decoding
	WorkItemVariant decoded = enc::toValue<WorkItemVariant>(encoded);
	EXPECT_TRUE(variant == decoded);

	// test is_encoding_of
	EXPECT_TRUE(enc::isEncodingOf<WorkItemVariant>(encoded));
	EXPECT_FALSE(enc::isEncodingOf<WorkItemVariant>(core::ExpressionPtr()));

	// apply IR semantic checks
	EXPECT_EQ("[]", toString(core::checks::check(encoded, core::checks::getFullCheck())));
}


TEST(RuntimeExtension, WorkItemImpl) {
	core::NodeManager manager;
	core::IRBuilder builder(manager);
	const RuntimeExtension& ext = manager.getLangExtension<RuntimeExtension>();

	// test type
	EXPECT_EQ(ext.getWorkItemImplType(), enc::getTypeFor<WorkItemImpl>(manager));

	// test encoding
	WorkItemImpl impl(toVector(WorkItemVariant(getDummyImpl(manager))));
	core::ExpressionPtr encoded = enc::toIR(manager, impl);
	EXPECT_TRUE(encoded);
	EXPECT_EQ("WorkItemImpl([WorkItemVariant(fun000)])",
	          toString(core::printer::PrettyPrinter(encoded, core::printer::PrettyPrinter::NO_LET_BINDINGS)));

	// test decoding
	WorkItemImpl decoded = enc::toValue<WorkItemImpl>(encoded);
	EXPECT_TRUE(impl == decoded);

	// test is_encoding_of
	EXPECT_TRUE(enc::isEncodingOf<WorkItemImpl>(encoded));
	EXPECT_FALSE(enc::isEncodingOf<WorkItemImpl>(core::ExpressionPtr()));

	// apply IR semantic checks
	EXPECT_EQ("[]", toString(core::checks::check(encoded, core::checks::getFullCheck())));
}

TEST(RuntimeExtension, DataItem) {
	core::NodeManager manager;
	core::IRBuilder builder(manager);

	core::TypeList list;
	core::TupleTypePtr tupleType = builder.tupleType(list);

	core::TypePtr lwDataItem = DataItem::toLWDataItemType(tupleType);

	EXPECT_EQ("irt_lwdi<()>", toString(*lwDataItem));
	EXPECT_TRUE(DataItem::isLWDataItemType(lwDataItem));
}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

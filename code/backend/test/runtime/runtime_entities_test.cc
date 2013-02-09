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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/extension.h"

#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/runtime/runtime_entities.h"

namespace insieme {
namespace backend {
namespace runtime {

namespace enc = insieme::core::encoder;

core::LambdaExprPtr getDummyImpl(core::NodeManager& manager) {
	core::IRBuilder builder(manager);
	const auto& basic = manager.getLangBasic();
	const auto& ext = manager.getLangExtension<Extensions>();

	core::VariablePtr param = builder.variable(builder.refType(ext.workItemType), 1);
	core::StatementPtr body = builder.getNoOp();
	return builder.lambdaExpr(basic.getUnit(), body, toVector(param));
}

core::LambdaExprPtr getDummyEffort(core::NodeManager& manager) {
	core::IRBuilder builder(manager);
	const auto& basic = manager.getLangBasic();

	core::VariablePtr a = builder.variable(basic.getInt8(),1);
	core::VariablePtr b = builder.variable(basic.getInt8(),2);
	core::StatementPtr body = builder.returnStmt(builder.castExpr(basic.getUInt8(), builder.sub(b, a)));
	return builder.lambdaExpr(basic.getUInt8(), body, toVector(a,b));
}


TEST(RuntimeExtensions, WorkItemVariant) {

	core::NodeManager manager;
	core::IRBuilder builder(manager);
	const Extensions& ext = manager.getLangExtension<Extensions>();

	// test type
	EXPECT_EQ(ext.workItemVariantType, enc::getTypeFor<WorkItemVariant>(manager));

	// test encoding
	WorkItemVariant variant(getDummyImpl(manager));
	core::ExpressionPtr encoded = enc::toIR(manager, variant);
	EXPECT_EQ("WorkItemVariant(fun(ref<irt_wi> v1) -> unit { }, unknownEffort, WorkItemVariantFeatures(0, 0, -1, -1))",
			toString(core::printer::PrettyPrinter(encoded, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE)));

	// test decoding
	WorkItemVariant decoded = enc::toValue<WorkItemVariant>(encoded);
	EXPECT_TRUE(variant == decoded);

	// test is_encoding_of
	EXPECT_TRUE(enc::isEncodingOf<WorkItemVariant>(encoded));
	EXPECT_FALSE(enc::isEncodingOf<WorkItemVariant>(core::ExpressionPtr()));

	// apply IR semantic checks
	EXPECT_EQ("[]", toString(core::checks::check(encoded, core::checks::getFullCheck())));



	// -- try something with known effort --
	WorkItemVariantFeatures features;
	features.effort = 15;
	features.opencl = 0;
	features.implicitRegionId = -1;
	features.suggestedThreadNum = 8;
	variant = WorkItemVariant(getDummyImpl(manager), getDummyEffort(manager), features);

	// test encoding
	encoded = enc::toIR(manager, variant);
	EXPECT_EQ("WorkItemVariant(fun(ref<irt_wi> v1) -> unit { }, fun(int<8> v1, int<8> v2) -> uint<8> {return CAST<uint<8>>((v2-v1));}, WorkItemVariantFeatures(15, 0, -1, 8))",
			toString(core::printer::PrettyPrinter(encoded, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE)));

	// test decoding
	decoded = enc::toValue<WorkItemVariant>(encoded);
	EXPECT_TRUE(variant == decoded);

	// test is_encoding_of
	EXPECT_TRUE(enc::isEncodingOf<WorkItemVariant>(encoded));
	EXPECT_FALSE(enc::isEncodingOf<WorkItemVariant>(core::ExpressionPtr()));

	// apply IR semantic checks
	EXPECT_EQ("[]", toString(core::checks::check(encoded, core::checks::getFullCheck())));

}



TEST(RuntimeExtensions, WorkItemImpl) {

	core::NodeManager manager;
	core::IRBuilder builder(manager);
	const Extensions& ext = manager.getLangExtension<Extensions>();

	// test type
	EXPECT_EQ(ext.workItemImplType, enc::getTypeFor<WorkItemImpl>(manager));

	// test encoding
	WorkItemImpl impl(toVector(WorkItemVariant(getDummyImpl(manager))));
	core::ExpressionPtr encoded = enc::toIR(manager, impl);
	EXPECT_TRUE(encoded);
	EXPECT_EQ("WorkItemImpl([WorkItemVariant(fun(ref<irt_wi> v1) -> unit { }, unknownEffort, WorkItemVariantFeatures(0, 0, -1, -1))])", toString(core::printer::PrettyPrinter(encoded, core::printer::PrettyPrinter::NO_LET_BINDINGS)));

	// test decoding
	WorkItemImpl decoded = enc::toValue<WorkItemImpl>(encoded);
	EXPECT_TRUE(impl == decoded);

	// test is_encoding_of
	EXPECT_TRUE(enc::isEncodingOf<WorkItemImpl>(encoded));
	EXPECT_FALSE(enc::isEncodingOf<WorkItemImpl>(core::ExpressionPtr()));

	// apply IR semantic checks
	EXPECT_EQ("[]", toString(core::checks::check(encoded, core::checks::getFullCheck())));
}

TEST(RuntimeExtensions, DataItem) {

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


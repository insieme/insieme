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

#include "insieme/core/transform/utils/member_access_literal_updater.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/statements.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

TEST(TransformUtils, MemberAccessLiteralUpdater) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	CompoundStmt::StatementList saStmts;
	// construct a struct variable
	{
		StructType::Entries fields;
		fields.push_back(std::make_pair(builder.identifier("first"), mgr.basic.getInt4()));
		fields.push_back(std::make_pair(builder.identifier("second"), mgr.basic.getReal8()));
		const VariablePtr& structVar = builder.variable(builder.refType(builder.structType(fields)));
		StructExpr::Members init;
		init.push_back(std::make_pair(builder.identifier("first"), builder.intLit(1)));
		init.push_back(std::make_pair(builder.identifier("second"), builder.literal(mgr.basic.getReal8(), "0.0")));
		saStmts.push_back(builder.declarationStmt(structVar, builder.callExpr(structVar->getType(), mgr.basic.getRefVar(), builder.structExpr(init))));
		// CompositeMemberAccess
		saStmts.push_back(builder.callExpr(mgr.basic.getInt8(), mgr.basic.getCompositeMemberAccess(),
				builder.callExpr(builder.structType(fields), mgr.basic.getRefDeref(), structVar),
				mgr.basic.getIdentifierLiteral(builder.identifier("first")), mgr.basic.getTypeLiteral(mgr.basic.getUInt2())));
		// CompositeRefElem
		saStmts.push_back(builder.callExpr(builder.refType(mgr.basic.getReal8()), mgr.basic.getCompositeRefElem(), structVar,
				mgr.basic.getIdentifierLiteral(builder.identifier("second")), mgr.basic.getTypeLiteral(mgr.basic.getReal4())));
		const StatementPtr& structAccess = builder.compoundStmt(saStmts);

		// test for errors
		auto errors = core::check(structAccess, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(4u, errors.size());

		// correct errors
		utils::MemberAccessLiteralUpdater malu(builder);
		NodePtr corrected = malu.mapElement(0, structAccess);

		// test for errors again
		errors = core::check(corrected, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(0u, errors.size());
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});
	}

	saStmts.clear();
	{
		// construct a tuple variable
		const TupleTypePtr& tupleTy = builder.tupleType(toVector(mgr.basic.getInt4(), mgr.basic.getChar()));
		const VariablePtr& tupleVar = builder.variable(builder.refType(tupleTy));
		std::vector<ExpressionPtr> init;
		init.push_back(builder.intLit(1));
		init.push_back(builder.literal(mgr.basic.getChar(), "a"));
		saStmts.push_back(builder.declarationStmt(tupleVar, builder.callExpr(tupleVar->getType(), mgr.basic.getRefVar(), builder.tupleExpr(init))));
		// TupleMemberAcces
		saStmts.push_back(builder.callExpr(mgr.basic.getUInt2(), mgr.basic.getTupleMemberAccess(), builder.callExpr(tupleTy, mgr.basic.getRefDeref(), tupleVar),
				builder.literal(mgr.basic.getUInt8(), "0"), mgr.basic.getTypeLiteral(mgr.basic.getInt4())));
		// TupleRefElem
		saStmts.push_back(builder.callExpr(mgr.basic.getChar(), mgr.basic.getTupleRefElem(), tupleVar, builder.castExpr(mgr.basic.getUInt8(), builder.intLit(1)),
				mgr.basic.getTypeLiteral(mgr.basic.getWChar())));
		const StatementPtr& tupleAccess = builder.compoundStmt(saStmts);

		// test for errors
		auto errors = core::check(tupleAccess, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(2u, errors.size());

		// correct errors
		utils::MemberAccessLiteralUpdater malu(builder);
		NodePtr corrected = malu.mapElement(0, tupleAccess);

		// test for errors again
		errors = core::check(corrected, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(0u, errors.size());
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});
	}
}

}
}
}

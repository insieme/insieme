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
#include "insieme/core/ir_statements.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

using namespace checks;

TEST(TransformUtils, MemberAccessLiteralUpdater) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	const auto& basic = mgr.getLangBasic();

	StatementList saStmts;
	// construct a struct variable
	{
		vector<NamedTypePtr> fields;
		fields.push_back(builder.namedType("first", basic.getInt4()));
		fields.push_back(builder.namedType("second", basic.getReal8()));
		const VariablePtr& structVar = builder.variable(builder.refType(builder.structType(fields)));

		vector<NamedValuePtr> init;
		init.push_back(builder.namedValue("first", builder.intLit(1)));
		init.push_back(builder.namedValue("second", builder.literal(basic.getReal8(), "0.0")));

		saStmts.push_back(builder.declarationStmt(structVar, builder.callExpr(structVar->getType(), basic.getRefVar(), builder.structExpr(init))));

		// CompositeMemberAccess
		saStmts.push_back(builder.callExpr(basic.getInt8(), basic.getCompositeMemberAccess(),
				builder.callExpr(builder.structType(fields), basic.getRefDeref(), structVar),
				builder.getIdentifierLiteral("first"), builder.getTypeLiteral(basic.getUInt2())));
		// CompositeRefElem
		saStmts.push_back(builder.callExpr(builder.refType(basic.getReal8()), basic.getCompositeRefElem(), structVar,
				builder.getIdentifierLiteral("second"), builder.getTypeLiteral(basic.getReal4())));
		const StatementPtr& structAccess = builder.compoundStmt(saStmts);

		// test for errors
		auto errors = check(structAccess, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(3u, errors.size());

		// correct errors
		utils::MemberAccessLiteralUpdater malu(builder);
		NodePtr corrected = malu.mapElement(0, structAccess);

		// test for errors again
		errors = check(corrected, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(0u, errors.size());
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const Message& cur) {
			LOG(INFO) << cur << std::endl;
		});
	}

	saStmts.clear();
	{
		// construct a tuple variable
		const TupleTypePtr& tupleTy = builder.tupleType(toVector(basic.getInt4(), basic.getChar()));
		const VariablePtr& tupleVar = builder.variable(builder.refType(tupleTy));
		std::vector<ExpressionPtr> init;
		init.push_back(builder.intLit(1));
		init.push_back(builder.literal(basic.getChar(), "'a'"));
		saStmts.push_back(builder.declarationStmt(tupleVar, builder.callExpr(tupleVar->getType(), basic.getRefVar(), builder.tupleExpr(init))));
		// TupleMemberAcces
		saStmts.push_back(builder.callExpr(basic.getUInt2(), basic.getTupleMemberAccess(), builder.callExpr(tupleTy, basic.getRefDeref(), tupleVar),
				builder.literal(basic.getUInt8(), "0"), builder.getTypeLiteral(basic.getInt4())));
		// TupleRefElem
		saStmts.push_back(builder.callExpr(basic.getChar(), basic.getTupleRefElem(), tupleVar, builder.castExpr(basic.getUInt8(), builder.intLit(1)),
				builder.getTypeLiteral(basic.getWChar())));
		const StatementPtr& tupleAccess = builder.compoundStmt(saStmts);

		// test for errors
		auto errors = check(tupleAccess, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(3u, errors.size());

		// correct errors
		utils::MemberAccessLiteralUpdater malu(builder);
		NodePtr corrected = malu.mapElement(0, tupleAccess);

		// test for errors again
		errors = check(corrected, insieme::core::checks::getFullCheck()).getAll();
		EXPECT_EQ(0u, errors.size());
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const Message& cur) {
			LOG(INFO) << cur << std::endl;
		});
	}
}

}
}
}

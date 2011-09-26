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
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

TEST(TransformUtils, MemberAccessLiteralUpdater) {
	NodeManager mgr;
	ASTBuilder builder(mgr);

	// construct a struct variable
	CompoundStmt::StatementList saStmts;
	StructType::Entries fields;
	StructExpr::Members init;
	fields.push_back(std::make_pair(builder.identifier("first"), mgr.basic.getInt4()));
	fields.push_back(std::make_pair(builder.identifier("second"), mgr.basic.getReal8()));
	const VariablePtr& structVar = builder.variable(builder.refType(builder.structType(fields)));
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

	std::cout << structAccess << std::endl;

	// test for errors
	auto errors = core::check(structAccess, insieme::core::checks::getFullCheck()).getAll();
	EXPECT_EQ(4u, errors.size());

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

}
}
}

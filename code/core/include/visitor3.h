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

#pragma once

#include <algorithm>
#include <cassert>

#include "annotated_ptr.h"
#include "definitions.h"
#include "expressions.h"
#include "programs.h"
#include "statements.h"
#include "types.h"


template<typename ReturnType = void>
class Visitor3 {

public:

	typedef ReturnType return_type;


	// ------------------ protected visitor methods -----------------------

#define VISIT_BASE(CLASS) \
	virtual ReturnType visit(const CLASS & node) { \
		return ReturnType(); \
	}

#define VISIT_DERIVED(CLASS, PARENT) \
	virtual ReturnType visit(const CLASS & node) { \
		return visit(static_cast<const PARENT &>(node)); \
	}

	VISIT_BASE(Type);

	VISIT_DERIVED(TypeVariable, Type);
	VISIT_DERIVED(FunctionType, Type);
	VISIT_DERIVED(TupleType, Type);

	VISIT_DERIVED(GenericType, Type);
	VISIT_DERIVED(ArrayType, GenericType);
	VISIT_DERIVED(VectorType, GenericType);
	VISIT_DERIVED(RefType, GenericType);
	VISIT_DERIVED(ChannelType, GenericType);

	VISIT_DERIVED(IntType, GenericType);
	VISIT_DERIVED(FloatType, GenericType);
	VISIT_DERIVED(BoolType, GenericType);
	VISIT_DERIVED(UnitType, GenericType);

	VISIT_DERIVED(NamedCompositeType, Type);
	VISIT_DERIVED(StructType, NamedCompositeType);
	VISIT_DERIVED(UnionType, NamedCompositeType);


	VISIT_BASE(Statement);

	VISIT_DERIVED(NoOpStmt, Statement);
	VISIT_DERIVED(BreakStmt, Statement);
	VISIT_DERIVED(ContinueStmt, Statement);
	VISIT_DERIVED(ReturnStmt, Statement);
	VISIT_DERIVED(DeclarationStmt, Statement);
	VISIT_DERIVED(CompoundStmt, Statement);
	VISIT_DERIVED(WhileStmt, Statement);
	VISIT_DERIVED(ForStmt, Statement);
	VISIT_DERIVED(IfStmt, Statement);
	VISIT_DERIVED(SwitchStmt, Statement);

	VISIT_DERIVED(Expression, Statement);
	VISIT_DERIVED(IntLiteral, Expression);
	VISIT_DERIVED(FloatLiteral, Expression);
	VISIT_DERIVED(BoolLiteral, Expression);
	VISIT_DERIVED(VarExpr, Expression);
	VISIT_DERIVED(ParamExpr, VarExpr);
	VISIT_DERIVED(LambdaExpr, Expression);
	VISIT_DERIVED(CallExpr, Expression);
	VISIT_DERIVED(CastExpr, Expression);


	VISIT_BASE(Program);

	VISIT_BASE(Definition);

#undef VISIT_DERIVED
#undef VISIT_BASE

	virtual int f(int s) {
		return 1;
	}

	virtual int f(double b) {
		return 1;
	}

	virtual int f(short s) {
		return 1;
	}

};



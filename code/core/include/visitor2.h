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

template<typename Derived, typename ReturnType = void>
class ProgramVisitor {

#define DISPATCH(CLASS, PTR) \
		static_cast<Derived*>(this)->dispatch ## CLASS(PTR)

#define VISIT(CLASS, PTR) \
		static_cast<Derived*>(this)->visit ## CLASS(PTR)

public:

	typedef ReturnType return_type;

	ReturnType visit(const ProgramPtr& program) {
		assert( program && "Cannot visit NULL program!");
		return DISPATCH(Program, program);
	}

	ReturnType visit(const DefinitionPtr& definition) {
		assert( definition && "Cannot visit NULL definition!");
		return DISPATCH(Definition, definition);
	}

	ReturnType visit(const StmtPtr& statement) {
		assert( statement && "Cannot visit NULL statement!");
		if (const ExprPtr& ptr = dynamic_pointer_cast<const Expression>(statement)) {
			return DISPATCH(Expr, ptr);
		}
		return DISPATCH(Stmt, statement);
	}

	ReturnType visit(const TypePtr& type) {
		assert( type && "Cannot visit NULL type!");
		return DISPATCH(Type, type);
	}

protected:

#define TRY_DISPATCH(PTR, CLASS) \
		if (const CLASS ## Ptr& ptr = dynamic_pointer_cast<const CLASS>(PTR)) \
			return DISPATCH(CLASS, ptr)


	// ---------------- Dispatcher ---------------------------------

	ReturnType dispatchStmt(const StmtPtr& statement) {
		assert ( statement && "Cannot dispatch NULL statement!");

		TRY_DISPATCH(statement, NoOpStmt);

		TRY_DISPATCH(statement, BreakStmt);
		TRY_DISPATCH(statement, ContinueStmt);
		TRY_DISPATCH(statement, ReturnStmt);

		TRY_DISPATCH(statement, DeclarationStmt);
		TRY_DISPATCH(statement, CompoundStmt);

		TRY_DISPATCH(statement, WhileStmt);
		TRY_DISPATCH(statement, ForStmt);
		TRY_DISPATCH(statement, IfStmt);
		TRY_DISPATCH(statement, SwitchStmt);

		assert ( false && "Cannot dispatch unknown statement pointer type." );
		return ReturnType();
	}

	ReturnType dispatchExpr(const ExprPtr& expression) {
		assert ( expression && "Cannot dispatch NULL expression!");

		TRY_DISPATCH(expression, IntLiteral);
		TRY_DISPATCH(expression, FloatLiteral);
		TRY_DISPATCH(expression, BoolLiteral);
		TRY_DISPATCH(expression, VarExpr);
		TRY_DISPATCH(expression, ParamExpr);
		TRY_DISPATCH(expression, LambdaExpr);
		TRY_DISPATCH(expression, CallExpr);
		TRY_DISPATCH(expression, CastExpr);

		assert ( false && "Cannot dispatch unknown expression pointer type." );
		return ReturnType();
	}



	ReturnType dispatchType(const TypePtr& type) {
		assert ( type && "Cannot dispatch NULL type!");

		TRY_DISPATCH(type, TypeVariable);
		TRY_DISPATCH(type, FunctionType);
		TRY_DISPATCH(type, TupleType);
		TRY_DISPATCH(type, GenericType);

		assert ( false && "Cannot dispatch unknown type pointer." );
		return ReturnType();
	}

	ReturnType dispatchGenericType(const GenericTypePtr& type) {
		assert ( type && "Cannot dispatch NULL pointer to type!");

		TRY_DISPATCH(type, ArrayType);
		TRY_DISPATCH(type, VectorType);
		TRY_DISPATCH(type, RefType);
		TRY_DISPATCH(type, ChannelType);

		TRY_DISPATCH(type, IntType);
		TRY_DISPATCH(type, FloatType);
		TRY_DISPATCH(type, BoolType);
		TRY_DISPATCH(type, UnitType);

		// just forward visit generic type
		return VISIT(GenericType, type);
	}

	ReturnType dispatchVarExpr(const VarExprPtr& expression) {
			assert ( expression && "Cannot dispatch NULL pointer!");

			// try only sub-type
			TRY_DISPATCH(expression, ParamExpr);

			// just forward visit generic type
			return VISIT(VarExpr, expression);
		}


#undef TRY_DISPATCH

	/**
	 * The following set of terminal dispatcher form the bridge between the dispatching
	 * and the visiting methods. Each of those terminals is just forwarding the dispatch
	 * request to the corresponding visitor method. Subclasses may overwrite their
	 * behavior to introduce further sub-types (e.g. a filtered set of generic types).
	 */

#define DISPATCH_TERMINAL(CLASS) \
	inline ReturnType dispatch ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot dispatch NULL pointer!"); \
		return VISIT(CLASS, ptr); \
	}

	DISPATCH_TERMINAL(Program);
	DISPATCH_TERMINAL(Definition);

	DISPATCH_TERMINAL(TypeVariable);
	DISPATCH_TERMINAL(FunctionType);
	DISPATCH_TERMINAL(TupleType);

	DISPATCH_TERMINAL(ArrayType);
	DISPATCH_TERMINAL(VectorType);
	DISPATCH_TERMINAL(RefType);
	DISPATCH_TERMINAL(ChannelType);

	DISPATCH_TERMINAL(IntType);
	DISPATCH_TERMINAL(FloatType);
	DISPATCH_TERMINAL(BoolType);
	DISPATCH_TERMINAL(UnitType);

	DISPATCH_TERMINAL(StructType);
	DISPATCH_TERMINAL(UnionType);


	DISPATCH_TERMINAL(NoOpStmt);
	DISPATCH_TERMINAL(BreakStmt);
	DISPATCH_TERMINAL(ContinueStmt);
	DISPATCH_TERMINAL(ReturnStmt);
	DISPATCH_TERMINAL(DeclarationStmt);
	DISPATCH_TERMINAL(CompoundStmt);
	DISPATCH_TERMINAL(WhileStmt);
	DISPATCH_TERMINAL(ForStmt);
	DISPATCH_TERMINAL(IfStmt);
	DISPATCH_TERMINAL(SwitchStmt);

	DISPATCH_TERMINAL(IntLiteral);
	DISPATCH_TERMINAL(FloatLiteral);
	DISPATCH_TERMINAL(BoolLiteral);
	DISPATCH_TERMINAL(ParamExpr);
	DISPATCH_TERMINAL(LambdaExpr);
	DISPATCH_TERMINAL(CallExpr);
	DISPATCH_TERMINAL(CastExpr);

#undef DISPATCH_TERMINAL
#undef VISIT
#undef DISPATCH

	// ------------------ protected visitor methods -----------------------

#define VISIT_NODE(CLASS, PARENT) \
	inline ReturnType visit ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot visit NULL pointer!"); \
		return static_cast<Derived*>(this)->visit ## PARENT(ptr); \
	}

	/**
	 * The base case - all type visits will be default be forwarded
	 * to this method.
	 *
	 * @param ptr the pointer to be visited
	 */
	ReturnType visitType(const TypePtr& ptr) {
		// base case
		return ReturnType();
	}

	VISIT_NODE(TypeVariable, Type);
	VISIT_NODE(FunctionType, Type);
	VISIT_NODE(TupleType, Type);

	VISIT_NODE(GenericType, Type);
	VISIT_NODE(ArrayType, GenericType);
	VISIT_NODE(VectorType, GenericType);
	VISIT_NODE(RefType, GenericType);
	VISIT_NODE(ChannelType, GenericType);

	VISIT_NODE(IntType, GenericType);
	VISIT_NODE(FloatType, GenericType);
	VISIT_NODE(BoolType, GenericType);
	VISIT_NODE(UnitType, GenericType);

	VISIT_NODE(NamedCompositeType, Type);
	VISIT_NODE(StructType, NamedCompositeType);
	VISIT_NODE(UnionType, NamedCompositeType);


	ReturnType visitStmt(const StmtPtr& ptr) {
		return ReturnType();
	}

	VISIT_NODE(NoOpStmt, Stmt);
	VISIT_NODE(BreakStmt, Stmt);
	VISIT_NODE(ContinueStmt, Stmt);
	VISIT_NODE(ReturnStmt, Stmt);
	VISIT_NODE(DeclarationStmt, Stmt);
	VISIT_NODE(CompoundStmt, Stmt);
	VISIT_NODE(WhileStmt, Stmt);
	VISIT_NODE(ForStmt, Stmt);
	VISIT_NODE(IfStmt, Stmt);
	VISIT_NODE(SwitchStmt, Stmt);

	VISIT_NODE(Expr, Stmt);
	VISIT_NODE(IntLiteral, Expr);
	VISIT_NODE(FloatLiteral, Expr);
	VISIT_NODE(BoolLiteral, Expr);
	VISIT_NODE(VarExpr, Expr);
	VISIT_NODE(ParamExpr, VarExpr);
	VISIT_NODE(LambdaExpr, Expr);
	VISIT_NODE(CallExpr, Expr);
	VISIT_NODE(CastExpr, Expr);


	ReturnType visitProgram(const ProgramPtr& program) {
		// by default, do nothing
		return ReturnType();
	}

	ReturnType visitDefinition(const DefinitionPtr& definition) {
		// by default, do nothing
		return ReturnType();
	}

#undef VISIT_NODE
};



/**
 * A visitor which by default is descending into all types.
 */
template<typename ReturnType>
class RecursiveProgramVisitor : public ProgramVisitor<RecursiveProgramVisitor<ReturnType>, ReturnType> {


public:

	ReturnType visitProgram(const ProgramPtr& program) {

	}


};




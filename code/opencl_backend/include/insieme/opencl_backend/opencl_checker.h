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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/statements.h"
#include "insieme/core/program.h"
#include "insieme/core/types.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"

#include "insieme/simple_backend/code_management.h"

namespace insieme {
namespace opencl_backend {

using namespace core;

/** Converts simple IR types to their corresponding C(++) representations.
 ** Examples of "simple" types are integers, booleans, reals and strings.
 ** */
class InternalOpenCLChecker : public ASTVisitor<bool> {
	int divergence_level;
	int pointer_level;

public:
	InternalOpenCLChecker() : divergence_level(0), pointer_level(0) { }

	// ===================================================================================== Checking types
	//TODO: Check in how deep we are pointing (array<array<...>> is disallowed!)
	bool visitRefType(const RefTypePtr& ptr) {
		//TODO ref type
		return true;
	}

	bool visitGenericType(const GenericTypePtr& ptr) {
		//TODO generic type (i.e. int/string/vector/... nearly all)
		return true;
	}

	bool visitStructType(const StructTypePtr& ptr) {
		//TODO struct type
		return true;
	}



	// ===================================================================================== Checking program
	//Must not gather a program
	bool visitProgram(const ProgramPtr&) {
		assert(0 && "ConvertVisitor should never encounter program node");
		return false;
	}


	// ===================================================================================== Checking easy statements
	//All breaks are OK.
	bool visitBreakStmt(const BreakStmtPtr&) {
		return true;
	}

	//Continue are OK
	bool visitContinueStmt(const ContinueStmtPtr&) {
		return true;
	}

	//All ; are OK. But check the Sub-Statements
	bool visitCompoundStmt(const CompoundStmtPtr& ptr) {
		bool res = true;
		for_each(ptr->getChildList(), [this, &res](const NodePtr& cur) {
			res &= this->visit(cur);
		});
		return res;
	}

	//For is OK as long as all threads have the same length
	//TODO: check wether all threads have same length
	bool visitForStmt(const ForStmtPtr& ptr) {
		return visit(ptr->getBody());
	}

	bool visitReturnStmt(const ReturnStmtPtr& ptr) {
		return visit(ptr->getReturnExpr());
	}


	// ===================================================================================== Checking easy expressions
	//Literals cannot be bad - hopefully
	//They are already of correct type et al
	bool visitLiteral(const LiteralPtr& ptr) {
		return true;
	}

	bool visitVariable(const VariablePtr& ptr) {
		//Check type
		if (!this->visit(ptr->getType())) return false;
		bool res = true;
		for_each(ptr->getChildList(), [this, &res](const NodePtr& cur) {
			res &= this->visit(cur);
		});
		return res;
	}

	bool visitTupleExpr(const TupleExprPtr& ptr) {
		//Backend optimizer should've killed all tuple expressions!
		return false;
	}


	// ===================================================================================== Checking divergence
	//if statements add to divergence
	bool visitIfStmt(const IfStmtPtr& ptr) {
		if (!visit(ptr->getCondition())) return false;
		divergence_level++;
		//TODO: check wether else or thenblock is NOP
		bool res = (visit(ptr->getThenBody())) && (visit(ptr->getElseBody()));
		divergence_level--;
		return res;
	}

	//switch statements add to divergence
	bool visitSwitchStmt(const SwitchStmtPtr& ptr) {
		if (!visit(ptr->getSwitchExpr())) return false;
		divergence_level++;
		bool res = true;
		for_each(ptr->getCases(), [this, &res](const SwitchStmt::Case& curCase) { // GCC sucks, yes indeed :)
			//this->visit(curCase.first); "first:second"
			res &= this->visit(curCase.second);
		});
		divergence_level--;
		return res;
	}

	//While statements do not harm in general,
	//but they can add to divergence level!
	bool visitWhileStmt(const WhileStmtPtr& ptr) {
		if (!visit(ptr->getCondition())) return false;
		divergence_level++;
		bool res = visit(ptr->getBody());
		divergence_level--;
		return res;
	}

	bool visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
		auto var = ptr->getVariable();
		if (!this->visit(var)) return false;
		//TODO: Check variable name is not a C99/OpenCL keyword et al
		return this->visit(ptr->getInitialization());
	}


	// ===================================================================================== Checking expressions
	// operator or funcall
	bool visitCallExpr(const CallExprPtr& ptr) {
		const std::vector<ExpressionPtr>& args = ptr->getArguments();
		auto funExp = ptr->getFunctionExpr();

		//Operators shall be always possible

		//TODO: "?:" is diverging as well
		if(funExp->getAnnotation(c_info::COpAnnotation::KEY)) {
			//TODO: Ommit expressions?
			return this->visit(args.front()) && this->visit(args.back());
		}


		//ref/deref and subscript already checked in type checking
		//TODO: recheck that!
		//TODO: Peter wants to do it more generic! Recheck with latest backend_convert
		if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {
	
			auto funName = literalFun->getValue();
			if(funName == "ref.deref") {
				return true; //unneccesary: visit(ptr->getArguments().front());

			} else if(funName == "subscript") {
				//front[back()]

				//visit(ptr->getArguments().front());
				//TODO: Ommit expressions?
				return this->visit(ptr->getArguments().back());
			}

			//TODO: check for built-in barriers and divergence level!
		}

		//TODO: check wether it is an external function (disallowed)
		
		// non built-in handling
		//res &= this->visit(funExp); //funname
		return all(args, [&](const ExpressionPtr& curArg) { return this->visit(curArg); });
	}

	//TODO: check for vector types
	bool visitCastExpr(const CastExprPtr& ptr) {
		if (!this->visit(ptr->getType())) return false;
		return visit(ptr->getSubExpression());
	}


	bool visitLambdaExpr(const LambdaExprPtr& ptr) {
		//TODO: Lambda check
		return true;
	}

	bool visitStructExpr(const StructExprPtr& ptr) {
		//TODO: struct expression
		return true;
	}

	bool visitUnionExpr(const UnionExprPtr& ptr) {
		//union expression
		return true;
	}


	// ===================================================================================== Checking parallels
	//Jobs: Only default job allowed for OpenCL, otherwise OpenCL optimizer needs to convert!
	bool visitJobExpr(const JobExprPtr& ptr) {
		//Only default job allowed!
		if (!ptr->getGuardedStmts().empty()) return false;
		return this->visit(ptr->getDefaultStmt());
	}

};


/** Checks all the program entry points for OpenCL compatibilies
 ** */
class OpenCLChecker {
public:
	OpenCLChecker()  { }
	
	bool check(const core::ProgramPtr& prog) {
		bool res = true;
		for_each(prog->getEntryPoints(), [&res, this](const ExpressionPtr& ep) {
			insieme::opencl_backend::InternalOpenCLChecker ioclc;
			res &= ioclc.visit(ep);
		});
		return res;
	}
};

}
}

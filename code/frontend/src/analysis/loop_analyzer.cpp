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

#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/convert.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/annotations/c/include.h"
#include "insieme/frontend/utils/castTool.h"

#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/ir_cast.h"

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtVisitor.h>

using namespace clang;

namespace {

using namespace insieme;

struct InitializationCollector : public core::IRVisitor<bool>{
	
	const core::ExpressionPtr& inductionExpr;
	core::ExpressionPtr init;
	core::StatementList leftoverStmts;
	bool isDecl;

	InitializationCollector(const core::ExpressionPtr& ind)
		: IRVisitor<bool> (false), inductionExpr(ind), isDecl(false)
	{
		assert(inductionExpr->getType().isa<core::RefTypePtr>() && "looking for an initialization, has to be writable");

		std::cout << "We look for: " << inductionExpr << std::endl;
		
	}

	bool visitStatement(const core::StatementPtr& stmt){
		std::cout << " ==== STMT === " << stmt << std::endl;

		if (stmt.isa<core::DeclarationStmtPtr>()) return this->visitDeclarationStmt(stmt.as<core::DeclarationStmtPtr>());
		auto& mgr(stmt->getNodeManager());

//		if (core::analysis::isCallOf(stmt, mgr.getLangBasic().getRefDeref())){
//			std::cout << "STUPID" << std::endl;
//			init = stmt.as<core::ExpressionPtr>();
//			return true;
//		}

		if (core::analysis::isCallOf(stmt, mgr.getLangBasic().getRefAssign())){
			core::ExpressionPtr left  = stmt.as<core::CallExprPtr>()[0].as<core::ExpressionPtr>();
			core::ExpressionPtr right = stmt.as<core::CallExprPtr>()[1].as<core::ExpressionPtr>();
			if (left == inductionExpr) init = right;
			else {
				leftoverStmts.push_back(stmt);
				std::cout << "    assign!! " << stmt <<std::endl;
			}
			return true;
		}
		return false;
	}

	bool visitDeclarationStmt(const core::DeclarationStmtPtr& decl){
		std::cout << " ==== DECL === " << std::endl;
		if (core::VariablePtr var = inductionExpr.isa<core::VariablePtr>()){
				// the initialization must be wrapped into a refvar or something, so we get pure value
			if (decl->getVariable() == var){
				init = decl->getInitialization().as<core::CallExprPtr>()[0];
				isDecl = true;
				std::cout << "    ok:  " << decl <<std::endl;
				return true;
			}
		}
		leftoverStmts.push_back(decl);
		std::cout << "    decl!! " << decl <<std::endl;
		return true;
	}
};

core::ExpressionPtr insertCeilFunc(const core::ExpressionPtr& expr, const core::TypePtr& type){
	core::IRBuilder builder( expr->getNodeManager() );
	auto ceilType = builder.functionType(expr->getType(), type);
	auto ceilLit = builder.literal("ceil", ceilType);
	insieme::annotations::c::attachInclude(ceilLit, "math.h");
	return builder.callExpr (type, ceilLit, expr);
}
}

namespace insieme {
namespace frontend {
namespace analysis {


LoopAnalyzer::LoopAnalyzer(const clang::ForStmt* forStmt, Converter& convFact): 
	convFact(convFact),
	loopToBounduary (false),
	restoreValue (false){
	
	if ( !forStmt->getInc() )   throw LoopNormalizationError(" no increment expression in loop");
	if ( !forStmt->getCond() )  throw LoopNormalizationError(" no condition expression in loop");

	std::cout << " ========================================================== " << std::endl;
	forStmt->dump();
	std::cout << " ========================================================== " << std::endl;

	// we look for the induction variable
	findInductionVariable(forStmt);
	// we know the induction variable, we analyze the increment expression
	handleIncrExpr(forStmt);
	// we look for the condition expression
	handleCondExpr(forStmt);

	// we finish the thing: build normalized induction expression and compute number of iterations

	std::cout << "original expr: " <<  originalInductionExpr << std::endl;  // New read only induction var
	std::cout << "increment: " << incrExpr << std::endl;	// normalized increment ( +1 )
	std::cout << "step: " << stepExpr << std::endl; 	// step of each iteration
	std::cout << "init: " << initValue<< std::endl;
	std::cout << "end: " << endValue << std::endl;
	
	// FIXME: formulas are incomplete, but this is the best option:
	// 	 - no support to compute with expressions
	// 	 - need to ceil a division, 
	// 	 - can i as when a resoutl is a rational or has being simplified??
//	try{
//
//		/////////////////////////////////////////////////////////////
//		//    #iterations = (up-low)/step  (+1 if EQ)
//
//		core::arithmetic::Formula low = core::arithmetic::toFormula(initValue);
//		core::arithmetic::Formula up = core::arithmetic::toFormula(endValue);
//		core::arithmetic::Formula range = (up - low);
//		core::arithmetic::Formula step = core::arithmetic::toFormula(stepExpr);
//		core::arithmetic::Formula n =  range / step.getTerms()[0];
//		
//			// if n is a rational, we need to add ceil function
//		if (loopToBounduary) n = n +1;
//
//		// WRONG
//		normalizedIterations = insertCeilFunc( core::arithmetic::toIR(convFact.getNodeManager(),n), originalInductionExpr->getType());
//
//		/////////////////////////////////////////////////////////////
//		//    iteration = (it* step)+init
//		
//		core::arithmetic::Formula v   = core::arithmetic::Formula(inductionVar);
//		core::arithmetic::Formula it  = (v * step) + low;
//		normalizedInductionExpr = core::arithmetic::toIR(convFact.getNodeManager(),it);
//
//		////////////////////////////////////////////////////////////
//		//   final value of the iteration variable: if not declared in the loop
//		//	( #iterations * step) + init
//		
//		if(restoreValue){
//			core::arithmetic::Formula postValue = (n * step) + low;
//			core::StatementPtr assign = convFact.getIRBuilder().assign(originalInductionExpr.as<core::CallExprPtr>()[0], 
//																	   core::arithmetic::toIR(convFact.getNodeManager(), postValue));
//			postStmts.push_back(assign);
//		}
//
//	}catch(const std::exception& error){
//		// it seems that we can not normalize the thing... just write the expression OLD SCHOOL!!!
// }

	// if the induction variable is a pointer we have to trick the bonduaries
	//LITERAL(ArrayRefDistance, 		"array.ref.distance", 	"(ref<array<'elem,1>>, ref<array<'elem,1>>) -> uint<8>")
	//

	const core::IRBuilder& builder = convFact.getIRBuilder();
	if (frontend::utils::isRefArray(inductionVar->getType())){
		// build the thing for pointers
		inductionVar =  convFact.getIRBuilder().variable(builder.getLangBasic().getUInt8());
		core::ExpressionPtr distance = builder.callExpr (builder.getLangBasic().getUInt8(), builder.getLangBasic().getArrayRefDistance(), endValue, initValue);
		normalizedIterations = insertCeilFunc(builder.div(frontend::utils::castScalar(builder.getLangBasic().getReal8(),distance),
														  frontend::utils::castScalar(builder.getLangBasic().getReal8(), stepExpr)), builder.getLangBasic().getUInt8());
		if (loopToBounduary) normalizedIterations = builder.add(normalizedIterations, builder.literal("1", builder.getLangBasic().getUInt8()));

		normalizedInductionExpr =  builder.callExpr(builder.getLangBasic().getArrayView(), initValue,
														  frontend::utils::castScalar(builder.getLangBasic().getInt8(), builder.mul(stepExpr, inductionVar) ));
		if(restoreValue){
			core::StatementPtr assign = builder.assign (originalInductionExpr.as<core::CallExprPtr>()[0], 
		 												builder.callExpr(builder.getLangBasic().getArrayView(), 
														initValue,
														frontend::utils::castScalar(builder.getLangBasic().getInt8(), normalizedIterations)));
			postStmts.push_back(assign);
		}
	}
	else{ // scalar iterated loop
		core::TypePtr itTy = inductionVar->getType();
		normalizedIterations = builder.sub(frontend::utils::castScalar(itTy, endValue), frontend::utils::castScalar(itTy, initValue));
		normalizedIterations = insertCeilFunc(builder.div(frontend::utils::castScalar(builder.getLangBasic().getReal8(),normalizedIterations),
														  frontend::utils::castScalar(builder.getLangBasic().getReal8(), stepExpr)), itTy);
		if (loopToBounduary) normalizedIterations = builder.add(normalizedIterations, builder.literal("1", itTy));

		normalizedInductionExpr = builder.add(builder.mul(frontend::utils::castScalar(itTy,inductionVar), 
														  frontend::utils::castScalar(itTy,stepExpr)), 
														  frontend::utils::castScalar(itTy,initValue));
		if(restoreValue){
			core::StatementPtr assign = builder.assign(originalInductionExpr.as<core::CallExprPtr>()[0], 
											builder.add(builder.mul(normalizedIterations, frontend::utils::castScalar(itTy, stepExpr)), initValue));
			postStmts.push_back(assign);
		}
	}

	std::cout << "num iterations: " << normalizedIterations << std::endl; // Expression to use as iterator (normalized)
	std::cout << "induction expr: " << normalizedInductionExpr << std::endl; // Expression to use as iterator (normalized)
	std::cout << "post stmt: " << postStmts << std::endl; // post stmts
	std::cout << "=============================================" << std::endl;
}

// to identify the induction variable, we cross the expressions in the increment with the expressions in the condition
// if there is a single expression, that is our induction expression
// 		is an expression because it can be a variable or a member access
//
void LoopAnalyzer::findInductionVariable(const clang::ForStmt* forStmt) {

	// convert to IR everithing in condition and increment
	core::ExpressionPtr condition = convFact.convertExpr (forStmt->getCond());
	core::ExpressionPtr incrementExpr;

	// we start looking in the increment expression, if there is no increment we can not generate a for loop
	const clang::BinaryOperator* binOp = llvm::dyn_cast<clang::BinaryOperator>( forStmt->getInc());
	const clang::UnaryOperator* unOp = llvm::dyn_cast<clang::UnaryOperator>( forStmt->getInc());
	if (binOp){
		if(binOp->getOpcode() == clang::BO_Comma ) throw LoopNormalizationError("more than one increment expression in loop"); // TODO: we cold support this
		if(binOp->getOpcode() != clang::BO_AddAssign && binOp->getOpcode() != clang::BO_SubAssign)
												   throw LoopNormalizationError("operation not supported for increment expression");
		// left side is our variable
		incrementExpr = convFact.convertExpr(binOp->getLHS());
	}
	else if(unOp) {
		// this has to be the one
		incrementExpr = convFact.convertExpr(unOp->getSubExpr());
	}
	else{
		throw LoopNormalizationError("malformed increment expression for for loop");
	}

	// we cross this expression with the ones evaluated in the condition
	// since the increment modifies the value of the induction var, it should be a ref, in the condition we check
	// the value, so it has to be deref
	const clang::Expr* cond = forStmt->getCond();
	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(cond) ) {
		core::ExpressionPtr left  = convFact.convertExpr(binOp->getLHS());
		core::ExpressionPtr right = convFact.convertExpr(binOp->getRHS());
		core::ExpressionPtr value = convFact.getIRBuilder().deref(incrementExpr);
			
		bool isRight = false;
		bool isLeft = false;
		core::visitDepthFirstOnce(left,  [&isLeft, &value] (const core::ExpressionPtr& expr){ if (expr == value) isLeft = true; });
		core::visitDepthFirstOnce(right, [&isRight,&value] (const core::ExpressionPtr& expr){ if (expr == value) isRight = true; });

		if (isLeft){
			// left is the induction expression, right is the up bounduary
			originalInductionExpr = left;
			endValue = right;
		}
		else{
			if (!isRight) throw LoopNormalizationError("induction variable could not be identified");
			// right is the induction expression, left is the up bounduary
			originalInductionExpr = right;
			endValue = left;
		}

		// strip possible casts
		if (core::CallExprPtr call = originalInductionExpr.isa<core::CallExprPtr>()){
			if (convFact.getIRBuilder().getLangBasic().isScalarCast (call.getFunctionExpr())){
				originalInductionExpr = call[0]; 
			}
		}

		inductionVar =  convFact.getIRBuilder().variable(originalInductionExpr->getType());
	}
	else throw LoopNormalizationError("Not supported condition");

	std::cout << "induction: " << originalInductionExpr <<  ": " << originalInductionExpr->getType() << std::endl;
	std::cout << "endValue: " << endValue <<  ": " << endValue->getType() << std::endl;
	std::cout << "induction var: " << inductionVar <<  ": " << inductionVar->getType() << std::endl;

	// now that we know the induction expression and we created a new var, we have to identify the lower bound
	const clang::Stmt* initStmt = forStmt->getInit();

	if (!initStmt){  // there is no init statement, therefore the initial value is the value of the induction expression at the begining of the loop
		initValue = originalInductionExpr;
	}
	else{
		// could be a declaration or could be an assigment
		core::StatementPtr initIR = convFact.convertStmt(initStmt);
		std::cout << initIR << std::endl;
		InitializationCollector collector(incrementExpr);
		visitDepthFirstPrunable(initIR, collector);
		initValue = collector.init;
		preStmts	 = collector.leftoverStmts;
		restoreValue = !collector.isDecl;
		std::cout << "PRE STMTs: " << preStmts << std::endl;
		if (!initValue) {
			// if we could not find any suitable init, the initialization is the value of the original variable at the begining of the loop
			initValue = originalInductionExpr;
		}
	}
	std::cout << "start val: " << initValue << std::endl;
}

void LoopAnalyzer::handleIncrExpr(const clang::ForStmt* forStmt) {
	assert(inductionVar && "Loop induction variable not found, impossible to handle increment expression.");

	// a normalized loop always steps +1
	// special case for arrays, since we iterate with an scalar, we generate a ponter wide iteration var (UINT 8)
	if (!frontend::utils::isRefArray(inductionVar->getType())) incrExpr = convFact.getIRBuilder().literal("1", originalInductionExpr->getType());
	else incrExpr = convFact.getIRBuilder().literal("1", convFact.getIRBuilder().getLangBasic().getUInt8());

	// but what is the real step??
	if( const UnaryOperator* unOp = dyn_cast<const UnaryOperator>(forStmt->getInc()) ) {
		switch(unOp->getOpcode()) {
		case UO_PreInc:
		case UO_PostInc:
			stepExpr =  incrExpr;
			direction = LOOP_UP;
			return;
		case UO_PreDec:
		case UO_PostDec:
			stepExpr = convFact.getIRBuilder().invertSign( incrExpr );
			direction = LOOP_DOWN;
			return;
		default:
			LoopNormalizationError("UnaryOperator different from post/pre inc/dec (++/--) not supported in loop increment expression");
		}
	}

	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(forStmt->getInc()) ) {
		auto tmpStep = convFact.convertExpr(binOp->getRHS ());
		switch(binOp->getOpcode()) {
		case BO_AddAssign:
			stepExpr = tmpStep;
			return;
		case BO_SubAssign: 
			stepExpr = convFact.getIRBuilder().invertSign( tmpStep);
			return;
		default:
			LoopNormalizationError("unable to produce a for loop with " +binOp->getOpcodeStr().str()+"condition");
		}
	}
	throw LoopNormalizationError("unable to use iteration variable increment in for loop");
}


void LoopAnalyzer::handleCondExpr(const clang::ForStmt* forStmt) {
	const clang::Expr* cond = forStmt->getCond();
	
	// if no condition, not FOR
	if (!cond)
		throw LoopNormalizationError("no condition -> no loop");

	// we know the up bonduary from the induction expression lookup, now we just need to determine whenever to stop at bounduary or before
	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(cond) ) {
		switch(binOp->getOpcode()) {
		case BO_LT:
		case BO_GT:
		case BO_NE:
			loopToBounduary = false;
			break;
		case BO_GE:
		case BO_LE:
		case BO_EQ:
			loopToBounduary = true;
			break;

		default:
			throw LoopNormalizationError("BinOp ("+binOp->getOpcodeStr().str()+") in ConditionExpression not supported");
		}

		// collect all variables in conditions to later check if modified
		core::VariableList vars;
		auto cond = convFact.convertExpr(binOp);
		visitDepthFirst(cond, [this, &vars] (const core::VariablePtr& var){ vars.push_back(var);});
		conditionVars = vars;
	
		return;
	}
	throw LoopNormalizationError("unable to identify the upper bonduary for this loop");
}

insieme::core::StatementPtr  LoopAnalyzer::getLoop(const insieme::core::StatementPtr& body) const{
	auto& mgr(body->getNodeManager());

	std::cout << "OLD BODY: " << std::endl;
	dumpPretty ( body);

	// if any of condition variables is not read only, we can not waranty the condition of the loop
	for(auto c : conditionVars ) {
		if(!core::analysis::isReadOnly(body, c))  throw analysis::LoopNormalizationError("Variable in condition expr is not readOnly");
	}

	// if the iteration variable is modified during loop body, we can not waranty for loop number of iterations (not static bounds)
	core::VariableList vars;
	visitDepthFirst(originalInductionExpr, [this, &vars] (const core::VariablePtr& var){ vars.push_back(var);});
	for(auto c : vars ) {
		if(!core::analysis::isReadOnly(body, c))  throw analysis::LoopNormalizationError("Induction variable is not preserved during loop");
	}

	// substite the induction expression by the induction var 
	core::StatementPtr newBody = core::transform::replaceAllGen(mgr, body, originalInductionExpr, normalizedInductionExpr, true);

	// allrighty... green light, append extra code that might be needed and we are done
	// TODO: reproduce first and last stmts
	
	std::cout << "New BODY: " << std::endl;
	dumpPretty ( body);
	std::cout << "==========================" << std::endl;
	
	core::ExpressionPtr zero = convFact.getIRBuilder().literal("0", inductionVar->getType());
	return convFact.getIRBuilder().forStmt(inductionVar, zero, normalizedIterations, incrExpr, newBody);
}

} // End analysis namespace
} // End froentend namespace
} // End insieme namespace

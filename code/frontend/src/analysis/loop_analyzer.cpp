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

struct InitializationCollector : public core::IRVisitor<bool,core::Address>{
	
	const core::ExpressionPtr& inductionExpr;
	core::ExpressionPtr init;
	core::StatementList leftoverStmts;
	bool isDecl;

	InitializationCollector(const core::ExpressionPtr& ind)
		: core::IRVisitor<bool,core::Address> (), inductionExpr(ind), isDecl(false)
	{
		assert(inductionExpr->getType().isa<core::RefTypePtr>() && "looking for an initialization, has to be writable");
	}

	bool visitStatement(const core::StatementAddress& stmt){

		auto& mgr(stmt->getNodeManager());

		if (stmt.isa<core::CallExprAddress>()){
			if (core::analysis::isCallOf(stmt.as<core::CallExprPtr>(), mgr.getLangBasic().getRefAssign())){
					// if there is coma (,) operator, we will find the assigments enclosed into a labmda, we need to translate the 
					// variable names
				core::ExpressionPtr left = stmt.as<core::CallExprPtr>()[0].as<core::ExpressionPtr>();
				if (stmt.as<core::CallExprPtr>()[0].isa<core::VariablePtr>()){
					core::VariableAddress var  = stmt.as<core::CallExprAddress>()[0].as<core::VariableAddress>();
					utils::map::PointerMap<core::VariableAddress, core::VariableAddress> paramName = 
						core::analysis::getRenamedVariableMap(toVector(var));
					left = paramName[var];
				}
				core::ExpressionPtr right = stmt.as<core::CallExprPtr>()[1].as<core::ExpressionPtr>();

				if (left == inductionExpr){
					init = right;
				}
				else {
					core::IRBuilder builder( stmt->getNodeManager() );
					leftoverStmts.push_back( builder.assign (left, right));
				}
				return true;
			}
//utils::map::PointerMap<VariableAddress, VariableAddress> getRenamedVariableMap(const std::vector<VariableAddress> varlist);
		}
		return false;
	}

	bool visitDeclarationStmt(const core::DeclarationStmtAddress& declAdr){
		core::DeclarationStmtPtr decl = declAdr.as<core::DeclarationStmtPtr>();
		if (core::VariablePtr var = inductionExpr.isa<core::VariablePtr>()){
				// the initialization must be wrapped into a refvar or something, so we get pure value
			if (decl->getVariable() == var){
				init = decl->getInitialization().as<core::CallExprPtr>()[0];
				isDecl = true;
				return true;
			}
		}
		leftoverStmts.push_back(decl);
		return true;
	}
};

core::ExpressionPtr absFunc(const core::ExpressionPtr& expr, const core::TypePtr& type){
	core::IRBuilder builder( expr->getNodeManager() );
	auto ceilType = builder.functionType(expr->getType(), type);
	auto ceilLit = builder.literal("abs", ceilType);
	insieme::annotations::c::attachInclude(ceilLit, "stdlib.h");
	return builder.callExpr (type, ceilLit, expr);
}

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

	// we look for the induction variable
	findInductionVariable(forStmt);
	// we know the induction variable, we analyze the increment expression
	handleIncrExpr(forStmt);
	// we look for the condition expression
	handleCondExpr(forStmt);

	// we finish the thing: build normalized induction expression and compute number of iterations

	//   FIXME: formulas do not work nice whith the paratesis, if all variables, the computation is truncated in several places
	//try{

	//	/////////////////////////////////////////////////////////////
	//	//    #iterations =  (up-low-1)/step   if <  N
	//	//    #iterations =  (up-low)/step     if <= N

	//	core::arithmetic::Formula low = core::arithmetic::toFormula(initValue);
	//	core::arithmetic::Formula up = core::arithmetic::toFormula(endValue);
	//	core::arithmetic::Formula range = (up - low);
	//	core::arithmetic::Formula step = core::arithmetic::toFormula(stepExpr);
	//	core::arithmetic::Formula n = range;
	//	if (!loopToBounduary)  n = (n + -1);
	//	n =  (n / step.getTerms()[0]) +1;

	//	normalizedIterations = core::arithmetic::toIR(convFact.getNodeManager(), n);
	//	normalizedIterations = frontend::utils::castScalar(inductionVar->getType(), normalizedIterations);
	//	/////////////////////////////////////////////////////////////
	//	//    iteration = (it* step)+init
	//	
	//	core::arithmetic::Formula v   = core::arithmetic::Formula(inductionVar);
	//	core::arithmetic::Formula it  = (v * step) + low;
	//	normalizedInductionExpr = core::arithmetic::toIR(convFact.getNodeManager(),it);

	//	////////////////////////////////////////////////////////////
	//	//   final value of the iteration variable: if not declared in the loop
	//	//	( #iterations * step) + init
	//	
	//	if(restoreValue){
	//		core::arithmetic::Formula postValue = (n * step) + low;
	//		core::StatementPtr assign = convFact.getIRBuilder().assign(originalInductionExpr.as<core::CallExprPtr>()[0], 
	//																   core::arithmetic::toIR(convFact.getNodeManager(), postValue));
	//		postStmts.push_back(assign);
	//	}

	//}catch(const std::exception& error){
		
		// it seems that we can not normalize the thing... just write the expression OLD SCHOOL!!! but only if are pointers
		const core::IRBuilder& builder = convFact.getIRBuilder();
		if (frontend::utils::isRefArray(inductionVar->getType())){
			
		throw LoopNormalizationError(" pointer for loop not supported yet!"); 
			//// build the thing for pointers
			//inductionVar =  convFact.getIRBuilder().variable(builder.getLangBasic().getUInt8());
			//core::ExpressionPtr distance = builder.callExpr (builder.getLangBasic().getUInt8(), 
			//												 builder.getLangBasic().getArrayRefDistance(), endValue, initValue);
			//normalizedIterations = insertCeilFunc(builder.div(frontend::utils::castScalar(builder.getLangBasic().getReal8(),distance),
			//												  frontend::utils::castScalar(builder.getLangBasic().getReal8(), stepExpr)), 
			//									  builder.getLangBasic().getUInt8());
			//if (loopToBounduary) normalizedIterations = builder.add(normalizedIterations, builder.literal("1", builder.getLangBasic().getUInt8()));

			//normalizedInductionExpr =  builder.callExpr(builder.getLangBasic().getArrayView(), initValue,
			//												  frontend::utils::castScalar(builder.getLangBasic().getInt8(), 
			//													  						  builder.mul(stepExpr, inductionVar) ));
			//if(restoreValue){
			//	core::StatementPtr assign = builder.assign (originalInductionExpr.as<core::CallExprPtr>()[0], 
			//												builder.callExpr(builder.getLangBasic().getArrayView(), 
			//																initValue,
			//																frontend::utils::castScalar(builder.getLangBasic().getInt8(), 
			//																						   normalizedIterations)));
			//	postStmts.push_back(assign);
			//}
		}
		else{
			auto one =  builder.literal("1", inductionVar->getType());
			core::ExpressionPtr tmp;
			endValue = frontend::utils::castScalar(inductionVar->getType(), endValue);
			if (whileLessThan) {  
				tmp= builder.sub(endValue, initValue);
				if (!loopToBounduary)  tmp = builder.sub(tmp, one);
			}
			else{
				tmp= builder.sub(initValue, endValue);
				if (!loopToBounduary)  tmp = builder.sub(tmp, one);
			}

			// assure right typing
			assert(initValue->getType() == endValue->getType());
			assert(inductionVar->getType() == endValue->getType());

			// we create a variable for the range (needs to be a var because we can not divide the whole arithmetic eq , we have no way to parenthesise the
			// upper term)
			core::VariablePtr range = builder.variable( inductionVar->getType());
			auto rangeVarDecl = builder.declarationStmt(range, tmp);
			preStmts.push_back(rangeVarDecl);

			normalizedIterations = builder.add(builder.div(range, absFunc(stepExpr, range->getType())), one);

			normalizedInductionExpr = builder.mul(frontend::utils::castScalar(stepExpr->getType(),inductionVar), stepExpr);
			normalizedInductionExpr = builder.add (frontend::utils::castScalar(initValue->getType(), normalizedInductionExpr), initValue);

			// if the variable is declared outside, we must give it a final value afer all iterations
			if(restoreValue){
				core::StatementPtr assign = builder.assign (originalInductionExpr.as<core::CallExprPtr>()[0], 
															builder.add(builder.mul(normalizedIterations, stepExpr), initValue));
				// if the induction variable is not scope defined, and there is actualy some init value assigned, we should
				// update this variable so the inner loop side effects have access to it
				postStmts.push_back(assign);  
				if ( initValue != originalInductionExpr){
					assign = builder.assign (originalInductionExpr.as<core::CallExprPtr>()[0], inductionVar);
					firstStmts.push_back(assign);
				}
			}
		}
//	}
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
			conditionLeft = true; 
		}
		else{
			if (!isRight) throw LoopNormalizationError("induction variable could not be identified");
			// right is the induction expression, left is the up bounduary
			originalInductionExpr = right;
			endValue = left;
			conditionLeft = false; 
		}

		// strip possible casts
		if (core::CallExprPtr call = originalInductionExpr.isa<core::CallExprPtr>()){
			if (convFact.getIRBuilder().getLangBasic().isScalarCast (call.getFunctionExpr())){
				originalInductionExpr = call[0]; 
			}
		}

		if (!core::analysis::isCallOf(originalInductionExpr, convFact.getNodeManager().getLangBasic().getRefDeref())){
			throw LoopNormalizationError("could not determine number of iterations, please simply the for loop condition to see it as a for loop");
		}

		inductionVar =  convFact.getIRBuilder().variable(originalInductionExpr->getType());
	}
	else throw LoopNormalizationError("Not supported condition");

	// now that we know the induction expression and we created a new var, we have to identify the lower bound
	const clang::Stmt* initStmt = forStmt->getInit();

	if (!initStmt){  // there is no init statement, therefore the initial value is the value of the induction expression at the begining of the loop
		initValue = originalInductionExpr;
	}
	else{
		// could be a declaration or could be an assigment
		core::StatementPtr initIR = convFact.convertStmt(initStmt);
		InitializationCollector collector(incrementExpr);
		visitDepthFirstPrunable(core::NodeAddress (initIR), collector);
		initValue = collector.init;
		preStmts	 = collector.leftoverStmts;
		restoreValue = !collector.isDecl;
		if (!initValue) {
			// if we could not find any suitable init, the initialization is the value of the original variable at the begining of the loop
			initValue = originalInductionExpr;
		}
	}
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
	// if comparator is ( it < N ) whileLessThan should be true already (because of left side) we invert it if no
	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(cond) ) {
		switch(binOp->getOpcode()) {
		case BO_LT:
			whileLessThan = conditionLeft;
			loopToBounduary = false;
			break;
		case BO_GT:
			whileLessThan = !conditionLeft;
			loopToBounduary = false;
			break;
		case BO_NE:
			whileLessThan = true;
			loopToBounduary = false;
			break;
		case BO_GE:
			whileLessThan = !conditionLeft;
			loopToBounduary = true;
			break;
		case BO_LE:
			whileLessThan = conditionLeft;
			loopToBounduary = true;
			break;
		case BO_EQ:
			loopToBounduary = true;
			whileLessThan = true;
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

insieme::core::ForStmtPtr  LoopAnalyzer::getLoop(const insieme::core::StatementPtr& body) const{
	auto& mgr(body->getNodeManager());

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
	// reproduce first and last stmts (like assign value if not loop local)
	core::StatementList tmp;
	tmp.insert(tmp.end(), firstStmts.begin(), firstStmts.end());
	tmp.push_back(newBody);
	tmp.insert(tmp.end(), lastStmts.begin(), lastStmts.end());
	core::CompoundStmtPtr finalBody = convFact.getIRBuilder().compoundStmt(tmp);
	
	// normalized loop iterates from 0
	core::ExpressionPtr zero = convFact.getIRBuilder().literal("0", inductionVar->getType());
	return convFact.getIRBuilder().forStmt(inductionVar, zero, normalizedIterations, incrExpr, finalBody);
}

} // End analysis namespace
} // End froentend namespace
} // End insieme namespace

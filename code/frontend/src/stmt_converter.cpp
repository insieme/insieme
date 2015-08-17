/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/stmt_converter.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/frontend/utils/error_report.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include <algorithm>

using namespace clang;

namespace {
	struct litCompare {
		bool operator()(const insieme::core::LiteralPtr& a, const insieme::core::LiteralPtr& b) const {
			return a->getStringValue() < b->getStringValue();
		}
	};
}


namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//							BASE STMT CONVERTER -- takes care of C nodes
	//---------------------------------------------------------------------------------------------------------------------
	stmtutils::StmtWrapper Converter::StmtConverter::VisitDeclStmt(clang::DeclStmt* declStmt) {
		stmtutils::StmtWrapper retIr;
		LOG_STMT_CONVERSION(declStmt, retIr);

		for(auto decl : declStmt->decls()) {
			// a decl can be either a variable decl, or e.g. a typedef
			if(clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(decl)) {
				// external declaration statement as per very early K&R C -> ignore
				if(varDecl->hasExternalStorage()) { continue; }
				// convert decl
				auto convertedDecl = converter.getDeclConverter()->convertVarDecl(varDecl);
				// check if we have an init expression
				core::ExpressionPtr initExp;
				if(convertedDecl.second) {
					initExp = builder.refVar(*convertedDecl.second);
				} else {
					// generate undefined initializer
					initExp = builder.undefinedVar(convertedDecl.first.getType());
				}
				retIr.push_back(builder.declarationStmt(convertedDecl.first, initExp));
			}
		}

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitReturnStmt(clang::ReturnStmt* retStmt) {
		stmtutils::StmtWrapper retIr;
		LOG_STMT_CONVERSION(retStmt, retIr);

		auto irRetStmt = builder.returnStmt();

		// check if we have a return value
		if(clang::Expr* expr = retStmt->getRetValue()) {
			irRetStmt = builder.returnStmt(converter.convertExpr(expr));
		}

		retIr.push_back(irRetStmt);
		return retIr;

		//core::ExpressionPtr retExpr;
		//core::TypePtr retTy;
		//QualType clangTy;
		//if(clang::Expr* expr = retStmt->getRetValue()) {
		//	retExpr = converter.convertExpr(expr);

		//	clangTy = expr->getType();
		//	retTy = converter.convertType(clangTy);

		//	// arrays and vectors in C are always returned as reference, so the type of the return
		//	// expression is of array (or vector) type we are sure we have to return a reference, in the
		//	// other case we can safely deref the retExpr
		//	if((retTy->getNodeType() == core::NT_ArrayType || retTy->getNodeType() == core::NT_VectorType)
		//	   && (!clangTy.getUnqualifiedType()
		//	            ->isVectorType())) { // this applies also for OpenCL ExtVectorType. If this is moved, take care it still works also for them.
		//		retTy = builder.refType(retTy);
		//		retExpr = core::types::smartCast(retTy, retExpr);
		//	} else if(builder.getLangBasic().isBool(retExpr->getType())) {
		//		// attention with this, bools cast not handled in AST in C
		//		retExpr = core::types::castScalar(retTy, retExpr);
		//	}

		//	if(retExpr->getType()->getNodeType() == core::NT_RefType) {
		//		// Obviously vectors are an exception and must be handled like scalars
		//		// no reference returned
		//		if(clangTy->isVectorType()) { // this applies also for OpenCL ExtVectorType. If this is moved, take care it still works also for them.
		//			retExpr = core::types::smartCast(retTy, retExpr);
		//		}

		//		// vector to array
		//		if(retTy->getNodeType() == core::NT_RefType) {
		//			core::TypePtr expectedTy = core::analysis::getReferencedType(retTy);
		//			core::TypePtr currentTy = core::analysis::getReferencedType(retExpr->getType());
		//			if(expectedTy->getNodeType() == core::NT_ArrayType && currentTy->getNodeType() == core::NT_VectorType) {
		//				retExpr = core::types::smartCast(retTy, retExpr);
		//			}
		//		}
		//	}

		//} else {
		//	// no return expression
		//	retExpr = gen.getUnitConstant();
		//	retTy = gen.getUnit();
		//}

		//vector<core::StatementPtr> stmtList;
		//retIr = builder.returnStmt(retExpr);
		//stmtList.push_back(retIr);
		//core::StatementPtr retStatement = builder.compoundStmt(stmtList);
		//stmtutils::StmtWrapper body = stmtutils::aggregateStmts(builder, stmtList);
		//return body;
	}

	struct ContinueStmtCollector : public core::IRVisitor<bool, core::Address> {
		vector<core::ContinueStmtAddress> conts;

		// do not visit types
		ContinueStmtCollector() : IRVisitor<bool, core::Address>(false) {}

		bool visitWhileStmt(const core::WhileStmtAddress& cur) {
			return true;
		}

		bool visitForStmt(const core::ForStmtAddress& cur) {
			return true;
		}

		bool visitLambdaExpr(const core::LambdaExprAddress& cur) {
			return true;
		}

		bool visitContinueStmt(const core::ContinueStmtAddress& cur) {
			conts.push_back(cur);
			return true;
		}
	};

	vector<core::ContinueStmtAddress> getContinues(const core::StatementPtr& mainBody) {
		ContinueStmtCollector collector;
		core::visitDepthFirstPrunable(core::NodeAddress(mainBody), collector);
		return collector.conts;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FOR STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitForStmt(clang::ForStmt* forStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(forStmt, retStmt);

		//try {
		//	// Analyze loop for induction variable
		//	analysis::LoopAnalyzer loopAnalysis(forStmt, converter);

		//	// convert the body
		//	core::StatementPtr body = converter.convertStmt(forStmt->getBody());

		//	// for loops with break statements are while loops
		//	bool breakStmtFound = false;
		//	core::visitDepthFirstPrunable(body, [&](const core::NodePtr& cur) -> bool {
		//		if(cur.isa<core::BreakStmtPtr>()) breakStmtFound = true;

		//		if(cur.isa<core::LambdaExprPtr>() || cur.isa<core::ForStmtPtr>() || cur.isa<core::WhileStmtPtr>() || cur.isa<core::SwitchStmtPtr>())
		//			return true;
		//		else {
		//			return false;
		//		}
		//	});
		//	if(breakStmtFound) { throw analysis::LoopNormalizationError("break statement not allowed in for loop"); }


		//	// we have to replace all ocurrences of the induction expression/var in the annotations of the body
		//	// by the new induction var
		//	core::visitDepthFirstPrunable(body, [&](const core::StatementPtr& stmt) -> bool {
		//		if(stmt->hasAnnotation(omp::BaseAnnotation::KEY)) {
		//			auto anno = stmt->getAnnotation(omp::BaseAnnotation::KEY);

		//			std::vector<core::VariablePtr> orgVars;
		//			std::vector<core::VariablePtr> trgVars;
		//			core::visitDepthFirstOnce(loopAnalysis.getOriginalInductionExpr(), [&](const core::VariablePtr& var) { orgVars.push_back(var); });
		//			core::visitDepthFirstOnce(loopAnalysis.getInductionExpr(), [&](const core::VariablePtr& var) { trgVars.push_back(var); });

		//			int i = 0;
		//			for(auto org : orgVars) {
		//				anno->replaceUsage(org, trgVars[i]);
		//				i++;
		//			}
		//		}
		//		if(stmt.isa<core::CompoundStmtPtr>() || stmt.isa<core::IfStmtPtr>() || stmt.isa<core::SwitchStmtPtr>())
		//			return false;
		//		else {
		//			return true;
		//		}
		//	});

		//	retStmt.insert(retStmt.end(), loopAnalysis.getPreStmts().begin(), loopAnalysis.getPreStmts().end());

		//	core::ForStmtPtr forIr = loopAnalysis.getLoop(body);
		//	frontend_assert(forIr && "Created for statement is not valid");

		//	retStmt.push_back(forIr);

		//	// incorporate statements do be done after loop and we are done
		//	retStmt.insert(retStmt.end(), loopAnalysis.getPostStmts().begin(), loopAnalysis.getPostStmts().end());

		//	return retStmt;

		//} catch(const analysis::LoopNormalizationError& e) {
		//	// The for loop cannot be normalized into an IR loop, therefore we create a while stmt
		//	stmtutils::StmtWrapper body = aggregateStmts(builder, Visit(forStmt->getBody()));

		//	clang::Stmt* initStmt = forStmt->getInit();
		//	if(initStmt) {
		//		stmtutils::StmtWrapper init = Visit(forStmt->getInit());
		//		std::copy(init.begin(), init.end(), std::back_inserter(retStmt));
		//	}

		//	if(clang::VarDecl* condVarDecl = forStmt->getConditionVariable()) {
		//		frontend_assert(forStmt->getCond() == NULL && "ForLoop condition cannot be a variable declaration and an expression");
		//		/*
		//		 * the for loop has a variable declared in the condition part, e.g.
		//		 *
		//		 * 		for(...; int a = f(); ...)
		//		 *
		//		 * to handle this kind of situation we have to move the declaration  outside the loop body inside a
		//		 * new context
		//		 */
		//		clang::Expr* expr = condVarDecl->getInit();
		//		condVarDecl->setInit(NULL); // set the expression to null (temporarely)
		//		core::StatementPtr declStmt = converter.convertVarDecl(condVarDecl);
		//		condVarDecl->setInit(expr); // restore the init value

		//		frontend_assert(false && "ForStmt with a declaration of a condition variable not supported");
		//		retStmt.push_back(declStmt);
		//	}

		//	core::StatementPtr irBody = stmtutils::aggregateStmts(builder, body);

		//	if(forStmt->getInc()) {
		//		vector<core::ContinueStmtAddress> conts = getContinues(irBody);

		//		if(!conts.empty()) {
		//			core::StatementList stmtList;
		//			stmtList.push_back(converter.convertExpr(forStmt->getInc()));
		//			stmtList.push_back(builder.continueStmt());
		//			core::CompoundStmtPtr incr = builder.compoundStmt(stmtList);
		//			std::map<core::NodeAddress, core::NodePtr> replacementsMap;
		//			for_each(conts.begin(), conts.end(), [&](core::ContinueStmtAddress& cur) { replacementsMap.insert({cur, incr}); });
		//			irBody = core::transform::replaceAll(builder.getNodeManager(), replacementsMap).as<core::StatementPtr>();
		//		}
		//	}

		//	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//	// analysis of loop structure failed, we have to build a while statement:
		//	//
		//	// 		for(init; cond; step) { body }
		//	//
		//	// Will be translated in the following while statement structure:
		//	//
		//	// 		{
		//	// 			init;
		//	// 			while(cond) {
		//	// 				{ body }
		//	// 				step;
		//	// 			}
		//	// 		}
		//	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//	core::ExpressionPtr condition;
		//	if(forStmt->getCond()) {
		//		condition = core::types::smartCast(builder.getLangBasic().getBool(), converter.convertExpr(forStmt->getCond()));
		//	} else {
		//		// we might not have condition, this is an infinite loop
		//		//    for (;;)
		//		condition = converter.builder.literal(std::string("true"), builder.getLangBasic().getBool());
		//	}

		//	core::StatementPtr whileStmt = builder.whileStmt(
		//	    condition, forStmt->getInc() ? builder.compoundStmt(toVector<core::StatementPtr>(irBody, converter.convertExpr(forStmt->getInc()))) : irBody);

		//	// handle eventual pragmas attached to the Clang node
		//	retStmt.push_back(whileStmt);

		//	if(!converter.getConversionSetup().hasOption(ConversionSetup::NoWarnings)) {
		//		std::cerr << std::endl;
		//		clang::Preprocessor& pp = converter.getPreprocessor();
		//		utils::clangPreprocessorDiag(pp, forStmt->getLocStart(), DiagnosticsEngine::Warning,
		//		                             std::string("For loop converted into while loop, cause: ") + e.what());
		//	}
		//}

		assert_not_implemented();
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								IF STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitIfStmt(clang::IfStmt* ifStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(ifStmt, retStmt);

		//core::StatementPtr thenBody = stmtutils::aggregateStmts(builder, Visit(ifStmt->getThen()));
		//frontend_assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

		//core::ExpressionPtr condExpr;
		//if(const clang::VarDecl* condVarDecl = ifStmt->getConditionVariable()) {
		//	/*
		//	 * we are in the situation where a variable is declared in the if condition, i.e.:
		//	 *
		//	 * 		if(int a = exp) { }
		//	 *
		//	 * this will be converted into the following IR representation:
		//	 *
		//	 * 		{
		//	 * 			int a = exp;
		//	 * 			if(cast<bool>(a)){ }
		//	 * 		}
		//	 */
		//	core::StatementPtr declStmt = converter.convertVarDecl(condVarDecl);
		//	retStmt.push_back(declStmt);

		//	frontend_assert(declStmt.isa<core::DeclarationStmtPtr>() && "declaring static variables within an if is not very polite");
		//}

		//const clang::Expr* cond = ifStmt->getCond();
		//frontend_assert(cond && "If statement with no condition.");

		//condExpr = converter.convertExpr(cond);

		//if(core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign())) {
		//	// an assignment as condition is not allowed in IR, prepend the assignment operation
		//	retStmt.push_back(condExpr);
		//	// use the first argument as condition
		//	condExpr = builder.deref(condExpr.as<core::CallExprPtr>()->getArgument(0));
		//}

		//frontend_assert(condExpr && "Couldn't convert 'condition' expression of the IfStmt");

		//if(!gen.isBool(condExpr->getType())) {
		//	// convert the expression to bool via the castToType utility routine
		//	condExpr = core::types::smartCast(gen.getBool(), condExpr);
		//}

		//core::StatementPtr elseBody = builder.compoundStmt();
		//// check for else statement
		//if(Stmt* elseStmt = ifStmt->getElse()) { elseBody = stmtutils::aggregateStmts(builder, Visit(elseStmt)); }
		//frontend_assert(elseBody && "Couldn't convert 'else' body of the IfStmt");

		//// adding the ifstmt to the list of returned stmts
		//retStmt.push_back(builder.ifStmt(condExpr, thenBody, elseBody));

		//// try to aggregate statements into a CompoundStmt if more than 1 statement has been created
		//// from this IfStmt
		//retStmt = aggregateStmts(builder, retStmt);

		//// otherwise we introduce an outer CompoundStmt
		//return retStmt;

		assert_not_implemented();
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							WHILE STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitWhileStmt(clang::WhileStmt* whileStmt) {
		stmtutils::StmtWrapper retStmt;

		//LOG_STMT_CONVERSION(whileStmt, retStmt);

		//core::StatementPtr body = aggregateStmts(builder, Visit(whileStmt->getBody()));
		//frontend_assert(body && "Couldn't convert body of the WhileStmt");

		//core::ExpressionPtr condExpr;
		//if(clang::VarDecl* condVarDecl = whileStmt->getConditionVariable()) {
		//	frontend_assert(!whileStmt->getCond() && "WhileStmt condition cannot contains both a variable declaration and an expression");

		//	/*
		//	 * we are in the situation where a variable is declared in the if condition, i.e.:
		//	 *
		//	 * 		while(int a = expr) { }
		//	 *
		//	 * this will be converted into the following IR representation:
		//	 *
		//	 * 		{
		//	 * 			int a = 0;
		//	 * 			while(a = expr){ }
		//	 * 		}
		//	 */
		//	clang::Expr* expr = condVarDecl->getInit();
		//	condVarDecl->setInit(NULL); // set the expression to null (temporarely)
		//	core::StatementPtr declStmt = converter.convertVarDecl(condVarDecl);
		//	condVarDecl->setInit(expr); // set back the value of init value

		//	retStmt.push_back(declStmt);
		//	// the expression will be an a = expr
		//	frontend_assert(false && "WhileStmt with a declaration of a condition variable not supported");
		//} else {
		//	const clang::Expr* cond = whileStmt->getCond();
		//	frontend_assert(cond && "WhileStmt with no condition.");

		//	condExpr = converter.convertExpr(cond);

		//	if(core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign())) {
		//		// an assignment as condition is not allowed in IR, prepend the assignment operation
		//		retStmt.push_back(condExpr);
		//		// use the first argument as condition
		//		condExpr = builder.deref(condExpr.as<core::CallExprPtr>()->getArgument(0));
		//	}
		//}

		//frontend_assert(condExpr && "Couldn't convert 'condition' expression of the WhileStmt");

		//if(!gen.isBool(condExpr->getType())) {
		//	// convert the expression to bool via the castToType utility routine
		//	condExpr = core::types::smartCast(gen.getBool(), condExpr);
		//}

		//retStmt.push_back(builder.whileStmt(condExpr, body));
		//retStmt = aggregateStmts(builder, retStmt);

		// otherwise we introduce an outer CompoundStmt

		assert_not_implemented();

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DO STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitDoStmt(clang::DoStmt* doStmt) {
		stmtutils::StmtWrapper retStmt;

		//LOG_STMT_CONVERSION(doStmt, retStmt);

		//core::CompoundStmtPtr body = builder.wrapBody(stmtutils::aggregateStmts(builder, Visit(doStmt->getBody())));
		//frontend_assert(body && "Couldn't convert body of the WhileStmt");

		//const clang::Expr* cond = doStmt->getCond();
		//frontend_assert(cond && "DoStmt must have a condition.");

		//core::ExpressionPtr condExpr = converter.convertExpr(cond);
		//frontend_assert(condExpr && "Couldn't convert 'condition' expression of the DoStmt");

		//frontend_assert(!core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign()) && "Assignment not allowd in condition expression");

		//if(!gen.isBool(condExpr->getType())) {
		//	// convert the expression to bool via the castToType utility routine
		//	condExpr = core::types::smartCast(gen.getBool(), condExpr);
		//}
		//condExpr = converter.tryDeref(condExpr);

		//StatementList stmts;
		//core::VariablePtr exitTest = builder.variable(builder.refType(gen.getBool()));
		//stmts.push_back(builder.declarationStmt(exitTest, builder.refVar(gen.getFalse())));
		//condExpr = builder.logicOr(builder.logicNeg(builder.deref(exitTest)), condExpr);
		//body = builder.compoundStmt({builder.assign(exitTest, gen.getTrue()), body});
		//stmts.push_back(builder.whileStmt(condExpr, body));

		//core::StatementPtr irNode = builder.compoundStmt(stmts);

		//// adding the WhileStmt to the list of returned stmts
		//retStmt.push_back(irNode);
		//retStmt = stmtutils::aggregateStmts(builder, retStmt);

		// otherwise we introduce an outer CompoundStmt

		assert_not_implemented();

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							SWITCH STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitSwitchStmt(clang::SwitchStmt* switchStmt) {
		stmtutils::StmtWrapper retStmt;

		//LOG_STMT_CONVERSION(switchStmt, retStmt);

		//core::ExpressionPtr condExpr;

		//if(const clang::VarDecl* condVarDecl = switchStmt->getConditionVariable()) {
		//	frontend_assert(!switchStmt->getCond() && "SwitchStmt condition cannot contains both a variable declaration and an expression");

		//	core::StatementPtr declStmt = converter.convertVarDecl(condVarDecl);
		//	retStmt.push_back(declStmt);

		//	frontend_assert(declStmt.isa<core::DeclarationStmtPtr>()
		//	                && " declaring a static variable in a switch condition??? you must have a very good reason to do this!!!");

		//	// the expression will be a reference to the declared variable
		//	condExpr = declStmt.as<core::DeclarationStmtPtr>()->getVariable();
		//} else {
		//	const clang::Expr* cond = switchStmt->getCond();
		//	frontend_assert(cond && "SwitchStmt with no condition.");
		//	condExpr = converter.tryDeref(converter.convertExpr(cond));

		//	// we create a variable to store the value of the condition for this switch
		//	core::VariablePtr condVar = builder.variable(gen.getInt4());
		//	// int condVar = condExpr;
		//	core::DeclarationStmtPtr declVar = builder.declarationStmt(condVar, builder.castExpr(gen.getInt4(), condExpr));
		//	retStmt.push_back(declVar);

		//	condExpr = condVar;
		//}

		//frontend_assert(condExpr && "Couldn't convert 'condition' expression of the SwitchStmt");

		//std::map<core::LiteralPtr, std::vector<core::StatementPtr>> caseMap;
		//vector<core::StatementPtr> decls;
		//std::vector<core::LiteralPtr> openCases;
		//auto defLit = builder.literal("__insieme_default_case", gen.getUnit());

		//auto addStmtToOpenCases = [&caseMap, &openCases](const core::StatementPtr& stmt) {
		//	// for each of the open cases, add the statement to their own stmt list
		//	for(const auto& caseLit : openCases) {
		//		caseMap[caseLit].push_back(stmt);
		//	}
		//};

		//// converts to literal the cases,
		//auto convertCase = [this, defLit](const clang::SwitchCase* switchCase) -> core::LiteralPtr {

		//	frontend_assert(switchCase);
		//	if(llvm::isa<clang::DefaultStmt>(switchCase)) { return defLit; }

		//	core::LiteralPtr caseLiteral;
		//	const clang::Expr* caseExpr = llvm::cast<clang::CaseStmt>(switchCase)->getLHS();

		//	// if the expr is an integerConstantExpr
		//	if(caseExpr->isIntegerConstantExpr(converter.getCompiler().getASTContext())) {
		//		llvm::APSInt result;
		//		// reduce it and store it in result -- done by clang
		//		caseExpr->isIntegerConstantExpr(result, converter.getCompiler().getASTContext());
		//		core::TypePtr type = converter.convertType(caseExpr->getType());
		//		caseLiteral = builder.literal(type, result.toString(10));
		//	} else {
		//		core::ExpressionPtr caseExprIr = converter.convertExpr(caseExpr);
		//		if(caseExprIr->getNodeType() == core::NT_CastExpr) {
		//			core::CastExprPtr cast = static_pointer_cast<core::CastExprPtr>(caseExprIr);
		//			if(cast->getSubExpression()->getNodeType() == core::NT_Literal) {
		//				core::LiteralPtr literal = static_pointer_cast<core::LiteralPtr>(cast->getSubExpression());
		//				caseExprIr = builder.literal(cast->getType(), literal->getValue());
		//			}
		//		}

		//		if(!caseExprIr.isa<core::LiteralPtr>()) {
		//			// clang casts the literal to fit the condition type... and is not a literal anymore
		//			// it might be a scalar cast, we retrive the literal
		//			caseLiteral = caseExprIr.as<core::CallExprPtr>()->getArgument(0).as<core::LiteralPtr>();
		//		} else {
		//			caseLiteral = caseExprIr.as<core::LiteralPtr>();
		//		}
		//	}

		//	return caseLiteral;
		//};

		//auto handleDeclStmt = [this, &decls](const clang::DeclStmt* declStmt) {
		//	auto result = converter.convertStmt(declStmt);
		//	core::DeclarationStmtPtr decl;
		//	if(result.isa<core::CompoundStmtPtr>()) {
		//		core::CompoundStmtPtr comp = result.as<core::CompoundStmtPtr>();
		//		for(auto st : comp) {
		//			frontend_assert(st.isa<core::DeclarationStmtPtr>() && "Declaration stmt inside of SwitchStmt contains unknown elements.");
		//			decl = st.as<core::DeclarationStmtPtr>();
		//			// remove the init, use undefinedvar
		//			// this is what GCC does, VC simply errors out
		//			decl = builder.declarationStmt(decl->getVariable(), builder.undefinedVar(decl->getInitialization()->getType()));
		//			decls.push_back(decl);
		//		}
		//	} else {
		//		decl = converter.convertStmt(declStmt).as<core::DeclarationStmtPtr>();
		//		// remove the init, use undefinedvar
		//		// this is what GCC does, VC simply errors out
		//		decl = builder.declarationStmt(decl->getVariable(), builder.undefinedVar(decl->getInitialization()->getType()));
		//		decls.push_back(decl);
		//	}
		//};

		//// looks for inner cases inside of cases stmt, and returns the compound attached
		//// 			case A
		//// 				case B
		//// 					stmt1
		//// 					stmt2
		//// 			break
		//auto lookForCases = [this, &caseMap, &openCases, &decls, convertCase, addStmtToOpenCases, handleDeclStmt](const clang::SwitchCase* caseStmt) {
		//	const clang::Stmt* stmt = caseStmt;

		//	// we might find some chained stmts
		//	while(stmt && llvm::isa<clang::SwitchCase>(stmt)) {
		//		const clang::SwitchCase* inCase = llvm::cast<clang::SwitchCase>(stmt);
		//		openCases.push_back(convertCase(inCase));
		//		caseMap[openCases.back()] = std::vector<core::StatementPtr>();
		//		stmt = inCase->getSubStmt();
		//	}

		//	// after the case statements, we might find the statements to be executed
		//	if(stmt) {
		//		// it may happen that the first stmt after the case is a declstmt.
		//		// in this case we need to convert it and save it in the decl list.
		//		if(const clang::DeclStmt* declStmt = dyn_cast<clang::DeclStmt>(stmt)) {
		//			handleDeclStmt(declStmt);
		//		} else {
		//			addStmtToOpenCases(converter.convertStmt(stmt));
		//		}
		//	}
		//};

		//// iterate throw statements inside of switch
		//clang::CompoundStmt* compStmt = dyn_cast<clang::CompoundStmt>(switchStmt->getBody());
		//frontend_assert(compStmt && "Switch statements doesn't contain a compound stmt");
		//for(auto it = compStmt->body_begin(), end = compStmt->body_end(); it != end; ++it) {
		//	clang::Stmt* currStmt = *it;
		//	// if is a case stmt, create a literal and open it
		//	if(const clang::SwitchCase* switchCaseStmt = llvm::dyn_cast<clang::SwitchCase>(currStmt)) {
		//		lookForCases(switchCaseStmt);
		//		continue;
		//	} else if(const clang::DeclStmt* declStmt = llvm::dyn_cast<clang::DeclStmt>(currStmt)) {
		//		// collect all declarations which are in de switch body and add them (without init) to
		//		// the cases
		//		handleDeclStmt(declStmt);
		//		continue;
		//	}

		//	// if is whatever other kind of stmt append it to each of the open cases list
		//	addStmtToOpenCases(converter.convertStmt(currStmt));
		//}

		//// we need to sort the elements to assure same output for different memory aligment, valgrinf problem
		//std::set<core::LiteralPtr, litCompare> caseLiterals;
		//for(auto pair : caseMap) {
		//	caseLiterals.insert(pair.first);
		//}

		//vector<core::SwitchCasePtr> cases;
		//// initialize the default case with an empty compoundstmt
		//core::CompoundStmtPtr defStmt = builder.compoundStmt();
		//for(auto literal : caseLiterals) {
		//	if(literal != defLit) {
		//		cases.push_back(builder.switchCase(literal, builder.wrapBody(stmtutils::aggregateStmts(builder, caseMap[literal]))));
		//	} else {
		//		defStmt = builder.wrapBody(stmtutils::aggregateStmts(builder, caseMap[literal]));
		//	}
		//}

		//core::StatementPtr irSwitch = builder.switchStmt(condExpr, cases, defStmt);

		//// add declarations at switch scope
		//for(auto decl : decls) {
		//	retStmt.push_back(decl);
		//}

		//retStmt.push_back(irSwitch);
		//retStmt = aggregateStmts(builder, retStmt);

		assert_not_implemented();

		return retStmt;
	}

	/*
	 * as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error
	 * in the case the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the
	 * visitor is not called on his subnodes
	 */
	stmtutils::StmtWrapper Converter::StmtConverter::VisitSwitchCase(clang::SwitchCase* caseStmt) {
		frontend_assert(false && "Visitor is visiting a 'case' stmt");
		return stmtutils::StmtWrapper();
	}

	stmtutils::StmtWrapper Converter::StmtConverter::VisitBreakStmt(clang::BreakStmt* breakStmt) {
		return stmtutils::StmtWrapper(builder.breakStmt());
	}

	stmtutils::StmtWrapper Converter::StmtConverter::VisitContinueStmt(clang::ContinueStmt* contStmt) {
		return stmtutils::StmtWrapper(builder.continueStmt());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {
		core::StatementPtr retIr;
		LOG_STMT_CONVERSION(compStmt, retIr);

		bool hasReturn = false;

		vector<core::StatementPtr> stmtList;
		std::for_each(compStmt->body_begin(), compStmt->body_end(), [&stmtList, this, &hasReturn](Stmt* stmt) {
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// A compoundstmt can contain declaration statements.This means that a clang
			// DeclStmt can be converted in multiple  StatementPtr because an initialization
			// list such as: int a,b=1; is converted into the following sequence of statements:
			//
			// 		int<a> a = 0; int<4> b = 1;
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			stmtutils::StmtWrapper convertedStmt;

			if(dyn_cast<clang::ReturnStmt>(stmt)) { hasReturn = true; }

			convertedStmt = Visit(stmt);
			copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));

		});

		retIr = builder.compoundStmt(stmtList);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitNullStmt(clang::NullStmt* nullStmt) {
		// TODO: Visual Studio 2010 fix: && removed
		core::StatementPtr retStmt = builder.getNoOp();
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							GOTO  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitGotoStmt(clang::GotoStmt* gotoStmt) {
		core::StatementPtr retIr;
		LOG_STMT_CONVERSION(gotoStmt, retIr);

		core::StringValuePtr str = builder.stringValue(gotoStmt->getLabel()->getName());
		retIr = builder.gotoStmt(str);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							LABEL  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitLabelStmt(clang::LabelStmt* labelStmt) {
		// core::StatementPtr retIr;
		// LOG_STMT_CONVERSION(labelStmt, retIr);
		core::StringValuePtr str = builder.stringValue(labelStmt->getName());
		stmtutils::StmtWrapper retIr = stmtutils::StmtWrapper(builder.labelStmt(str));

		clang::Stmt* stmt = labelStmt->getSubStmt();
		stmtutils::StmtWrapper retIr2 = Visit(stmt);
		std::copy(retIr2.begin(), retIr2.end(), std::back_inserter(retIr));
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ASM STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitAsmStmt(clang::AsmStmt* asmStmt) {
		// two subclasses - gccasmstmt/msasmstmt
		frontend_assert(false && "currently not implemented");
		return stmtutils::StmtWrapper();
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitStmt(clang::Stmt* stmt) {
		// frontend_assert(false && "this code looks malform and no used") << stmt->getStmtClassName(); -- guess what, it is used!
		std::for_each(stmt->child_begin(), stmt->child_end(), [this](clang::Stmt* stmt) { this->Visit(stmt); });
		return stmtutils::StmtWrapper();
	}

	//---------------------------------------------------------------------------------------------------------------------
	//							CLANG STMT CONVERTER
	//							takes care of C nodes
	//							C nodes implemented in base: StmtConverter
	//---------------------------------------------------------------------------------------------------------------------

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CStmtConverter::Visit(clang::Stmt* stmt) {
		VLOG(2) << "C";

		// iterate frontend extension list and check if a extension wants to convert the stmt
		stmtutils::StmtWrapper retStmt;
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			retStmt = extension->Visit(stmt, converter);
			if(retStmt.size()) { break; }
		}
		if(retStmt.size() == 0) {
			converter.trackSourceLocation(stmt);
			retStmt = StmtVisitor<CStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);
			converter.untrackSourceLocation();
		}

		// print diagnosis messages
		converter.printDiagnosis(stmt->getLocStart());

		// Deal with pragmas
		core::NodeList list(retStmt.begin(), retStmt.end());
		list = pragma::attachPragma(list, stmt, converter);
		retStmt.clear();
		for(const auto& e : list) {
			retStmt.push_back(e.as<core::StatementPtr>());
		}

		// call frontend extension post visitors
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			retStmt = extension->PostVisit(stmt, retStmt, converter);
		}

		return retStmt;
	}
}
}
}

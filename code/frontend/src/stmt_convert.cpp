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

#include "insieme/frontend/convert.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/statements.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/c_info/naming.h"
#include "insieme/c_info/location.h"

#include "insieme/core/transform/node_replacer.h"

#include "clang/AST/StmtVisitor.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace {

//------------------- StmtWrapper -------------------------------------------------------------
/*
 * Utility class used as a return type for the StmtVisitor. It can store a list of statement
 * as conversion of a single C stmt can result in multiple IR statements.
 */
struct StmtWrapper: public StatementList {
	StmtWrapper(): StatementList() { }
	StmtWrapper(const core::StatementPtr& stmt): StatementList() { push_back(stmt); }

	core::StatementPtr getSingleStmt() const {
		assert(size() == 1 && "More than 1 statement present");
		return front();
	}

	bool isSingleStmt() const { return size() == 1; }
};

// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
core::StatementPtr tryAggregateStmts(const core::ASTBuilder& builder, const StatementList& stmtVect) {
	if( stmtVect.size() == 1 )
		return stmtVect.front();
	return builder.compoundStmt(stmtVect);
}

core::ExpressionPtr makeOperation(const core::ASTBuilder& builder, const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs,
		const core::lang::BasicGenerator::Operator& op) {
	return builder.callExpr(lhs->getType(), builder.getBasicGenerator().getOperator(lhs->getType(), op), toVector<core::ExpressionPtr>(lhs, rhs));
}

}

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define FORWARD_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.convertExpr(stmt) ); }


#define START_LOG_STMT_CONVERSION(stmt) \
	assert(convFact.currTU); \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting statement [class: '" << stmt->getStmtClassName() << "'] \n" \
			 << "-> at location: (" << utils::location(stmt->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang statement:\n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		stmt->dump(convFact.currTU->getCompiler().getSourceManager()); \
	}

#define END_LOG_STMT_CONVERSION(stmt) \
	VLOG(1) << "Converted 'statement' into IR stmt: "; \
	VLOG(1) << "\t" << *stmt;


//#############################################################################
//
//							CLANG STMT CONVERTER
//
//############################################################################
class ConversionFactory::ClangStmtConverter: public StmtVisitor<ClangStmtConverter, StmtWrapper> {
	ConversionFactory& convFact;

public:
	ClangStmtConverter(ConversionFactory& convFact): convFact(convFact) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// In clang a declstmt is represented as a list of VarDecl
	StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt) {
		// if there is only one declaration in the DeclStmt we return it
		if( declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl()) ) {
			try {
				core::DeclarationStmtPtr&& retStmt = convFact.convertVarDecl( dyn_cast<clang::VarDecl>(declStmt->getSingleDecl()) );
				// handle eventual OpenMP pragmas attached to the Clang node
				core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(retStmt, declStmt, convFact);
				return StmtWrapper( annotatedNode );
			} catch(const GlobalVariableDeclarationException& err) {
				return StmtWrapper();
			}
		}

		// otherwise we create an an expression list which contains the multiple declaration inside the statement
		StmtWrapper retList;
		for(clang::DeclStmt::decl_iterator it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it)
			if( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) ) {
				try {
					core::DeclarationStmtPtr&& retStmt = convFact.convertVarDecl(varDecl);
					// handle eventual OpenMP pragmas attached to the Clang node
					core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(retStmt, declStmt, convFact);
					retList.push_back( annotatedNode );
				} catch(const GlobalVariableDeclarationException& err) { }
			}
		return retList;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitReturnStmt(ReturnStmt* retStmt) {
		START_LOG_STMT_CONVERSION(retStmt);
		core::ExpressionPtr retExpr;

		core::TypePtr retTy;
		if(Expr* expr = retStmt->getRetValue()) {
			retExpr = convFact.convertExpr( expr );
			retTy = convFact.convertType( expr->getType().getTypePtr() );
		} else {
			retExpr = convFact.builder.getBasicGenerator().getUnitConstant();
			retTy = convFact.builder.getBasicGenerator().getUnit();
		}

		// arrays and vectors in C are always returned as reference, so the type of
		// the return expression is of array (or vector) type we are sure we have
		// to return a reference, in the other case we can safely deref the retExpr
		if(retTy->getNodeType() != core::NT_ArrayType && retTy->getNodeType() != core::NT_VectorType) {
			retExpr = convFact.tryDeref(retExpr);
		}

		core::StatementPtr&& ret = convFact.builder.returnStmt(retExpr);
		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(ret, retStmt, convFact);

		END_LOG_STMT_CONVERSION( ret );
		return StmtWrapper( annotatedNode );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FOR STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitForStmt(ForStmt* forStmt) {
		START_LOG_STMT_CONVERSION(forStmt);
		const core::ASTBuilder& builder = convFact.builder;
		VLOG(2) << "{ Visit ForStmt }";

		StmtWrapper retStmt;
		StmtWrapper&& body = Visit(forStmt->getBody());

		try {
			// Analyze loop for induction variable
			analysis::LoopAnalyzer loopAnalysis(forStmt, convFact);

			core::ExpressionPtr&& incExpr = loopAnalysis.getIncrExpr();
			core::ExpressionPtr&& condExpr = loopAnalysis.getCondExpr();

			StmtWrapper&& initExpr = Visit( forStmt->getInit() );
			// induction variable for this loop
			core::VariablePtr oldInductionVar;
			core::ExpressionPtr&& inductionVar = convFact.lookUpVariable(loopAnalysis.getInductionVar());

			if( isa<ParmVarDecl>(loopAnalysis.getInductionVar()) ) {
				const core::VariablePtr& indVar = core::static_pointer_cast<const core::Variable>(inductionVar);
				auto fit = convFact.ctx.wrapRefMap.find(indVar);
				if(fit == convFact.ctx.wrapRefMap.end()) {
					fit = convFact.ctx.wrapRefMap.insert(
						std::make_pair(indVar, builder.variable( builder.refType(inductionVar->getType()) ))
					).first;
				}
				oldInductionVar = indVar;
				inductionVar = fit->second;
			}
			assert(inductionVar && inductionVar->getType()->getNodeType() == core::NT_RefType);

			if( !initExpr.isSingleStmt() ) {
				assert(core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr[0]) &&
						"Not a declaration statement");
				/*
				 * We have a multiple declaration in the initialization part of the stmt, e.g.
				 *
				 * 		for(int a,b=0; ...)
				 *
				 * to handle this situation we have to create an outer block in order to declare
				 * the variables which are not used as induction variable:
				 *
				 * 		{
				 * 			int a=0;
				 * 			for(int b=0;...) { }
				 * 		}
				 */
				typedef std::function<bool (const core::StatementPtr&)> InductionVarFilterFunc;
				InductionVarFilterFunc inductionVarFilter =
					[ this, inductionVar ](const core::StatementPtr& curr) -> bool {
						core::DeclarationStmtPtr&& declStmt =
								core::dynamic_pointer_cast<const core::DeclarationStmt>(curr);
						assert(declStmt && "Not a declaration statement");
						return declStmt->getVariable() == inductionVar;
					};

				std::function<bool (const InductionVarFilterFunc& functor, const core::StatementPtr& curr)> negation =
					[] (const InductionVarFilterFunc& functor, const core::StatementPtr& curr) -> bool {
						return !functor(curr);
					};

				/*
				 * we insert all the variable declarations (excluded the induction
				 * variable) before the body of the for loop
				 */
				std::copy_if(initExpr.begin(), initExpr.end(), std::back_inserter(retStmt),
						std::bind(negation, inductionVarFilter, std::placeholders::_1) );

				// we now look for the declaration statement which contains the induction variable
				std::vector<core::StatementPtr>::const_iterator fit =
						std::find_if(initExpr.begin(), initExpr.end(),
								std::bind( inductionVarFilter, std::placeholders::_1 )
						);

				assert(fit!=initExpr.end() && "Induction variable not declared in the loop initialization expression");
				// replace the initExpr with the declaration statement of the induction variable
				initExpr = *fit;
			}

			assert(initExpr.isSingleStmt() && "Init expression for loop statement contains multiple statements");

			// We are in the case where we are sure there is exactly 1 element in the initialization expression
			core::DeclarationStmtPtr&& declStmt =
					core::dynamic_pointer_cast<const core::DeclarationStmt>( initExpr.getSingleStmt() );

			bool iteratorChanged = false;
			core::VariablePtr newIndVar;
			if( !declStmt ) {
				/*
				 * the init expression is not a declaration stmt, it could be a situation where
				 * it is an assignment operation, eg:
				 *
				 * 		for( i=exp; ...) { i... }
				 *
				 * or, it is missing, or is a reference to a global variable.
				 *
				 * In this case we have to replace the old induction variable with a new one and
				 * replace every occurrence of the old variable with the new one. Furthermore,
				 * to maintain the correct semantics of the code, the value of the old induction
				 * variable has to be restored when exiting the loop.
				 *
				 * 		{
				 * 			for(_i = init; _i < cond; _i += step) { _i... }
				 * 			i = ceil((cond-init)/step) * step + init;
				 * 		}
				 */
				core::ExpressionPtr&& init =
						core::dynamic_pointer_cast<const core::Expression>( initExpr.getSingleStmt() );

				assert(init && "Initialization statement for loop is not an expression");

				const core::TypePtr& varTy = inductionVar->getType();

				/*
				 * we create a new induction variable, we don't register it to the variable
				 * map as it will be valid only within this for statement
				 */
				newIndVar = builder.variable(varTy);

				// we have to define a new induction variable for the loop and replace every
				// instance in the loop with the new variable
				VLOG(2) << "Substituting loop induction variable: " << loopAnalysis.getInductionVar()->getNameAsString()
						<< " with variable: v" << newIndVar->getId();

				// Initialize the value of the new induction variable with the value of the old one
				if( core::analysis::isCallOf(init, convFact.mgr.basic.getRefAssign()) ) {
					init = core::static_pointer_cast<const core::CallExpr>(init)->getArguments()[1]; // getting RHS
				} else if(init->getNodeType() != core::NT_Variable){
					/*
					 * the initialization variable is in a form which is not yet handled
					 * therefore, the for loop is transformed into a while loop
					 */
					throw analysis::LoopNormalizationError();
				}

				// because the variable was coming from an input parameter, a deref of the new
				// induction variable is necessary to maintain the correct semantics
				core::ExpressionPtr&& replacement =
						(oldInductionVar ? builder.deref(newIndVar) : static_cast<core::ExpressionPtr>(newIndVar));

				declStmt = builder.declarationStmt( newIndVar, builder.refVar(init) );
				core::NodePtr&& ret = core::transform::replaceAll(builder.getNodeManager(), body.getSingleStmt(), inductionVar, replacement, true);
				if(oldInductionVar)
					ret = core::transform::replaceAll(builder.getNodeManager(), body.getSingleStmt(), oldInductionVar, replacement, true);

				// replace the body with the newly modified one
				body = StmtWrapper( core::dynamic_pointer_cast<const core::Statement>(ret) );

				// we have to remember that the iterator has been changed for this loop
				iteratorChanged = true;
			}

			assert(declStmt && "Failed conversion of loop init expression");

			core::ExpressionPtr init = declStmt->getInitialization();
			if(core::analysis::isCallOf(init, convFact.mgr.basic.getRefVar())) {
				const core::CallExprPtr& callExpr = core::static_pointer_cast<const core::CallExpr>(init);
				assert(callExpr->getArguments().size() == 1);
				init = callExpr->getArguments()[0];
				assert(init->getType()->getNodeType() != core::NT_RefType &&
					"Initialization value of induction variable must be of non-ref type");
			}

			if(loopAnalysis.isInverted()) {
				// invert init value
				core::ExpressionPtr&& invInitExpr = builder.invertSign( init );
				declStmt = builder.declarationStmt( declStmt->getVariable(), builder.refVar(invInitExpr) );
				assert(declStmt->getVariable()->getType()->getNodeType() == core::NT_RefType);

				// invert the sign of the loop index in body of the loop
				core::ExpressionPtr&& inductionVar = builder.invertSign(builder.deref(declStmt->getVariable()));
				core::NodePtr&& ret = core::transform::replaceAll(builder.getNodeManager(), body.getSingleStmt(), builder.deref(declStmt->getVariable()),
						inductionVar, true);
				body = StmtWrapper( core::dynamic_pointer_cast<const core::Statement>(ret) );
			}
//			else {
//				const core::RefTypePtr& varTy =
//						core::static_pointer_cast<const core::RefType>(declStmt->getVariable()->getType());
//
//				// The ref induction variable
//				core::VariablePtr&& nonRefInductionVar = builder.variable(varTy->getElementType());
//
//				core::NodePtr&& ret = core::transform::replaceAll(
//						builder.getNodeManager(), body.getSingleStmt(), builder.deref(declStmt->getVariable()),
//						nonRefInductionVar, true
//					);
//				body = StmtWrapper( core::dynamic_pointer_cast<const core::Statement>(ret) );
//				declStmt = builder.declarationStmt(nonRefInductionVar, init );
//			}

			// We finally create the IR ForStmt
			core::ForStmtPtr&& irFor = builder.forStmt(declStmt, body.getSingleStmt(), condExpr, incExpr);
			assert(irFor && "Created for statement is not valid");

			// handle eventual pragmas attached to the Clang node
			core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(irFor, forStmt, convFact);
			retStmt.push_back( annotatedNode );

			if(iteratorChanged) {
				// in the case we replace the loop iterator with a temporary variable,
				// we have to assign the final value of the iterator to the old variable
				// so we don't change the semantics of the code
				core::TypePtr iterType = (inductionVar->getType()->getNodeType() == core::NT_RefType) ?
						core::static_pointer_cast<const core::RefType>(inductionVar->getType())->getElementType() :
						inductionVar->getType();

				core::ExpressionPtr&& cond = convFact.tryDeref(loopAnalysis.getCondExpr());
				core::ExpressionPtr&& step = convFact.tryDeref(loopAnalysis.getIncrExpr());

				core::FunctionTypePtr&& ceilTy =
						builder.functionType(toVector<core::TypePtr>(convFact.mgr.basic.getDouble()), convFact.mgr.basic.getDouble());

				core::ExpressionPtr&& finalVal =
					makeOperation(builder,
						init, // init +
						makeOperation(builder,
							builder.castExpr(iterType, // ( cast )
								builder.callExpr(
									convFact.mgr.basic.getDouble(),
									builder.literal(ceilTy, "ceil"), // ceil()
									makeOperation( // (cond-init)/step
										builder,
										builder.castExpr(convFact.mgr.basic.getDouble(),
											makeOperation(builder, cond, init, core::lang::BasicGenerator::Sub) // cond - init
										),
										builder.castExpr(convFact.mgr.basic.getDouble(), step),
										core::lang::BasicGenerator::Div
									)
								)
							),
							step,
							core::lang::BasicGenerator::Mul
						),
						core::lang::BasicGenerator::Add
					);

				retStmt.push_back( builder.callExpr( convFact.mgr.basic.getUnit(),
						convFact.mgr.basic.getRefAssign(), inductionVar, finalVal ));

			}

		} catch(const analysis::LoopNormalizationError& e) {

			Stmt* initStmt = forStmt->getInit();
			// if there is no initialization stmt, we transform the ForStmt into a WhileStmt
			if( initStmt ) {
				StmtWrapper init = Visit( forStmt->getInit() );
				std::copy(init.begin(), init.end(), std::back_inserter(retStmt));
			}

			if( VarDecl* condVarDecl = forStmt->getConditionVariable() ) {
				assert(forStmt->getCond() == NULL && "ForLoop condition cannot be a variable declaration and an expression");
				//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
				// the for loop has a variable declared in the condition part, e.g.
				//
				// 		for(...; int a = f(); ...)
				//
				// to handle this kind of situation we have to move the declaration
				// outside the loop body inside a new context
				//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
				Expr* expr = condVarDecl->getInit();
				condVarDecl->setInit(NULL); // set the expression to null (temporarely)
				core::DeclarationStmtPtr&& declStmt = convFact.convertVarDecl(condVarDecl);
				condVarDecl->setInit(expr); // restore the init value

				assert(false && "ForStmt with a declaration of a condition variable not supported");
				retStmt.push_back( declStmt );
			}

			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// analysis of loop structure failed, we have to build a while statement
			//
			//		for(init; cond; step) { body }
			//
			// Will be translated in the following while statement structure:
			//
			//		{
			//			init;
			//			while(cond) {
			//				{ body }
			//				step;
			//			}
			//		}
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

			core::StatementPtr&& whileStmt = builder.whileStmt(
				convFact.convertExpr( forStmt->getCond() ), // cond
				forStmt->getInc() ?
					builder.compoundStmt( toVector<core::StatementPtr>( tryAggregateStmts(builder, body), convFact.convertExpr( forStmt->getInc() ) ) ) :
					tryAggregateStmts(builder, body)
				);

			// handle eventual pragmas attached to the Clang node
			core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(whileStmt, forStmt, convFact);
			retStmt.push_back( annotatedNode );

		    clang::Preprocessor& pp = convFact.currTU->getCompiler().getPreprocessor();
		    pp.Diag(forStmt->getLocStart(),
		    		pp.getDiagnostics().getCustomDiagID(Diagnostic::Warning, std::string("For loop converted into while loop, cause: ") + e.what() ));
		}
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								IF STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitIfStmt(IfStmt* ifStmt) {
		START_LOG_STMT_CONVERSION(ifStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ Visit IfStmt }";
		core::StatementPtr&& thenBody = tryAggregateStmts( builder, Visit( ifStmt->getThen() ) );
		assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

		core::ExpressionPtr condExpr;
		if( const VarDecl* condVarDecl = ifStmt->getConditionVariable() ) {
			assert(ifStmt->getCond() == NULL && "IfStmt condition cannot contains both a variable declaration and an expression");
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// we are in the situation where a variable is declared in the if condition, i.e.:
			//		if(int a = exp) { }
			//
			// this will be converted into the following IR representation:
			// 		{
			//			int a = exp;
			//			if(cast<bool>(a)){ }
			//		}
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			core::DeclarationStmtPtr&& declStmt = convFact.convertVarDecl(condVarDecl);
			retStmt.push_back( declStmt );

			// the expression will be a cast to bool of the declared variable
			condExpr = builder.castExpr(convFact.mgr.basic.getBool(), declStmt->getVariable());
		} else {
			const Expr* cond = ifStmt->getCond();
			assert(cond && "If statement with no condition.");

			condExpr = convFact.tryDeref(convFact.convertExpr( cond ));
			if(*condExpr->getType() != *convFact.mgr.basic.getBool()) {
				// add a cast expression to bool
				condExpr = builder.castExpr(convFact.mgr.basic.getBool(), condExpr);
			}
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the IfStmt");

		core::StatementPtr elseBody;
		// check for else statement
		if(Stmt* elseStmt = ifStmt->getElse()) {
			elseBody = tryAggregateStmts( builder, Visit( elseStmt ) );
		} else {
			// create an empty compound statement in the case there is no else stmt
			elseBody = builder.compoundStmt();
		}
		assert(elseBody && "Couldn't convert 'else' body of the IfStmt");

		core::StatementPtr&& irNode = builder.ifStmt(condExpr, thenBody, elseBody);

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, ifStmt, convFact);

		// adding the ifstmt to the list of returned stmts
		retStmt.push_back( annotatedNode );

		// try to aggregate statements into a CompoundStmt if more than 1 statement
		// has been created from this IfStmt
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		// otherwise we introduce an outer CompoundStmt
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							WHILE STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitWhileStmt(WhileStmt* whileStmt) {
		START_LOG_STMT_CONVERSION(whileStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ WhileStmt }";
		core::StatementPtr&& body = tryAggregateStmts( builder, Visit( whileStmt->getBody() ) );
		assert(body && "Couldn't convert body of the WhileStmt");

		core::ExpressionPtr condExpr;
		if( VarDecl* condVarDecl = whileStmt->getConditionVariable() ) {
			assert(whileStmt->getCond() == NULL &&
					"WhileStmt condition cannot contains both a variable declaration and an expression");

			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// we are in the situation where a variable is declared in the if condition, i.e.:
			//
			//		 while(int a = expr) { }
			//
			// this will be converted into the following IR representation:
			//
			// 		{
			//			int a = 0;
			//			while(a = expr){ }
			//		}
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			Expr* expr = condVarDecl->getInit();
			condVarDecl->setInit(NULL); // set the expression to null (temporarily)
			core::DeclarationStmtPtr&& declStmt = convFact.convertVarDecl(condVarDecl);
			condVarDecl->setInit(expr); // set back the value of init value

			retStmt.push_back( declStmt );
			// the expression will be an a = expr
			// core::ExpressionPtr&& condExpr = convFact.convertExpr(expr);
			assert(false && "WhileStmt with a declaration of a condition variable not supported");
		} else {
			const Expr* cond = whileStmt->getCond();
			assert(cond && "WhileStmt with no condition.");
			condExpr = convFact.convertExpr( cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the WhileStmt");

		condExpr = convFact.tryDeref(condExpr);
		if(*condExpr->getType() != *convFact.mgr.basic.getBool()) {
			// add cast to bool FIXME
			condExpr = builder.castExpr(convFact.mgr.basic.getBool(), condExpr);
		}

		core::StatementPtr&& irNode = builder.whileStmt(condExpr, body);

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, whileStmt, convFact);

		// adding the WhileStmt to the list of returned stmts
		retStmt.push_back( annotatedNode );
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		// otherwise we introduce an outer CompoundStmt
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DO STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitDoStmt(DoStmt* doStmt) {
		START_LOG_STMT_CONVERSION(doStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ DoStmt }";
		core::StatementPtr&& body = tryAggregateStmts( builder, Visit( doStmt->getBody() ) );
		assert(body && "Couldn't convert body of the WhileStmt");

		const Expr* cond = doStmt->getCond();
		assert(cond && "DoStmt with no condition.");
		core::ExpressionPtr&& condExpr = convFact.tryDeref( convFact.convertExpr( cond ) );
		assert(condExpr && "Couldn't convert 'condition' expression of the DoStmt");

		if(*condExpr->getType() != *convFact.mgr.basic.getBool()) {
			// add cast to bool FIXME
			condExpr = builder.castExpr(convFact.mgr.basic.getBool(), condExpr);
		}

		StatementList stmts;
		if(core::CompoundStmtPtr&& compStmt = core::dynamic_pointer_cast<const core::CompoundStmt>(body)) {
			std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), std::back_inserter(stmts));
		}
		stmts.push_back(builder.whileStmt(condExpr, body));

		core::StatementPtr&& irNode = builder.compoundStmt(stmts);

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, doStmt, convFact);

		// adding the WhileStmt to the list of returned stmts
		retStmt.push_back( annotatedNode );
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		// otherwise we introduce an outer CompoundStmt
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							SWITCH STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitSwitchStmt(SwitchStmt* switchStmt) {
		START_LOG_STMT_CONVERSION(switchStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ Visit SwitchStmt }";
		core::ExpressionPtr condExpr(NULL);
		if( const VarDecl* condVarDecl = switchStmt->getConditionVariable() ) {
			assert(switchStmt->getCond() == NULL && "SwitchStmt condition cannot contains both a variable declaration and an expression");

			core::DeclarationStmtPtr&& declStmt = convFact.convertVarDecl(condVarDecl);
			retStmt.push_back( declStmt );

			// the expression will be a reference to the declared variable
			condExpr = declStmt->getVariable();
		} else {
			const Expr* cond = switchStmt->getCond();
			assert(cond && "SwitchStmt with no condition.");
			condExpr = convFact.tryDeref( convFact.convertExpr( cond ) );

			// we create a variable to store the value of the condition for this switch
			core::VariablePtr&& condVar = builder.variable(convFact.mgr.basic.getInt4());
			// int condVar = condExpr;
			core::DeclarationStmtPtr&& declVar = builder.declarationStmt(condVar, builder.castExpr(convFact.mgr.basic.getInt4(), condExpr));
			retStmt.push_back(declVar);

			condExpr = condVar;
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the SwitchStmt");

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// this Switch stamtement has a body, i.e.:
		//
		// 		switch(e) {
		// 	 		{ body }
		// 	 		case x:...
		// 		}
		//
		// As the IR doens't allow a body to be represented inside the switch stmt
		// we bring this code outside after the declaration of the eventual conditional
		// variable.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		vector<core::SwitchStmt::Case> cases;
		// marks the beginning of a case expression
		vector<std::pair<core::ExpressionPtr,size_t>> caseExprs;
		size_t defaultStart = 0;
		// collected statements that will be part of the next case statement
		vector<core::StatementPtr> caseStmts;
		bool caseStart = false;
		bool breakEncountred = false;
		bool isDefault = false;
		core::StatementPtr&& defStmt = builder.compoundStmt();

		CompoundStmt* compStmt = dyn_cast<CompoundStmt>(switchStmt->getBody());
		assert(compStmt && "Switch statements doesn't contain a compound stmt");

		// lambda function which creates a case stmt using the accumulated statements
		auto addCase = [this, &cases, &caseStmts, &caseExprs, &defaultStart, &defStmt, &isDefault]() -> void {
			std::for_each(caseExprs.begin(), caseExprs.end(),
				[ &cases, &caseStmts, this ](const std::pair<core::ExpressionPtr,size_t>& curr) {
					std::vector<core::StatementPtr> stmtList(caseStmts.size() - curr.second);
					std::copy(caseStmts.begin() + curr.second, caseStmts.end(), stmtList.begin());
					cases.push_back( core::SwitchStmt::Case(curr.first, tryAggregateStmts( this->convFact.builder, stmtList )) );
				}
			);
			if(isDefault) {
				std::vector<core::StatementPtr> stmtList(caseStmts.size() - defaultStart);
				std::copy(caseStmts.begin() + defaultStart, caseStmts.end(), stmtList.begin());
				defStmt = tryAggregateStmts( this->convFact.builder, stmtList );
			}
		};

		for(auto it = compStmt->body_begin(), end = compStmt->body_end(); it != end; ++it) {
			Stmt* curr = *it;
			// statements which are before the first case.
			if(!caseStart && !isa<SwitchCase>(curr)) {
				StmtWrapper&& visitedStmt = this->Visit(curr);
				// append these statements before the switch statement
				std::copy(visitedStmt.begin(), visitedStmt.end(), std::back_inserter(retStmt));
				continue;
			}
			// we encounter a case statement
			caseStart=true;
			while( CaseStmt* caseStmt = dyn_cast<CaseStmt>(curr) ) {
				caseExprs.push_back( std::make_pair(this->convFact.convertExpr( caseStmt->getLHS() ), caseStmts.size()) );

				core::StatementPtr subStmt;
				if( const Expr* rhs = caseStmt->getRHS() ) {
					assert(!caseStmt->getSubStmt() && "Case stmt cannot have both a RHS and and sub statement.");
					subStmt = this->convFact.convertExpr( rhs );
				} else if( Stmt* sub = caseStmt->getSubStmt() ) {
					// if the sub statement is a case, skip until the end of the loop
					if( isa<SwitchCase>(sub) ) {
						curr = sub;
						continue;
					}

					subStmt = tryAggregateStmts( this->convFact.builder, this->Visit( const_cast<Stmt*>(sub) ) );
					// if the sub-statement is a BreakStmt we have to replace it with a noOp and remember to reset the caseStmts
					if(core::dynamic_pointer_cast<const core::BreakStmt>(subStmt)) {
						subStmt = convFact.mgr.basic.getNoOp();
						breakEncountred = true;
					}
				}
				// add the statements defined by this case to the list of
				// statements which has to executed by this case
				caseStmts.push_back(subStmt);
				break;
			}

			if(const DefaultStmt* defCase = dyn_cast<const DefaultStmt>(curr)) {
				isDefault = true;
				defaultStart = caseStmts.size();

				core::StatementPtr&& subStmt = tryAggregateStmts( convFact.builder, Visit( const_cast<Stmt*>(defCase->getSubStmt())) );
				if(core::dynamic_pointer_cast<const core::BreakStmt>(subStmt)) {
					subStmt = convFact.mgr.basic.getNoOp();
					breakEncountred = true;
				}
				caseStmts.push_back(subStmt);
			}

			// if the current statement is a break, or we encountred a break in the current case
			// we create a new case and add to the list of cases for this switch statement
			if(breakEncountred || isa<const BreakStmt>(curr)) {
				addCase();
				// clear the list of statements collected until now
				caseExprs.clear();
				caseStmts.clear();

				breakEncountred = false;
			} else if(!isa<SwitchCase>(curr)) {
				StmtWrapper&& visitedStmt = Visit( const_cast<Stmt*>(curr));
				std::copy(visitedStmt.begin(), visitedStmt.end(), std::back_inserter(caseStmts));
			}
		}
		// we still have some statement pending
		if(!caseStmts.empty())
			addCase();

		// initialize the default case with an empty compoundstmt
		core::StatementPtr&& irNode = builder.switchStmt(condExpr, cases, defStmt);

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, switchStmt, convFact);

		// Appends the switchstmt to the current list of stmt
		retStmt.push_back( annotatedNode );
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		return retStmt;
	}

	// as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error in the case
	// the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the visitor is not called on his subnodes
	StmtWrapper VisitSwitchCase(SwitchCase* caseStmt) { assert(false && "Visitor is visiting a 'case' stmt"); }

	StmtWrapper VisitBreakStmt(BreakStmt* breakStmt) { return StmtWrapper( convFact.builder.breakStmt() ); }
	StmtWrapper VisitContinueStmt(ContinueStmt* contStmt) { return StmtWrapper( convFact.builder.continueStmt() ); }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitCompoundStmt(CompoundStmt* compStmt) {
		START_LOG_STMT_CONVERSION(compStmt);
		vector<core::StatementPtr> stmtList;
		std::for_each( compStmt->body_begin(), compStmt->body_end(),
			[ &stmtList, this ] (Stmt* stmt) {
				// A compoundstmt can contain declaration statements.This means that a clang DeclStmt can be converted in multiple
				// StatementPtr because an initialization list such as: int a,b=1; is converted into the following sequence of statements:
				// int<a> a = 0; int<4> b = 1;
				StmtWrapper&& convertedStmt = this->Visit(stmt);
				std::copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));
			}
		);
		core::StatementPtr&& retStmt = convFact.builder.compoundStmt(stmtList);

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(retStmt, compStmt, convFact);

		END_LOG_STMT_CONVERSION(retStmt);
		return StmtWrapper( annotatedNode );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitNullStmt(NullStmt* nullStmt) {
		//TODO: Visual Studio 2010 fix: && removed
		core::StatementPtr&& retStmt = convFact.mgr.basic.getNoOp();

		// handle eventual OpenMP pragmas attached to the Clang node
		core::StatementPtr&& annotatedNode = omp::attachOmpAnnotation(retStmt, nullStmt, convFact);

		return StmtWrapper( annotatedNode );
	}

	FORWARD_VISITOR_CALL(IntegerLiteral)
	FORWARD_VISITOR_CALL(FloatingLiteral)
	FORWARD_VISITOR_CALL(CharacterLiteral)
	FORWARD_VISITOR_CALL(StringLiteral)

	FORWARD_VISITOR_CALL(BinaryOperator)
	FORWARD_VISITOR_CALL(UnaryOperator)
	FORWARD_VISITOR_CALL(ConditionalOperator)

	FORWARD_VISITOR_CALL(CastExpr)
	FORWARD_VISITOR_CALL(ImplicitCastExpr)
	FORWARD_VISITOR_CALL(PredefinedExpr)
	FORWARD_VISITOR_CALL(DeclRefExpr)
	FORWARD_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_VISITOR_CALL(CallExpr)
	FORWARD_VISITOR_CALL(ParenExpr)
	FORWARD_VISITOR_CALL(MemberExpr)
	FORWARD_VISITOR_CALL(CompoundLiteralExpr)

	StmtWrapper VisitStmt(Stmt* stmt) {
		std::for_each( stmt->child_begin(), stmt->child_end(), [ this ] (Stmt* stmt) { this->Visit(stmt); });
		return StmtWrapper();
	}
};

ConversionFactory::ClangStmtConverter* ConversionFactory::makeStmtConvert(ConversionFactory& fact) {
	return new ConversionFactory::ClangStmtConverter(fact);
}

void ConversionFactory::cleanStmtConvert(ClangStmtConverter* stmtConv) {
	delete stmtConv;
}

core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt* stmt) const {
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return stmtConv->Visit( const_cast<Stmt*>(stmt) ).getSingleStmt();
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

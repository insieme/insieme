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

#include "insieme/frontend/stmt_converter.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/castTool.h"

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/mpi/mpi_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/core/transform/node_replacer.h"

using namespace clang;

namespace stmtutils {

using namespace insieme::core;

// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
StatementPtr tryAggregateStmts(const IRBuilder& builder, const StatementList& stmtVect) {
	return (stmtVect.size() == 1) ? tryAggregateStmt(builder, stmtVect.front()) : builder.compoundStmt(stmtVect);
}

StatementPtr tryAggregateStmt(const IRBuilder& builder, const StatementPtr& stmt) {
	return stmt->getNodeType() == NT_CompoundStmt ?
		tryAggregateStmts(builder, stmt.as<CompoundStmtPtr>()->getStatements())	: stmt;
}

ExpressionPtr makeOperation(const IRBuilder& builder,
			  			 	const ExpressionPtr& lhs,
			  				const ExpressionPtr& rhs,
					  		const lang::BasicGenerator::Operator& op)
{
	return builder.callExpr(lhs->getType(), // return type
			builder.getLangBasic().getOperator(lhs->getType(), op), // get the operator
			lhs, rhs	 // LHS and RHS of the operation
		);
}

} // end stmtutils namespace

namespace insieme {
namespace frontend {
namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//							BASE STMT CONVERTER -- takes care of C nodes
//---------------------------------------------------------------------------------------------------------------------
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitDeclStmt(clang::DeclStmt* declStmt) {
	// if there is only one declaration in the DeclStmt we return it

	if (declStmt->isSingleDecl() && llvm::isa<clang::VarDecl>(declStmt->getSingleDecl())) {

		stmtutils::StmtWrapper retList;
		clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(declStmt->getSingleDecl());

		auto retStmt = convFact.convertVarDecl(varDecl);
		if (core::DeclarationStmtPtr decl = retStmt.isa<core::DeclarationStmtPtr>()){

			// check if there is a kernelFile annotation
			ocl::attatchOclAnnotation(decl->getInitialization(), declStmt, convFact);
			// handle eventual OpenMP pragmas attached to the Clang node
			retList.push_back( omp::attachOmpAnnotation(decl, declStmt, convFact) );
		}
		else{
			retList.push_back(retStmt);
		}

		return retList;
	}

	// otherwise we create an an expression list which contains the multiple declaration inside the statement
	stmtutils::StmtWrapper retList;
	for (auto it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it )
	if ( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) ) {
//
//try {
			auto retStmt = convFact.convertVarDecl(varDecl);
			// handle eventual OpenMP pragmas attached to the Clang node
			retList.push_back( omp::attachOmpAnnotation(retStmt, declStmt, convFact) );

//		} catch ( const GlobalVariableDeclarationException& err ) {}
	}

	return retList;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							RETURN STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitReturnStmt(clang::ReturnStmt* retStmt) {

	core::StatementPtr retIr;
	LOG_STMT_CONVERSION(retStmt, retIr);

	core::ExpressionPtr retExpr;
	core::TypePtr retTy;
	QualType clangTy;
	if ( clang::Expr* expr = retStmt->getRetValue()) {
		retExpr = convFact.convertExpr(expr);

		clangTy = expr->getType();
		retTy = convFact.convertType(clangTy.getTypePtr());

		// arrays and vectors in C are always returned as reference, so the type of the return
		// expression is of array (or vector) type we are sure we have to return a reference, in the
		// other case we can safely deref the retExpr
		if ((retTy->getNodeType() == core::NT_ArrayType || retTy->getNodeType() == core::NT_VectorType) &&
						!clangTy.getUnqualifiedType()->isExtVectorType()) {
			retTy = builder.refType(retTy);
			retExpr = utils::cast(retExpr, retTy);
		}
		else if ( builder.getLangBasic().isBool( retExpr->getType())){
			// attention with this, bools cast not handled in AST in C
			retExpr = utils::castScalar(retTy, retExpr);
		}

		if (retExpr->getType()->getNodeType() == core::NT_RefType) {

			// Obviously Ocl vectors are an exception and must be handled like scalars
			// no reference returned
			if (clangTy->isExtVectorType()) {
				retExpr = utils::cast(retExpr, retTy);
			}

			// vector to array
			if(retTy->getNodeType() == core::NT_RefType){
				core::TypePtr expectedTy = core::analysis::getReferencedType(retTy) ;
				core::TypePtr currentTy = core::analysis::getReferencedType(retExpr->getType()) ;
				if (expectedTy->getNodeType() == core::NT_ArrayType &&
					currentTy->getNodeType()  == core::NT_VectorType){
					retExpr = utils::cast(retExpr, retTy);
				}
			}
		}

	} else {
		// no return expression
		retExpr = gen.getUnitConstant();
		retTy = gen.getUnit();
	}

	vector<core::StatementPtr> stmtList;
	retIr = builder.returnStmt(retExpr);
	stmtList.push_back(retIr);
	core::StatementPtr retStatement = builder.compoundStmt(stmtList);
	stmtutils::StmtWrapper body = stmtutils::tryAggregateStmts(builder,stmtList );

	return body;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								FOR STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitForStmt(clang::ForStmt* forStmt) {
	VLOG(2) << "{ Visit ForStmt }";

	stmtutils::StmtWrapper retStmt;
    LOG_STMT_CONVERSION( forStmt, retStmt );

	bool addDeclStmt = false;

	try {
		// Analyze loop for induction variable
		analysis::LoopAnalyzer loopAnalysis(forStmt, convFact);
		const clang::VarDecl* iv = loopAnalysis.getInductionVar();

		// before the body is visited we have to make sure to register the loop induction variable
		// with the correct type:
		//
		// we need 3 IR variables:
		// 	- the conversion of the original loop: ( might localy declared, or in an outer scope)
		// 	- the new iterator: will be a constant scalar (not ref<type...>)
		// 	- the use of the iterator: any use of the iterator will be wrapped in a new variable
		// 		assigned with the right iteration value at the begining of the loop body
		//
		core::ExpressionPtr fakeInductionVar= convFact.lookUpVariable(iv);
		core::VariablePtr 	inductionVar 	= builder.variable(convFact.convertType(GET_TYPE_PTR(iv)));
		core::VariablePtr	 itUseVar     	= builder.variable(builder.refType(inductionVar->getType()));

		// polute wrapped vars cache to avoid inner replacements
		if(llvm::isa<clang::ParmVarDecl> (iv)){
			convFact.ctx.wrapRefMap.insert(std::make_pair(itUseVar,itUseVar));
		}

		// polute vars cache to make sure that the right value is used
		auto cachedPair = convFact.ctx.varDeclMap.find(iv);
		if (cachedPair != convFact.ctx.varDeclMap.end()) {
			cachedPair->second = itUseVar;
		} else {
			cachedPair = convFact.ctx.varDeclMap.insert(std::make_pair(loopAnalysis.getInductionVar(), itUseVar)).first;
		}

		// Visit Body
		StatementList stmtsOld = Visit(forStmt->getBody());

		// restore original variable in cache
		if (fakeInductionVar->getNodeType() == core::NT_Variable)
			cachedPair->second = core::static_pointer_cast<const core::VariablePtr>(fakeInductionVar);
		else
			convFact.ctx.varDeclMap.erase(cachedPair);

		if(llvm::isa<clang::ParmVarDecl> (iv)){
			convFact.ctx.wrapRefMap.erase(itUseVar);
		}

		assert(*inductionVar->getType() == *builder.deref(itUseVar)->getType() && "different induction var types");
		if (core::analysis::isReadOnly(builder.compoundStmt(stmtsOld), itUseVar)) {
			// convert iterator variable into read-only value
			auto deref = builder.deref(itUseVar);
			auto mat = builder.refVar(inductionVar);
			for(auto& cur : stmtsOld) {
				cur = core::transform::replaceAllGen(mgr, cur, deref, inductionVar, true);
				cur = core::transform::replaceAllGen(mgr, cur, itUseVar, mat, true);

				// TODO: make this more efficient
				// also update potential omp annotations
				core::visitDepthFirstOnce(cur, [&] (const core::StatementPtr& node){
					//if we have a OMP annotation
					if (node->hasAnnotation(omp::BaseAnnotation::KEY)){
						auto anno = node->getAnnotation(omp::BaseAnnotation::KEY);
						anno->replaceUsage(deref, inductionVar);
						anno->replaceUsage(itUseVar, inductionVar);
					}
				});
			}
		} else {
			//TODO: check for free return/break/continue
			// -- see core/transform/manipulation/isOulineAble() for "free return/break/continue" check

			//if the inductionVar is materialized into the itUseVar, the changes are not on the inductionVar
			//example: for(int i;...;i++) { i++; } the increment in the loop body is lost

			//if the inductionvar is not readonly convert into while
			throw analysis::InductionVariableNotReadOnly();
		}

		// build body
		stmtutils::StmtWrapper body = stmtutils::tryAggregateStmts(builder, stmtsOld);

		core::ExpressionPtr incExpr  = loopAnalysis.getIncrExpr();
		incExpr = utils::castScalar(inductionVar->getType(), incExpr);
		core::ExpressionPtr condExpr = loopAnalysis.getCondExpr();
		condExpr = utils::castScalar(inductionVar->getType(), condExpr);

		assert(inductionVar->getType()->getNodeType() != core::NT_RefType);

		stmtutils::StmtWrapper initExpr = Visit( forStmt->getInit() );

		if (!initExpr.isSingleStmt()) {
			assert(core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr[0]) &&
					"Not a declaration statement"
				);
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// We have a multiple declaration in the initialization part of the stmt, e.g.
			//
			// 		for(int a,b=0; ...)
			//
			// to handle this situation we have to create an outer block in order to declare the
			// variables which are not used as induction variable:
			//
			// 		{
			// 			int a=0;
			// 			for(int b=0;...) { }
			// 		}
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			typedef std::function<bool(const core::StatementPtr&)> InductionVarFilterFunc;

			auto inductionVarFilter =
			[ & ](const core::StatementPtr& curr) -> bool {
				core::DeclarationStmtPtr declStmt = curr.as<core::DeclarationStmtPtr>();
				assert(declStmt && "Not a declaration statement");
				return declStmt->getVariable() == fakeInductionVar;
			};

			auto negation =
			[] (const InductionVarFilterFunc& functor, const core::StatementPtr& curr) -> bool {
				return !functor(curr);
			};

			if (!initExpr.empty()) {
				addDeclStmt = true;
			}

			/*
			 * we insert all the variable declarations (excluded the induction
			 * variable) before the body of the for loop
			 */
			std::copy_if(initExpr.begin(), initExpr.end(), std::back_inserter(retStmt),
					std::bind(negation, inductionVarFilter, std::placeholders::_1));

			// we now look for the declaration statement which contains the induction variable
			auto fit = std::find_if(initExpr.begin(), initExpr.end(),
						std::bind( inductionVarFilter, std::placeholders::_1 )
				);

			assert( fit!=initExpr.end() && "Induction variable not declared in the loop initialization expression");
			// replace the initExpr with the declaration statement of the induction variable
			initExpr = *fit;
		}

		assert( initExpr.isSingleStmt() && "Init expression for loop statement contains multiple statements");

		// We are in the case where we are sure there is exactly 1 element in the initialization expression
		core::DeclarationStmtPtr declStmt =
			core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr.getSingleStmt());

		bool iteratorChanged = false;

		if (!declStmt) {
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// the init expression is not a declaration stmt, it could be a situation where
			// it is an assignment operation, eg:
			//
			// 		for( i=exp; ...) { i... }
			//
			// or, it is missing, or is a reference to a global variable.
			//
			// In this case we have to replace the old induction variable with a new one and
			// replace every occurrence of the old variable with the new one. Furthermore,
			// to maintain the correct semantics of the code, the value of the old induction
			// variable has to be restored when exiting the loop.
			//
			// 		{
			// 			for(int _i = init; _i < cond; _i += step) { _i... }
			// 			i = ceil((cond-init)/step) * step + init;
			// 		}
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			core::ExpressionPtr init = initExpr.getSingleStmt().as<core::ExpressionPtr>();

			assert(init && "Initialization statement for loop is not an expression");

			assert(inductionVar->getType()->getNodeType() != core::NT_RefType);

			// Initialize the value of the new induction variable with the value of the old one
			if ( core::analysis::isCallOf(init, gen.getRefAssign()) ) {
				init = init.as<core::CallExprPtr>()->getArguments()[1]; // getting RHS
			} else if ( init->getNodeType() != core::NT_Variable ) {
				/*
				 * the initialization variable is in a form which is not yet handled
				 * therefore, the for loop is transformed into a while loop
				 */
				throw analysis::LoopNormalizationError();
			}

			declStmt = builder.declarationStmt( inductionVar, init );

			// we have to remember that the iterator has been changed for this loop
			iteratorChanged = true;
		}

		assert(declStmt && "Failed conversion of loop init expression");
		core::ExpressionPtr init = declStmt->getInitialization();

		if (core::analysis::isCallOf(init, gen.getRefVar())) {
			core::CallExprPtr callExpr = init.as<core::CallExprPtr>();
			assert(callExpr->getArguments().size() == 1);

			init = callExpr->getArgument(0);

			assert(init->getType()->getNodeType() != core::NT_RefType &&
					"Initialization value of induction variable must be of non-ref type"
				);

		} else if (init->getType()->getNodeType() == core::NT_RefType) {
			init = builder.deref(init);
		}

		assert( init->getType()->getNodeType() != core::NT_RefType);

		declStmt = builder.declarationStmt(inductionVar, init);
		assert(init->getType()->getNodeType() != core::NT_RefType);

		if (loopAnalysis.isInverted()) {
			// invert init value
			core::ExpressionPtr invInitExpr = builder.invertSign( init );
			declStmt = builder.declarationStmt( declStmt->getVariable(), invInitExpr );
			assert(declStmt->getVariable()->getType()->getNodeType() != core::NT_RefType);

			// invert the sign of the loop index in body of the loop
			core::ExpressionPtr inductionVar = builder.invertSign(declStmt->getVariable());
			core::NodePtr ret = core::transform::replaceAll(
					builder.getNodeManager(),
					body.getSingleStmt(),
					declStmt->getVariable(),
					inductionVar
			);
			body = stmtutils::StmtWrapper( ret.as<core::StatementPtr>() );
		}

		// Now replace the induction variable of type ref<int<4>> with the non ref type. This
		// requires to replace any occurence of the iterator in the code with new induction
		// variable.
		assert(declStmt->getVariable()->getNodeType() == core::NT_Variable);

		// We finally create the IR ForStmt
		core::ForStmtPtr forIr =
		builder.forStmt(declStmt, condExpr, incExpr, stmtutils::tryAggregateStmt(builder, body.getSingleStmt()));

		assert(forIr && "Created for statement is not valid");

		// check for datarange pragma
		attatchDatarangeAnnotation(forIr, forStmt, convFact);
		attatchLoopAnnotation(forIr, forStmt, convFact);

		retStmt.push_back(omp::attachOmpAnnotation(forIr, forStmt, convFact));
		assert( retStmt.back() && "Created for statement is not valid");

		if (iteratorChanged) {
			/*
			 * in the case we replace the loop iterator with a temporary variable, we have to assign the final value
			 * of the iterator to the old variable so we don't change the semantics of the code:
			 *
			 * 		i.e: oldIter = ceil((cond-init)/step) * step + init;
			 */
			core::TypePtr iterType =
					(fakeInductionVar->getType()->getNodeType() == core::NT_RefType) ?
							core::static_pointer_cast<const core::RefType>(fakeInductionVar->getType())->getElementType() :
							fakeInductionVar->getType();

			core::ExpressionPtr cond = convFact.tryDeref(loopAnalysis.getCondExpr());
			core::ExpressionPtr step = convFact.tryDeref(loopAnalysis.getIncrExpr());

			core::FunctionTypePtr ceilTy = builder.functionType(
					toVector<core::TypePtr>(gen.getDouble()),
					gen.getDouble()
			);

			core::ExpressionPtr finalVal = builder.add(
					init, // init +
					builder.mul(
						builder.castExpr(iterType,// ( cast )
								builder.callExpr(
										gen.getDouble(),
										builder.literal(ceilTy, "ceil"),// ceil()
										stmtutils::makeOperation(// (cond-init)/step
												builder,
												builder.castExpr(gen.getDouble(),
														stmtutils::makeOperation(builder,
																cond, init, core::lang::BasicGenerator::Sub
														)// cond - init
												),
												builder.castExpr(gen.getDouble(), step),
												core::lang::BasicGenerator::Div
										)
								)
						),
						builder.castExpr(init->getType(), step)
					)
			);

			if (loopAnalysis.isInverted()) {
				// we have to restore the sign
				finalVal = builder.invertSign(finalVal);
			}

			// even though, might be the fist use of a value parameter variable.
			// needs to be wrapped
			if(llvm::isa<clang::ParmVarDecl> (iv)){
				auto fit = convFact.ctx.wrapRefMap.find(fakeInductionVar.as<core::VariablePtr>());
				if (fit == convFact.ctx.wrapRefMap.end()) {
					fit = convFact.ctx.wrapRefMap.insert(std::make_pair(fakeInductionVar.as<core::VariablePtr>(),
												  					   builder.variable(builder.refType(fakeInductionVar->getType())))).first;
				}
				core::ExpressionPtr wrap = fit->second;
				fakeInductionVar = wrap.as<core::ExpressionPtr>();
			}

			retStmt.push_back(builder.assign(fakeInductionVar, finalVal));
		}

	} catch (const analysis::LoopNormalizationError& e) {
		// The for loop cannot be normalized into an IR loop, therefore we create a while stmt
		stmtutils::StmtWrapper body = tryAggregateStmts(builder, Visit(forStmt->getBody()));

		clang::Stmt* initStmt = forStmt->getInit();
		if( initStmt ) {
			stmtutils::StmtWrapper init = Visit( forStmt->getInit() );
			std::copy(init.begin(), init.end(), std::back_inserter(retStmt));
		}

		if( clang::VarDecl* condVarDecl = forStmt->getConditionVariable() ) {
			assert(forStmt->getCond() == NULL &&
					"ForLoop condition cannot be a variable declaration and an expression");
			/*
			 * the for loop has a variable declared in the condition part, e.g.
			 *
			 * 		for(...; int a = f(); ...)
			 *
			 * to handle this kind of situation we have to move the declaration  outside the loop body inside a
			 * new context
			 */
			clang::Expr* expr = condVarDecl->getInit();
			condVarDecl->setInit(NULL); // set the expression to null (temporarely)
			core::StatementPtr declStmt = convFact.convertVarDecl(condVarDecl);
			condVarDecl->setInit(expr);// restore the init value

			assert(false && "ForStmt with a declaration of a condition variable not supported");
			retStmt.push_back( declStmt );
		}

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// analysis of loop structure failed, we have to build a while statement:
		//
		// 		for(init; cond; step) { body }
		//
		// Will be translated in the following while statement structure:
		//
		// 		{
		// 			init;
		// 			while(cond) {
		// 				{ body }
		// 				step;
		// 			}
		// 		}
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr condition;
		if (forStmt->getCond()){
			condition = utils::cast( convFact.convertExpr(forStmt->getCond()), builder.getLangBasic().getBool());
		}else {
			// we might not have condition, this is an infinite loop
			//    for (;;)
			condition =	convFact.builder.literal(std::string("true"), builder.getLangBasic().getBool());
		}

		core::StatementPtr whileStmt = builder.whileStmt(
				condition,
				forStmt->getInc() ?
				builder.compoundStmt( toVector<core::StatementPtr>(
						stmtutils::tryAggregateStmts(builder, body), convFact.convertExpr( forStmt->getInc() ) )
				)
				: stmtutils::tryAggregateStmts(builder, body)
		);

		// handle eventual pragmas attached to the Clang node
		retStmt.push_back( omp::attachOmpAnnotation(whileStmt, forStmt, convFact) );
		std::cerr << "foor loop converted in while" << std::endl;
		/*
		 * TODO: using insiemeCC we loose the preprocessor. 
		 *       find a solution for this
		clang::Preprocessor& pp = convFact.getCurrentPreprocessor();
		pp.Diag(forStmt->getLocStart(),
				pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Warning,
						std::string("For loop converted into while loop, cause: ") + e.what() )
		);
		*/
	}

	if (addDeclStmt) {
		retStmt = stmtutils::tryAggregateStmts(builder, retStmt);
	}

	return retStmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								IF STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitIfStmt(clang::IfStmt* ifStmt) {
	stmtutils::StmtWrapper retStmt;

	VLOG(2) << "{ Visit IfStmt }";
	LOG_STMT_CONVERSION(ifStmt, retStmt );

	core::StatementPtr thenBody = stmtutils::tryAggregateStmts( builder, Visit( ifStmt->getThen() ) );
	assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

	core::ExpressionPtr condExpr;
	if ( const clang::VarDecl* condVarDecl = ifStmt->getConditionVariable()) {
		assert(ifStmt->getCond() == NULL && "IfStmt condition cannot contains both a variable declaration and an expression");
		/*
		 * we are in the situation where a variable is declared in the if condition, i.e.:
		 *
		 * 		if(int a = exp) { }
		 *
		 * this will be converted into the following IR representation:
		 *
		 * 		{
		 * 			int a = exp;
		 * 			if(cast<bool>(a)){ }
		 * 		}
		 */
		core::StatementPtr declStmt = convFact.convertVarDecl(condVarDecl);
		retStmt.push_back(declStmt);

		assert(declStmt.isa<core::DeclarationStmtPtr>() && "declaring static variables within an if is not very polite");

		// the expression will be a cast to bool of the declared variable
		condExpr = builder.castExpr(gen.getBool(), declStmt.as<core::DeclarationStmtPtr>()->getVariable());

	} else {

		const clang::Expr* cond = ifStmt->getCond();
		assert( cond && "If statement with no condition." );

		condExpr = convFact.convertExpr(cond);

		if (core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign())) {
			// an assignment as condition is not allowed in IR, prepend the assignment operation
			retStmt.push_back( condExpr );
			// use the first argument as condition
			condExpr = builder.deref( condExpr.as<core::CallExprPtr>()->getArgument(0) );
		}
	}

	assert( condExpr && "Couldn't convert 'condition' expression of the IfStmt");

	if (!gen.isBool(condExpr->getType())) {
		// convert the expression to bool via the castToType utility routine
		condExpr = utils::cast(condExpr, gen.getBool());
	}

	core::StatementPtr elseBody = builder.compoundStmt();
	// check for else statement
	if ( Stmt* elseStmt = ifStmt->getElse()) {
		elseBody = stmtutils::tryAggregateStmts(builder, Visit(elseStmt));
	}
	assert(elseBody && "Couldn't convert 'else' body of the IfStmt");

	// adding the ifstmt to the list of returned stmts
	retStmt.push_back(builder.ifStmt(condExpr, thenBody, elseBody));

	// try to aggregate statements into a CompoundStmt if more than 1 statement has been created
	// from this IfStmt
	retStmt = tryAggregateStmts(builder, retStmt);

	// otherwise we introduce an outer CompoundStmt
	return retStmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							WHILE STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitWhileStmt(clang::WhileStmt* whileStmt) {
	stmtutils::StmtWrapper retStmt;

	VLOG(2)	<< "{ WhileStmt }";
	LOG_STMT_CONVERSION(whileStmt, retStmt);

	core::StatementPtr body = tryAggregateStmts( builder, Visit( whileStmt->getBody() ) );
	assert(body && "Couldn't convert body of the WhileStmt");

	core::ExpressionPtr condExpr;
	if ( clang::VarDecl* condVarDecl = whileStmt->getConditionVariable()) {
		assert(	!whileStmt->getCond() &&
				"WhileStmt condition cannot contains both a variable declaration and an expression"
			);

		/*
		 * we are in the situation where a variable is declared in the if condition, i.e.:
		 *
		 * 		while(int a = expr) { }
		 *
		 * this will be converted into the following IR representation:
		 *
		 * 		{
		 * 			int a = 0;
		 * 			while(a = expr){ }
		 * 		}
		 */
		clang::Expr* expr = condVarDecl->getInit();
		condVarDecl->setInit(NULL); // set the expression to null (temporarely)
		core::StatementPtr declStmt = convFact.convertVarDecl(condVarDecl);
		condVarDecl->setInit(expr); // set back the value of init value

		retStmt.push_back(declStmt);
		// the expression will be an a = expr
		assert( false && "WhileStmt with a declaration of a condition variable not supported");
	} else {
		const clang::Expr* cond = whileStmt->getCond();
		assert( cond && "WhileStmt with no condition.");

		condExpr = convFact.convertExpr(cond);

		if (core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign())) {
			// an assignment as condition is not allowed in IR, prepend the assignment operation
			retStmt.push_back( condExpr );
			// use the first argument as condition
			condExpr = builder.deref( condExpr.as<core::CallExprPtr>()->getArgument(0) );
		}
	}

	assert( condExpr && "Couldn't convert 'condition' expression of the WhileStmt");

	if (!gen.isBool(condExpr->getType())) {
		// convert the expression to bool via the castToType utility routine
		condExpr = utils::cast(condExpr, gen.getBool());
	}

	retStmt.push_back( builder.whileStmt(condExpr, body) );
	retStmt = tryAggregateStmts(builder, retStmt);

	// otherwise we introduce an outer CompoundStmt
	return retStmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							DO STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitDoStmt(clang::DoStmt* doStmt) {
	stmtutils::StmtWrapper retStmt;

	VLOG(2) << "{ DoStmt }";
	LOG_STMT_CONVERSION(doStmt, retStmt);

	core::CompoundStmtPtr body = builder.wrapBody( stmtutils::tryAggregateStmts( builder, Visit( doStmt->getBody() ) ) );
	assert(body && "Couldn't convert body of the WhileStmt");

	const clang::Expr* cond = doStmt->getCond();
	assert(cond && "DoStmt must have a condition.");

	core::ExpressionPtr condExpr = convFact.convertExpr(cond);
	assert(condExpr && "Couldn't convert 'condition' expression of the DoStmt");

	assert(!core::analysis::isCallOf(condExpr, builder.getLangBasic().getRefAssign()) &&
			"Assignment not allowd in condition expression");

	if (!gen.isBool(condExpr->getType())) {
		// convert the expression to bool via the castToType utility routine
		condExpr = utils::cast(condExpr, gen.getBool());
	}
	condExpr = convFact.tryDeref(condExpr);

	StatementList stmts;
	std::copy(body->getStatements().begin(), body->getStatements().end(), std::back_inserter(stmts));
	stmts.push_back(builder.whileStmt(condExpr, body));

	core::StatementPtr irNode = builder.compoundStmt(stmts);

	// handle eventual OpenMP pragmas attached to the Clang node
	core::StatementPtr annotatedNode = omp::attachOmpAnnotation(irNode, doStmt, convFact);

	// adding the WhileStmt to the list of returned stmts
	retStmt.push_back(annotatedNode);
	retStmt = stmtutils::tryAggregateStmts(builder, retStmt);

	// otherwise we introduce an outer CompoundStmt
	return retStmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							SWITCH STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitSwitchStmt(clang::SwitchStmt* switchStmt) {
	stmtutils::StmtWrapper retStmt;

	VLOG(2) << "{ Visit SwitchStmt }";
	LOG_STMT_CONVERSION(switchStmt, retStmt);

	core::ExpressionPtr condExpr;

	if ( const clang::VarDecl* condVarDecl = switchStmt->getConditionVariable()) {
		assert(	!switchStmt->getCond() &&
				"SwitchStmt condition cannot contains both a variable declaration and an expression");

		core::StatementPtr declStmt = convFact.convertVarDecl(condVarDecl);
		retStmt.push_back(declStmt);

		assert(declStmt.isa<core::DeclarationStmtPtr>() && 
				" declaring a static variable in a switch condition??? you must have a very good reason to do this!!!");

		// the expression will be a reference to the declared variable
		condExpr = declStmt.as<core::DeclarationStmtPtr>()->getVariable();
	} else {
		const clang::Expr* cond = switchStmt->getCond();
		assert(cond && "SwitchStmt with no condition.");
		condExpr = convFact.tryDeref(convFact.convertExpr(cond));

		// we create a variable to store the value of the condition for this switch
		core::VariablePtr condVar = builder.variable(gen.getInt4());
		// int condVar = condExpr;
		core::DeclarationStmtPtr declVar =
		builder.declarationStmt(condVar, builder.castExpr(gen.getInt4(), condExpr));
		retStmt.push_back(declVar);

		condExpr = condVar;
	}

	assert( condExpr && "Couldn't convert 'condition' expression of the SwitchStmt");

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// this Switch stamtement has a body, i.e.:
	//
	// 		switch(e) { { body } case x:...  }
	//
	// As the IR doens't allow a body to be represented inside the switch stmt we bring this
	// code outside after the declaration of the eventual conditional variable.
	//
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	vector<core::SwitchCasePtr> cases;
	// marks the beginning of a case expression
	vector<std::pair<core::LiteralPtr, size_t>> caseExprs;
	size_t defaultStart = 0;
	// collected statements that will be part of the next case statement
	vector<core::StatementPtr> caseStmts;
	bool caseStart = false;
	bool breakEncountred = false;
	bool isDefault = false;
	core::CompoundStmtPtr defStmt = builder.compoundStmt();

	clang::CompoundStmt* compStmt = dyn_cast<clang::CompoundStmt>(switchStmt->getBody());assert(
			compStmt && "Switch statements doesn't contain a compound stmt");

	// lambda function which creates a case stmt using the accumulated statements
	auto addCase =
			[this, &cases, &caseStmts, &caseExprs, &defaultStart, &defStmt, &isDefault, &builder]() -> void {
				std::for_each(caseExprs.begin(), caseExprs.end(),
						[ &cases, &caseStmts, &builder, this ](const std::pair<core::LiteralPtr,size_t>& curr) {
							size_t size = caseStmts.size() - curr.second;
							std::vector<core::StatementPtr> stmtList(size);
							std::copy(caseStmts.begin() + curr.second, caseStmts.end(), stmtList.begin());
							cases.push_back(
									builder.switchCase(curr.first, builder.wrapBody(stmtutils::tryAggregateStmts( this->builder, stmtList )))
							);
						}
				);
				if ( isDefault ) {
					std::vector<core::StatementPtr> stmtList(caseStmts.size() - defaultStart);
					std::copy(caseStmts.begin() + defaultStart, caseStmts.end(), stmtList.begin());
					defStmt = builder.wrapBody(stmtutils::tryAggregateStmts( builder, stmtList ));
				}
			};

	for (auto it = compStmt->body_begin(), end = compStmt->body_end(); it != end; ++it) {
		clang::Stmt* curr = *it;
		// statements which are before the first case.
		if (!caseStart && !llvm::isa<clang::SwitchCase>(curr)) {
			stmtutils::StmtWrapper visitedStmt = this->Visit(curr);
			// append these statements before the switch statement
			std::copy(visitedStmt.begin(), visitedStmt.end(), std::back_inserter(retStmt));
			continue;
		}
		// we encounter a case statement
		caseStart = true;
		while (clang::CaseStmt * caseStmt = dyn_cast<clang::CaseStmt>(curr)) {

			// make sure case expression is a literal
			core::ExpressionPtr caseExpr = this->convFact.convertExpr(caseStmt->getLHS());
			if (caseExpr->getNodeType() == core::NT_CastExpr) {
				core::CastExprPtr cast = static_pointer_cast<core::CastExprPtr>(caseExpr);
				if (cast->getSubExpression()->getNodeType() == core::NT_Literal) {
					core::LiteralPtr literal = static_pointer_cast<core::LiteralPtr>(cast->getSubExpression());
					caseExpr = builder.literal(cast->getType(), literal->getValue());
				}
			}

			core::LiteralPtr caseLiteral;
			if (caseExpr->getNodeType() != core::NT_Literal){
				// clang casts the literal to fit the condition type... and is not a literal anymore
				// it might be a scalar cast, we retrive the literal
				//FIXME: checks here?
				caseLiteral= caseExpr.as<core::CallExprPtr>()->getArgument(0).as<core::LiteralPtr>();
			}
			else{
				//caseLiteral = static_pointer_cast<core::LiteralPtr>(caseExpr);
				caseLiteral = caseExpr.as<core::LiteralPtr>();
			}

			caseExprs.push_back(std::make_pair(caseLiteral, caseStmts.size()));

			core::StatementPtr subStmt;
			if ( const clang::Expr* rhs = caseStmt->getRHS()) {
				assert( !caseStmt->getSubStmt() && "Case stmt cannot have both a RHS and and sub statement.");
				subStmt = this->convFact.convertExpr(rhs);
			} else if ( clang::Stmt* sub = caseStmt->getSubStmt()) {
				// if the sub statement is a case, skip until the end of the loop
				if (llvm::isa<clang::SwitchCase>(sub)) {
					curr = sub;
					continue;
				}

				subStmt = stmtutils::tryAggregateStmts(this->builder, this->Visit(const_cast<clang::Stmt*>(sub)));
				/*
				 * if the sub-statement is a BreakStmt we have to replace it with a noOp and remember to reset the
				 * caseStmts
				 */
				if (subStmt->getNodeType() == core::NT_BreakStmt) {
					subStmt = builder.getNoOp();
					breakEncountred = true;
				}
			}

			// add the statements defined by this case to the list of statements which has to executed by this case
			caseStmts.push_back(subStmt);
			break;
		}

		if ( const clang::DefaultStmt* defCase = dyn_cast<const clang::DefaultStmt>(curr)) {
			isDefault = true;
			defaultStart = caseStmts.size();

			core::StatementPtr subStmt =
			stmtutils::tryAggregateStmts( builder, Visit( const_cast<clang::Stmt*>(defCase->getSubStmt())) );

			if (subStmt->getNodeType() == core::NT_BreakStmt) {
				subStmt = builder.getNoOp();
				breakEncountred = true;
			}
			caseStmts.push_back(subStmt);
		}

		if (llvm::isa<const clang::ContinueStmt>(curr) || llvm::isa<const clang::ReturnStmt>(curr)) {
			core::StatementPtr subStmt = stmtutils::tryAggregateStmts(builder, Visit(const_cast<clang::Stmt*>(curr)));
			breakEncountred = true;
			caseStmts.push_back(subStmt);
		}
		/*
		 * if the current statement is a break, or we encountred a break in the current case we
		 * create a new case and add to the list of cases for this switch statement
		 */
		if (breakEncountred || llvm::isa<const clang::BreakStmt>(curr)) {
			addCase();
			// clear the list of statements collected until now
			caseExprs.clear();
			caseStmts.clear();

			breakEncountred = false;
		} else if (!llvm::isa<clang::SwitchCase>(curr)) {
			stmtutils::StmtWrapper visitedStmt = Visit( const_cast<clang::Stmt*>(curr));
			std::copy(visitedStmt.begin(), visitedStmt.end(), std::back_inserter(caseStmts));
		}
	}
	// we still have some statement pending
	if (!caseStmts.empty()) {
		addCase();
	}

	// initialize the default case with an empty compoundstmt
	core::StatementPtr irNode = builder.switchStmt(condExpr, cases, defStmt);

	// handle eventual OpenMP pragmas attached to the Clang node
	core::StatementPtr annotatedNode = omp::attachOmpAnnotation(irNode, switchStmt, convFact);

	// Appends the switchstmt to the current list of stmt
	retStmt.push_back(annotatedNode);
	retStmt = tryAggregateStmts(builder, retStmt);

	return retStmt;
}

/*
 * as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error
 * in the case the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the
 * visitor is not called on his subnodes
 */
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitSwitchCase(clang::SwitchCase* caseStmt) {
	assert(false && "Visitor is visiting a 'case' stmt");
	return stmtutils::StmtWrapper();
}

stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitBreakStmt(clang::BreakStmt* breakStmt) {
	return stmtutils::StmtWrapper(builder.breakStmt());
}

stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitContinueStmt(clang::ContinueStmt* contStmt) {
	return stmtutils::StmtWrapper(builder.continueStmt());
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							COMPOUND STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {

	core::StatementPtr retIr;
	LOG_STMT_CONVERSION(compStmt, retIr);

	bool hasReturn = false;

	vector<core::StatementPtr> stmtList;
	std::for_each(compStmt->body_begin(), compStmt->body_end(), [ &stmtList, this, &hasReturn ] (Stmt* stmt) {
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// A compoundstmt can contain declaration statements.This means that a clang
		// DeclStmt can be converted in multiple  StatementPtr because an initialization
		// list such as: int a,b=1; is converted into the following sequence of statements:
		//
		// 		int<a> a = 0; int<4> b = 1;
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		stmtutils::StmtWrapper convertedStmt;

		if(dyn_cast<clang::ReturnStmt>(stmt)) {
			hasReturn = true;
		}

		convertedStmt = Visit(stmt);
		copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));

	});

	retIr = builder.compoundStmt(stmtList);

	// check for datarange pragma
	attatchDatarangeAnnotation(retIr, compStmt, convFact);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							NULL STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitNullStmt(clang::NullStmt* nullStmt) {
	//TODO: Visual Studio 2010 fix: && removed
	core::StatementPtr retStmt = builder.getNoOp();
	return retStmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							GOTO  STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitGotoStmt(clang::GotoStmt* gotoStmt) {
	clang::Preprocessor& pp = convFact.getCurrentPreprocessor();
	pp.Diag(
			gotoStmt->getLocStart(),
			pp.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
					"Gotos are not handled by the Insieme compielr"));
	assert(false);
	return stmtutils::StmtWrapper();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							  STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::StmtConverter::VisitStmt(clang::Stmt* stmt) {
	assert(false && "this code looks malform and no used");
	std::for_each(stmt->child_begin(), stmt->child_end(), [ this ] (clang::Stmt* stmt) {
			this->Visit(stmt);
	});
	return stmtutils::StmtWrapper();
}

//---------------------------------------------------------------------------------------------------------------------
//							CLANG STMT CONVERTER
//							teakes care of C nodes
//							C nodes implemented in base: StmtConverter
//---------------------------------------------------------------------------------------------------------------------

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::CStmtConverter::Visit(clang::Stmt* stmt) {
	VLOG(2) << "C";

	stmtutils::StmtWrapper retStmt = StmtVisitor<CStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);

	// print diagnosis messages
	convFact.printDiagnosis(stmt->getLocStart());

	// build the wrapper for single statements
	if ( retStmt.isSingleStmt() ) {
		core::StatementPtr irStmt = retStmt.getSingleStmt();

		// Deal with mpi pragmas
		mpi::attachMPIStmtPragma(irStmt, stmt, convFact);

		// Deal with transfromation pragmas
		irStmt = pragma::attachPragma(irStmt,stmt,convFact).as<core::StatementPtr>();

		// Deal with omp pragmas
		if ( irStmt->getAnnotations().empty() )
			return omp::attachOmpAnnotation(irStmt, stmt, convFact);

		return stmtutils::StmtWrapper(irStmt);
	}
	return retStmt;
}

}
}
}

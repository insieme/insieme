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

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backend.h"
/**************************************************************************************************
 * INCLUDE THE BACKENDS 
 * this is needed for compiling this class as templates are used and the template specializations
 * for Sets and Maps are needed in order to compile this translation unit 
 *************************************************************************************************/
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "isl/flow.h"

#define POLY_BACKEND ISL

/*************************************************************************************************/

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/numeric_cast.h"
#include <stack>

#define AS_STMT_ADDR(addr) static_address_cast<const Statement>(addr)
#define AS_EXPR_ADDR(addr) static_address_cast<const Expression>(addr)

#define STACK_SIZE_GUARD \
	auto checkPostCond = [&](size_t stackInitialSize) -> void { 	 \
		assert(regionStmts.size() == stackInitialSize);				 \
	};																 \
	FinalActions __check_stack_size( std::bind(checkPostCond, regionStmts.size()) );

namespace {

using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::analysis;
using namespace insieme::analysis::poly;
using namespace insieme::analysis::scop;

void postProcessSCoP(const NodeAddress& scop, AddressList& scopList);

/**************************************************************************************************
 * Expection which is thrown when a particular tree is defined to be not a static control part. This
 * exception has to be forwarded until the root containing this node which has to be defined as a
 * non ScopRegion
 * 
 * Because this exception is only used within the implementation of the ScopRegion visitor, it is
 * defined in the anonymous namespace and therefore not visible outside this translation unit.
 *************************************************************************************************/
class NotASCoP : public std::exception {
	NodePtr root;
	std::string msg;
public:
	NotASCoP( const NodePtr& root ) : root(root) { 
		std::ostringstream ss;
		ss << "Node '" << *root << "' is not a Static Control Part";
		msg = ss.str();
	}

	virtual const char* what() const throw() { return msg.c_str(); }
	
	const NodePtr& node() const { return root; }

	virtual ~NotASCoP() throw() { }
};

class DiscardSCoPException: public NotASCoP {
	const ExpressionPtr expr;
public: 
	DiscardSCoPException(const NodePtr& root, const ExpressionPtr& expr) : 
		NotASCoP(root), expr(expr) { }

	const ExpressionPtr& expression() const { return expr; }

	virtual ~DiscardSCoPException() throw() { }
};

/**************************************************************************************************
 * Extract constraints from a conditional expression. This is used for determining constraints for 
 * if and for statements. 
 *************************************************************************************************/
IterationDomain extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();
	assert (cond->getNodeType() == NT_CallExpr);

	const CallExprPtr& callExpr = static_pointer_cast<const CallExpr>(cond);
	if ( mgr.getLangBasic().isIntCompOp(callExpr->getFunctionExpr()) || 
		 mgr.getLangBasic().isUIntCompOp(callExpr->getFunctionExpr()) ) 
	{
		assert(callExpr->getArguments().size() == 2 && "Malformed expression");

		// First of all we check whether this condition is a composed by multiple conditions
		// connected through || or && statements 
		BasicGenerator::Operator&& op = 
			mgr.getLangBasic().getOperator( static_pointer_cast<const Literal>(callExpr->getFunctionExpr()) ); 

		switch (op) {
		case BasicGenerator::LOr:
		case BasicGenerator::LAnd:
			{
				IterationDomain&& lhs = extractFromCondition(iv, callExpr->getArgument(0));
				IterationDomain&& rhs = extractFromCondition(iv, callExpr->getArgument(1));

				if (op == BasicGenerator::LAnd)	{ return lhs && rhs; }
				else 							{ return lhs || rhs; }
			}
		case BasicGenerator::LNot:
			 return !extractFromCondition(iv, callExpr->getArgument(0));
		default:
			break;
		}
		// A constraints is normalized having a 0 on the right side, therefore we build a
		// temporary expression by subtracting the rhs to the lhs, Example: 
		//
		// if (a<b) { }    ->    if( a-b<0 ) { }
		try {
			IRBuilder builder(mgr);
			ExpressionPtr&& expr = builder.callExpr( 
					mgr.getLangBasic().getSignedIntSub(), callExpr->getArgument(0), callExpr->getArgument(1) 
				);
			AffineFunction af(iv, expr);
			// Determine the type of this constraint
			Constraint<AffineFunction>::Type type;
			switch (op) {
				case BasicGenerator::Eq: type = Constraint<AffineFunction>::EQ; break;
				case BasicGenerator::Ne: type = Constraint<AffineFunction>::NE; break;
				case BasicGenerator::Lt: type = Constraint<AffineFunction>::LT; break;
				case BasicGenerator::Le: type = Constraint<AffineFunction>::LE; break;
				case BasicGenerator::Gt: type = Constraint<AffineFunction>::GT; break;
				case BasicGenerator::Ge: type = Constraint<AffineFunction>::GE; break;
				default:
					assert(false && "Operation not supported!");
			}
			return IterationDomain( makeCombiner( Constraint<AffineFunction>(af, type) ) );
		} catch (arithmetic::NotAFormulaException&& e) { 
			throw NotASCoP(e.getExpr()); 
		} catch (NotAffineExpr&& e) { 
			throw NotASCoP(cond);
		}
	}
	// LOG(ERROR) << "Condition Expression not supported: " << *cond;
	throw NotASCoP(cond);
}

IterationVector markAccessExpression(const ExpressionPtr& expr) {
	// If the expression is already annotated because shared, then we simply return the iteration
	// vector precendly computed (because by definition it has to be the same)
	if (expr->hasAnnotation(scop::AccessFunction::KEY)) {
		return expr->getAnnotation(scop::AccessFunction::KEY)->getIterationVector();
	}

	// otherwise we have to build an affine function from the IR access expression
	try {
		IterationVector it;
		expr->addAnnotation( std::make_shared<AccessFunction>( it, AffineFunction(it, expr)) );
		return it;
	} catch(NotAffineExpr&& e) { 
		throw NotASCoP(expr);
	} catch(arithmetic::NotAFormulaException&& e) {
		throw NotASCoP(e.getExpr()); 
	}	 
}

SubScopList toSubScopList(const IterationVector& iterVec, const AddressList& scops) {
	SubScopList subScops;
	for_each(scops.begin(), scops.end(), [&](const NodeAddress& cur) { 
			subScops.push_back( SubScop(cur, IterationDomain(iterVec)) );
	});
	return subScops;
}

//===== ScopVisitor ================================================================================

struct ScopVisitor : public IRVisitor<IterationVector, Address> {

	AddressList& scopList;
	AddressList subScops;

	// Stack utilized to keep track of statements which are inside a SCoP.
	// because not all the compound statements of the IR are annotated by a SCoPRegion annotation,
	// we need to have a way to collect statements which can be inside nested scopes.
	typedef std::stack<ScopRegion::StmtVect> RegionStmtStack;
	RegionStmtStack regionStmts;

	ScopVisitor(AddressList& scopList) : 
		IRVisitor<IterationVector, Address>(false), scopList(scopList) 
	{
		regionStmts.push( RegionStmtStack::value_type() );
	}

	
	// Visit the body of a SCoP. This requires to collect the iteration vector for the body and
	// eventual ref access statements existing in the body. For each array access we extract the
	// equality constraint used for accessing each dimension of the array and store it in the
	// AccessFunction annotation.
	RefList collectRefs(IterationVector& iterVec, const StatementAddress& body) { 
		RefList&& refs = collectDefUse( body.getAddressedNode() );

		// For now we only consider array access. 
		//
		// FIXME: In the future also scalars should be properly handled using technique like scalar
		// arrays and so forth
		std::for_each(refs.begin(), refs.end(),
			[&](const RefPtr& cur) { 

				switch(cur->getType()) {
				case Ref::ARRAY:
				{
					const ArrayRef& arrRef = static_cast<const ArrayRef&>(*cur); 
					const ArrayRef::ExpressionList& idxExprs = arrRef.getIndexExpressions();
					std::for_each(idxExprs.begin(), idxExprs.end(), 
						[&](const ExpressionAddress& cur) { 
							iterVec = merge(iterVec, markAccessExpression(cur));
					});
					break;
				}
				case Ref::SCALAR:
				case Ref::CALL:
				case Ref::MEMBER:
					break;
				default:
					LOG(WARNING) << "Reference of type " << Ref::refTypeToStr(cur->getType()) << " not handled!";
				}
			});
		return refs;
	}

	IterationVector visitStmt(NodeAddress addr) {
		STACK_SIZE_GUARD;

		assert(subScops.empty());

		while(addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr) {
			addr = addr.getAddressOfChild(1); // sub-statement or expression
		}
		IterationVector&& ret = visit(addr);

		if ( addr->getNodeType() == NT_CallExpr) {
			CallExprAddress callExpr = static_address_cast<const CallExpr>(addr);
			if(callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
				return ret;
			}
		}

		if ( subScops.empty() ) {
			// this is a single stmt, therefore we can collect the references inside
			RefList&& refs = collectRefs(ret, AS_STMT_ADDR(addr));
			// Add this statement to the scope for the parent node 
			regionStmts.top().push_back( ScopRegion::Stmt(AS_STMT_ADDR(addr), refs) );
		} else {
			// the substatement is a 
			regionStmts.top().push_back( ScopRegion::Stmt(AS_STMT_ADDR(addr), RefList()) );
		}
		return ret;
	}

	/**********************************************************************************************
	 * Visit of If Statements: 
	 * Visits the then and else body checking whether they are SCoPs. In the case both the branches
	 * are SCoPs the condition is evaluated and a constraint created out of it. Two annotations will
	 * be then created, one with the positive condition and the second one with the negated
	 * condition which will be attached respectively to the then and the else body of the if
	 * statement. In the case one of the two branches is not a SCoP, the SCoP branch is inserted in
	 * the list of root scops (scopList) and the NotAScop exception thrown to the parent node. 
	 *********************************************************************************************/
	IterationVector visitIfStmt(const IfStmtAddress& ifStmt) {
		STACK_SIZE_GUARD;

		IterationVector ret, saveThen, saveElse;
		bool isThenSCOP = true, isElseSCOP = true;
		
		if ( ifStmt->hasAnnotation(ScopRegion::KEY) ) {
			ScopRegion& ann = *ifStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { throw NotASCoP(ifStmt.getAddressedNode()); }

			// if the SCopRegion annotation is already attached, it means we already visited this
			// function, therefore we can return the iteration vector already precomputed 
			subScops.push_back( ifStmt );
			return ann.getIterationVector();
		}

		ExpressionAddress condAddr = ifStmt->getCondition();
		StatementAddress  thenAddr = ifStmt->getThenBody();
		StatementAddress  elseAddr = ifStmt->getElseBody();

		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions faThen( [&] () -> void { regionStmts.pop(); } );

		try {
			subScops.clear();
			// check the then body
			saveThen = visitStmt(thenAddr);
		} catch (NotASCoP&& e) { isThenSCOP = false; }

		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions faElse( [&] () -> void { regionStmts.pop(); } );

		try {
			subScops.clear();
			// check the else body
			saveElse = visitStmt(elseAddr);
		} catch (NotASCoP&& e) { isElseSCOP = false; }
	
		if ( isThenSCOP && !isElseSCOP ) {
			// then is a root of a ScopRegion, we add it to the list of scops
			postProcessSCoP( thenAddr, scopList );
		}

		if ( !isThenSCOP && isElseSCOP ) {
			// else is a root of a ScopRegion, we add it to the list of scops
			postProcessSCoP( elseAddr, scopList );
		}

		// if either one of the branches is not a ScopRegion it means the if statement is not a
		// ScopRegion, therefore we can re-throw the exception and invalidate this region
		if (!(isThenSCOP && isElseSCOP)) {
			throw NotASCoP( ifStmt.getAddressedNode() ); 
		}

		// reset the value of the iteration vector 
		ret = IterationVector();

		// check the condition expression
		IterationDomain&& cond = extractFromCondition(ret, condAddr.getAddressedNode());
	
		// At this point we are sure that both the then, else body are SCoPs and the condition of
		// this If statement is also an affine linear function. 
		ret = merge(ret, merge(saveThen, saveElse));
	
		ScopRegion::StmtVect ifScopStmts;

		assert(regionStmts.top().size() == 1);
		ifScopStmts.push_back( regionStmts.top().front() );

		// we saved the else body statements, therefore we can pop the record we allocated for it
		regionStmts.pop();
		faElse.setEnabled(false);

		assert(regionStmts.top().size() == 1);
		ifScopStmts.push_back( regionStmts.top().front() );

		// we saved the then body statements, therefore we can pop the record we allocated for it		
		regionStmts.pop();
		faThen.setEnabled(false);

		ifStmt->addAnnotation( 
			std::make_shared<ScopRegion>(
				ifStmt.getAddressedNode(), 
				ret, 
				IterationDomain(ret), 
				ScopRegion::StmtVect( ifScopStmts.rbegin(),ifScopStmts.rend() ), 
				SubScopList( { SubScop(thenAddr, cond), SubScop(elseAddr, !cond) } )
			)
		);
		
		// Checkpost conditions 
		assert (ifStmt->hasAnnotation(ScopRegion::KEY) && 
				ifStmt->getThenBody()->hasAnnotation(ScopRegion::KEY) && 
				ifStmt->getElseBody()->hasAnnotation(ScopRegion::KEY) && 
				"IfStmt Post-Conditions check failed"
			);
		subScops.push_back( ifStmt );
		return ret;
	}
	

	/**********************************************************************************************
	 * SwitchStmt: for each of the cases of the switch statement we create a constraint enforcing
	 * conditionExpr - caseExpr == 0. The constraint for the default case is created by a
	 * conjunction of the negated case constraints. 
	 *********************************************************************************************/
	IterationVector visitSwitchStmt(const SwitchStmtAddress& switchStmt) {
		STACK_SIZE_GUARD;
		
		if ( switchStmt->hasAnnotation(ScopRegion::KEY) ) {
			ScopRegion& ann = *switchStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { throw NotASCoP(switchStmt.getAddressedNode()); }

			// if the SCopRegion annotation is already attached, it means we already visited this
			// compoundstmt, therefore we can return the iteration vector already precomputed 
			subScops.push_back(switchStmt);
			return ann.getIterationVector();
		}

		typedef std::vector<SwitchCasePtr> CaseList;
		typedef std::vector<IterationVector> IterationVectorList;

		IterationVector ret;
		
		bool isSCoP = true;
		IRBuilder builder( switchStmt->getNodeManager() );
			
		SubScopList scops;
		IterationDomain defaultCons(ret); // FIXME: replace with universe constraint
	
		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions fa( [&] () -> void { regionStmts.pop(); } );

		SwitchCasesAddress cases = switchStmt->getCases();
		for(auto it = cases.begin(); it != cases.end(); ++it) {
			SwitchCaseAddress curCase = *it;

			// get the addess of the expression of this case stmt 
			ExpressionAddress exprAddr = curCase->getGuard();

			ExpressionPtr&& expr =
				builder.callExpr(
					builder.getLangBasic().getOperator( 
						switchStmt->getSwitchExpr()->getType(), BasicGenerator::Sub
					),
					switchStmt->getSwitchExpr() /* switchExpr*/, 
					exprAddr.getAddressedNode()
				);

			try {
				IterationVector iv;

				StatementAddress stmtAddr = curCase->getBody();

				subScops.clear();
					// build an address for the expression and the statement 
				ret = merge(iv, visitStmt(stmtAddr));

				// If the case statement is not a compound statement, the ScopRegion annotation will
				// not be inserted by default. Therefore we add the annotation to simplify the
				// resolution of the SCoP when the analysis is invoked 
				assert (stmtAddr->hasAnnotation(ScopRegion::KEY)); 

				IterationDomain caseCons( 
						makeCombiner(Constraint<AffineFunction>(AffineFunction(ret, expr), Constraint<AffineFunction>::EQ)) 
					);
				defaultCons &= !caseCons;

				// Add this statement to the subScops
				scops.push_back( SubScop(stmtAddr, caseCons) );

			} catch (NotASCoP&& e) { isSCoP = false; }
		} 

		if (switchStmt->getDefaultCase()) {
			try {
				StatementAddress defAddr = switchStmt->getDefaultCase();
				subScops.clear();

				IterationVector&& iv = visitStmt(defAddr);

				// If the case statement is not a compound statement, the ScopRegion annotation will
				// not be inserted by default. Therefore we add the annotation to simplify the
				// resolution of the SCoP when the analysis is invoked 
				assert (defAddr->hasAnnotation(ScopRegion::KEY)); 

				ret = merge(ret, iv);
				scops.push_back( SubScop(defAddr, defaultCons) );
			} catch (NotASCoP&& e) { isSCoP = false; }
		}

		if ( !isSCoP ) {
			// Add the entry points to the ScopList 
			for(size_t caseID = 0; caseID < cases.size(); ++caseID) {
				// get the addess of the expression of this case stmt 
				StatementAddress caseStmtAddr = cases[caseID]->getBody();

				if( caseStmtAddr->hasAnnotation(ScopRegion::KEY) ) {
					postProcessSCoP(caseStmtAddr, scopList) ;

				}

			}
			throw NotASCoP( switchStmt.getAddressedNode() );
		}

		switchStmt->addAnnotation( 
				std::make_shared<ScopRegion>(
					switchStmt.getAddressedNode(), 
					ret, 
					IterationDomain(ret), 
					regionStmts.top(), 
					scops
				) 
			);

		subScops.clear();
		subScops.push_back(switchStmt);

		return ret;
	}

	IterationVector visitForStmt(const ForStmtAddress& forStmt) {
		STACK_SIZE_GUARD;
	
		assert(subScops.empty());

		// if we already visited this forStmt, just return the precomputed iteration vector 
		if (forStmt->hasAnnotation(ScopRegion::KEY)) {
			ScopRegion& ann = *forStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { throw NotASCoP(forStmt.getAddressedNode()); }

			// return the cached value
			subScops.push_back(forStmt);
			return ann.getIterationVector();
		}

		// Create a new scope for region stmts
		regionStmts.push( RegionStmtStack::value_type() );

		{
			// remove element from the stack of statements from all the exit paths 
			FinalActions fa( [&] () -> void { regionStmts.pop(); subScops.clear(); } );

			IterationVector&& bodyIV = visitStmt( forStmt->getBody() ), ret;

			ForStmtPtr forPtr = forStmt.getAddressedNode();
			ret.add( Iterator(forPtr->getIterator()) );
			
			NodeManager& mgr = forStmt->getNodeManager();
			IRBuilder builder(mgr);

			try {
				ret = merge(ret, bodyIV);	

				// We assume the IR loop semantics to be the following: 
				// i: lb...ub:s 
				// which spawns a domain: lw <= i < ub exists x in Z : lb + x*s = i
				// Check the lower bound of the loop
				AffineFunction lb(ret, 
						builder.callExpr(mgr.getLangBasic().getSignedIntSub(),
								forPtr->getIterator(), forPtr->getStart())
					);

				// check the upper bound of the loop
				AffineFunction ub(ret, builder.callExpr(mgr.getLangBasic().getSignedIntSub(),
							forPtr->getIterator(), forPtr->getEnd())
						);
				// set the constraint: iter >= lb && iter < ub

				poly::ConstraintCombinerPtr<AffineFunction>&& loopBounds = 
					Constraint<AffineFunction>(lb, Constraint<AffineFunction>::GE) and 
					Constraint<AffineFunction>(ub, Constraint<AffineFunction>::LT);

				// extract the Formula object 
				const ExpressionPtr& step = forStmt.getAddressedNode()->getStep();
				arithmetic::Formula&& formula = arithmetic::toFormula( step );
				
				if ( !(formula.isLinear() || formula.isOne()) && !formula.isConstant() ) 
					throw NotAffineExpr( step );

				if ( !formula.isOne() ) {
					// Add a new constraint to the loop bound which satisfy the step 

					// We add a new dimension to the iteration vector (an unbounded parameter) and
					// set a new constraint in the form : exist(a: step*a = i) 
					
					assert(formula.isConstant() && "Stride value of for loop is not constant.");

					VariablePtr existenceVar = IRBuilder(mgr).variable(mgr.getLangBasic().getInt4());
					ret.add( Iterator( existenceVar, true ) );

					AffineFunction existenceCons( ret );
					existenceCons.setCoeff( existenceVar, -formula.getTerms().front().second );
					existenceCons.setCoeff( forPtr->getIterator(), 1 );

					// WE still have to make sure the loop iterator assume the value given by the
					// loop lower bound, therefore i == lb
					AffineFunction lowerBound( ret, 
						builder.callExpr(mgr.getLangBasic().getSignedIntSub(), forPtr->getIterator(),
							forPtr->getStart())
						);

					loopBounds = loopBounds and 
						(Constraint<AffineFunction>(lowerBound, Constraint<AffineFunction>::EQ) or 
						 Constraint<AffineFunction>( existenceCons, Constraint<AffineFunction>::EQ ) );
				}

				IterationDomain cons( loopBounds );

				forStmt->addAnnotation( 
						std::make_shared<ScopRegion>(
							forStmt.getAddressedNode(),
							ret, 
							cons, 
							regionStmts.top(), 
							toSubScopList(ret, subScops)
						) 
					); 
				
				fa.setEnabled(false);

				regionStmts.pop();
				subScops.clear();

				// add this statement as a subscop
				subScops.push_back(forStmt);
			
			} catch (NotAffineExpr&& e) { 
				// one of the expressions are not affine constraints, therefore we set this loop to be a
				// non ScopRegion
				throw NotASCoP( forStmt.getAddressedNode() );

			}catch(arithmetic::NotAFormulaException&& e) {
				throw NotASCoP( e.getExpr() ); 
			}	 
			
			return ret;
		}
	}

	/*************************************************************************************************
	 * While stmts cannot be represented in the polyhedral form (at least in the general case). In
	 * the future we may develop a more advanced analysis capable of representing while loops in the
	 * polyhedral model 
	 ************************************************************************************************/
	IterationVector visitWhileStmt(const WhileStmtAddress& whileStmt) { 
		throw NotASCoP( whileStmt.getAddressedNode() );
	}

	IterationVector visitCompoundStmt(const CompoundStmtAddress& compStmt) {
		STACK_SIZE_GUARD;

		IterationVector ret;
		bool isSCOP = true;
		AddressList scops;
	
		assert(subScops.empty());

		if ( compStmt->hasAnnotation(ScopRegion::KEY) ) {
			ScopRegion& ann = *compStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { throw NotASCoP(compStmt.getAddressedNode()); }

			// if the SCopRegion annotation is already attached, it means we already visited this
			// compoundstmt, therefore we can return the iteration vector already precomputed 
			subScops.push_back(compStmt);
			return ann.getIterationVector();
		}

		regionStmts.push( RegionStmtStack::value_type() );

		FinalActions fa( [&] () -> void { regionStmts.pop(); } );

		for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
			// make sure at every iteration the stack size is not growing within this compound stmt
			STACK_SIZE_GUARD;
			try {
				// clear Sub scops
				subScops.clear();
				NodeAddress&& nodeAddr = compStmt.getAddressOfChild(i);
				ret = merge(ret, visitStmt( nodeAddr ));
				// copy the sub spawned scops 
				std::copy(subScops.begin(), subScops.end(), std::back_inserter(scops));
			} catch(NotASCoP&& e) { 
				isSCOP = false; 
			}
		}

		// make the SCoPs available for the parent node 	
		subScops = scops;

		if (!isSCOP) { 

			subScops.clear();

			// FIXME: Use the subScops 
			// one of the statements in this compound statement broke a ScopRegion therefore we add
			// to the scop list the roots for valid ScopRegions inside this compound statement 
			for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
				StatementAddress addr = AS_STMT_ADDR(compStmt.getAddressOfChild(i));
				
				// Get rid of marker statements 
				while(addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr) {
					addr = AS_STMT_ADDR(addr.getAddressOfChild(1));
				}

				if (addr->hasAnnotation(ScopRegion::KEY)) { 
					postProcessSCoP( addr, scopList );
				}
			}
			throw NotASCoP(compStmt.getAddressedNode()); 
		}

		// Mark this CompoundStmts because it is a Scop
		compStmt->addAnnotation( 
			std::make_shared<scop::ScopRegion>(
				compStmt.getAddressedNode(),
				ret, 
				IterationDomain(ret), 
				regionStmts.top(), 
				toSubScopList(ret, subScops)
			) 
		);

		subScops.clear();
		subScops.push_back( compStmt );

		return ret;
	}

	IterationVector visitMarkerStmt(const MarkerStmtAddress& mark) {
		return visit( mark->getSubStatement() );
	}

	IterationVector visitMarkerExpr(const MarkerExprAddress& mark) {
		return visit( mark->getSubExpression() );
	}

	IterationVector visitLambda(const LambdaAddress& lambda) {	
		STACK_SIZE_GUARD;
		assert( subScops.empty() );

		if ( lambda->hasAnnotation(ScopRegion::KEY) ) {
			// if the SCopRegion annotation is already attached, it means we already visited this
			// function, therefore we can return the iteration vector already precomputed 
			subScops.push_back( lambda );
			return lambda->getAnnotation(ScopRegion::KEY)->getIterationVector();
		}

		IterationVector bodyIV;
		// otherwise we have to visit the body and attach the ScopRegion annotation 
		{
			regionStmts.push( RegionStmtStack::value_type() );
			// remove element from the stack of statements from all the exit paths 
			FinalActions fa( [&] () -> void { regionStmts.pop(); subScops.clear(); } );
		
			StatementAddress addr = AS_STMT_ADDR(lambda->getBody() );  /*getBody()*/
			bodyIV = visitStmt( addr );

			assert (addr->hasAnnotation(ScopRegion::KEY));
			assert(subScops.size() == 1 && "A Lambda cannot have more than one sub SCoP");
			
			lambda->addAnnotation( 
					std::make_shared<ScopRegion>(
						lambda.getAddressedNode(),
						bodyIV, 
						IterationDomain(bodyIV), 
						regionStmts.top(), 
						toSubScopList(bodyIV, subScops)
					) 
				);
	
			postProcessSCoP(addr, scopList) ;

			//fa.setEnabled( false );
		}
		
		subScops.push_back( lambda );

		return bodyIV;
	}

	IterationVector visitCallExpr(const CallExprAddress& callExpr) {
		STACK_SIZE_GUARD;

		const NodeAddress& func = callExpr->getFunctionExpr();
		const BasicGenerator& gen = callExpr->getNodeManager().getLangBasic();
		
		if ( func->getNodeType() != NT_LambdaExpr && !gen.isBuiltIn(func) ) {

			// Check whether the arguments of the functions are non-refs
			const vector<ExpressionPtr>& args = callExpr.getAddressedNode()->getArguments();
			bool isPure=true;
			std::for_each(args.begin(), args.end(), [&](const ExpressionPtr& cur) { 
					if(cur->getType()->getNodeType() == NT_RefType) { isPure = false; }
			} );
			
			if ( !isPure ) { throw NotASCoP(callExpr.getAddressedNode()); }
		}

		IterationVector iterVec;

		// Visit the arguments of this call expression using the evaluation order of C
		// (right-to-left). This will make the ordering of the statements inside the SCoP
		// consistent.
		const vector<ExpressionAddress>&& arguments = callExpr->getArguments();
		for(auto it = arguments.rbegin(); it != arguments.rend(); ++it) {
			iterVec = merge( iterVec, visit(*it) );
		}

		AddressList scops(subScops);
		subScops.clear();

		NodeAddress lambdaScop;

		// Visit the body of the function 
		iterVec = visitNode(func);

		if ( func->getNodeType() == NT_LambdaExpr ) {
			assert( subScops.size() == 1 );

			lambdaScop = subScops.front();
		}
	
		if ( func->getNodeType() == NT_LambdaExpr ) {
			assert( lambdaScop );
			
			throw NotASCoP( callExpr.getAddressedNode() ); // FIXME:

			const ScopRegion& lambda = *lambdaScop->getAnnotation(ScopRegion::KEY);
			const ScopRegion::StmtVect& stmts = lambda.getDirectRegionStmts();

			std::copy( stmts.begin(), stmts.end(), std::back_inserter(regionStmts.top()) );
			
			auto lambdaSubScops = lambda.getSubScops();
			for_each(lambdaSubScops.begin(), lambdaSubScops.end(), [&](const SubScop& cur) { scops.push_back(cur.first); });	
		}

		subScops.clear();
        std::copy(scops.begin(), scops.end(), std::back_inserter(subScops));

		return iterVec;
	}

	IterationVector visitBreakStmt(const BreakStmtAddress& breakStmt) {
		throw NotASCoP( breakStmt.getAddressedNode() );
	}

	IterationVector visitContinueStmt(const ContinueStmtAddress& contStmt) {
		throw NotASCoP( contStmt.getAddressedNode() );
	}

	// FIXME: for now we force to break a SCoP anytime a RetStmt is encountred. This infact would
	// mean a function is returning from anypoint and makes it complex to be supported in the
	// polyhedral model. However function which returns as last operation of the body can be
	// supported. A solution for have better support for function would be inlining. 
	IterationVector visitReturnStmt(const ReturnStmtAddress& retStmt) {
		throw NotASCoP( retStmt.getAddressedNode() );
	}

	IterationVector visitProgram(const ProgramAddress& prog) {
		for(size_t i=0, end=prog->getEntryPoints().size(); i!=end; ++i) {
			try { 
				NodeAddress&& addr = prog.getAddressOfChild(i);
				visit( addr ); 
				assert( subScops.size() == 1 );
				postProcessSCoP( subScops.front(), scopList );
			} catch(NotASCoP&& e) { 
				subScops.empty(); 
			}
		} 
		return IterationVector();
	}

	// Generic method which recursively visit IR nodes and merges the resulting 
	// iteration vectors 
	IterationVector visitNode(const NodeAddress& node) {
 		IterationVector ret;
 		for_each(node->getChildList(), [&](const NodeAddress& cur){
			ret = merge(ret, this->visit(cur));
		});
		return ret;
	}
};

/**************************************************************************************************
 * After the entry level of a SCoP has been found (using the bottom-up technique) we start the
 * top-down pass to detect conditions which would invalidate the SCoP. For example assignment to
 * parameters of the iteration vector is invalid, in order to detect this we have to consider the
 * top-level iteration vector and visit all the statements inside the scop looking for assignment
 * statements where the left side is one of the parameters of iter vec. 
 **************************************************************************************************/ 
void detectInvalidSCoPs(const IterationVector& iterVec, const NodeAddress& scop) {
	assert ( scop->hasAnnotation(ScopRegion::KEY) );

	ScopRegion& region = *scop->getAnnotation( ScopRegion::KEY );
	const ScopRegion::StmtVect& stmts = region.getDirectRegionStmts();

	std::for_each(stmts.begin(), stmts.end(), [&](const ScopRegion::Stmt& curStmt) {
		const RefAccessList& ail = curStmt.getRefAccesses();

		std::for_each(ail.begin(), ail.end(), 
			[&] (const RefPtr& cur) {

				// if( usage != Ref::SCALAR && usage != Ref::MEMBER) { continue; }

				const ExpressionAddress& addr = cur->getBaseExpression();
				switch ( cur->getUsage() ) {
				case Ref::DEF:
				case Ref::UNKNOWN:
					if ( iterVec.getIdx( addr.getAddressedNode() ) != -1 ) {
						// This SCoP has to be discarded because one of the iterators or parameters
						// of the iteration domain has been overwritten within the body of the SCoP
						throw DiscardSCoPException( 
							curStmt.getAddr().getAddressedNode(), addr.getAddressedNode() 
						);
					}
				default:
					break;
				}
			});
		});

	// now check stmts of the subScops
	const SubScopList& subScops = region.getSubScops();
	std::for_each(subScops.begin(), subScops.end(), [&](const SubScop& cur) { 
		detectInvalidSCoPs(iterVec, cur.first); 
	} );
}

void postProcessSCoP(const NodeAddress& scop, AddressList& scopList) {
	assert ( scop->hasAnnotation(ScopRegion::KEY) );

	ScopRegion& region = *scop->getAnnotation( ScopRegion::KEY );
	const IterationVector& iterVec = region.getIterationVector();
	
	if (iterVec.getIteratorNum() == 0) {
		// A top level SCoP containing no loops. This is considered not a SCoP in the terminology of
		// the polyhedral model, therefore is discarded. However we don't set the flag to invalid
		// because this region could be inside another SCoP contanining loops therefore forming a
		// valid SCoP
		LOG(WARNING) << "Invalidating SCoP because it contains no loops "; 
		return;
	}

	try {

		detectInvalidSCoPs(iterVec, scop);
		scopList.push_back( scop );

	} catch( DiscardSCoPException e ) { 
		LOG(WARNING) << "Invalidating SCoP because iterator/parameter '" << 
					*e.expression() << "' is being assigned in stmt: '" << *e.node() << "'";

		// Recur on every subscop to identify minimal SCoPs
		std::for_each(region.getSubScops().begin(), region.getSubScops().end(), 
				[&](const SubScop& subScop) { postProcessSCoP(subScop.first, scopList); });

		// Invalidate the annotation for this SCoP, we can set the valid flag to false because we
		// are sure that within this SCoP there are issues with makes the SCoP not valid. 
		region.setValid(false);
	} 

}

} // end namespace anonymous 

namespace insieme {
namespace analysis {
namespace scop {

using namespace core;
using namespace poly;

//===== ScopRegion =================================================================================
const string ScopRegion::NAME = "SCoPAnnotation";
const utils::StringKey<ScopRegion> ScopRegion::KEY("SCoPAnnotationKey");

std::ostream& ScopRegion::printTo(std::ostream& out) const {
	out << "IterVec: " << iterVec;
	out << "\\nIterDom: ";
	if (domain.isEmpty()) 	out << "{ }";
	else					out << domain;
	
	out << "\\n# of direct stmts: " << stmts.size();
	out << "\\nSub SCoPs: {";
	std::for_each(subScops.begin(), subScops.end(),
		[&](const SubScop& cur) { out << cur.second << ", "; });
	return out << "}";
}

bool ScopRegion::containsLoopNest() const {
	return iterVec.getIteratorNum() > 0;
}

/**************************************************************************************************
 * Recursively process ScopRegions caching the information related to access functions and
 * scattering matrices for the statements contained in this Scop region
 *************************************************************************************************/
void resolveScop(const poly::IterationVector& 	iterVec, 
				 poly::IterationDomain		 	parentDomain, 
	   		   	 const ScopRegion& 				region,
				 size_t&						pos,
				 size_t&						id,
 				 const AffineSystem&	 		curScat,
				 ScopRegion::IteratorOrder&		iterators,
				 poly::Scop& 					scat,
				 size_t&						sched_dim) 
{
	typedef std::set<Iterator> IteratorSet;
	// assert( parentDomain->getIterationVector() == iterVec );
	IterationDomain currDomain = parentDomain && IterationDomain(iterVec, region.getDomainConstraints());
	const ScopRegion::StmtVect& scopStmts = region.getDirectRegionStmts();
	
	// for every access in this region, convert the affine constraint to the new iteration vector 
	std::for_each(scopStmts.begin(), scopStmts.end(), [&] (const ScopRegion::Stmt& cur) { 
			
		StatementPtr&& curPtr = cur.getAddr().getAddressedNode();
		assert(curPtr->getNodeType() != core::NT_MarkerExpr && curPtr->getNodeType() != core::NT_MarkerStmt);
	
		IterationDomain thisDomain = currDomain;

		AffineSystem newScat(curScat);
		const IterationVector& iterVec = curScat.getIterationVector();
		assert(&newScat.getIterationVector() == &iterVec); 
		AffineFunction af( iterVec );

		// check wheather the statement is a SCoP
		auto fit = std::find_if(region.getSubScops().begin(), region.getSubScops().end(), 
			[&](const SubScop& subScop) -> bool { 
				return subScop.first.getAddressedNode() == cur.getAddr().getAddressedNode(); 
			} 
		);

		if (fit != region.getSubScops().end() ) {
			// add the IterationDomain stored in the pointer to the current domain and recursively
			// resolve the ScopRegion 
			thisDomain &= IterationDomain(iterVec, fit->second);

			if(curPtr->getNodeType() != NT_ForStmt) {
				assert(cur->hasAnnotation(ScopRegion::KEY));
				resolveScop( iterVec, 
							 thisDomain, 
							 *cur->getAnnotation(ScopRegion::KEY), 
							 pos, 
							 id,
							 curScat, 
							 iterators, 
							 scat, 
							 sched_dim
						   );
				return;
			}
		}

		af.setCoeff(poly::Constant(), pos++);
		newScat.append( af );

		// this is a sub scop
		if (curPtr->hasAnnotation(ScopRegion::KEY)) {

			if ( curPtr->getNodeType() == NT_ForStmt ) {
				// if the statement is a loop, then we append a dimension with the corresponding
				// iterator variable and we go recursively to visit the body  
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(curPtr);
				const VariablePtr& iter = forStmt->getIterator();

				AffineFunction newAf( iterVec );
				newAf.setCoeff( poly::Iterator(iter), 1 );
				newScat.append(newAf); 
				
				iterators.push_back(poly::Iterator(iter));
			} 

			size_t nestedPos = 0;
			resolveScop( iterVec, 
						 thisDomain, 
						 *cur->getAnnotation(ScopRegion::KEY), 
						 nestedPos, 
						 id,
						 newScat, 
						 iterators, 
						 scat, 
						 sched_dim
					   );

			// pop back the iterator in the case the statement was a for stmt
			if ( curPtr->getNodeType() == NT_ForStmt ) { iterators.pop_back(); }
			return;
		}

		// Access expressions 
		const RefAccessList& refs = cur.getRefAccesses();
		poly::AccessList accInfo;
		std::for_each(refs.begin(), refs.end(), [&] (const RefPtr& curRef) {
				poly::AffineSystemPtr idx = std::make_shared<poly::AffineSystem>(iterVec);
				switch(curRef->getType()) {
				case Ref::SCALAR:
				case Ref::MEMBER:
					// A scalar is treated as a zero dimensional array 
					idx->append( AffineFunction(iterVec) );
					break;
				case Ref::ARRAY:
				{
					const ArrayRef& array = static_cast<const ArrayRef&>(*curRef);
					std::for_each(array.getIndexExpressions().begin(), array.getIndexExpressions().end(), 
						[&](const ExpressionAddress& cur) { 
							assert(cur->hasAnnotation(scop::AccessFunction::KEY));
							scop::AccessFunction& ann = *cur->getAnnotation(scop::AccessFunction::KEY);
							idx->append( ann.getAccessFunction().toBase(iterVec) );
						}
					);
					break;
				}
				default:
					LOG(WARNING) << "Reference of type " << Ref::refTypeToStr(curRef->getType()) << " not handled!";
				}

				accInfo.push_back( 
					poly::AccessInfo( 
							AS_EXPR_ADDR( concat<Node>(cur.getAddr(), curRef->getBaseExpression() ) ), 
							curRef->getType(), 
							curRef->getUsage(), 
							idx
						)
					);
		});

		IteratorSet nested_iters(iterators.begin(), iterators.end()), 
					domain_iters(iterVec.iter_begin(), iterVec.iter_end()), 
					notUsed;

		// Remove iterators which do not belong to this nested region
		std::set_difference(
				domain_iters.begin(), domain_iters.end(), nested_iters.begin(), nested_iters.end(), 
				std::inserter(notUsed, notUsed.begin())
			);
			
		// save the domain 
		ConstraintCombinerPtr<AffineFunction> saveDomain = currDomain.getConstraint();

		// set to zero all the not used iterators 
		std::for_each(notUsed.begin(), notUsed.end(), 
				[&] (const poly::Iterator& curIt) { 
					if ( curIt.isExistential() ) { return; }

					AffineFunction af(iterVec);
					af.setCoeff(curIt, 1);
					af.setCoeff(poly::Constant(), 0);
					saveDomain = saveDomain and Constraint<AffineFunction>(af, Constraint<AffineFunction>::EQ);
				}
			);

		IterationDomain iterDom = saveDomain ? IterationDomain(saveDomain) : IterationDomain(iterVec);
		scat.push_back( poly::Stmt( id++, cur.getAddr(), iterDom, newScat, accInfo ) );
	
		// keep track of the max dimension of the scheduling matrix 
		if (newScat.size() > sched_dim) {
			sched_dim = newScat.size();
		}

	} ); 
}

void ScopRegion::resolve() {
	assert( isValid() && "Error Try to resolve an invalid SCoP");

	// If the region has been already resolved, we simply return the cached result
	if ( isResolved() ) { return; }

	// we compute the full scattering information for this domain and we cache the result for
	// later use. 
	scattering = std::make_shared<ScopRegion::ScatteringPair>( 0, iterVec );

	AffineSystem sf( getIterationVector() );
	ScopRegion::IteratorOrder iterOrder;
	
	// in the case the entry point of this scop is a forloop, then we build the scattering matrix
	// using the loop iterator index 
	if (annNode->getNodeType() == NT_ForStmt) {
		AffineFunction af( getIterationVector() );
		poly::Iterator iter = poly::Iterator(core::static_pointer_cast<const ForStmt>(annNode)->getIterator());
		af.setCoeff( iter, 1 );
		sf.append( af );
		iterOrder.push_back(iter);
	}

	size_t pos=0, id=0;
	resolveScop(
			getIterationVector(), 
			poly::IterationDomain(getIterationVector()), 
			*this, 
			pos, 
			id,
			sf, 
			iterOrder, 
			scattering->second, 
			scattering->first
		);

	assert( isResolved() );
}

//===== AccessFunction ============================================================
const string AccessFunction::NAME = "AccessFuncAnn";
const utils::StringKey<AccessFunction> AccessFunction::KEY("AccessFuncAnnKey");

std::ostream& AccessFunction::printTo(std::ostream& out) const {
	return out << "IV: " << iterVec << ", Access: " << access;
}

//===== mark ======================================================================
AddressList mark(const core::NodePtr& root) {
	AddressList ret;
	LOG(DEBUG) << std::setfill('=') << std::setw(80) << std::left << "# Starting SCoP analysis";
	ScopVisitor sv(ret);
	try {
		sv.visit( NodeAddress(root) );
	} catch (NotASCoP&& e) { LOG(WARNING) << e.what(); }
	LOG(DEBUG) << ret.size() << std::setfill(' ');
	return ret;
}

namespace {

// Creates the scattering map for a statement inside the SCoP. This is done by building the domain
// for such statement (adding it to the outer domain). Then the scattering map which maps this
// statement to a logical execution date is transformed into a corresponding Map 
poly::MapPtr<BackendTraits<POLY_BACKEND>::ctx_type> 
createScatteringMap(
		BackendTraits<POLY_BACKEND>::ctx_type& 					ctx, 
		const poly::IterationVector&							iterVec,
		poly::SetPtr<BackendTraits<POLY_BACKEND>::ctx_type>& 	outer_domain, 
		const poly::Stmt& 										cur, 
		size_t 													scat_size ) 
{
	// Creates a name mapping which maps an entity of the IR (StmtAddress) 
	// to a name utilied by the framework as a placeholder 
	TupleName tn(cur.getAddr(), "S" + utils::numeric_cast<std::string>(cur.getId()));

	auto&& domainSet = makeSet<POLY_BACKEND>(ctx, cur.getDomain(), tn);
	assert( domainSet && "Invalid domain" );
	outer_domain = set_union(ctx, *outer_domain, *domainSet);

	AffineSystem sf = cur.getSchedule();
	// Because the scheduling of every statement has to have the same number of elements
	// (same dimensions) we append zeros until the size of the affine system is equal to 
	// the number of dimensions used inside this SCoP for the scheduling functions 
	for ( size_t s = sf.size(); s < scat_size; ++s ) {
		sf.append( AffineFunction(iterVec) );
	}

	return makeMap<POLY_BACKEND>(ctx, sf, tn);
}

} // end anonymous namespace 

core::NodePtr toIR(const core::NodePtr& root) {

	if( !root->hasAnnotation( ScopRegion::KEY ) ) {
		LOG(WARNING) << "Not possible to compute dependence information from a non static control region.";
		return core::NodePtr();
	}
	
	// We are in a Scop 
	ScopRegion& ann = *root->getAnnotation( ScopRegion::KEY );
	
	ann.resolve();

	const ScopRegion::ScatteringPair&& scat = ann.getScatteringInfo();
	const IterationVector& iterVec = ann.getIterationVector();
	auto&& ctx = BackendTraits<POLY_BACKEND>::ctx_type();

	// universe set 
	auto&& domain = makeSet<POLY_BACKEND>(ctx, IterationDomain(iterVec));
	auto&& schedule = makeEmptyMap<POLY_BACKEND>(ctx, iterVec);
	
	std::for_each(scat.second.begin(), scat.second.end(), 
		[ & ] (const poly::Stmt& cur) { 
			schedule = map_union(ctx, *schedule, *createScatteringMap(ctx, iterVec, domain, cur, scat.first));
		}
	);

	return poly::toIR(root->getNodeManager(), ann.getIterationVector(), ctx, *domain, *schedule);
}

void computeDataDependence(const NodePtr& root) {

	if( !root->hasAnnotation( ScopRegion::KEY ) ) {
		LOG(WARNING) << "Not possible to compute dependence information from a non static control region.";
		return ;
	}
	
	// We are in a Scop 
	ScopRegion& ann = *root->getAnnotation( ScopRegion::KEY );

	ann.resolve();

	const ScopRegion::ScatteringPair&& scat = ann.getScatteringInfo();
	const IterationVector& iterVec = ann.getIterationVector();
	auto&& ctx = BackendTraits<POLY_BACKEND>::ctx_type();

	// universe set 
	auto&& domain = makeSet<POLY_BACKEND>(ctx, IterationDomain(iterVec));
	auto&& schedule = makeEmptyMap<POLY_BACKEND>(ctx, iterVec);
	auto&& reads = makeEmptyMap<POLY_BACKEND>(ctx, iterVec);
	auto&& writes = makeEmptyMap<POLY_BACKEND>(ctx, iterVec);

	std::for_each(scat.second.begin(), scat.second.end(), 
		[ & ] (const poly::Stmt& cur) { 
			TupleName tn(cur.getAddr(), "S"+utils::numeric_cast<std::string>(cur.getId()));
			schedule = map_union(ctx, *schedule, *createScatteringMap(ctx, iterVec, domain, cur, scat.first));
				
			// Access Functions 
			std::for_each(cur.access_begin(), cur.access_end(), [&](const poly::AccessInfo& cur){
				const AffineSystemPtr& accessInfo = cur.getAccess();

				if (accessInfo) {
					auto&& access = makeMap<POLY_BACKEND>(ctx, *accessInfo, tn, TupleName(cur.getExpr(), cur.getExpr()->toString()));

					switch ( cur.getUsage() ) {
					case Ref::USE: 		reads  = map_union(ctx, *reads, *access); 	break;
					case Ref::DEF: 		writes = map_union(ctx, *writes, *access);	break;
					case Ref::UNKNOWN:	reads  = map_union(ctx, *reads, *access);
										writes = map_union(ctx, *writes, *access);
										break;
					default:
						assert( false && "Usage kind not defined!" );
					}
				}
			});
		}
	);

	LOG(DEBUG) << "Print Scattering";
	map_intersect_domain(ctx, *schedule, *domain)->printTo(std::cout);

	LOG(DEBUG) << "Computing RAW dependencies: ";
	DependenceInfo<IslCtx> depInfo = 
		buildDependencies(ctx, *domain, *schedule, *reads, *writes, *makeEmptyMap<POLY_BACKEND>(ctx, iterVec));
	LOG(DEBUG) << depInfo;
	LOG(DEBUG) << "Empty?: " << depInfo.isEmpty();
	
	std::cout << std::endl;

	LOG(DEBUG) << "Computing WAW dependencies: ";	
	depInfo = buildDependencies(ctx, *domain, *schedule, *writes, *writes, *makeEmptyMap<POLY_BACKEND>(ctx, iterVec));
	std::cout << std::endl;
	LOG(DEBUG) << depInfo;

	LOG(DEBUG) << "Empty?: " << depInfo.isEmpty();


	LOG(DEBUG) << "Computing WAR dependencies: ";
	depInfo = buildDependencies(ctx, *domain, *schedule, *writes, *reads, *makeEmptyMap<POLY_BACKEND>(ctx, iterVec));
	std::cout << std::endl;
	LOG(DEBUG) << depInfo;

	LOG(DEBUG) << "Empty?: " << depInfo.isEmpty();

}

bool ScopRegion::isParallel() {
	assert(false && "Not yet implemented!");
}

#define MSG_WIDTH 100
//===== printSCoP ===================================================================
void printSCoP(std::ostream& out, const core::NodePtr& scop) {
	out << std::endl << std::setfill('=') << std::setw(MSG_WIDTH) << std::left << "@ SCoP PRINT";	
	// out << *scop;
	// check whether the IR node has a SCoP annotation
	if( !scop->hasAnnotation( ScopRegion::KEY ) ) {
		out << "{ }\n";
		return ;
	}
	
	// auto&& ctx = BackendTraits<POLY_BACKEND>::ctx_type();
	ScopRegion& ann = *scop->getAnnotation( ScopRegion::KEY );
	ann.resolve();
	const poly::Scop& scat = ann.getScop();
	out << "\nNumber of sub-statements: " << scat.size() << std::endl;
		
	out << "IV: " << ann.getIterationVector() << std::endl;
	for_each(scat, [&](const poly::Stmt& cur) {
		out << std::setfill('~') << std::setw(MSG_WIDTH) << "" << std::endl << cur; 
	} );

	LOG(DEBUG) << std::endl << std::setfill('=') << std::setw(MSG_WIDTH) << "";
}

// This function determines the maximum number of loop nests within this region 
// The analysis should be improved in a way that also the loopnest size is weighted with the number
// of statements present at each loop level.
size_t calcLoopNest(const IterationVector& iterVec, const poly::Scop& scat) {
	size_t max_loopnest=0;
	for_each(scat.begin(), scat.end(), 
		[&](const poly::Stmt& scopStmt) { 
			size_t cur_loopnest=0;
			for_each(scopStmt.getSchedule().begin(), scopStmt.getSchedule().end(), 
				[&](const AffineFunction& cur) { 
					for(auto&& it=cur.begin(), end=cur.end(); it!=end; ++it) {
						if((*it).second != 0 && (*it).first.getType() == Element::ITER) { 
							++cur_loopnest; 
							break;
						}
					}
				} );
			if (cur_loopnest > max_loopnest) {
				max_loopnest = cur_loopnest;
			}
		} );
	return max_loopnest;
}

} // end namespace scop
} // end namespace analysis
} // end namespace insieme




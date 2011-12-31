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

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/exception_utils.h"
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
class NotASCoP : public insieme::utils::InsiemeException {
	NodePtr root;

public:
	template <class SubExTy>
	NotASCoP(const std::string& msg, 
			 const char*		ex_type,
			 const char* 		file_name, 
			 int 				line_no, 
			 const SubExTy*		sub_ex,
			 const NodePtr& 	root ) 
	: InsiemeException(msg, ex_type, file_name, line_no, sub_ex), root(root) { 
		std::ostringstream ss;
		ss << "Node: \n" << insieme::core::printer::PrettyPrinter(root) << "\nNot a Static Control Part";
		setMessage(ss.str());
	}

	NotASCoP(const std::string& msg, 
			 const char*		ex_type,
			 const char* 		file_name, 
			 int 				line_no, 
			 const NodePtr& 	root ) 
	: InsiemeException(msg, ex_type, file_name, line_no), root(root) { 
		std::ostringstream ss;
		ss << "Node: \n" << insieme::core::printer::PrettyPrinter(root) << "\nNot a Static Control Part";
		setMessage(ss.str());
	}

	const NodePtr& node() const { return root; }

	virtual ~NotASCoP() throw() { }
};

class DiscardSCoPException: public NotASCoP {
	const ExpressionPtr expr;
public: 
	DiscardSCoPException(const std::string& 	msg, 
						 const char*			ex_type,
						 const char* 			file_name, 
						 int 					line_no, 
						 const NodePtr& 		root, 
						 const ExpressionPtr&	expr) 
		: NotASCoP(msg, ex_type, file_name, line_no, root), expr(expr) { }

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
			ConstraintType type;
			switch (op) {
				case BasicGenerator::Eq: type = ConstraintType::EQ; break;
				case BasicGenerator::Ne: type = ConstraintType::NE; break;
				case BasicGenerator::Lt: type = ConstraintType::LT; break;
				case BasicGenerator::Le: type = ConstraintType::LE; break;
				case BasicGenerator::Gt: type = ConstraintType::GT; break;
				case BasicGenerator::Ge: type = ConstraintType::GE; break;
				default:
					assert(false && "Operation not supported!");
			}
			return IterationDomain( AffineConstraint(af, type) );
		} catch (arithmetic::NotAFormulaException&& e) { 
			RETHROW_EXCEPTION(NotASCoP, e, "", e.getCause()); 
		} catch (NotAffineExpr&& e) { 
			RETHROW_EXCEPTION(NotASCoP, e, "", cond);
		}
	}
	// LOG(ERROR) << "Condition Expression not supported: " << *cond;
	THROW_EXCEPTION(NotASCoP, "", cond);
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
		RETHROW_EXCEPTION(NotASCoP, e, "", expr);
	} catch(arithmetic::NotAFormulaException&& e) {
		RETHROW_EXCEPTION(NotASCoP, e, "", e.getCause());
	}	 
}

SubScopList toSubScopList(const IterationVector& iterVec, const AddressList& scops) {
	SubScopList subScops;
	for_each(scops.begin(), scops.end(), [&](const NodeAddress& cur) { 
			subScops.push_back( SubScop(cur, IterationDomain(iterVec)) );
	});
	return subScops;
}

using namespace insieme::utils;

struct FromPiecewiseVisitor : public RecConstraintVisitor<arithmetic::Formula> {
	
	AffineConstraintPtr curr;
	IterationVector& iterVec;

	FromPiecewiseVisitor(IterationVector& iterVec) : iterVec(iterVec) { }

	void visit(const RawConstraintCombiner<arithmetic::Formula>& rcc) { 
		const arithmetic::Formula& func = rcc.getConstraint().getFunction();

		curr = makeCombiner( 
				AffineConstraint(AffineFunction(iterVec, func), rcc.getConstraint().getType()) 
			);
	}

	void visit(const NegatedConstraintCombiner<arithmetic::Formula>& ucc) {

		ucc.getSubConstraint()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		curr = not_(curr);
	}

	void visit(const BinaryConstraintCombiner<arithmetic::Formula>& bcc) {

		bcc.getLHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		AffineConstraintPtr lhs = curr;

		bcc.getRHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		AffineConstraintPtr rhs = curr;

		curr = bcc.getType() == BinaryConstraintCombiner<arithmetic::Formula>::OR ? lhs or rhs : lhs and rhs; 
	}

};

AffineConstraintPtr fromPiecewise( IterationVector& iterVect, const arithmetic::Piecewise::PredicatePtr& pred ) {

	FromPiecewiseVisitor pwv(iterVect);
	pred->accept( pwv );

	return pwv.curr;
	
}

// Extraction of loop bounds
AffineConstraintPtr extractLoopBound( IterationVector& 		ret, 
				  					  const VariablePtr& 	loopIter, 
									  const ExpressionPtr& 	expr, 
									  const ConstraintType& ct,
									  AffineFunction&		aff) 
{
	using namespace arithmetic;
	using arithmetic::Piecewise;
	using arithmetic::Formula;
	
	// LOG(DEBUG) << *expr ;
	NodeManager& mgr = expr->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& basic = mgr.getLangBasic();

	try {

		Piecewise&& pw = toPiecewise( builder.invertSign( expr ) );
		
		if ( pw.isFormula() ) {
			AffineFunction bound(ret, static_cast<Formula>(pw));
			bound.setCoeff(loopIter, 1);
				
			for_each(bound.begin(), bound.end(), [&](const AffineFunction::Term t) { 
					aff.setCoeff(t.first, t.second); 
				});

			return makeCombiner( AffineConstraint(bound, ct) );
		}

		Piecewise::const_iterator it = pw.begin();
		AffineConstraintPtr boundCons = fromPiecewise( ret, it->first );
		// iter >= val
		AffineFunction af(ret, it->second);
		af.setCoeff(loopIter, 1);
		boundCons = boundCons and AffineConstraint( af, ct );
		++it;
		for ( Piecewise::const_iterator end=pw.end(); it != end; ++it ) {
			AffineFunction bound(ret, it->second);
			bound.setCoeff(loopIter, 1);

			boundCons = boundCons or ( fromPiecewise( ret, it->first ) and AffineConstraint( bound, ct ) );
		}
		return boundCons;

	} catch ( NotAPiecewiseException&& e ) {

		CallExprPtr callExpr;
		size_t coeff;

		if ( (callExpr = static_pointer_cast<const CallExpr>( e.getCause() )) && 
			 ((coeff = -1, analysis::isCallOf( callExpr, basic.getCloogFloor() ) ) ||
			  (coeff = 1, analysis::isCallOf( callExpr, basic.getCloogCeil() ) ) || 
			  (coeff = 0, analysis::isCallOf( callExpr, basic.getCloogMod() ) ) ) 
		   ) 
		{
			// in order to handle the ceil case we have to set a number of constraint
			// which solve a linear system determining the value of those operations
			Formula&& den = toFormula(callExpr->getArgument(1));
			assert( callExpr && den.isConstant() );
			
			int denVal = den.getTerms().front().second.getNum();

			// The result of the floor/ceil/mod operation will be represented in the passed
			// epxression by a new variable which is herein introduced 
			VariablePtr&& var = builder.variable( basic.getInt4() );
			ret.add( Iterator(var) );

			// An existential variable is required in order to set the system of equalities 
			VariablePtr&& exist = builder.variable( basic.getInt4() );
			ret.add( Iterator(exist, true) );

			AffineFunction af1( ret, callExpr->getArgument(0) );
			// (NUM) + var*DEN + exist == 0
			af1.setCoeff( var, -denVal );

			af1.setCoeff( exist, coeff);
			AffineConstraintPtr boundCons = makeCombiner( AffineConstraint( af1, ConstraintType::EQ ) );

			// FIXME for ceil and mod 
			//
			// exist -DEN < 0
			AffineFunction af2( ret );
			af2.setCoeff( exist, -denVal);
			boundCons = boundCons and AffineConstraint( af2, ConstraintType::LT );

			// exist >= 0
			AffineFunction af3( ret );
			af3.setCoeff( exist, 1);
			boundCons = boundCons and AffineConstraint( af3, ConstraintType::GE );
	
			// Now we can replace the floor/ceil/mod expression from the original expression with
			// the newly introduced variable
			ExpressionPtr newExpr = static_pointer_cast<const Expression>( 
					transform::replaceAll(mgr, expr, callExpr, var) 
				);
			return boundCons and extractLoopBound( ret, loopIter, newExpr, ct, aff );
		}
		throw e;
	}

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
					VLOG(1) << "Reference of type " << Ref::refTypeToStr(cur->getType()) << " not handled!";
				}
			});
		return refs;
	}

	IterationVector visitStmt(NodeAddress addr) {
		STACK_SIZE_GUARD;

		assert(subScops.empty());

		while(addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr) {
			addr = addr.getAddressOfChild( (addr->getNodeType()==NT_MarkerStmt?1:2) ); // sub-statement or expression
		}
		IterationVector&& ret = visit(addr);

		if ( addr->getNodeType() == NT_CallExpr) {
			CallExprAddress callExpr = static_address_cast<const CallExpr>(addr);
			if(callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
				return ret;
			}
		}


		if ( subScops.empty() ) {
			// std::cout << *addr.getParentNode() << std::endl;
			// std::cout << addr.getAddressedNode()->getNodeType() << std::endl;
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
			if ( !ann.isValid() ) { THROW_EXCEPTION(NotASCoP, "", ifStmt.getAddressedNode()); }

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

		std::shared_ptr<NotASCoP> ex;
		try {
			subScops.clear();
			// check the then body
			saveThen = visitStmt(thenAddr);
		} catch (NotASCoP&& e) { 
			isThenSCOP = false; 
			ex = std::make_shared<NotASCoP>(e);
		}

		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions faElse( [&] () -> void { regionStmts.pop(); } );

		try {
			subScops.clear();
			// check the else body
			saveElse = visitStmt(elseAddr);
		} catch (NotASCoP&& e) { 
			isElseSCOP = false; 
			ex = std::make_shared<NotASCoP>(e);
		}
	
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
			assert(ex);
			RETHROW_EXCEPTION(NotASCoP, *ex, "", ifStmt.getAddressedNode() ); 
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
			if ( !ann.isValid() ) { THROW_EXCEPTION(NotASCoP, "", switchStmt.getAddressedNode()); }

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
				
				AffineFunction af(ret, arithmetic::toFormula(switchStmt->getSwitchExpr()) - 
					arithmetic::toFormula(exprAddr.getAddressedNode()));

				IterationDomain caseCons(AffineConstraint(af, ConstraintType::EQ));
				defaultCons &= !caseCons;

				// Add this statement to the subScops
				scops.push_back( SubScop(stmtAddr, caseCons) );

			} catch (arithmetic::NotAFormulaException&& e) { 
				isSCoP = false; 
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
			THROW_EXCEPTION(NotASCoP, "", switchStmt.getAddressedNode());
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
	
		//LOG(DEBUG) << "Analyzing " << *forStmt;
		assert(subScops.empty());

		// if we already visited this forStmt, just return the precomputed iteration vector 
		if (forStmt->hasAnnotation(ScopRegion::KEY)) {
			ScopRegion& ann = *forStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { THROW_EXCEPTION(NotASCoP, "", forStmt.getAddressedNode()); }

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
	
				AffineFunction lbAff(ret);
				AffineConstraintPtr&& lbCons = 
					extractLoopBound( ret, forPtr->getIterator(), forPtr->getStart(), ConstraintType::GE, lbAff );

				AffineFunction ubAff(ret);
				AffineConstraintPtr&& ubCons = 
					extractLoopBound( ret, forPtr->getIterator(), forPtr->getEnd(), ConstraintType::LT, ubAff);

				assert( lbCons && ubCons );

				// set the constraint: iter >= lb && iter < ub
				AffineConstraintPtr&& loopBounds = lbCons and ubCons;

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

					// LOG(DEBUG) << lbAff; //FIXME
					lbAff.setCoeff( existenceVar, -formula.getTerms().front().second.getNum() );

					loopBounds = loopBounds and AffineConstraint( lbAff, ConstraintType::EQ);
				}

				IterationDomain cons( loopBounds );
				// std::cout << "Loop bounds" << cons << std::endl;

				assert( !forStmt->hasAnnotation( ScopRegion::KEY ) );
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
				RETHROW_EXCEPTION(NotASCoP, e, "", forStmt.getAddressedNode());

			}catch(arithmetic::NotAFormulaException&& e) {
				RETHROW_EXCEPTION(NotASCoP, e, "", e.getCause()); 
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
		THROW_EXCEPTION(NotASCoP, "", whileStmt.getAddressedNode());
	}

	IterationVector visitCompoundStmt(const CompoundStmtAddress& compStmt) {
		STACK_SIZE_GUARD;

		IterationVector ret;
		bool isSCOP = true;
		AddressList scops;
	
		assert(subScops.empty());

		if ( compStmt->hasAnnotation(ScopRegion::KEY) ) {
			ScopRegion& ann = *compStmt->getAnnotation(ScopRegion::KEY);
			if ( !ann.isValid() ) { THROW_EXCEPTION(NotASCoP, "", compStmt.getAddressedNode()); }

			// if the SCopRegion annotation is already attached, it means we already visited this
			// compoundstmt, therefore we can return the iteration vector already precomputed 
			subScops.push_back(compStmt);
			return ann.getIterationVector();
		}

		regionStmts.push( RegionStmtStack::value_type() );

		FinalActions fa( [&] () -> void { regionStmts.pop(); } );

		std::shared_ptr<NotASCoP> cause;

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
				if (!cause) {
					cause = std::make_shared<NotASCoP>(e);
				}
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
					addr = AS_STMT_ADDR(addr.getAddressOfChild( (addr->getNodeType()==NT_MarkerStmt?1:2) ) ); // sub-statement or expression
				}

				if (addr->hasAnnotation(ScopRegion::KEY)) { 
					postProcessSCoP( addr, scopList );
				}
			}
			assert(cause);
			RETHROW_EXCEPTION(NotASCoP, *cause, "", compStmt.getAddressedNode()); 
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
			
			if ( !isPure ) { THROW_EXCEPTION(NotASCoP, "", callExpr.getAddressedNode()); }
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
			
			THROW_EXCEPTION(NotASCoP, "", callExpr.getAddressedNode() ); // FIXME:

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
		THROW_EXCEPTION(NotASCoP, "", breakStmt.getAddressedNode() );
	}

	IterationVector visitContinueStmt(const ContinueStmtAddress& contStmt) {
		THROW_EXCEPTION(NotASCoP, "", contStmt.getAddressedNode() );
	}

	// FIXME: for now we force to break a SCoP anytime a RetStmt is encountred. This infact would
	// mean a function is returning from anypoint and makes it complex to be supported in the
	// polyhedral model. However function which returns as last operation of the body can be
	// supported. A solution for have better support for function would be inlining. 
	IterationVector visitReturnStmt(const ReturnStmtAddress& retStmt) {
		THROW_EXCEPTION(NotASCoP, "", retStmt.getAddressedNode());
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

				ExpressionAddress addr = cur->getBaseExpression();
				switch ( cur->getUsage() ) {
				case Ref::DEF:
				case Ref::UNKNOWN:
				{
					if ( iterVec.getIdx( addr.getAddressedNode() ) != -1 ) {
						// This SCoP has to be discarded because one of the iterators or parameters
						// of the iteration domain has been overwritten within the body of the SCoP
						THROW_EXCEPTION(DiscardSCoPException, "",  
							curStmt.getAddr().getAddressedNode(), addr.getAddressedNode() 
						);
					}
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
		VLOG(1) << "Invalidating SCoP because it contains no loops "; 
		return;
	}

	try {

		detectInvalidSCoPs(iterVec, scop);
		scopList.push_back( scop );

	} catch( DiscardSCoPException e ) { 
		VLOG(1) << "Invalidating SCoP because iterator/parameter '" << 
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
const utils::StringKey<ScopRegion> ScopRegion::KEY("SCoPAnnotation");

std::ostream& ScopRegion::printTo(std::ostream& out) const {
	out << "IterVec: " << iterVec;
	out << "\\nIterDom: ";
	if (domain.empty()) 	out << "{ }";
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
				 poly::Scop& 					scat) 
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
							 scat
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
						 scat
					   );

			// pop back the iterator in the case the statement was a for stmt
			if ( curPtr->getNodeType() == NT_ForStmt ) { iterators.pop_back(); }
			return;
		}

		// Access expressions 
		const RefAccessList& refs = cur.getRefAccesses();
		poly::AccessList accInfo;
		std::for_each(refs.begin(), refs.end(), [&] (const RefPtr& curRef) {
				poly::AffineSystem idx(iterVec);
				switch(curRef->getType()) {
				case Ref::SCALAR:
				case Ref::MEMBER:
					// A scalar is treated as a zero dimensional array 
					idx.append( AffineFunction(iterVec) );
					break;
				case Ref::ARRAY:
				{
					const ArrayRef& array = static_cast<const ArrayRef&>(*curRef);
					std::for_each(array.getIndexExpressions().begin(), array.getIndexExpressions().end(), 
						[&](const ExpressionAddress& cur) { 
							assert(cur->hasAnnotation(scop::AccessFunction::KEY));
							scop::AccessFunction& ann = *cur->getAnnotation(scop::AccessFunction::KEY);
							idx.append( ann.getAccessFunction().toBase(iterVec) );
						}
					);
					break;
				}
				default:
					VLOG(1) << "Reference of type " << Ref::refTypeToStr(curRef->getType()) << " not handled!";
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
		AffineConstraintPtr saveDomain = currDomain.getConstraint();

		// set to zero all the not used iterators 
		std::for_each(notUsed.begin(), notUsed.end(), 
				[&] (const poly::Iterator& curIt) { 
					if ( curIt.isExistential() ) { return; }

					AffineFunction af(iterVec);
					af.setCoeff(curIt, 1);
					af.setCoeff(poly::Constant(), 0);
					saveDomain = saveDomain and AffineConstraint(af, ConstraintType::EQ);
				}
			);

		IterationDomain iterDom = saveDomain ? IterationDomain(saveDomain) : IterationDomain(iterVec);

		// std::cout << "DOM" << iterDom << std::endl;
		scat.push_back( poly::Stmt( id++, cur.getAddr(), iterDom, newScat, accInfo ) );
	
	} ); 
}

void ScopRegion::resolve() {
	assert( isValid() && "Error Try to resolve an invalid SCoP");

	// If the region has been already resolved, we simply return the cached result
	if ( isResolved() ) { return; }

	// we compute the full scattering information for this domain and we cache the result for
	// later use. 
	scopInfo = std::make_shared<poly::Scop>( iterVec );

	AffineSystem sf( getIterationVector() );
	ScopRegion::IteratorOrder iterOrder;
	
	// std::cout << *annNode << std::endl;
	// in the case the entry point of this scop is a forloop, then we build the scattering matrix
	// using the loop iterator index 
	if (annNode->getNodeType() == NT_ForStmt) {
		AffineFunction af( getIterationVector() );
		sf.append( af ); // the first dimension is composed by all zeros
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
			*scopInfo
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
	VLOG(1) << std::setfill('=') << std::setw(80) << std::left << "# Starting SCoP analysis";
	ScopVisitor sv(ret);
	try {
		sv.visit( NodeAddress(root) );
	} catch (NotASCoP&& e) { 
		LOG(DEBUG) << e.what(); 
	}
	return ret;
}

} // end namespace scop
} // end namespace analysis
} // end namespace insieme




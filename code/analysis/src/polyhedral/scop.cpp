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
#include "insieme/analysis/func_sema.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/printer/pretty_printer.h"

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
using namespace insieme::analysis::polyhedral;
using namespace insieme::analysis::polyhedral::scop;

using arithmetic::Piecewise;
using arithmetic::Formula;

bool postProcessSCoP(const NodeAddress& scop, AddressList& scopList);



class DiscardSCoPException: public NotASCoP {
	const ExpressionPtr expr;
public: 
	DiscardSCoPException(const std::string&   	   msg, 
						 const char*		   ex_type,
						 const char* 	     file_name, 
						 int 				   line_no, 
						 const std::exception*	sub_ex,
						 const NodePtr& 		  root, 
						 const ExpressionPtr&	  expr) 
		: NotASCoP(msg, ex_type, file_name, line_no, sub_ex, root), 
		  expr(expr) { }

	const ExpressionPtr& expression() const { return expr; }

	virtual ~DiscardSCoPException() throw() { }
};

using namespace insieme::utils;

// Extract a constraint from a piecewise expression
AffineConstraintPtr fromConstraint( IterationVector& iterVect, const arithmetic::Constraint& constraint ) {

	// initialize result with false ...
	AffineConstraintPtr res;
	for_each(constraint.toDNF(), [&](const arithmetic::Constraint::Conjunction& conjunct) {

		// initialize product with true ..
		AffineConstraintPtr product;
		for_each(conjunct, [&](const arithmetic::Constraint::Literal& lit) {
			const arithmetic::Formula& func = lit.first.getFormula();

			// create atom
			AffineConstraintPtr atom = makeCombiner(
					AffineConstraint(AffineFunction(iterVect, func), ConstraintType::LE)
			);

			if (!lit.second) {
				atom = not_(atom);
			}

			product = (!product) ? atom : product and atom;
		});

		// if product is still not set, set it to true
		if (!product) {
			product = makeCombiner(AffineConstraint(AffineFunction(iterVect), ConstraintType::EQ));
		}

		// combine product and overall result
		res = (!res) ? product : res or product;
	});
	
	// if result still not set, set it to false
	if (!res) {
		res = makeCombiner(AffineConstraint(AffineFunction(iterVect), ConstraintType::NE));
	}

	return res;
}

// This function returns a set of constraints utilized to represent a piecewise expression,
// a piecewise is in the form PW: { (cond1) -> a; (cond2) -> b }. We assume that the piecewise is
// utilized in the following form. 
//
// EXPR - PW {LT,LE,EQ,NE,GT,GT} 0
//
// Where EXPR can be any IR expression or a variable (for example a loop iterator)
AffineConstraintPtr extractFrom( IterationVector& iterVec, 
								 const Piecewise& pw, 
								 const ExpressionPtr& compExpr,
								 const ConstraintType ct )
{
	if (pw.isFormula()) {
		AffineFunction bound(iterVec, -pw.toFormula() + arithmetic::toFormula(compExpr));
		return makeCombiner( AffineConstraint(bound, ct) );
	}
	// this is a real piecewise
	auto it = pw.getPieces().begin(), piecesEnd = pw.getPieces().end();
	AffineConstraintPtr boundCons = fromConstraint( iterVec, it->first );

	// iter >= val
	AffineFunction af(iterVec, -it->second + arithmetic::toFormula(compExpr));
	// if this is a bound for a loop iterator then set the iterator coeff to be 1
	boundCons = boundCons and AffineConstraint(af,ct);

	++it;
	for ( ; it != piecesEnd; ++it ) {
		AffineFunction bound(iterVec, -it->second + arithmetic::toFormula(compExpr));

		boundCons = boundCons or 
			( fromConstraint( iterVec, it->first ) and AffineConstraint( bound, ct ) );
	}
	return boundCons;
}

AffineConstraintPtr extractFrom( IterationVector& iterVec, 
								 const ExpressionPtr& expr, 
								 const ExpressionPtr& trg, 
								 const ConstraintType& ct ) 
{	
	using namespace arithmetic;
	using arithmetic::Piecewise;
	using arithmetic::Formula;

	NodeManager& mgr = expr->getNodeManager();

	IRBuilder builder(mgr);
	const lang::BasicGenerator& basic = mgr.getLangBasic();

	try {

		// Try to convert the expression as a piecewise
		return extractFrom(iterVec, arithmetic::toPiecewise(expr), trg, ct);

	}catch( NotAPiecewiseException&& e ) {

		CallExprPtr callExpr;
		int coeff;
		bool isModulus = false;

		// If the function is not an affine function and nor a piecewise affine function 
		// then we enter in the special cases of function which can be transfotmed (via 
		// some manipulation) into piecewise affine functions. For example floor, ceil, 
		// min, max.
		if ( (callExpr = dynamic_pointer_cast<const CallExpr>( e.getCause() ) ) && 
			 ((coeff = -1, analysis::isCallOf( callExpr, basic.getCloogFloor() ) ) ||
			  (coeff = 1, analysis::isCallOf( callExpr, basic.getCloogCeil() ) ) || 
			  (coeff = -1, isModulus=true, analysis::isCallOf( callExpr, basic.getCloogMod() ) ) ||
			  (coeff = -1, isModulus=true, analysis::isCallOf( callExpr, basic.getSignedIntMod() ) ) ||
			  (coeff = -1, isModulus=true, analysis::isCallOf( callExpr, basic.getUnsignedIntMod() ) ) ) 
		   ) 
		{
			// in order to handle the ceil case we have to set a number of constraint
			// which solve a linear system determining the value of those operations
			Formula&& den = toFormula(callExpr->getArgument(1));
			assert( callExpr ); 
			if (!den.isConstant()) {
				THROW_EXCEPTION(NotASCoP, "Denominator in modulo operation must be a constant", callExpr);
			}
			
			int denVal = den.getTerms().front().second.getNumerator();

			// The result of the floor/ceil/mod operation will be represented in the passed
			// epxression by a new variable which is herein introduced 
			VariablePtr var = builder.variable( basic.getInt4() );

			iterVec.add( Iterator(var, true) ); // make this iterator an existential iterator 

			// An existential variable is required in order to set the system of equalities 
			VariablePtr&& exist = builder.variable( basic.getInt4() );

			iterVec.add( Iterator(exist, true) ); // make this iterator an existential iterator 

			AffineFunction af1( iterVec, callExpr->getArgument(0) );
			// (NUM) + var*DEN + exist == 0
			af1.setCoeff( var, -denVal );
			af1.setCoeff( exist, coeff );
			// set the stride
			AffineConstraintPtr boundCons = makeCombiner( AffineConstraint( af1, ConstraintType::EQ ) );

			// FIXME for ceil and mod 
			//-----------------------
			// exist -DEN < 0
			AffineFunction af2( iterVec);
			af2.setCoeff(exist, 1);
			af2.setCoeff(Constant(), -denVal);
			boundCons = boundCons and AffineConstraint( af2, ConstraintType::LT );

			// exist >= 0
			AffineFunction af3( iterVec);
			af3.setCoeff(exist, 1);
			boundCons = boundCons and AffineConstraint( af3, ConstraintType::GE );

			ExpressionPtr res = !isModulus ? var : exist;
			// Now we can replace the floor/ceil/mod expression from the original expression with
			// the newly introduced variable
			ExpressionPtr&& newExpr = transform::replaceAll(mgr, expr, callExpr, res).as<ExpressionPtr>();

			return boundCons and extractFrom( iterVec, newExpr, trg, ct );
		}
		RETHROW_EXCEPTION(NotASCoP, e, "During conversion to piecewise", expr);
	}
}

template <class BoundType>
AffineConstraintPtr buildStridedDomain( NodeManager&		mgr,
										IterationVector& 	ret, 
										const VariablePtr& 	iter, 
										const BoundType& 	lb, 
										const BoundType& 	ub,
										const Formula& 		stride)
{ 
	AffineConstraintPtr&& lbCons = extractFrom( ret, lb, iter, ConstraintType::GE );
	AffineConstraintPtr&& ubCons = extractFrom( ret, ub, iter, ConstraintType::LT );

	assert( lbCons && ubCons && "Wrong conversion of lb and ub to set of constraints" );

	// set the constraint: iter >= lb && iter < ub
	AffineConstraintPtr&& domain = lbCons and ubCons;

	// extract the Formula object 
	if ( !(stride.isLinear() || stride.isOne()) && !stride.isConstant() ) 
		throw NotAffineExpr( toIR(mgr, stride) );

	if ( stride.isOne() ) { return domain; }

	assert(stride.isConstant() && "Stride value of for loop is not constant.");

	int stride_size = stride.getTerms().front().second.getNumerator();

	// We add a new dimension to the iteration vector (an unbounded parameter) and
	// set a new constraint in the form : exist(a: step*a = i) 
	VariablePtr existenceVar = IRBuilder(mgr).variable(mgr.getLangBasic().getInt4());
	ret.add( Iterator( existenceVar, true ) );

	// Gets the list of lower bounds as a disjunction of eleemnts 
	DisjunctionList&& bounds = getConjunctions(toDNF(lbCons));
	assert(!bounds.empty() && !bounds.front().empty());

	// If we are in a situation where we only have a single bound for the loop then we can set the
	// stride starting from this bound 
	if (bounds.size() == 1) {
		
		for_each(bounds.front(), [&](const AffineConstraintPtr& cur) {
				// detect whether this constraint is an equality
				assert(cur->getCombinerType() == CombinerType::CT_RAW);

				const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);

				// We are not interested in equality constraints 
				if (rc.getConstraint().getType() == ConstraintType::EQ) { return; }
				
				const AffineFunction& func = rc.getConstraint().getFunction();
				// We are not interested in constraints where the loop iterator is not utilized
				if (func.getCoeff(iter) == 0) { return; }

				// Copy the constraint 
				AffineFunction funccp(ret, func);
				// add the stride
				funccp.setCoeff(existenceVar, -stride_size);

				domain = domain and AffineConstraint(funccp, ConstraintType::EQ);
			});

		return domain;
	}

	// we have a disjunction of constraints which need to be tiled: i > (A ^ B) 
	//
	// which needs to be tiled in a way that (A ^ B) - i - Ts == 0
	//
	// This is obtained by introducing a new variable e, which is the actual lower bound 
	// e >= A && e >= B and use e for defining the strided domain:  (i -e -Ts == 0)
	VariablePtr boundVar = IRBuilder(mgr).variable(mgr.getLangBasic().getInt4());
	ret.add( Iterator(boundVar, true) );

	AffineConstraintPtr newBound, boundEq;
	for_each(bounds, [&](const ConjunctionList& cur) {
			
			for_each(cur, [&](const AffineConstraintPtr& cur) {
					assert(cur->getCombinerType() == CombinerType::CT_RAW);
					const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);

					// We are not interested in equality constraints 
					if (rc.getConstraint().getType() == ConstraintType::EQ) { return; }
					
					const AffineFunction& func = rc.getConstraint().getFunction();
					// We are not interested in constraints where the loop iterator is not utilized
					int coeff = func.getCoeff(iter);
					if (coeff == 0) { return; }
					
					// Copy the constraint 
					AffineFunction funccp(ret, func);
					// add the stride
					funccp.setCoeff(boundVar, coeff);
					funccp.setCoeff(iter, 0);

					AffineConstraint newBC(funccp, rc.getConstraint().getType());
					AffineConstraint newBEq(funccp, ConstraintType::EQ);

					newBound = newBound ? (newBound and newBC) : makeCombiner(newBC);
					boundEq = boundEq ? (boundEq or newBEq) : makeCombiner(newBEq);
				});
		});

	assert(newBound && boundEq);
	
	// we now impose the strided domain on the boundVar iterator previously introduced
	AffineFunction f(ret);
	f.setCoeff(existenceVar, -stride_size);
	f.setCoeff(iter, 1);
	f.setCoeff(boundVar, -1);

	return domain and newBound and boundEq and AffineConstraint(f, ConstraintType::EQ);
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
		RETHROW_EXCEPTION(NotASCoP, e, "Array access expression is not affine", expr);
	} catch(arithmetic::NotAFormulaException&& e) {
		RETHROW_EXCEPTION(NotASCoP, e, "Array access expression is not a valid formula", e.getCause());
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
		for_each(refs, [&](const RefPtr& cur) { 
			try {
				switch(cur->getType()) {

				case Ref::ARRAY:
				{
					const ArrayRef& arrRef = static_cast<const ArrayRef&>(*cur); 
					const ArrayRef::ExpressionList& idxExprs = arrRef.getIndexExpressions();

					if (idxExprs.empty()) { 
						THROW_EXCEPTION(NotASCoP, "Array utilized without proper indexing", 
							arrRef.getBaseExpression().getAddressedNode());
					}

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

			} catch (NotASCoP&& ex) {
				RETHROW_EXCEPTION(NotASCoP, ex, "", cur->getBaseExpression() ); 
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

			// A call expression should have not created sub scop
			assert(subScops.empty());

			// We have to make sure this is a call to a literal which is not a builtin literal
			ExpressionPtr func = callExpr->getFunctionExpr();
			if ( func->getNodeType() == NT_Literal && !addr->getNodeManager().getLangBasic().isBuiltIn(func) ) {

				FunctionSema&& sema = extractSemantics(callExpr);
				if (sema.containsReferenceAccesses()) {
					NodeManager& mgr = callExpr->getNodeManager();

					// We have semantics information we can build up 
					ScopRegion::Stmt::RefAccessList accesses;

					for_each(sema.accesses_begin(), sema.accesses_end(), [&](const FunctionSema::ReferenceAccess& cur) { 

							// Extract the reference being accessed from this argument 
							ExpressionAddress ref = cur.first.getReference();
							
							// If the displacement is constant and unary, then we don't need to have a ranged access 
							// but we can simply mark the usage of the accessed memory location 
							arithmetic::Piecewise displ = std::get<2>(cur.second) - std::get<1>(cur.second);

							IterationVector iv;

							if (displ.isFormula() && displ.toFormula() == 1) {
								assert(std::get<1>(cur.second).isFormula() && "The access index is not a Formula");

								ExpressionPtr&& index = arithmetic::toIR(mgr, std::get<1>(cur.second).toFormula());
								index->addAnnotation(std::make_shared<AccessFunction>(iv, AffineFunction(iv, index)));
								
								ret = merge(ret, iv);

								accesses.push_back( std::make_shared<ScopRegion::Reference>(
										ref, 
										std::get<0>(cur.second), 
										cur.first.getType(), 
										std::vector<ExpressionPtr>({ index })  // Generated the index manually
									) );
								return;
							}

							VariablePtr fakeIter = IRBuilder(mgr).variable(mgr.getLangBasic().getInt4());
							iv.add( Iterator(fakeIter) );
			
							// Compute the actual LB and UB
							AffineConstraintPtr bounds = 
								buildStridedDomain(mgr, iv, fakeIter, 
										std::get<1>(cur.second),  std::get<2>(cur.second), std::get<3>(cur.second)
									);

							ret = merge(ret, iv);

							accesses.push_back( std::make_shared<ScopRegion::Reference>(
									ref, 
									std::get<0>(cur.second), 
									cur.first.getType(), 
									std::vector<ExpressionPtr>({ fakeIter }), 
									iv,
									bounds
								) ); 
						});

					regionStmts.top().push_back( ScopRegion::Stmt(AS_STMT_ADDR(addr), accesses) );
					return ret;
				}
			}
		}

		if ( subScops.empty() ) {
			// this is a single stmt, therefore we can collect the references inside
			RefList&& refs = collectRefs(ret, AS_STMT_ADDR(addr));

			ScopRegion::Stmt::RefAccessList refList;
			for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) { 
					std::vector<ExpressionPtr> indeces;
					if (cur->getType() == Ref::ARRAY) {
						ArrayRef& arrRef = *std::dynamic_pointer_cast<ArrayRef>(cur);
						for_each(arrRef.getIndexExpressions(), 
							[&](const ExpressionAddress& cur) { indeces.push_back(cur.getAddressedNode()); });
					}
					refList.push_back(std::make_shared<ScopRegion::Reference>(
							cur->getBaseExpression(), cur->getUsage(), cur->getType(), indeces)
						);
				});
			// Add this statement to the scope for the parent node 
			regionStmts.top().push_back( ScopRegion::Stmt(AS_STMT_ADDR(addr), refList) );
		} else {
			// the substatement is a 
			regionStmts.top().push_back( ScopRegion::Stmt(AS_STMT_ADDR(addr), ScopRegion::Stmt::RefAccessList()) );
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
		//LOG(INFO) << "visitIfStmt:\n" << printer::PrettyPrinter(ifStmt);

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
			if(ex) RETHROW_EXCEPTION(NotASCoP, *ex, "", ifStmt.getAddressedNode() ); 

			THROW_EXCEPTION(NotASCoP, "One of the if branches invalided the SCoP", ifStmt.getAddressedNode());
		}

		// reset the value of the iteration vector 
		ret = IterationVector();

		// check the condition expression
		IterationDomain&& cond = extractFromCondition(ret, condAddr.getAddressedNode());
		// LOG(INFO) << cond;
	
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
		//LOG(INFO) << "visitSwitchStmt:\n" << printer::PrettyPrinter(switchStmt);
		
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
	
		insieme::utils::ExceptionPtr ex;

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
				ex = std::make_shared<const arithmetic::NotAFormulaException>(e);
			} catch (NotASCoP&& e) { 
				isSCoP = false; 
				ex = std::make_shared<const NotASCoP>(e);
			}
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
			} catch (NotASCoP&& e) { 
				isSCoP = false; 
				ex = std::make_shared<const NotASCoP>(e);
			}
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
			assert (ex);
			RETHROW_EXCEPTION(NotASCoP, *ex, "", switchStmt.getAddressedNode());
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
		
				Formula&& step = arithmetic::toFormula(forPtr->getStep());
				if (!step.isConstant()) {
					THROW_EXCEPTION(NotASCoP, "Non constant stride in for statement not supported", 
						forStmt.getAddressedNode()
					);
				}

				AffineConstraintPtr bounds = 
					buildStridedDomain<ExpressionPtr>(mgr, ret, forStmt->getIterator(), 
						forStmt->getStart(),  
						forPtr->getEnd(), 
						step
					);

				IterationDomain cons( bounds );
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

			} catch(arithmetic::NotAFormulaException&& e) {
				RETHROW_EXCEPTION(NotASCoP, e, "", forStmt.getAddressedNode()); 
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
		STACK_SIZE_GUARD;
		// We visit the body of the while stmt because there could be SCoPs inside
		visit(whileStmt->getBody());

		// Add eventual sub scops to the list of discovered top level scops
		if (whileStmt->getBody()->hasAnnotation(ScopRegion::KEY)) { 
			postProcessSCoP( whileStmt->getBody(), scopList );
		}

		// even if the body of the while stmt was a SCoP we have to discard the entire stmt for being 
		// a SCoP because while statement is not supported in the polyhedral model 
		THROW_EXCEPTION(NotASCoP, "While Statements not (yet) supported in the polyhedral model", 
				whileStmt.getAddressedNode()
			);
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
		//LOG(INFO) << "visitLambda:\n" << printer::PrettyPrinter(lambda);
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
		}
		
		subScops.push_back( lambda );

		return bodyIV;
	}

	IterationVector visitCallExpr(const CallExprAddress& callExpr) {
		STACK_SIZE_GUARD;
		//LOG(INFO) << "visitCallExpr:\n" << printer::PrettyPrinter(callExpr);

		const NodeAddress& func = callExpr->getFunctionExpr();
		const BasicGenerator& gen = callExpr->getNodeManager().getLangBasic();
		
		if ( func->getNodeType() == NT_Literal && !gen.isBuiltIn(func) ) {

			FunctionSema&& usage = extractSemantics(callExpr);
			// We cannot deal with function with side-effects as the polyhedral model could decide to split the function
			// into consecutive calls and this will break the semantics of the program 
			if ( usage.hasSideEffects() ) { 
				THROW_EXCEPTION(NotASCoP, "Call to a non-pure function", callExpr.getAddressedNode()); 
			}
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
			
			// check if called function has multiple returns
			LambdaExprPtr lex = static_pointer_cast<const LambdaExpr>(func.getAddressedNode());
			bool outlineAble = transform::isOutlineAble(lex->getBody());
			if(!outlineAble) THROW_EXCEPTION(NotASCoP, "Lambda with multiple return paths called", callExpr.getAddressedNode() ); // FIXME:

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
		THROW_EXCEPTION(NotASCoP, "Break statements not supported by the polyhedral model", 
				breakStmt.getAddressedNode() 
			);
	}

	IterationVector visitContinueStmt(const ContinueStmtAddress& contStmt) {
		THROW_EXCEPTION(NotASCoP, "Continue statements not supported by the polyhedral model",
				contStmt.getAddressedNode() 
			);
	}

	// FIXME: for now we force to break a SCoP anytime a RetStmt is encountred. This infact would
	// mean a function is returning from anypoint and makes it complex to be supported in the
	// polyhedral model. However function which returns as last operation of the body can be
	// supported. A solution for have better support for function would be inlining. 
	IterationVector visitReturnStmt(const ReturnStmtAddress& retStmt) {
		THROW_EXCEPTION(NotASCoP, "Return statements not supported by the polyhedral model", 
				retStmt.getAddressedNode()
			);
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
				VLOG(1) << e.what(); 
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
		
		const ScopRegion::Stmt::RefAccessList& ail = curStmt.getRefAccesses();

		std::for_each(ail.begin(), ail.end(), [&] (const ScopRegion::ReferencePtr& cur) {

				// if( usage != Ref::SCALAR && usage != Ref::MEMBER) { continue; }

				ExpressionPtr addr = cur->refExpr;
				switch ( cur->usage ) {
				case Ref::DEF:
				case Ref::UNKNOWN:
				{
					if ( iterVec.getIdx( addr ) != -1 ) {
						// This SCoP has to be discarded because one of the iterators or parameters
						// of the iteration domain has been overwritten within the body of the SCoP
						THROW_EXCEPTION(DiscardSCoPException, "Assignment to element of the iteration vector detected",  
							curStmt.getAddr().getAddressedNode(), addr 
						);
					}
				}
				default:
					break;
				}
			});

		// otherwise if one of the parameters of the SCoP is being defined in the body of the SCoP,
		// then this region must be invalided as well
		if (curStmt.getAddr()->getNodeType() == NT_DeclarationStmt) {
			
			// std::cout << iterVec << std::endl;
			// std::cout << *curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode() << std::endl;

			// make sure the declared variable is not one of the parameters of the SCoP
			if ( iterVec.contains( curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode() ) ) {
				THROW_EXCEPTION(DiscardSCoPException, "Declaration for one of the parameters of the SCoP is within the SCoP",  
						curStmt.getAddr().getAddressedNode(), curStmt.getAddr().as<DeclarationStmtAddress>()->getVariable().getAddressedNode()
					);
			}
		}

		});

	// now check stmts of the subScops
	const SubScopList& subScops = region.getSubScops();
	std::for_each(subScops.begin(), subScops.end(), [&](const SubScop& cur) { 
		detectInvalidSCoPs(iterVec, cur.first); 
	} );
}

bool postProcessSCoP(const NodeAddress& scop, AddressList& scopList) {
	assert ( scop->hasAnnotation(ScopRegion::KEY) );

	ScopRegion& region = *scop->getAnnotation( ScopRegion::KEY );
	if (!region.isValid()) { return false; }

	const IterationVector& iterVec = region.getIterationVector();

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
		return false;
	} 
	
// 	if (iterVec.getIteratorNum() == 0) {
// 		// A top level SCoP containing no loops. This is considered not a SCoP in the terminology of
// 		// the polyhedral model, therefore is discarded. However we don't set the flag to invalid
// 		// because this region could be inside another SCoP contanining loops therefore forming a
// 		// valid SCoP
// 		VLOG(1) << "Invalidating SCoP because it contains no loops "; 
// 		return true;
// 	}

	return true;

}

} // end namespace anonymous 

namespace insieme { namespace analysis { namespace polyhedral { 
	
/**************************************************************************************************
 * Extract constraints from a conditional expression. This is used for determining constraints for 
 * if and for statements. 
 *************************************************************************************************/
IterationDomain extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();

	if (cond->getNodeType() == NT_CastExpr) 
		return extractFromCondition(iv, cond.as<CastExprPtr>()->getSubExpression());

	if (cond->getNodeType() == NT_Variable && mgr.getLangBasic().isBool(cond->getType())) {
		THROW_EXCEPTION(NotASCoP, 
			"Condition expression is a boolean variable not supported by formulas", 
			cond
		);
	}

	assert (cond->getNodeType() == NT_CallExpr);

	const CallExprPtr& callExpr = cond.as<CallExprPtr>();

	if ( mgr.getLangBasic().isLogicOp(callExpr->getFunctionExpr()) )
	{
		// First of all we check whether this condition is a composed by multiple conditions
		// connected through || or && statements 
		BasicGenerator::Operator&& op = 
			mgr.getLangBasic().getOperator( callExpr->getFunctionExpr().as<LiteralPtr>() ); 

		switch (op) {
		case BasicGenerator::LOr:
		case BasicGenerator::LAnd: 
			{
				IterationDomain&& lhs = extractFromCondition(iv, callExpr->getArgument(0));
				// try to evaluate lazy arg 2 for analysis
				IterationDomain&& rhs = extractFromCondition(iv, 
					insieme::core::transform::evalLazy(mgr, callExpr->getArgument(1)));

				if (op == BasicGenerator::LAnd)	{ return lhs && rhs; }
				else 							{ return lhs || rhs; }
			}
		case BasicGenerator::LNot:
			 return !extractFromCondition(iv, callExpr->getArgument(0));
		default:
			assert(false && "Unsupported boolean operator");
			break;
		}
	}

	if ( mgr.getLangBasic().isIntCompOp(callExpr->getFunctionExpr()) || 
		 mgr.getLangBasic().isUIntCompOp(callExpr->getFunctionExpr()) ) 
	{
		assert(callExpr->getArguments().size() == 2 && "Malformed expression");

		BasicGenerator::Operator&& op = 
			mgr.getLangBasic().getOperator( callExpr->getFunctionExpr().as<LiteralPtr>() ); 

		// A constraints is normalized having a 0 on the right side, therefore we build a
		// temporary expression by subtracting the rhs to the lhs, Example: 
		//
		// if (a<b) { }    ->    if( a-b<0 ) { }
		try {
			IRBuilder builder(mgr);

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

			return IterationDomain( 
					::extractFrom(iv, 
						builder.sub(callExpr->getArgument(1), callExpr->getArgument(0)), 
						builder.intLit(0), 
						type
					) );

		} catch (arithmetic::NotAFormulaException&& e) { 
			RETHROW_EXCEPTION(NotASCoP, e, "Occurred during convertion of condition", e.getCause()); 

		} catch (NotAffineExpr&& e) { 
			RETHROW_EXCEPTION(NotASCoP, e, "Occurred during convertion of condition", cond);
		}
	}
	THROW_EXCEPTION(NotASCoP, "Condition expression cannot be converted into polyhedral model", cond);
}
	
namespace scop {

using namespace core;

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
void resolveScop(const IterationVector& 		iterVec, 
				 IterationDomain			 	parentDomain, 
	   		   	 const ScopRegion& 				region,
				 size_t&						pos,
				 size_t&						id,
 				 const AffineSystem&	 		curScat,
				 ScopRegion::IteratorOrder&		iterators,
				 Scop& 							scat) 
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
				assert(curPtr->hasAnnotation(ScopRegion::KEY));
				resolveScop( iterVec, 
							 thisDomain, 
							 *curPtr->getAnnotation(ScopRegion::KEY), 
							 pos, 
							 id,
							 curScat, 
							 iterators, 
							 scat
						   );
				return;
			}
		}

		af.setCoeff(Constant(), pos++);
		newScat.append( af );

		// this is a sub scop
		if (curPtr->hasAnnotation(ScopRegion::KEY)) {

			if ( curPtr->getNodeType() == NT_ForStmt ) {
				// if the statement is a loop, then we append a dimension with the corresponding
				// iterator variable and we go recursively to visit the body  
				const ForStmtPtr& forStmt = curPtr.as<ForStmtPtr>();
				const VariablePtr& iter = forStmt->getIterator();

				AffineFunction newAf( iterVec );
				newAf.setCoeff( Iterator(iter), 1 );
				newScat.append(newAf); 
				
				iterators.push_back(Iterator(iter));
			} 

			size_t nestedPos = 0;
			resolveScop( iterVec, 
						 thisDomain, 
						 *curPtr->getAnnotation(ScopRegion::KEY), 
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

		// Because some statements may introduce fake iterators we have to be sure to not nullify that value afterwards
		// This array will store all the fake iterators introduced by the current statement 
		std::set<VariablePtr> fakeIterators;

		// Access expressions 
		const ScopRegion::Stmt::RefAccessList& refs = cur.getRefAccesses();
		AccessList accInfo;
		std::for_each(refs.begin(), refs.end(), [&] (const ScopRegion::ReferencePtr& curRef) {
				AffineSystem idx(iterVec);

				std::shared_ptr<IterationDomain> domain = std::make_shared<IterationDomain>(iterVec);

				switch (curRef->type) {
				case Ref::SCALAR:
				case Ref::MEMBER:
					// A scalar is treated as a zero dimensional array 
					idx.append( AffineFunction(iterVec) );
					break;
				case Ref::ARRAY:
				{
					assert ((!curRef->indecesExpr.empty() || curRef->range) && "Array access without index specifier");

					if (curRef->range) {
						// This is a range access
						assert(curRef->indecesExpr.size() == 1);
						domain = std::make_shared<IterationDomain>( cloneConstraint(iterVec, curRef->range) );	

						// This statements introduced an iterator, therefore we have to make sure we do not set it zero
						// afterwards
						fakeIterators.insert( curRef->indecesExpr.front().as<VariablePtr>() );
						
						// create the affine function 1*fakeIter
						AffineFunction af(iterVec);
						af.setCoeff(curRef->indecesExpr.front().as<VariablePtr>(), 1);
						idx.append( af );
						break;
					}

					for_each(curRef->indecesExpr, [&](const ExpressionPtr& cur) { 
							assert(cur->hasAnnotation(scop::AccessFunction::KEY));
							scop::AccessFunction& ann = *cur->getAnnotation(scop::AccessFunction::KEY);
							idx.append( ann.getAccessFunction().toBase(iterVec) );
						}
					);
					break;
				}
				default:
					VLOG(1) << "Reference of type " << Ref::refTypeToStr(curRef->type) << " not handled!";
				}

				assert(domain);

				accInfo.push_back( 
					std::make_shared<AccessInfo>( 
							AS_EXPR_ADDR( concat<Node>(cur.getAddr(), curRef->refExpr ) ), 
							curRef->type, 
							curRef->usage, 
							idx,
							*domain
						)
					);
		});

		// save the domain 
		AffineConstraintPtr saveDomain = currDomain.getConstraint();

		IteratorSet nested_iters(iterators.begin(), iterators.end()), 
					domain_iters(iterVec.iter_begin(), iterVec.iter_end()), 
					used = getIterators(saveDomain),
					notUsed;
		
		std::copy(fakeIterators.begin(), fakeIterators.end(), std::inserter(nested_iters, nested_iters.begin()));
		std::copy(used.begin(), used.end(), std::inserter(nested_iters, nested_iters.begin()));

		// Remove iterators which do not belong to this nested region
		std::set_difference(
			domain_iters.begin(), domain_iters.end(), nested_iters.begin(), nested_iters.end(), 
			std::inserter(notUsed, notUsed.begin())
		);

		// set to zero all the not used iterators 
		std::for_each(notUsed.begin(), notUsed.end(), 
				[&] (const Iterator& curIt) { 
					if ( curIt.isExistential() ) { return; }

					AffineFunction af(iterVec);
					af.setCoeff(curIt, 1);
					af.setCoeff(Constant(), 0);
					saveDomain = saveDomain and AffineConstraint(af, ConstraintType::EQ);
				}
			);

		IterationDomain iterDom = saveDomain ? IterationDomain(saveDomain) : IterationDomain(iterVec);

		scat.push_back( Stmt( id++, cur.getAddr(), iterDom, newScat, accInfo ) );
	
	} ); 
}

namespace {

	/**
	 * This utility function is collecting all locally declared variables within the given
	 * code fragment and maps it to the list of enclosing iterator variables (in the corresponding order).
	 *
	 * @param cur the code fragment to be searched
	 * @return a map mapping all locally declared variables to the surrounding iterator variables
	 */
	std::map<core::VariablePtr, core::VariableList> collectLocalVars(const core::NodePtr& cur) {

		// collect all local declarations
		std::vector<core::DeclarationStmtAddress> decls;
		core::visitDepthFirst(core::NodeAddress(cur), [&](const core::DeclarationStmtAddress& cur) {
			// we only collect declaration statements outside for loops
			if (!cur.isRoot() && cur.getParentNode()->getNodeType() != core::NT_ForStmt) {
				decls.push_back(cur);
			}
		});

		// compute list of enclosing iterator variables
		std::map<core::VariablePtr, core::VariableList> res;
		for_each(decls, [&](const core::DeclarationStmtAddress& cur) {

			core::VariablePtr var = cur->getVariable();

			// collect iterator variables
			auto collector = core::makeLambdaVisitor([&](const ForStmtPtr& cur) {
				res[var].push_back(cur->getIterator());
			});

			core::visitPathTopDown(cur, collector);
		});

		return res;
	}
}


void ScopRegion::resolve() const {
	assert( isValid() && "Error Try to resolve an invalid SCoP");

	// If the region has been already resolved, we simply return the cached result
	if ( isResolved() ) { return; }

	// we compute the full scattering information for this domain and we cache the result for
	// later use. 
	scopInfo = std::make_shared<Scop>( iterVec );

	AffineSystem sf( getIterationVector() );
	ScopRegion::IteratorOrder iterOrder;
	
	// std::cout << *annNode << std::endl;
	// in the case the entry point of this scop is a forloop, then we build the scattering matrix
	// using the loop iterator index 
	if (annNode->getNodeType() == NT_ForStmt) {
		AffineFunction af( getIterationVector() );
		sf.append( af ); // the first dimension is composed by all zeros
		Iterator iter = Iterator(core::static_pointer_cast<const ForStmt>(annNode)->getIterator());
		af.setCoeff( iter, 1 );
		sf.append( af );
		iterOrder.push_back(iter);
	}

	size_t pos=0, id=0;
	resolveScop(
			getIterationVector(), 
			IterationDomain(getIterationVector()), 
			*this, 
			pos, 
			id,
			sf, 
			iterOrder, 
			*scopInfo
		);


	// --- fix interpretation of local variables ---

	// a map linking all locally declared variables to the list of surrounding iterators
	std::map<core::VariablePtr, core::VariableList> localVar = collectLocalVars(annNode);

	// update access functions for local variables
	for_each(*scopInfo, [&](StmtPtr& stmt) {

		// search accesses
		for_each(stmt->getAccess(), [&](AccessInfoPtr& access) {

			// extract variable (if there is one)
			core::ExpressionPtr expr = access->getExpr().getAddressedNode();
			if (expr->getNodeType() != core::NT_Variable) {
				return;
			}

			core::VariablePtr var = expr.as<core::VariablePtr>();

			// check whether variable is local
			auto pos = localVar.find(var);
			if (pos == localVar.end()) {
				return;	// it is not
			}

			// update affine access function
			AffineSystem&  accessFunctions = access->getAccess();

			// re-build access functions
			accessFunctions.clear();

			for_each(pos->second, [&](const core::VariablePtr& iter) {
				vector<int> coefficients;
				for_each(iterVec, [&](const Element& cur) {
					coefficients.push_back((cur.getType() == Element::ITER && *static_cast<const Iterator&>(cur).getVariable() == *iter) ? 1 : 0);
				});
				accessFunctions.append(AffineFunction(iterVec, coefficients));
			});

		});
	});

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

		if (root->hasAnnotation(ScopRegion::KEY)) {
			ret.push_back( NodeAddress(root) );
		}
	} catch (NotASCoP&& e) {
		LOG(WARNING) << e.what();
	}

	AddressList final;
	for(const auto& scop : ret) {
		postProcessSCoP(scop, final);
	}

	LOG(DEBUG) << "%%%% mark END\n" << final << "\n//%%\n";
	return final;
}

} } } } // end namespace insimee::analysis::polyhedral::scop




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

#include <stack>

#include "insieme/analysis/func_sema.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/except.h"
#include "insieme/analysis/polyhedral/iter_dom.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/scopregion.h"
#include "insieme/analysis/polyhedral/scopvisitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/constraint.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/set_utils.h"

using namespace insieme::core;
using namespace insieme::core::lang;

using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

namespace insieme { namespace analysis { namespace polyhedral {

void postProcessSCoP(const NodeAddress& scop, AddressList& scopList);
#define STACK_SIZE_GUARD \
	auto checkPostCond = [&](size_t stackInitialSize) -> void { 	 \
		assert(regionStmts.size() == stackInitialSize);				 \
	};																 \
	FinalActions __check_stack_size( std::bind(checkPostCond, regionStmts.size()) );
#define AS_STMT_ADDR(addr) static_address_cast<const Statement>(addr)

/** Stack utilized to keep track of statements which are inside a SCoP.
	because not all the compound statements of the IR are annotated by a SCoPRegion annotation,
	we need to have a way to collect statements which can be inside nested scopes. **/
typedef std::stack<std::vector<scop::Stmt> > RegionStmtStack;
RegionStmtStack regionStmts;

ScopVisitor::ScopVisitor(std::vector<NodeAddress> &scopList): IRVisitor<IterationVector, Address>(false), scopList(scopList) {
	regionStmts.push(RegionStmtStack::value_type());
}

IterationVector ScopVisitor::markAccessExpression(const ExpressionPtr& expr) {
	// If the expression is already annotated because shared, then we simply return the iteration
	// vector precendly computed (because by definition it has to be the same)
	if (expr->hasAnnotation(scop::AccessFunction::KEY)) {
		return expr->getAnnotation(scop::AccessFunction::KEY)->getIterationVector();
	}

	// otherwise we have to build an affine function from the IR access expression
	try {
		IterationVector it;
		expr->addAnnotation( std::make_shared<scop::AccessFunction>( it, AffineFunction(it, expr)) );
		return it;
	} catch(const NotAffineExpr& e) {
		RETHROW_EXCEPTION(NotASCoP, e, "Array access expression is not affine", expr);
	} catch(const arithmetic::NotAFormulaException& e) {
		RETHROW_EXCEPTION(NotASCoP, e, "Array access expression is not a valid formula", e.getCause());
	}
}

/** Visit the body of a SCoP. This requires to collect the iteration vector for the body and
	eventual ref access statements existing in the body. For each array access we extract the
	equality constraint used for accessing each dimension of the array and store it in the
	AccessFunction annotation. **/
RefList ScopVisitor::collectRefs(IterationVector& iterVec, const StatementAddress& body) {

	RefList&& refs = collectDefUse(body.getAddressedNode());

	// For now we only consider array access.
	//
	// TODO: In the future also scalars should be properly handled using technique like scalar
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

		} catch (const NotASCoP& ex) {
			RETHROW_EXCEPTION(NotASCoP, ex, "", cur->getBaseExpression());
		}
	});
	return refs;
}

/// Extract a constraint from a piecewise expression; called in the Piecewise version of extractFrom
AffineConstraintPtr ScopVisitor::fromConstraint( IterationVector& iterVect, const arithmetic::Constraint& constraint ) {

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
			if (!lit.second) atom = not_(atom);
			product = (!product) ? atom : product and atom;
		});

		// if product is still not set, set it to true
		if (!product)
			product = makeCombiner(AffineConstraint(AffineFunction(iterVect), ConstraintType::EQ));

		// combine product and overall result
		res = (!res) ? product : res or product;
	});

	// if result still not set, set it to false
	if (!res) res = makeCombiner(AffineConstraint(AffineFunction(iterVect), ConstraintType::NE));

	return res;
}

/** This function returns a set of constraints utilized to represent a piecewise expression,
a piecewise is in the form PW: { (cond1) -> a; (cond2) -> b }. We assume that the piecewise is
utilized in the following form.

EXPR - PW {LT,LE,EQ,NE,GT,GT} 0

Where EXPR can be any IR expression or a variable (for example a loop iterator) */
AffineConstraintPtr ScopVisitor::extractFrom(IterationVector& iterVec, const Piecewise& pw,
											   const ExpressionPtr& compExpr, const ConstraintType ct) {
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

AffineConstraintPtr ScopVisitor::extractFrom( IterationVector& iterVec, const ExpressionPtr& expr,
											  const ExpressionPtr& trg, const ConstraintType& ct) {
	using namespace arithmetic;
	using arithmetic::Piecewise;
	using arithmetic::Formula;

	NodeManager& mgr = expr->getNodeManager();

	IRBuilder builder(mgr);
	const lang::BasicGenerator& basic = mgr.getLangBasic();

	try {

		// Try to convert the expression as a piecewise
		return extractFrom(iterVec, arithmetic::toPiecewise(expr), trg, ct);

	} catch(const NotAPiecewiseException& e ) {

		CallExprPtr callExpr;
		int coeff;
		bool isModulus = false;

		// If the function is not an affine function and nor a piecewise affine function
		// then we enter in the special cases of function which can be transfotmed (via
		// some manipulation) into piecewise affine functions. For example floor, ceil,
		// min, max.
		if ( (callExpr = dynamic_pointer_cast<const CallExpr>( e.getCause() ) ) &&
			 ((coeff = -1, insieme::core::analysis::isCallOf( callExpr, basic.getCloogFloor() ) ) ||
			  (coeff = 1, insieme::core::analysis::isCallOf( callExpr, basic.getCloogCeil() ) ) ||
			  (coeff = -1, isModulus=true, insieme::core::analysis::isCallOf( callExpr, basic.getCloogMod() ) ) ||
			  (coeff = -1, isModulus=true, insieme::core::analysis::isCallOf( callExpr, basic.getSignedIntMod() ) ) ||
			  (coeff = -1, isModulus=true, insieme::core::analysis::isCallOf( callExpr, basic.getUnsignedIntMod() ) ) )
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
			VariablePtr var = iterVec.getFreshVariable(mgr);

			iterVec.add( Iterator(var, true) ); // make this iterator an existential iterator

			// An existential variable is required in order to set the system of equalities
			VariablePtr&& exist = iterVec.getFreshVariable(mgr);

			iterVec.add( Iterator(exist, true) ); // make this iterator an existential iterator

			AffineFunction af1( iterVec, callExpr->getArgument(0) );
			// (NUM) + var*DEN + exist == 0
			af1.setCoeff( var, -denVal );
			af1.setCoeff( exist, coeff );
			// set the stride
			AffineConstraintPtr boundCons = makeCombiner( AffineConstraint( af1, ConstraintType::EQ ) );

			// TODO: add code for ceil and mod
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

template <class BoundType> AffineConstraintPtr ScopVisitor::buildStridedDomain(NodeManager& mgr,
	IterationVector& ret, const VariablePtr& iter, const BoundType& lb, const BoundType& ub, const Formula& stride) {

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
	VariablePtr existenceVar = ret.getFreshVariable(mgr);
	ret.add( Iterator( existenceVar, true ) );

	// Gets the list of lower bounds as a disjunction of eleemnts
	DisjunctionList&& bounds = getConjunctions(toDNF(lbCons));
	assert(!bounds.empty() && !bounds.front().empty());

	// If we are in a situation where we only have a single bound for the loop then we can set the
	// stride starting from this bound
	if (bounds.size() == 1) {

		for_each(bounds.front(), [&](const AffineConstraintPtr& cur) {
				// detect whether this constraint is an equality
				assert(cur->getCombinerType() == insieme::utils::CombinerType::CT_RAW);

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
	VariablePtr boundVar = ret.getFreshVariable(mgr);
	ret.add( Iterator(boundVar, true) );

	AffineConstraintPtr newBound, boundEq;
	for_each(bounds, [&](const ConjunctionList& cur) {

			for_each(cur, [&](const AffineConstraintPtr& cur) {
					assert(cur->getCombinerType() == insieme::utils::CombinerType::CT_RAW);
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

/** Visit the body of a SCoP. */
IterationVector ScopVisitor::visitStmt(NodeAddress addr) {
	STACK_SIZE_GUARD;

	assert(subScops.empty());

	while(addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr) {
		addr = addr.getAddressOfChild((addr->getNodeType()==NT_MarkerStmt?1:2)); // sub-statement or expression
	}
	IterationVector&& ret = visit(addr);

	if (addr->getNodeType() == NT_CallExpr) {
		CallExprAddress callExpr = static_address_cast<const CallExpr>(addr);

		ExpressionPtr func = callExpr->getFunctionExpr();
		bool isBuiltIn = addr->getNodeManager().getLangBasic().isBuiltIn(func);

		if(!isBuiltIn && !callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
			return ret;
		}

		// A call expression should have not created sub scop - I am not sure this also holds for a lambda expr (tp)
		if (!subScops.empty() && func->getNodeType()==NT_Literal && !isBuiltIn)
			std::cerr << "Beware of possible bugs in " << __FILE__ << ":" << std::to_string(__LINE__) << std::endl;

		// We have to make sure this is a call to a literal which is not a builtin literal
		if (subScops.empty() && func->getNodeType()==NT_Literal && !isBuiltIn) {

			FunctionSema&& sema = extractSemantics(callExpr);
			if (sema.containsReferenceAccesses()) {
				NodeManager& mgr = callExpr->getNodeManager();

				// We have semantics information we can build up
				scop::Stmt::RefAccessList accesses;

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
						index->addAnnotation(std::make_shared<scop::AccessFunction>(iv, AffineFunction(iv, index)));

						ret = merge(ret, iv);

						accesses.push_back(std::make_shared<scop::Reference>(
												ref,
												std::get<0>(cur.second),
												cur.first.getType(),
												std::vector<ExpressionPtr>({ index })  // Generated the index manually
												));
						return;
					}

					VariablePtr fakeIter = iv.getFreshVariable(mgr);
					iv.add(Iterator(fakeIter));

					// Compute the actual LB and UB
					AffineConstraintPtr bounds=buildStridedDomain<insieme::core::arithmetic::Piecewise>
							(mgr, iv, fakeIter, std::get<1>(cur.second), std::get<2>(cur.second), std::get<3>(cur.second));

					ret = merge(ret, iv);

					accesses.push_back(std::make_shared<scop::Reference>(
											ref,
											std::get<0>(cur.second),
											cur.first.getType(),
											std::vector<ExpressionPtr>({ fakeIter }),
											iv,
											bounds
											));
				});

				regionStmts.top().push_back(scop::Stmt(AS_STMT_ADDR(addr), accesses));
				return ret;
			}
		}
	}

	if (subScops.empty()) {
		// this is a single stmt, therefore we can collect the references inside
		RefList&& refs = collectRefs(ret, AS_STMT_ADDR(addr));

		scop::Stmt::RefAccessList refList;
		for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) {
			std::vector<ExpressionPtr> indeces;
			if (cur->getType() == Ref::ARRAY) {
				ArrayRef& arrRef = *std::dynamic_pointer_cast<ArrayRef>(cur);
				for_each(arrRef.getIndexExpressions(),
						 [&](const ExpressionAddress& cur) { indeces.push_back(cur.getAddressedNode()); });
			}
			refList.push_back(std::make_shared<scop::Reference>(
								  cur->getBaseExpression(), cur->getUsage(), cur->getType(), indeces)
							 );
		});
		// Add this statement to the scope for the parent node
		regionStmts.top().push_back(scop::Stmt(AS_STMT_ADDR(addr), refList));
	} else {
		// the substatement is a
		regionStmts.top().push_back(scop::Stmt(AS_STMT_ADDR(addr), scop::Stmt::RefAccessList()));
	}
	return ret;
}

/** Extract constraints from a conditional expression. This is used for determining constraints for if and for
statements. */
IterationDomain ScopVisitor::extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();

	if (cond->getNodeType() == NT_CastExpr)
		return extractFromCondition(iv, cond.as<CastExprPtr>()->getSubExpression());

	if ((cond->getNodeType() == NT_Variable) || (cond->getNodeType() == NT_Literal)){
		THROW_EXCEPTION(NotASCoP,
			"Condition expression is a variable or literal not supported by formulas",
			cond
		);
	}

	if (cond->getNodeType() == NT_Variable && mgr.getLangBasic().isBool(cond->getType())) {
		THROW_EXCEPTION(NotASCoP,
			"Condition expression is a boolean variable not supported by formulas",
			cond
		);
	}

	assert (cond->getNodeType() == NT_CallExpr);
	const CallExprPtr& callExpr = cond.as<CallExprPtr>();

	// skip scalar casts
	if (mgr.getLangBasic().isScalarCast(callExpr->getFunctionExpr()))
		return extractFromCondition(iv, callExpr[0]);

	if ( mgr.getLangBasic().isLogicOp(callExpr->getFunctionExpr()) )
	{
		// First of all we check whether this condition is a composed by multiple conditions
		// connected through || or && statements
		BasicGenerator::Operator&& op =
			mgr.getLangBasic().getOperator( callExpr->getFunctionExpr() );

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

			core::ExpressionPtr&& leftExpr = callExpr->getArgument(0);
			core::ExpressionPtr&& rightExpr= callExpr->getArgument(1);


			//NOTE: no char substraction, if expr are char, need to cast to int.
			if (builder.getLangBasic().isChar(leftExpr->getType()) ||
				builder.getLangBasic().isChar(rightExpr->getType() )){
				THROW_EXCEPTION(NotASCoP, "char comparison can not be handled by polyedral model", cond);

			}

			return IterationDomain(extractFrom(iv, builder.sub(rightExpr, leftExpr), builder.intLit(0), type));

		} catch (const arithmetic::NotAFormulaException& e) {
			RETHROW_EXCEPTION(NotASCoP, e, "Occurred during convertion of condition", e.getCause());

		} catch (const NotAffineExpr& e) {
			RETHROW_EXCEPTION(NotASCoP, e, "Occurred during convertion of condition", cond);
		}
	}
	THROW_EXCEPTION(NotASCoP, "Condition expression cannot be converted into polyhedral model", cond);
}

/** Visit of If Statements: Visits the then and else body checking whether they are SCoPs. In the case both the
	branches are SCoPs the condition is evaluated and a constraint created out of it. Two annotations will be then
	created, one with the positive condition and the second one with the negated condition which will be attached
	respectively to the then and the else body of the if statement. In the case one of the two branches is not a SCoP, the
	SCoP branch is inserted in the list of root scops (scopList) and the NotAScop exception thrown to the parent node. */
IterationVector ScopVisitor::visitIfStmt(const IfStmtAddress& ifStmt) {
	STACK_SIZE_GUARD;
	//LOG(INFO) << "visitIfStmt:\n" << printer::PrettyPrinter(ifStmt);

	IterationVector ret, saveThen, saveElse;
	bool isThenSCOP = true, isElseSCOP = true;

	if (ifStmt->hasAnnotation(scop::ScopRegion::KEY)) {
		scop::ScopRegion& ann = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
		if (!ann.valid) { THROW_EXCEPTION(NotASCoP, "", ifStmt.getAddressedNode()); }

		// if the SCopRegion annotation is already attached, it means we already visited this
		// function, therefore we can return the iteration vector already precomputed
		subScops.push_back(ifStmt);
		return ann.getIterationVector();
	}

	ExpressionAddress condAddr = ifStmt->getCondition();
	StatementAddress  thenAddr = ifStmt->getThenBody();
	StatementAddress  elseAddr = ifStmt->getElseBody();

	regionStmts.push(RegionStmtStack::value_type());
	FinalActions faThen([&] () -> void { regionStmts.pop(); });

	std::shared_ptr<NotASCoP> ex;
	try {
		subScops.clear();
		// check the then body
		saveThen = visitStmt(thenAddr);
	} catch (const NotASCoP& e) {
		isThenSCOP = false;
		ex = std::make_shared<NotASCoP>(e);
	}

	regionStmts.push(RegionStmtStack::value_type());
	FinalActions faElse([&] () -> void { regionStmts.pop(); });

	try {
		subScops.clear();
		// check the else body
		saveElse = visitStmt(elseAddr);
	} catch (const NotASCoP& e) {
		isElseSCOP = false;
		ex = std::make_shared<NotASCoP>(e);
	}

	if (isThenSCOP && !isElseSCOP) {
		// then is a root of a ScopRegion, we add it to the list of scops
		postProcessSCoP(thenAddr, scopList);
	}

	if (!isThenSCOP && isElseSCOP) {
		// else is a root of a ScopRegion, we add it to the list of scops
		postProcessSCoP(elseAddr, scopList);
	}

	// if either one of the branches is not a ScopRegion it means the if statement is not a
	// ScopRegion, therefore we can re-throw the exception and invalidate this region
	if (!(isThenSCOP && isElseSCOP)) {
		if(ex) RETHROW_EXCEPTION(NotASCoP, *ex, "", ifStmt.getAddressedNode());

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

	std::vector<scop::Stmt> ifScopStmts;

	assert(regionStmts.top().size() == 1);
	ifScopStmts.push_back(regionStmts.top().front());

	// we saved the else body statements, therefore we can pop the record we allocated for it
	regionStmts.pop();
	faElse.setEnabled(false);

	assert(regionStmts.top().size() == 1);
	ifScopStmts.push_back(regionStmts.top().front());

	// we saved the then body statements, therefore we can pop the record we allocated for it
	regionStmts.pop();
	faThen.setEnabled(false);

	ifStmt->addAnnotation(
				std::make_shared<scop::ScopRegion>(
					ifStmt.getAddressedNode(),
					ret,
					IterationDomain(ret),
					std::vector<scop::Stmt>(ifScopStmts.rbegin(),ifScopStmts.rend()),
					std::list<SubScop>({SubScop(thenAddr, cond), SubScop(elseAddr, !cond) })
					)
				);

	// Checkpost conditions
	assert (ifStmt->hasAnnotation(scop::ScopRegion::KEY) &&
			ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY) &&
			ifStmt->getElseBody()->hasAnnotation(scop::ScopRegion::KEY) &&
			"IfStmt Post-Conditions check failed"
			);

	subScops.push_back(ifStmt);
	return ret;
}


/** SwitchStmt: for each of the cases of the switch statement we create a constraint enforcing conditionExpr -
	caseExpr == 0. The constraint for the default case is created by a conjunction of the negated case constraints. */
IterationVector ScopVisitor::visitSwitchStmt(const SwitchStmtAddress& switchStmt) {
	STACK_SIZE_GUARD;
	//LOG(INFO) << "visitSwitchStmt:\n" << printer::PrettyPrinter(switchStmt);

	if (switchStmt->hasAnnotation(scop::ScopRegion::KEY)) {
		scop::ScopRegion& ann = *switchStmt->getAnnotation(scop::ScopRegion::KEY);
		if (!ann.valid) { THROW_EXCEPTION(NotASCoP, "", switchStmt.getAddressedNode()); }

		// if the SCopRegion annotation is already attached, it means we already visited this
		// compoundstmt, therefore we can return the iteration vector already precomputed
		subScops.push_back(switchStmt);
		return ann.getIterationVector();
	}

	IterationVector ret;

	bool isSCoP = true;
	IRBuilder builder(switchStmt->getNodeManager());

	std::list<SubScop> scops;
	IterationDomain defaultCons(ret); // TODO: replace with universe constraint

	regionStmts.push(RegionStmtStack::value_type());
	FinalActions fa([&] () -> void { regionStmts.pop(); });

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
			assert (stmtAddr->hasAnnotation(scop::ScopRegion::KEY));

			AffineFunction af(ret, arithmetic::toFormula(switchStmt->getSwitchExpr()) -
							  arithmetic::toFormula(exprAddr.getAddressedNode()));

			IterationDomain caseCons(AffineConstraint(af, ConstraintType::EQ));
			defaultCons &= !caseCons;

			// Add this statement to the subScops
			scops.push_back(SubScop(stmtAddr, caseCons));

		} catch (const arithmetic::NotAFormulaException& e) {
			isSCoP = false;
			ex = std::make_shared<const arithmetic::NotAFormulaException>(e);
		} catch (const NotASCoP& e) {
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
			assert (defAddr->hasAnnotation(scop::ScopRegion::KEY));

			ret = merge(ret, iv);
			scops.push_back(SubScop(defAddr, defaultCons));
		} catch (const NotASCoP& e) {
			isSCoP = false;
			ex = std::make_shared<const NotASCoP>(e);
		}
	}

	if (!isSCoP) {
		// Add the entry points to the ScopList
		for(size_t caseID = 0; caseID < cases.size(); ++caseID) {
			// get the addess of the expression of this case stmt
			StatementAddress caseStmtAddr = cases[caseID]->getBody();

			if(caseStmtAddr->hasAnnotation(scop::ScopRegion::KEY)) {
				postProcessSCoP(caseStmtAddr, scopList) ;
			}

		}
		assert (ex);
		RETHROW_EXCEPTION(NotASCoP, *ex, "", switchStmt.getAddressedNode());
	}

	switchStmt->addAnnotation(
				std::make_shared<scop::ScopRegion>(
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

IterationVector ScopVisitor::visitForStmt(const ForStmtAddress& forStmt) {
	STACK_SIZE_GUARD;

	assert(subScops.empty());

	// if we already visited this forStmt, just return the precomputed iteration vector
	if (forStmt->hasAnnotation(scop::ScopRegion::KEY)) {
		scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
		if (!ann.valid) { THROW_EXCEPTION(NotASCoP, "", forStmt.getAddressedNode()); }

		// return the cached value
		subScops.push_back(forStmt);
		return ann.getIterationVector();
	}

	// Create a new scope for region stmts
	regionStmts.push(RegionStmtStack::value_type());

	{
		// remove element from the stack of statements from all the exit paths
		FinalActions fa([&] () -> void { regionStmts.pop(); subScops.clear(); });

		IterationVector&& bodyIV = visitStmt(forStmt->getBody()), ret;

		ForStmtPtr forPtr = forStmt.getAddressedNode();
		ret.add(Iterator(forPtr->getIterator()));

		NodeManager& mgr = forStmt->getNodeManager();
		IRBuilder builder(mgr);

		try {
			ret= merge(ret, bodyIV);

			// We assume the IR loop semantics to be the following:
			// i: lb...ub:s
			// which spawns a domain: lw <= i < ub exists x in Z : lb +
			// x*s = i
			// Check the lower bound of the loop

			Formula &&step= arithmetic::toFormula(forPtr->getStep());
			if (!step.isConstant()) {
				THROW_EXCEPTION(
					NotASCoP, "Non constant stride in for statement not supported", forStmt.getAddressedNode());
			}

			AffineConstraintPtr bounds= buildStridedDomain<ExpressionPtr>(
				mgr, ret, forStmt->getIterator(), forStmt->getStart(), forPtr->getEnd(), step);

			IterationDomain cons(bounds);
			assert(!forStmt->hasAnnotation(scop::ScopRegion::KEY));

			forStmt->addAnnotation(std::make_shared<scop::ScopRegion>(
				forStmt.getAddressedNode(), ret, cons, regionStmts.top(), toSubScopList(ret, subScops)));

			fa.setEnabled(false);

			regionStmts.pop();
			subScops.clear();

			// add this statement as a subscop
			subScops.push_back(forStmt);
		} catch (const NotAffineExpr &e) {
			// one of the expressions are not affine constraints, therefore we set this loop to be a
			// non ScopRegion
			RETHROW_EXCEPTION(NotASCoP, e, "", forStmt.getAddressedNode());

		} catch(const arithmetic::NotAFormulaException& e) {
			RETHROW_EXCEPTION(NotASCoP, e, "", forStmt.getAddressedNode());
		}

		return ret;
	}
}

/** While stmts cannot be represented in the polyhedral form (at least in the general case). In the future we may
	develop a more advanced analysis capable of representing while loops in the polyhedral model. */
IterationVector ScopVisitor::visitWhileStmt(const WhileStmtAddress& whileStmt) {
	STACK_SIZE_GUARD;
	// We visit the body of the while stmt because there could be SCoPs inside
	visit(whileStmt->getBody());

	// Add eventual sub scops to the list of discovered top level scops
	if (whileStmt->getBody()->hasAnnotation(scop::ScopRegion::KEY)) {
		postProcessSCoP(whileStmt->getBody(), scopList);
	}

	// even if the body of the while stmt was a SCoP we have to discard the entire stmt for being
	// a SCoP because while statement is not supported in the polyhedral model
	THROW_EXCEPTION(NotASCoP, "While Statements not (yet) supported in the polyhedral model",
					whileStmt.getAddressedNode()
					);
}

IterationVector ScopVisitor::visitCompoundStmt(const CompoundStmtAddress& compStmt) {
	STACK_SIZE_GUARD;

	IterationVector ret;
	bool isSCOP = true;
	std::vector<NodeAddress> scops;

	assert(subScops.empty());

	if (compStmt->hasAnnotation(scop::ScopRegion::KEY)) {
		scop::ScopRegion& ann = *compStmt->getAnnotation(scop::ScopRegion::KEY);
		if (!ann.valid) { THROW_EXCEPTION(NotASCoP, "", compStmt.getAddressedNode()); }

		// if the SCopRegion annotation is already attached, it means we already visited this
		// compoundstmt, therefore we can return the iteration vector already precomputed
		subScops.push_back(compStmt);
		return ann.getIterationVector();
	}

	regionStmts.push(RegionStmtStack::value_type());

	FinalActions fa([&] () -> void { regionStmts.pop(); });

	std::shared_ptr<NotASCoP> cause;

	for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
		// make sure at every iteration the stack size is not growing within this compound stmt
		STACK_SIZE_GUARD;
		try {
			// clear Sub scops
			subScops.clear();
			NodeAddress&& nodeAddr = compStmt.getAddressOfChild(i);
			ret = merge(ret, visitStmt(nodeAddr));
			// copy the sub spawned scops
			std::copy(subScops.begin(), subScops.end(), std::back_inserter(scops));
		} catch(const NotASCoP& e) {
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

		// TODO: Use the subScops
		// one of the statements in this compound statement broke a ScopRegion therefore we add
		// to the scop list the roots for valid ScopRegions inside this compound statement
		for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
			StatementAddress addr = AS_STMT_ADDR(compStmt.getAddressOfChild(i));

			// Get rid of marker statements
			while(addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr) {
				addr = AS_STMT_ADDR(addr.getAddressOfChild((addr->getNodeType()==NT_MarkerStmt?1:2))); // sub-statement or expression
			}

			if (addr->hasAnnotation(scop::ScopRegion::KEY)) {
				postProcessSCoP(addr, scopList);
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
	subScops.push_back(compStmt);

	return ret;
}

IterationVector ScopVisitor::visitMarkerStmt(const MarkerStmtAddress& mark) {
	return visit(mark->getSubStatement());
}

IterationVector ScopVisitor::visitMarkerExpr(const MarkerExprAddress& mark) {
	return visit(mark->getSubExpression());
}

IterationVector ScopVisitor::visitLambda(const LambdaAddress& lambda) {
	STACK_SIZE_GUARD;

	//LOG(INFO) << "visitLambda:\n" << printer::PrettyPrinter(lambda);
	assert(subScops.empty());

	if (lambda->hasAnnotation(scop::ScopRegion::KEY)) {
		// if the SCopRegion annotation is already attached, it means we already visited this
		// function, therefore we can return the iteration vector already precomputed
		subScops.push_back(lambda);
		return lambda->getAnnotation(scop::ScopRegion::KEY)->getIterationVector();
	}

	IterationVector bodyIV;
	// otherwise we have to visit the body and attach the ScopRegion annotation
	{
		regionStmts.push(RegionStmtStack::value_type());
		// remove element from the stack of statements from all the exit paths
		FinalActions fa([&] () -> void { regionStmts.pop(); subScops.clear(); });

		StatementAddress addr = AS_STMT_ADDR(lambda->getBody());  /*getBody()*/
		bodyIV = visitStmt(addr);

		assert (addr->hasAnnotation(scop::ScopRegion::KEY));
		assert(subScops.size() == 1 && "A Lambda cannot have more than one sub SCoP");

		lambda->addAnnotation(
					std::make_shared<scop::ScopRegion>(
						lambda.getAddressedNode(),
						bodyIV,
						IterationDomain(bodyIV),
						regionStmts.top(),
						toSubScopList(bodyIV, subScops)
						)
					);

		postProcessSCoP(addr, scopList) ;
	}

	subScops.push_back(lambda);

	return bodyIV;
}

IterationVector ScopVisitor::visitCallExpr(const CallExprAddress& callExpr) {
	STACK_SIZE_GUARD;
	//LOG(INFO) << "visitCallExpr:\n" << printer::PrettyPrinter(callExpr);

	const NodeAddress& func = callExpr->getFunctionExpr();
	const BasicGenerator& gen = callExpr->getNodeManager().getLangBasic();

	// do not look into built-in functions
	if (gen.isBuiltIn(func)) {
		return IterationVector();
	}

	if (func->getNodeType() == NT_Literal && !gen.isBuiltIn(func)) {

		FunctionSema&& usage = extractSemantics(callExpr);
		// We cannot deal with function with side-effects as the polyhedral model could decide to split the function
		// into consecutive calls and this will break the semantics of the program
		if (usage.hasSideEffects()) {
			THROW_EXCEPTION(NotASCoP, "Call to a non-pure function", callExpr.getAddressedNode());
		}
	}

	IterationVector iterVec;

	// Visit the arguments of this call expression using the evaluation order of C
	// (right-to-left). This will make the ordering of the statements inside the SCoP
	// consistent.
	const vector<ExpressionAddress>&& arguments = callExpr->getArguments();
	for(auto it = arguments.rbegin(); it != arguments.rend(); ++it) {
		iterVec = merge(iterVec, visit(*it));
	}

	std::vector<NodeAddress> scops(subScops);
	subScops.clear();

	NodeAddress lambdaScop;

	// Visit the body of the function
	iterVec = visitNode(func);

	if (func->getNodeType() == NT_LambdaExpr) {
		assert(subScops.size() == 1);

		lambdaScop = subScops.front();
	}

	if (func->getNodeType() == NT_LambdaExpr) {
		assert(lambdaScop);

		// check if called function has multiple returns
		LambdaExprPtr lex = static_pointer_cast<const LambdaExpr>(func.getAddressedNode());
		bool outlineAble = transform::isOutlineAble(lex->getBody());
		if(!outlineAble) THROW_EXCEPTION(NotASCoP, "Lambda with multiple return paths called", callExpr.getAddressedNode());

		const scop::ScopRegion& lambda = *lambdaScop->getAnnotation(scop::ScopRegion::KEY);
		const std::vector<scop::Stmt>& stmts = lambda.getDirectRegionStmts();

		std::copy(stmts.begin(), stmts.end(), std::back_inserter(regionStmts.top()));

		auto lambdaSubScops = lambda.getSubScops();
		for_each(lambdaSubScops.begin(), lambdaSubScops.end(), [&](const SubScop& cur) { scops.push_back(cur.first); });
	}

	subScops.clear();
	std::copy(scops.begin(), scops.end(), std::back_inserter(subScops));

	return iterVec;
}

IterationVector ScopVisitor::visitBreakStmt(const BreakStmtAddress& breakStmt) {
	THROW_EXCEPTION(NotASCoP, "Break statements not supported by the polyhedral model",
					breakStmt.getAddressedNode()
					);
}

IterationVector ScopVisitor::visitContinueStmt(const ContinueStmtAddress& contStmt) {
	THROW_EXCEPTION(NotASCoP, "Continue statements not supported by the polyhedral model",
					contStmt.getAddressedNode()
					);
}

// TODO: for now we force to break a SCoP anytime a RetStmt is encountred. This infact would
// mean a function is returning from anypoint and makes it complex to be supported in the
// polyhedral model. However function which returns as last operation of the body can be
// supported. A solution for have better support for function would be inlining.
IterationVector ScopVisitor::visitReturnStmt(const ReturnStmtAddress& retStmt) {
	THROW_EXCEPTION(NotASCoP, "Return statements not supported by the polyhedral model",
					retStmt.getAddressedNode()
					);
}

IterationVector ScopVisitor::visitProgram(const ProgramAddress& prog) {
	for(size_t i=0, end=prog->getEntryPoints().size(); i!=end; ++i) {
		try {
			NodeAddress&& addr = prog.getAddressOfChild(i);
			visit(addr);
			assert(subScops.size() == 1);
			postProcessSCoP(subScops.front(), scopList);
		} catch(const NotASCoP& e) {
			subScops.empty();
			VLOG(1) << e.what();
		}
	}
	return IterationVector();
}

// Generic method which recursively visit IR nodes and merges the resulting
// iteration vectors
IterationVector ScopVisitor::visitNode(const NodeAddress& node) {
	IterationVector ret;
	for_each(node->getChildList(), [&](const NodeAddress& cur){
		ret = merge(ret, this->visit(cur));
	});
	return ret;
}

}}}

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

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/numeric_cast.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace arithmetic {

NotAFormulaException::NotAFormulaException(const NodePtr& expr) :expr(expr) {
	if (!expr) { return; }

	std::ostringstream ss;
	ss << "Cannot convert expression '" << *expr << "', not a formula!";
	msg = ss.str();
}

const char* NotAFormulaException::what() const throw() {	
	return msg.c_str(); 
}

namespace {

	class FormulaConverter : public IRVisitor<Formula> {

		const lang::BasicGenerator& lang;

	public:

		FormulaConverter(const lang::BasicGenerator& lang) : IRVisitor(false), lang(lang) {}

	protected:

		Formula visitLiteral(const LiteralPtr& cur) {
			checkType(cur);
			return utils::numeric_cast<int>(cur->getValue()->getValue());
		}

		Formula visitVariable(const VariablePtr& cur) {
			checkType(cur);
			return cur;
		}

		Formula visitCallExpr(const CallExprPtr& call) {
			checkType(call);

			// check for terminal values
			if (Value::isValue(call)) {
				return Value(call);
			}

			// check number of arguments
			if (call->getArguments().size() != static_cast<std::size_t>(2)) {
				throw NotAFormulaException(call);
			}

			// check function
			ExpressionPtr fun = call->getFunctionExpr();

			// special handling of division
			if (lang.isSignedIntDiv(fun) || lang.isUnsignedIntDiv(fun)) {
				ExpressionPtr argA = call->getArgument(0);
				if (argA->getNodeType() != NT_Literal) {
					throw NotAFormulaException(call);
				}
				LiteralPtr lit = static_pointer_cast<const Literal>(argA);
				if (!lang.isInt(lit->getType()) || lit->getValue()->getValue() != "1") {
					throw NotAFormulaException(call);
				}

				// convert second parameter
				Formula f = visit(call->getArgument(1));
				if (f.getTerms().size() > static_cast<std::size_t>(1)) {
					throw NotAFormulaException(call);
				}

				// return 1/second argument
				return Product()/f.getTerms()[0].first;
			}

			// handle remaining operators as usual
			Formula a = visit(call->getArgument(0));
			Formula b = visit(call->getArgument(1));

			if (lang.isSignedIntAdd(fun) || lang.isUnsignedIntAdd(fun)) {
				return a + b;
			}
			if (lang.isSignedIntSub(fun) || lang.isUnsignedIntSub(fun)) {
				return a - b;
			}
			if (lang.isSignedIntMul(fun) || lang.isUnsignedIntMul(fun)) {
				return a * b;
			}

			// no supported formula
			throw NotAFormulaException(call);
		}

		Formula visitCastExpr(const CastExprPtr& cur) {
			checkType(cur);
			return visit(cur->getSubExpression());
		}

		Formula visitNode(const NodePtr& cur) {
			throw NotAFormulaException(cur);
		}

	private:

		void checkType(const ExpressionPtr& expr) {
			// check that current expression is a integer expression
			if (!lang.isInt(expr->getType())) {
				throw NotAFormulaException(expr);
			}
		}

	};

} // end anonumous namespace


Formula toFormula(const ExpressionPtr& expr) {
	// the magic is done by the formula converter
	return FormulaConverter(expr->getNodeManager().getLangBasic()).visit(expr);
}

Piecewise toPiecewise(const ExpressionPtr& expr) {
	
	NodeManager& mgr = expr->getNodeManager();
	try {
		Formula&& f = toFormula( expr );
		return Piecewise( makeCombiner(Piecewise::Predicate(0, Piecewise::PredicateType::EQ)), f );
	} catch( NotAFormulaException&& e ) {
		
		if (CallExprPtr callExpr = dynamic_pointer_cast<const CallExpr>( e.getCause() )) {
			
			Piecewise::PredicatePtr pred;
			if ( analysis::isCallOf(callExpr, mgr.getLangBasic().getSelect() ) ) {
				// build a piecewise 
				assert( callExpr->getArguments().size() == 3 );
				
				NodePtr comp = callExpr->getArgument(2);
				Piecewise::PredicateType compTy;
				if (*comp == *mgr.getLangBasic().getSignedIntLt()) {
					compTy = Piecewise::PredicateType::LT;
				} else if (*comp == *mgr.getLangBasic().getSignedIntGt()) {
					compTy = Piecewise::PredicateType::GT;
				} else if (*comp == *mgr.getLangBasic().getSignedIntGe()) {
					compTy = Piecewise::PredicateType::GE;
				} else if (*comp == *mgr.getLangBasic().getSignedIntLe()) {
					compTy = Piecewise::PredicateType::LE;
				} else if (*comp == *mgr.getLangBasic().getSignedIntEq()) {
					compTy = Piecewise::PredicateType::EQ;
				} else if (*comp == *mgr.getLangBasic().getSignedIntNe()) {
					compTy = Piecewise::PredicateType::NE;
				} else { assert ( false && "Comparator not recognized"); }
				
				Piecewise&& lhsPw = toPiecewise( static_pointer_cast<const Expression> (
							transform::replaceAll(mgr, expr, callExpr, callExpr->getArgument(0)) 
						) );

				Piecewise&& rhsPw = toPiecewise( static_pointer_cast<const Expression> ( 
							transform::replaceAll(mgr, expr, callExpr, callExpr->getArgument(1))
						) );

				// When the lhs and rhs operation are formulas we can easily build a if-then-else
				// piecewise expreession 
				if ( isFormula(lhsPw) && isFormula(rhsPw) ) {
					Formula&& lhs = toFormula(callExpr->getArgument(0));
					Formula&& rhs = toFormula(callExpr->getArgument(1));
					Piecewise::Predicate pred(lhs - rhs, compTy);

					return Piecewise( makeCombiner(pred), toFormula(lhsPw), toFormula(rhsPw) );
				}
			
				// Otherwise we have to take care of merging the inner piecewises 
				std::vector<Piecewise::Piece> pieces;
				// assert(innerPwTrue.isFormula());
				for_each(lhsPw, [&] (const Piecewise::Piece& lhsCur) {
					for_each(rhsPw, [&] (const Piecewise::Piece& rhsCur) {
								pieces.push_back( 
									Piecewise::Piece( lhsCur.first and rhsCur.first and 
										Piecewise::Predicate(lhsCur.second - rhsCur.second, compTy), lhsCur.second)
									); 
							});
						});

				return Piecewise(pieces);
			}
		}
		throw NotAPiecewiseException(e.getCause());
	}
}

namespace {

	const TypePtr getBiggerType(const TypePtr& a, const TypePtr& b) {
		assert(a->getNodeManager().getLangBasic().isInt(a) && "Has to be a IntegerType!");
		assert(b->getNodeManager().getLangBasic().isInt(b) && "Has to be a IntegerType!");

		// quick exit
		if (*a == *b) {
			return a;
		}

		const GenericTypePtr& t1 = static_pointer_cast<const GenericType>(a);
		const GenericTypePtr& t2 = static_pointer_cast<const GenericType>(b);
		const IntTypeParamPtr& p1 = t1->getIntTypeParameter()[0];
		const IntTypeParamPtr& p2 = t2->getIntTypeParameter()[0];

		// if both are signed / unsigned
		if (t1->getFamilyName() == t2->getFamilyName()) {
			return p1<p2?b:a;
		}

		// for the rest => use generic solution
		return getSmallestCommonSuperType(a, b);
	}

	ExpressionPtr createCall(IRBuilder& builder, const ExpressionPtr& fun, const ExpressionPtr& a, const ExpressionPtr& b) {
		return builder.callExpr(getBiggerType(a->getType(), b->getType()), fun, a, b);
	}

}


ExpressionPtr toIR(NodeManager& manager, const Product::Factor& factor) {
	assert(factor.second != 0 && "Factor's exponent must not be 0!");

	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();
	auto MulOp = basic.getSignedIntMul();

	// determine absolute exponent
	int exponent = factor.second;
	if (exponent < 0) {
		exponent = -exponent;
	}

	// handle exponent
	ExpressionPtr res = factor.first;
	for (int i=1; i<exponent; ++i) {
		res = createCall(builder, MulOp, res, factor.first);
	}

	// handle negative exponent
	if (factor.second < 0) {
		auto DivOp = basic.getSignedIntDiv();
		auto one = builder.intLit(1);
		return createCall(builder, DivOp, one, res);
	}

	// done
	return res;

}

ExpressionPtr toIR(NodeManager& manager, const Product& product) {

	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();
	auto MulOp = basic.getSignedIntMul();

	if (product.isOne()) {
		return builder.intLit(1);
	}

	auto it = product.getFactors().begin();
	auto end = product.getFactors().end();
	assert(it != end && "Must not be empty!");

	// append first
	ExpressionPtr res = toIR(manager, *it);
	++it;

	// append rest
	for_each(it, end, [&](const Product::Factor& cur) {
		res = createCall(builder, MulOp, res, toIR(manager, cur));
	});

	// done
	return res;
}

ExpressionPtr toIR(NodeManager& manager, const Div& div) {

	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	if (div.isInteger())
		// there is no denominator therefore we can return the integer
		// numerator
		return builder.intLit( div.getNum() );

	return builder.callExpr(basic.getSignedIntDiv(), 
				builder.intLit(div.getNum()), 
				builder.intLit(div.getDen())
			);
}

ExpressionPtr toIR(NodeManager& manager, const Formula& formula) {

	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	auto AddOp = basic.getSignedIntAdd();
	auto SubOp = basic.getSignedIntSub();
	auto MulOp = basic.getSignedIntMul();


	if (formula.isZero()) {
		return builder.intLit(0);
	}

	auto it = formula.getTerms().begin();
	auto end = formula.getTerms().end();

	ExpressionPtr res;
	if (it->second == Div(1) && it->first.isOne()) {
		res = builder.intLit(1);
	} else if (it->second == Div(1)) {
		res = toIR(manager, it->first);
	} else if (it->first.isOne()) {
		res = toIR(manager, it->second);
	} else {
		res = createCall(builder, MulOp, toIR(manager, it->second), toIR(manager, it->first));
	}
	++it;

	for_each(it, end, [&](const Formula::Term& cur) {

		Div factor;
		ExpressionPtr op;
		if (cur.second.getNum() > 0) {
			factor = cur.second;
			op = AddOp;
		} else {
			factor = Div( -cur.second.getNum(), cur.second.getDen() );
			op = SubOp;
		}

		if (factor == Div(1) && cur.first.isOne()) {
			res = builder.callExpr(op, res, builder.intLit(1));
		} else if (factor == Div(1)) {
			res = createCall(builder, op, res, toIR(manager, cur.first));
		} else if (cur.first.isOne()) {
			res = createCall(builder, op, res, toIR(manager, factor));
		} else {
			res = createCall(builder, op, res, createCall(builder, MulOp, toIR(manager, factor), toIR(manager, cur.first)));
		}
	});

	// done
	return res;
}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

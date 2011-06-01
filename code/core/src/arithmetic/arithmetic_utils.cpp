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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_visitor.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace arithmetic {


namespace {

	class FormulaConverter : public ASTVisitor<Formula> {

		const lang::BasicGenerator& lang;



	public:

		FormulaConverter(const lang::BasicGenerator& lang) : ASTVisitor(false), lang(lang) {}

	protected:

		Formula visitLiteral(const LiteralPtr& cur) {
			checkType(cur);
			return utils::numeric_cast<int>(cur->getValue());
		}

		Formula visitVariable(const VariablePtr& cur) {
			checkType(cur);
			return cur;
		}

		Formula visitCallExpr(const CallExprPtr& call) {
			checkType(call);

			// check number of arguments
			if (call->getArguments().size() != static_cast<std::size_t>(2)) {
				throw NotAFormulaException();
			}

			// check function
			ExpressionPtr fun = call->getFunctionExpr();

			// special handling of division
			if (lang.isSignedIntDiv(fun) || lang.isUnsignedIntDiv(fun)) {
				ExpressionPtr argA = call->getArgument(0);
				if (argA->getNodeType() != NT_Literal) {
					throw NotAFormulaException();
				}
				LiteralPtr lit = static_pointer_cast<const Literal>(argA);
				if (!lang.isInt(lit->getType()) || lit->getValue() != "1") {
					throw NotAFormulaException();
				}

				// convert second parameter
				Formula f = visit(call->getArgument(1));
				if (f.getTerms().size() > static_cast<std::size_t>(1)) {
					throw NotAFormulaException();
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
			throw NotAFormulaException();
		}

		Formula visitCastExpr(const CastExprPtr& cur) {
			checkType(cur);
			return visit(cur->getSubExpression());
		}

		Formula visitNode(const NodePtr& cur) {
			throw NotAFormulaException();
		}

	private:

		void checkType(const ExpressionPtr& expr) {
			// check that current expression is a integer expression
			if (!lang.isInt(expr->getType())) {
				throw NotAFormulaException();
			}
		}

	};

}


Formula toFormula(const ExpressionPtr& expr) {
	// the magic is done by the formula converter
	return FormulaConverter(expr->getNodeManager().getBasicGenerator()).visit(expr);
}


namespace {



}


ExpressionPtr toIR(NodeManager& manager, const Product::Factor& factor) {
	assert(factor.second != 0 && "Factor's exponent must not be 0!");

	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getBasicGenerator();
	auto MulOp = basic.getSignedIntMul();

	// determine absolute exponent
	int exponent = factor.second;
	if (exponent < 0) {
		exponent = -exponent;
	}

	// handle exponent
	ExpressionPtr res = factor.first;
	for (int i=1; i<exponent; ++i) {
		res = builder.callExpr(MulOp, res, factor.first);
	}

	// handle negative exponent
	if (factor.second < 0) {
		auto DivOp = basic.getSignedIntDiv();
		auto one = builder.intLit(1);
		return builder.callExpr(DivOp, one, res);
	}

	// done
	return res;

}

ExpressionPtr toIR(NodeManager& manager, const Product& product) {

	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getBasicGenerator();
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
		res = builder.callExpr(MulOp, res, toIR(manager, cur));
	});

	// done
	return res;
}


ExpressionPtr toIR(NodeManager& manager, const Formula& formula) {

	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getBasicGenerator();

	auto AddOp = basic.getSignedIntAdd();
	auto SubOp = basic.getSignedIntSub();
	auto MulOp = basic.getSignedIntMul();


	if (formula.isZero()) {
		return builder.intLit(0);
	}

	auto it = formula.getTerms().begin();
	auto end = formula.getTerms().end();

	ExpressionPtr res;
	if (it->second == 1 && it->first.isOne()) {
		res = builder.intLit(1);
	} else if (it->second == 1) {
		res = toIR(manager, it->first);
	} else if (it->first.isOne()) {
		res = builder.intLit(it->second);
	} else {
		res = builder.callExpr(MulOp, builder.intLit(it->second), toIR(manager, it->first));
	}
	++it;

	for_each(it, end, [&](const Formula::Term& cur) {

		int factor;
		ExpressionPtr op;
		if (cur.second > 0) {
			factor = cur.second;
			op = AddOp;
		} else {
			factor = -cur.second;
			op = SubOp;
		}

		if (cur.second == 1 && cur.first.isOne()) {
			res = builder.callExpr(op, res, builder.intLit(1));
		} else if (cur.second == 1) {
			res = builder.callExpr(op, res, toIR(manager, cur.first));
		} else if (cur.first.isOne()) {
			res = builder.callExpr(op, res, builder.intLit(factor));
		} else {
			res = builder.callExpr(op, res, builder.callExpr(MulOp,builder.intLit(factor), toIR(manager, cur.first)));
		}
	});


	// done
	return res;
}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

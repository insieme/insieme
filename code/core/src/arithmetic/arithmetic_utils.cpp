/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace arithmetic {

	NotAFormulaException::NotAFormulaException(const ExpressionPtr& expr)
	    : expr(expr), msg(format("Unable to convert expression %s - it is not a formula!", toString(*expr))) {}

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

				// if it is a value => use it as a value
				if(Value::isValue(cur)) { return Value(cur); }

				// check whether it is a integer constant
				try {
					return utils::numeric_cast<int64_t>(cur->getValue()->getValue());
				} catch(boost::bad_lexical_cast) {
					// The literal wasn't a signed int, try unsigned
					// we cannot cast the literal to an integer value (probably because it was a double)
					throw NotAFormulaException(cur);
				}
			}

			Formula visitVariable(const VariablePtr& cur) {
				checkType(cur);
				return cur;
			}

			Formula visitCallExpr(const CallExprPtr& call) {
				checkType(call);

				// check for terminal values
				if(Value::isValue(call)) { return Value(call); }

				// handle casts
				if(lang.isNumericCast(call->getFunctionExpr())) {
					return visit(transform::extractInitExprFromDecl(call[0])); // ignore casts
				}

				// handle selects
				if(lang.isSelect(call->getFunctionExpr())) {
					// try resolving it as a piecewise formula
					Piecewise piecewise = toPiecewise(call);
					if(piecewise.isFormula()) { return piecewise.toFormula(); }

					// this is not a formula
					throw NotAFormulaException(call);
				}

				// check number of arguments
				if(call->getNumArguments() != static_cast<std::size_t>(2)) { throw NotAFormulaException(call); }

				// check function
				ExpressionPtr fun = call->getFunctionExpr();

				// special handling of division

				// handle remaining operators as usual
				Formula a = visit(transform::extractInitExprFromDecl(call[0]));
				Formula b = visit(transform::extractInitExprFromDecl(call[1]));

				if(lang.isSignedIntAdd(fun) || lang.isUnsignedIntAdd(fun)) { return a + b; }
				if(lang.isSignedIntSub(fun) || lang.isUnsignedIntSub(fun)) { return a - b; }
				if(lang.isSignedIntMul(fun) || lang.isUnsignedIntMul(fun)) { return a * b; }
				if(lang.isSignedIntDiv(fun) || lang.isUnsignedIntDiv(fun)) {
					// check whether divisor is 1 or -1
					if(b.isConstant()) {
						if(b.isOne()) { return a; }
						if((-b).isOne()) { return -a; }
					}

					// integer division can only be safely converted in case both operators are constants
					if(a.isInteger() && b.isInteger()) { return static_cast<int64_t>(a.getConstantValue()) / static_cast<int64_t>(b.getConstantValue()); }

					// one exception: if variables are eliminated during computation - e.g. x/x  = 1 and 7x/3x = 2
					if(b.getTerms().size() == static_cast<std::size_t>(1)) {
						Formula res = a / b.getTerms()[0];
						if(res.isConstant()) {
							// get integer part of constant result
							return static_cast<int64_t>(res.getConstantValue());
						}
					}
				}

				// no supported formula
				throw NotAFormulaException(call);
			}

			Formula visitCastExpr(const CastExprPtr& cur) {
				checkType(cur);
				return visit(cur->getSubExpression());
			}

			Formula visitExpression(const ExpressionPtr& cur) {
				throw NotAFormulaException(cur);
			}

			Formula visitNode(const NodePtr& cur) {
				throw NotAFormulaException(ExpressionPtr());
			}

		  private:
			void checkType(const ExpressionPtr& expr) {
				// check that current expression is a integer expression
				if(!lang.isInt(expr->getType())) { throw NotAFormulaException(expr); }
			}
		};

	} // end anonymous namespace


	boost::optional<int64_t> toConstantInt(const ExpressionPtr& expr) {
		try {
			auto form = toFormula(expr);
			if(form.isInteger()) return form.getIntegerValue();
		} catch(NotAFormulaException) {}
		return {};
	}

	Formula toFormula(const ExpressionPtr& expr) {
		// the magic is done by the formula converter
		return FormulaConverter(expr->getNodeManager().getLangBasic()).visit(expr);
	}

	namespace {

		class ConstraintConverter : public IRVisitor<Constraint> {
			const lang::BasicGenerator& lang;
			detail::BDDManagerPtr bddManager;

		  public:
			ConstraintConverter(const lang::BasicGenerator& lang) : IRVisitor(false), lang(lang), bddManager(detail::createBDDManager()) {}

		  protected:
			Constraint visitLiteral(const LiteralPtr& cur) {
				checkType(cur);
				if(lang.isTrue(cur)) { return Constraint::getTrue(bddManager); }
				if(lang.isFalse(cur)) { return Constraint::getFalse(bddManager); }
				throw NotAConstraintException(cur);
			}

			Constraint visitVariable(const VariablePtr& cur) {
				// boolean variables are not supported directly
				throw NotAConstraintException(cur);
			}

			Constraint visitCallExpr(const CallExprPtr& call) {
				checkType(call);

				// check boolean constraints
				ExpressionPtr fun = call->getFunctionExpr();

				// --- logical operations between constraints ---

				// handle logic and
				if(lang.isBoolLAnd(fun)) {
					// get left and right constraint
					Constraint a = visit(call->getArgument(0));
					Constraint b = visit(transform::evalLazy(call->getNodeManager(), call->getArgument(1)));
					return a && b;
				}

				// handle logic or
				if(lang.isBoolLOr(fun)) {
					// get left and right constraint
					Constraint a = visit(call->getArgument(0));
					Constraint b = visit(transform::evalLazy(call->getNodeManager(), call->getArgument(1)));
					return a || b;
				}

				// handle logic negation
				if(lang.isBoolLNot(fun)) { return !visit(call->getArgument(0)); }

				// handle equality
				if(lang.isBoolEq(fun)) {
					Constraint a = visit(call->getArgument(0));
					Constraint b = visit(call->getArgument(1));
					return (a && b) || (!a && !b);
				}

				// handle not-equal
				if(lang.isBoolNe(fun)) {
					Constraint a = visit(call->getArgument(0));
					Constraint b = visit(call->getArgument(1));
					return (a && !b) || (!a && b);
				}

				// --- comparison operators between formulas ---

				// check whether it is a comparison operator
				if(call->getNumArguments() != 2u || fun->getNodeType() != core::NT_Literal || !lang.isCompOp(fun)) { throw NotAConstraintException(call); }

				// handle arguments (need to be formulas
				Formula a;
				Formula b;
				try {
					a = toFormula(call->getArgument(0));
					b = toFormula(call->getArgument(1));
				} catch(const NotAFormulaException& nfe) {
					// if it is not a formula, it is also not a constraint
					throw NotAConstraintException(call);
				}

				// process according to comparison operator encountered
				switch(lang.getOperator(fun.as<LiteralPtr>())) {
				case core::lang::BasicGenerator::Operator::Eq: return eq(a, b);
				case core::lang::BasicGenerator::Operator::Ne: return ne(a, b);
				case core::lang::BasicGenerator::Operator::Lt: return a < b;
				case core::lang::BasicGenerator::Operator::Le: return a <= b;
				case core::lang::BasicGenerator::Operator::Gt: return a > b;
				case core::lang::BasicGenerator::Operator::Ge: return a >= b;
				default: break; // ignore
				}

				// no supported formula
				throw NotAConstraintException(call);
			}

			Constraint visitExpression(const ExpressionPtr& cur) {
				throw NotAConstraintException(cur);
			}

			Constraint visitNode(const NodePtr& cur) {
				throw NotAConstraintException(ExpressionPtr());
			}

		  private:
			void checkType(const ExpressionPtr& expr) {
				// check that current expression is a integer expression
				if(!lang.isBool(expr->getType())) { throw NotAConstraintException(expr); }
			}
		};

	} // end anonymous namespace

	Constraint toConstraint(const ExpressionPtr& expr) {
		// the magic is done by the constraint converter
		return ConstraintConverter(expr->getNodeManager().getLangBasic()).visit(expr);
	}

	namespace {

		class PiecewiseConverter : public IRVisitor<Piecewise> {
			FormulaConverter formulaConverter;

			const lang::BasicGenerator& lang;

		  public:
			PiecewiseConverter(const lang::BasicGenerator& lang) : IRVisitor(false), formulaConverter(lang), lang(lang) {}

			Piecewise visit(const NodePtr& node) {
				try {
					// special handling for select statements (to break recursive cycle with formula converter).
					if(analysis::isCallOf(node, lang.getSelect())) { return visitCallExpr(node.as<CallExprPtr>()); }

					// try whether it is a regular formula
					return formulaConverter.visit(node);

				} catch(const NotAFormulaException& nafe) {
					// convert pieces => using visitor mechanism
				}

				return IRVisitor<Piecewise>::visit(node);
			}

		  protected:
			Piecewise visitCallExpr(const CallExprPtr& call) {
				checkType(call);

				// it can be assumed that it is not a formula (yet the type is correct)

				// check function
				ExpressionPtr fun = call->getFunctionExpr();

				// process selects
				if(lang.isSelect(fun)) {
					assert_eq(call->getNumArguments(), 3u);

					// arguments must be formulas
					Piecewise a = visit(call->getArgument(0));
					Piecewise b = visit(call->getArgument(1));
					const auto& pred = call->getArgument(2);

					if(lang.isSignedIntLt(pred) || lang.isUnsignedIntLt(pred)) {
						return min(a, b);
					} else if(lang.isSignedIntLe(pred) || lang.isUnsignedIntLe(pred)) {
						return min(a, b);
					} else if(lang.isSignedIntGt(pred) || lang.isUnsignedIntGt(pred)) {
						return max(a, b);
					} else if(lang.isSignedIntGe(pred) || lang.isUnsignedIntGe(pred)) {
						return max(a, b);
					}

					// unsupported operation encountered
					throw NotAPiecewiseException(call);
				}

				if(call->getNumArguments() == 2) {
					// handle remaining integer operators as usual
					Piecewise a = visit(call->getArgument(0));
					Piecewise b = visit(call->getArgument(1));

					if(lang.isSignedIntAdd(fun) || lang.isUnsignedIntAdd(fun)) { return a + b; }
					if(lang.isSignedIntSub(fun) || lang.isUnsignedIntSub(fun)) { return a - b; }
					if(lang.isSignedIntMul(fun) || lang.isUnsignedIntMul(fun)) { return a * b; }
				}

				// NOTE: integer division can only be supported on the formula level!
				// TODO: add support for real division ..

				// no supported formula
				throw NotAPiecewiseException(call);
			}

			Piecewise visitCastExpr(const CastExprPtr& cur) {
				checkType(cur);
				return visit(cur->getSubExpression());
			}

			Piecewise visitNode(const NodePtr& cur) {
				throw NotAPiecewiseException(ExpressionPtr());
			}

		  private:
			void checkType(const ExpressionPtr& expr) {
				// check that current expression is a integer expression
				if(!lang.isInt(expr->getType())) { throw NotAPiecewiseException(expr); }
			}
		};

	} // end anonymous namespace

	Piecewise toPiecewise(const ExpressionPtr& expr) {
		// the magic is done by the piecewise converter
		return PiecewiseConverter(expr->getNodeManager().getLangBasic()).visit(expr);
	}

	namespace {

		const TypePtr getBiggerType(const TypePtr& a, const TypePtr& b) {
			assert_true(a->getNodeManager().getLangBasic().isInt(a)) << "Has to be a IntegerType!";
			assert_true(b->getNodeManager().getLangBasic().isInt(b)) << "Has to be a IntegerType!";

			// quick exit
			if(*a == *b) { return a; }

			const GenericTypePtr& t1 = a.as<GenericTypePtr>();
			const GenericTypePtr& t2 = b.as<GenericTypePtr>();
			const NumericTypePtr& p1 = t1->getTypeParameter(0).as<NumericTypePtr>();
			const NumericTypePtr& p2 = t2->getTypeParameter(0).as<NumericTypePtr>();

			// if both are signed / unsigned
			if(t1->getFamilyName() == t2->getFamilyName()) {
				if(p1->isConstant() && p2->isConstant()) {
					boost::optional<int> n1 = p1->getValue().as<LiteralPtr>()->getValueAs<int>();
					boost::optional<int> n2 = p2->getValue().as<LiteralPtr>()->getValueAs<int>();
					assert_true(n1) << "Cast error: Cannot cast LiteralPtr 'n1' to int!";
					assert_true(n2) << "Cast error: Cannot cast LiteralPtr 'n2' to int!";
					return (*n1 < *n2) ? b : a;
				}
			}

			// for the rest => use generic solution
			TypePtr res = types::getSmallestCommonSuperType(a, b);
			assert_false(a->getNodeManager().getLangBasic().isUnit(res)) << "Invalid arguments passed to function!";
			return res;
		}

		ExpressionPtr createCall(IRBuilder& builder, const ExpressionPtr& fun, const ExpressionPtr& a, const ExpressionPtr& b) {
			return builder.callExpr(getBiggerType(a->getType(), b->getType()), fun, a, b);
		}
	}


	ExpressionPtr toIR(NodeManager& manager, const Rational& value) {
		IRBuilder builder(manager);

		// check for pure integer
		if(value.isInteger()) {
			// there is no denominator therefore we can return the integer numerator
			return builder.intLit(value.getNumerator());
		}

		// if we have to produce a division, members have to be real, otherwise C will round them (will produce int division)
		return builder.div(builder.floatLit(value.getNumerator()), builder.floatLit(value.getDenominator()));
	}

	ExpressionPtr toIR(NodeManager& manager, const Product::Factor& factor) {
		assert_ne(factor.second, 0) << "Factor's exponent must not be 0!";

		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();
		auto MulOp = basic.getSignedIntMul();

		// determine absolute exponent
		int exponent = factor.second;
		if(exponent < 0) { exponent = -exponent; }

		// handle value
		ExpressionPtr res = factor.first;
		if(analysis::isRefType(res->getType())) { res = builder.deref(res); }

		// handle exponent
		for(int i = 1; i < exponent; ++i) {
			res = createCall(builder, MulOp, res, factor.first);
		}

		// handle negative exponent
		if(factor.second < 0) {
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

		if(product.isOne()) { return builder.intLit(1); }

		auto it = product.getFactors().begin();
		auto end = product.getFactors().end();
		assert(it != end && "Must not be empty!");

		// append first
		ExpressionPtr res = toIR(manager, *it);
		++it;

		// append rest
		for_each(it, end, [&](const Product::Factor& cur) { res = createCall(builder, MulOp, res, toIR(manager, cur)); });

		// done
		return res;
	}

	ExpressionPtr toIR(NodeManager& manager, const Formula& formula) {
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		auto AddOp = basic.getSignedIntAdd();
		auto SubOp = basic.getSignedIntSub();
		auto MulOp = basic.getSignedIntMul();


		if(formula.isZero()) { return builder.intLit(0); }

		auto it = formula.getTerms().begin();
		auto end = formula.getTerms().end();

		ExpressionPtr res;
		if(it->second.isOne() && it->first.isOne()) {
			res = builder.intLit(1);
		} else if(it->second == Rational(1)) {
			res = toIR(manager, it->first);
		} else if(it->first.isOne()) {
			res = toIR(manager, it->second);
		} else {
			res = createCall(builder, MulOp, toIR(manager, it->second), toIR(manager, it->first));
		}
		++it;

		for_each(it, end, [&](const Formula::Term& cur) {

			Rational factor;
			ExpressionPtr op;
			if(cur.second.getNumerator() > 0) {
				factor = cur.second;
				op = AddOp;
			} else {
				factor = -cur.second;
				op = SubOp;
			}

			if(factor.isOne() && cur.first.isOne()) {
				res = createCall(builder, op, res, builder.intLit(1));
			} else if(factor.isOne()) {
				res = createCall(builder, op, res, toIR(manager, cur.first));
			} else if(cur.first.isOne()) {
				res = createCall(builder, op, res, toIR(manager, factor));
			} else {
				res = createCall(builder, op, res, createCall(builder, MulOp, toIR(manager, factor), toIR(manager, cur.first)));
			}
		});

		// done
		return res;
	}

	ExpressionPtr toIR(NodeManager& manager, const Constraint& constraint) {
		IRBuilder builder(manager);

		// deal with valid and unsatisfiable
		if(constraint.isValid()) { return builder.boolLit(true); }
		if(constraint.isUnsatisfiable()) { return builder.boolLit(false); }

		// obtain zero for comparison
		ExpressionPtr zero = toIR(manager, Formula());

		// just use DNF to produce the corresponding IR structure
		ExpressionPtr res;
		for_each(constraint.toDNF(), [&](const Constraint::Conjunction& conjunct) {
			ExpressionPtr product;

			for_each(conjunct, [&](const Constraint::Literal& lit) {

				// convert literal
				ExpressionPtr cur = builder.le(toIR(manager, lit.first.getFormula()), zero);

				// negate if necessary
				if(!lit.second) { cur = builder.logicNeg(cur); }

				// combine literals
				product = (product) ? builder.logicAnd(product, builder.wrapLazy(cur)) : cur;
			});

			assert_true(product) << "There should not be an empty conjunction!";

			// combine conjunctions
			res = (res) ? builder.logicOr(res, builder.wrapLazy(product)) : product;
		});

		assert_true(res) << "There should not be an empty DNF!";

		// return result
		return res;
	}

	namespace {

		typedef vector<Piecewise::Piece>::const_iterator piece_iterator;

		ExpressionPtr convertPieceList(const IRBuilder& builder, piece_iterator cur, piece_iterator end) {
			NodeManager& manager = builder.getNodeManager();

			// check whether it is done
			if(cur + 1 == end) {
				// this is the last piece, so the condition can be skipped
				return toIR(manager, cur->second);
			}

			// process current piece and rest
			return builder.ite(toIR(manager, cur->first), builder.wrapLazy(toIR(manager, cur->second)),
			                   builder.wrapLazy(convertPieceList(builder, cur + 1, end)));
		}
	}


	ExpressionPtr toIR(NodeManager& manager, const Piecewise& piecewise) {
		// handle formulas more directly
		if(piecewise.isFormula()) { return toIR(manager, piecewise.toFormula()); }

		// convert piecewise step by step
		auto start = piecewise.getPieces().begin();
		auto end = piecewise.getPieces().end();
		assert(start != end && "Should not be empty if it is not a formula!");
		return convertPieceList(IRBuilder(manager), start, end);
	}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

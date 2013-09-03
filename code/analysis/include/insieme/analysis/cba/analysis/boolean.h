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

#pragma once

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/arithmetic.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::set;

	// ----------------- booleans analysis ---------------

	template<typename C> class BooleanConstraintGenerator;
	typedef TypedSetType<bool,BooleanConstraintGenerator> BooleanSetType;

	extern const BooleanSetType B;
	extern const BooleanSetType b;

	namespace {

		using namespace insieme::core;

		template<
			typename F,
			typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
			typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
			typename R = typename lambda_traits<F>::result_type
		>
		struct pair_wise {
			F f;
			pair_wise(const F& f) : f(f) {}
			set<R> operator()(const set<A>& a, const set<B>& b) const {
				set<R> res;
				for(auto& x : a) {
					for(auto& y : b) {
						res.insert(f(x,y));
					}
				}
				return res;
			}
		};

		template<typename F>
		pair_wise<F> pairwise(const F& f) {
			return pair_wise<F>(f);
		}

		template<typename Comparator>
		std::function<set<bool>(const set<Formula>&,const set<Formula>&)> compareFormula(const Comparator& fun) {
			return [=](const set<Formula>& a, const set<Formula>& b)->set<bool> {
				static const set<bool> unknown({true, false});

				set<bool> res;

				// quick check
				for(auto& x : a) if (!x) return unknown;
				for(auto& x : b) if (!x) return unknown;

				// check out pairs
				bool containsTrue = false;
				bool containsFalse = false;
				for(auto& x : a) {
					for(auto& y : b) {
						if (containsTrue && containsFalse) {
							return res;
						}

						// pair< valid, unsatisfiable >
						pair<bool,bool> validity = fun(*x.formula, *y.formula);
						if (!containsTrue && !validity.second) {
							res.insert(true);
							containsTrue = true;
						}

						if (!containsFalse && !validity.first) {
							res.insert(false);
							containsFalse = true;
						}
					}
				}
				return res;
			};
		}

		bool isBooleanSymbol(const ExpressionPtr& expr) {
			auto& gen = expr->getNodeManager().getLangBasic();
			return expr.isa<LiteralPtr>() && !gen.isTrue(expr) && !gen.isFalse(expr);
		}

	}

	template<typename Context>
	class BooleanConstraintGenerator : public BasicDataFlowConstraintGenerator<bool,BooleanSetType,Context> {

		typedef BasicDataFlowConstraintGenerator<bool,BooleanSetType,Context> super;

		const core::lang::BasicGenerator& base;

		CBA& cba;

	public:

		BooleanConstraintGenerator(CBA& cba)
			: super(cba, cba::B, cba::b),
			  base(cba.getRoot()->getNodeManager().getLangBasic()),
			  cba(cba)
		{ };

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// only interested in boolean literals
			if (!base.isBool(literal->getType())) return;

			// add constraint literal \in A(lit)
			bool isTrue = base.isTrue(literal);
			bool isFalse = base.isFalse(literal);

			auto l_lit = cba.getLabel(literal);

			if (isTrue  || (!isTrue && !isFalse)) constraints.add(elem(true, cba.getSet(B, l_lit, ctxt)));
			if (isFalse || (!isTrue && !isFalse)) constraints.add(elem(false, cba.getSet(B, l_lit, ctxt)));

		}


		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// conduct std-procedure
			super::visitCallExpr(call, ctxt, constraints);

			// only care for integer expressions calling literals
			if (!base.isBool(call->getType())) return;

			// check whether it is a literal => otherwise basic data flow is handling it
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>()) return;

			// get some labels / ids
			auto B_res = cba.getSet(B, cba.getLabel(call), ctxt);

			// handle unary literals
			if (call.size() == 1u) {

				// check whether it is a de-ref
				if (base.isRefDeref(fun)) {
					return;		// has been handled by super!
				}

				// support negation
				if (base.isBoolLNot(fun)) {
					auto B_arg = cba.getSet(B, cba.getLabel(call[0]), ctxt);
					constraints.add(subsetUnary(B_arg, B_res, [](const set<bool>& in)->set<bool> {
						set<bool> out;
						for(bool cur : in) out.insert(!cur);
						return out;
					}));
					return;
				}
			}

			// and binary operators
			if (call.size() != 2u) {
				// this value is unknown => might be both
				constraints.add(elem(true, B_res));
				constraints.add(elem(false, B_res));
				return;
			}


			// boolean relations
			{
				// get sets for operators
				auto B_lhs = cba.getSet(B, cba.getLabel(call[0]), ctxt);
				auto B_rhs = cba.getSet(B, cba.getLabel(call[1]), ctxt);

				if (base.isBoolEq(fun)) {
					// equality is guaranteed if symbols are identical - no matter what the value is
					if (isBooleanSymbol(call[0]) && isBooleanSymbol(call[1])) {
						constraints.add(elem(call[0].as<ExpressionPtr>() == call[1].as<ExpressionPtr>(), B_res));
					} else {
						constraints.add(subsetBinary(B_lhs, B_rhs, B_res, pairwise([](bool a, bool b) { return a == b; })));
					}
					return;
				}

				if (base.isBoolNe(fun)) {
					// equality is guaranteed if symbols are identical - no matter what the value is
					if (isBooleanSymbol(call[0]) && isBooleanSymbol(call[1])) {
						constraints.add(elem(call[0].as<ExpressionPtr>() != call[1].as<ExpressionPtr>(), B_res));
					} else {
						constraints.add(subsetBinary(B_lhs, B_rhs, B_res, pairwise([](bool a, bool b) { return a != b; })));
					}
					return;
				}
			}

			// arithmetic relations
			{
				auto A_lhs = cba.getSet(cba::A, cba.getLabel(call[0]), ctxt);
				auto A_rhs = cba.getSet(cba::A, cba.getLabel(call[1]), ctxt);

				typedef core::arithmetic::Formula F;
				typedef core::arithmetic::Inequality Inequality;		// shape: formula <= 0

				if(base.isSignedIntLt(fun) || base.isUnsignedIntLt(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
						// a < b  ... if !(a >= b) = !(b <= a) = !(b-a <= 0)
						Inequality i(b-a);
						return std::make_pair(i.isUnsatisfiable(), i.isValid());
					})));
					return;
				}

				if(base.isSignedIntLe(fun) || base.isUnsignedIntLe(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
						// a <= b ... if (a-b <= 0)
						Inequality i(a-b);
						return std::make_pair(i.isValid(), i.isUnsatisfiable());
					})));
					return;
				}

				if(base.isSignedIntGe(fun) || base.isUnsignedIntGe(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b){
						// a >= b ... if (b <= a) = (b-a <= 0)
						Inequality i(b-a);
						return std::make_pair(i.isValid(), i.isUnsatisfiable());
					})));
					return;
				}

				if(base.isSignedIntGt(fun) || base.isUnsignedIntGt(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b){
						// a > b ... if !(a <= b) = !(a-b <= 0)
						Inequality i(a-b);
						return std::make_pair(i.isUnsatisfiable(), i.isValid());
					})));
					return;
				}

				if(base.isSignedIntEq(fun) || base.isUnsignedIntEq(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
						// just compare formulas (in normal form)
						bool equal = (a==b);
						return std::make_pair(equal, !equal && a.isConstant() && b.isConstant());
					})));
					return;
				}

				if(base.isSignedIntNe(fun) || base.isUnsignedIntNe(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
						// just compare formulas (in normal form)
						bool equal = (a==b);
						return std::make_pair(!equal && a.isConstant() && b.isConstant(), equal);
					})));
					return;
				}
			}

			// otherwise it is unknown, hence both may be possible
			constraints.add(elem(true, B_res));
			constraints.add(elem(false, B_res));
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

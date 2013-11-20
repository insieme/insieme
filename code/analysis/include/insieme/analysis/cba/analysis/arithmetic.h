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

#include <boost/optional.hpp>

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/framework/entities/formula.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::set;

	// ----------------- arithmetic analysis ---------------

	template<typename C> class ArithmeticConstraintGenerator;
	typedef DataAnalysisType<Formula,ArithmeticConstraintGenerator> ArithmeticSetType;

	extern const ArithmeticSetType A;
	extern const ArithmeticSetType a;



	namespace {

		template<typename A, typename B, typename R>
		class total_binary_op {
			typedef std::function<R(const A&, const B&)> fun_type;
			fun_type fun;
		public:
			total_binary_op(const fun_type& fun) : fun(fun) {}

			R operator()(const A& a, const B& b) const {
				static const R fail;
				if (!a) return fail;
				if (!b) return fail;
				return fun(a,b);
			}
		};

		template<
			typename F,
			typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
			typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
			typename R = typename lambda_traits<F>::result_type
		>
		total_binary_op<A,B,R> total(const F& fun) {
			return total_binary_op<A,B,R>(fun);
		}

		template<typename A, typename B, typename R>
		class cartesion_product_binary_op {

			typedef std::function<R(const A&, const B&)> fun_type;
			fun_type fun;

		public:

			cartesion_product_binary_op(const fun_type& fun) : fun(fun) {}

			set<R> operator()(const set<A>& a, const set<B>& b) const {
				set<R> res;

				// if there is any undefined included => it is undefined
				if (any(a, [](const A& a)->bool { return !a; }) || any(b, [](const B& b)->bool { return !b; })) {
					res.insert(R());
					return res;
				}

				// compute the cross-product
				for(auto& x : a) {
					for (auto& y : b) {
						res.insert(fun(x,y));
					}
				}

				if (res.size() > 10) {
					// build a set only containing the unknown value
					set<R> res;
					res.insert(R());
					return res;
				}

				return res;
			}

		};

		template<
			typename F,
			typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
			typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
			typename R = typename lambda_traits<F>::result_type
		>
		cartesion_product_binary_op<A,B,R> cartesion_product(const F& fun) {
			return cartesion_product_binary_op<A,B,R>(fun);
		}

	}


	template<typename Context>
	class ArithmeticConstraintGenerator : public BasicDataFlowConstraintGenerator<Formula, ArithmeticSetType, Context> {

		typedef BasicDataFlowConstraintGenerator<Formula, ArithmeticSetType, Context> super;
		typedef typename ArithmeticSetType::lattice_type::value_type value_type;

		const core::lang::BasicGenerator& base;

		CBA& cba;

	public:

		ArithmeticConstraintGenerator(CBA& cba)
			: super(cba, cba::A, cba::a),
			  base(cba.getRoot()->getNodeManager().getLangBasic()),
			  cba(cba)
		{ };

		using super::elem;
		using super::pack;

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// only interested in integer literals
			if (!base.isInt(literal->getType())) return;

			// add constraint literal \in A(lit)
			auto value = core::arithmetic::toFormula(literal);
			auto l_lit = cba.getLabel(literal);

			auto A_lit = cba.getSet(A, l_lit, ctxt);
			constraints.add(elem(value, A_lit));

		}

		void visitVariable(const VariableAddress& var, const Context& ctxt, Constraints& constraints) {

			// special handling: if variable is a loop iterator, use symbolic value
			auto def = getDefinitionPoint(var);
			if (def != var || var.getDepth() < 2 || !var.getParentNode(2).isa<ForStmtPtr>()) {
				// => not a iterator, let parent handle the case
				super::visitVariable(var, ctxt, constraints);
				return;
			}

			// it is a iterator!
			//	=> use symbolic value
			auto value = core::arithmetic::toFormula(var);

			auto v_var = cba.getVariable(var);
			auto l_var = cba.getLabel(var);

			auto a_var = cba.getSet(cba::a, v_var, ctxt);
			auto A_var = cba.getSet(cba::A, l_var, ctxt);

			constraints.add(elem(value, a_var));
			constraints.add(subset(a_var, A_var));

		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
			static const value_type unknown;

			// conduct std-procedure
			super::visitCallExpr(call, ctxt, constraints);

			// only care for integer expressions calling literals
			if (!base.isInt(call->getType())) return;

			// check whether it is a literal => otherwise basic data flow is handling it
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>()) return;

			// get some labels / ids
			auto A_res = cba.getSet(A, cba.getLabel(call), ctxt);

			// handle unary literals
			if (call.size() == 1u) {
				if (base.isRefDeref(fun)) {
					return;		// has been handled by super!
				}
			}

			// and binary operators
			if (call.size() != 2u) {
				// this value is unknown (by default)
				return;
			}

			// get sets for operators
			auto A_lhs = cba.getSet(A, cba.getLabel(call[0]), ctxt);
			auto A_rhs = cba.getSet(A, cba.getLabel(call[1]), ctxt);

			// special handling for functions
			if (base.isSignedIntAdd(fun) || base.isUnsignedIntAdd(fun)) {
				constraints.add(subsetBinary(A_lhs, A_rhs, A_res, pack(cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
					return *a.formula + *b.formula;
				})))));
				return;
			}

			if (base.isSignedIntSub(fun) || base.isUnsignedIntSub(fun)) {
				constraints.add(subsetBinary(A_lhs, A_rhs, A_res, pack(cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
					return *a.formula - *b.formula;
				})))));
				return;
			}

			if (base.isSignedIntMul(fun) || base.isUnsignedIntMul(fun)) {
				constraints.add(subsetBinary(A_lhs, A_rhs, A_res, pack(cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
					return *a.formula * *b.formula;
				})))));
				return;
			}

			// otherwise it is unknown
			constraints.add(elem(unknown, A_res));
		}

		void visitCastExpr(const CastExprAddress& cast, const Context& ctxt, Constraints& constraints) {
			// for this analysis we are ignoring casts
			constraints.add(subset(
					cba.getSet(A, cast->getSubExpression(), ctxt),
					cba.getSet(A, cast, ctxt)
			));

			// also run standard operations
			super::visitCastExpr(cast, ctxt, constraints);
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

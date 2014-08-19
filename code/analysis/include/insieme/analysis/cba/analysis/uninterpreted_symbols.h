/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/arithmetic.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/cartesian_product_iterator.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::set;

	template<typename A, typename B, typename C> class DataFlowConstraintGenerator;

	// ----------------- booleans analysis ---------------

	template<typename C> class UninterpretedSymbolsConstraintGenerator;

	struct uninterpreted_symbols_analysis_data : public data_analysis<ExpressionPtr, UninterpretedSymbolsConstraintGenerator> {};
	struct uninterpreted_symbols_analysis_var  : public data_analysis<ExpressionPtr, UninterpretedSymbolsConstraintGenerator> {};

	extern const uninterpreted_symbols_analysis_data U;
	extern const uninterpreted_symbols_analysis_var  u;

	namespace {

		template<typename A, typename B>
		vector<Variable> combine(const A& a, const vector<B>& b) {
			vector<Variable> res;
			res.push_back(a);
			for(auto& cur : b) res.push_back(cur);
			return res;
		}

		template<
			typename Functions,
			typename Symbols
		>
		class CallConstraint : public Constraint {

			typedef typename Symbols::less_op_type less_op;
			typedef typename Symbols::meet_assign_op_type meet_assign_op;
			typedef typename Symbols::value_type value_type;
			typedef typename Symbols::manager_type manager_type;

			const TypedVariable<Functions> funs;
			const vector<TypedVariable<Symbols>> args;
			const TypedVariable<Symbols> out;

			core::NodeManager& nodeMgr;
			manager_type& mgr;

		public:

			CallConstraint(
					core::NodeManager& nodeMgr,
					const TypedVariable<Functions>& funs,
					const vector<TypedVariable<Symbols>>& args,
					const TypedVariable<Symbols>& out,
					manager_type& mgr
			)
				: Constraint(combine(funs, args), toVector<Variable>(out), false),
				  funs(funs), args(args), out(out), nodeMgr(nodeMgr), mgr(mgr) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				const static meet_assign_op meet_assign;

				// get reference to current value
				value_type& value = ass[out];

				// compute new value
				value_type updated = getUpdatedValue(ass);

				// update value and return change-summary
				return (meet_assign(value, updated)) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool check(const Assignment& ass) const {
				const static less_op less;
				return less(getUpdatedValue(ass), ass[out]);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				out << funs << " -> " << this->out << "[label=\"" << *this << "\"]\n";
				for (auto arg : args) {
					out << arg << " -> " << this->out << "[label=\"" << *this << "\"]\n";
				}
				return out;
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "call " << funs << " (" << join(",",args) << ")";
			}

		private:

			bool isUndefined(const value_type& value) const {
				const set<ExpressionPtr>& set = value;
				return contains(set, ExpressionPtr());
			}

			value_type undefined() const {
				set<ExpressionPtr> res;
				res.insert(ExpressionPtr());
				return mgr.atomic(res);
			}

			value_type getUpdatedValue(const Assignment& ass) const {

				set<ExpressionPtr> res;

				// get list of potential functions
				const set<Callee>& callees = ass[funs];

				// a container for all function and argument symbols
				vector<set<ExpressionPtr>> elements;

				// get node manager (to filter known operators)
				auto& base = nodeMgr.getLangBasic();

				elements.push_back(set<ExpressionPtr>());
				set<ExpressionPtr>& fs = elements.back();
				for(const auto& cur : callees) {
					if (cur.isLiteral() && !base.isRefDeref(cur.getDefinition())) {
						fs.insert(cur.getDefinition().template as<ExpressionPtr>());
					}
				}

				// add sets of arguments
				for(const auto& arg : args) {
					const value_type& value = ass[arg];

					// if an argument is undefined, the result will be as well
					if (isUndefined(value)) {
						return undefined();
					}

					elements.push_back(value);
				}

				// compute Cartesian product of functions and arguments
				core::IRBuilder builder(nodeMgr);
				for(auto& cur : utils::cartesian_product(elements)) {

					auto f = cur[0];
					auto a = std::vector<ExpressionPtr>(cur.begin() + 1, cur.end());

					// and create one call for each of those
					res.insert(builder.callExpr(f,a));

					// add an upper limit for the number of values
					if (res.size() > 10) return undefined();
				}

				// convert set into proper lattice-value
				return mgr.atomic(res);
			}


		};


		template<typename Functions, typename Symbols>
		ConstraintPtr callConstraint(core::NodeManager& nodeMgr, const TypedVariable<Functions> funs, const vector<TypedVariable<Symbols>> args, const TypedVariable<Symbols> out, typename Symbols::manager_type& mgr) {
			return std::make_shared<CallConstraint<Functions, Symbols>>(nodeMgr, funs, args, out, mgr);
		}


	}

	template<typename Context>
	class UninterpretedSymbolsConstraintGenerator : public DataFlowConstraintGenerator<uninterpreted_symbols_analysis_data, uninterpreted_symbols_analysis_var, Context> {

		typedef DataFlowConstraintGenerator<uninterpreted_symbols_analysis_data, uninterpreted_symbols_analysis_var, Context> super;

		const core::lang::BasicGenerator& base;

		CBA& cba;

	public:

		UninterpretedSymbolsConstraintGenerator(CBA& cba)
			: super(cba, cba::U, cba::u, cba.getDataManager<lattice<uninterpreted_symbols_analysis_data>::type>().atomic(utils::set::toSet<set<ExpressionPtr>>(ExpressionPtr()))),
			  base(cba.getRoot()->getNodeManager().getLangBasic()),
			  cba(cba)
		{ };

		using super::atomic;
		using super::elem;
		using super::pack;

		void visitLiteral(const LiteralInstance& literal, const Context& ctxt, Constraints& constraints) {

			// a literal is interpreted as itself
			constraints.add(elem(literal.as<LiteralPtr>(), cba.getVar(U, literal, ctxt)));

		}


		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, Constraints& constraints) {

			// conduct std-procedure
			super::visitCallExpr(call, ctxt, constraints);

			// collect targeted functions and arguments
			auto funs = cba.getVar(F, call.getFunctionExpr(), ctxt);
			auto res = cba.getVar(U, call, ctxt);

			vector<decltype(res)> args;
			for (auto& arg : call) {
				args.push_back(cba.getVar(U, arg, ctxt));
			}

			// special case if target is a literal
			constraints.add(callConstraint(cba.getRoot()->getNodeManager(), funs, args, res, this->getValueManager()));
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

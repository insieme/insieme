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
#pragma once

#include <vector>
#include <memory>

#include "insieme/utils/constraint/variables.h"
#include "insieme/utils/constraint/assignment.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace utils {
namespace constraint {

	using std::vector;

	class Constraint;

	typedef std::shared_ptr<Constraint> ConstraintPtr;

	// ----------------------------- Constraints ------------------------------

	// a common base type for all kind of constraints

	class Constraint : public VirtualPrintable {
		vector<Variable> inputs;
		vector<Variable> outputs;

		bool assignmentDependentDependencies; // there is a fixed list, but only partially used
		bool dynamicDependencies;             // there is a flexible list

	  public:
		enum UpdateResult { Unchanged, Incremented, Altered };

		Constraint(const vector<Variable>& in, const vector<Variable>& out, bool assignmentDependentDependencies = false, bool dynamicDependencies = false)
		    : inputs(in), outputs(out), assignmentDependentDependencies(assignmentDependentDependencies || dynamicDependencies),
		      dynamicDependencies(dynamicDependencies) {}

		virtual ~Constraint(){};

		virtual void init(Assignment& ass, vector<Variable>& workList) const {
			// update dynamic dependencies if necessary
			if(dynamicDependencies) { updateDynamicDependencies(ass); }

			if(update(ass) != Unchanged) {
				for(auto cur : getOutputs()) {
					workList.push_back(cur);
				}
			}
		}

		virtual UpdateResult update(Assignment& /*ass*/) const {
			return Unchanged;
		};
		virtual bool check(const Assignment& ass) const = 0;

		virtual std::ostream& writeDotEdge(std::ostream& out) const = 0;
		virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& /*ass*/) const {
			return writeDotEdge(out);
		}

		const vector<Variable>& getInputs() const {
			return inputs;
		};
		const vector<Variable>& getOutputs() const {
			return outputs;
		};

		bool hasAssignmentDependentDependencies() const {
			return assignmentDependentDependencies;
		};
		bool hasDynamicDependencies() const {
			return dynamicDependencies;
		}

		virtual bool updateDynamicDependencies(const Assignment& /*ass*/) const {
			return dynamicDependencies; // return whether something might have changed
		}

		virtual const vector<Variable>& getUsedInputs(const Assignment& /*ass*/) const {
			assert_false(assignmentDependentDependencies) << "Needs to be implemented by constraints exhibiting assignment based dependencies.";
			return inputs;
		}
	};

	namespace detail {

		typedef vector<Variable> Variables;

		inline Variables combine(const Variables& a, const Variables& b) {
			Variables res = a;
			for(auto x : b) {
				if(!contains(a, x)) { res.push_back(x); }
			}
			return res;
		}


		template <typename Filter, typename Executor>
		class ComposedConstraint : public Constraint {
			Filter filter;
			Executor executor;

			mutable vector<Variable> usedInputs;
			mutable bool satisfied; // a flag remembering whether the filter used to be satisfied the last time

		  public:
			ComposedConstraint(const Filter& filter, const Executor& executor)
			    : Constraint(combine(filter.getInputs(), executor.getInputs()), executor.getOutputs(), !Filter::is_true), filter(filter), executor(executor),
			      satisfied(false) {}

			virtual UpdateResult update(Assignment& ass) const {
				bool fit = filter(ass);

				// if no longer fitting
				if(satisfied && !fit) {
					// reset satisfaction state
					satisfied = false;

					// reset output values
					for(const auto& cur : executor.getOutputs()) {
						ass.clear(cur);
					}

					// return info that value has been completely altered
					return Altered;
				}

				// if the filter is not matching => no change necessary
				if(!fit) { return Unchanged; }

				// filter is matching
				satisfied = true;
				return executor.update(ass) ? Incremented : Unchanged;
			}

			virtual bool check(const Assignment& ass) const {
				return !filter(ass) || executor.check(ass);
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(!Filter::is_true) {
					filter.print(out);
					out << " => ";
				}
				executor.print(out);
				return out;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				std::stringstream label;
				label << "[label=\"" << *this << "\"]\n";
				executor.writeDotEdge(out, label.str());
				return out;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& ass) const {
				std::stringstream label;
				label << "[label=\"" << *this << "\"";
				if(!filter(ass)) { label << " style=dotted"; }
				label << "]\n";
				executor.writeDotEdge(out, label.str());
				return out;
			}

			virtual const vector<Variable>& getUsedInputs(const Assignment& ass) const {
				usedInputs.clear();
				filter.addUsedInputs(ass, usedInputs);
				if(filter(ass)) { executor.addUsedInputs(ass, usedInputs); }
				return usedInputs;
			}
		};

		// -------------------- Filter --------------------------------

		template <bool isTrue = false>
		struct Filter {
			enum { is_true = isTrue };
		};

		struct TrueFilter : public Filter<true> {
			bool operator()(const Assignment& /*ass*/) const {
				return true;
			}
			void print(std::ostream& out) const {
				out << "true";
			}
			const Variables& getInputs() const {
				static const Variables empty;
				return empty;
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& /*used*/) const {}
		};

		template <typename A, typename B>
		struct AndFilter : public Filter<> {
			A a;
			B b;
			AndFilter(const A& a, const B& b) : a(a), b(b) {}
			bool operator()(const Assignment& ass) const {
				return a(ass) && b(ass);
			}
			void print(std::ostream& out) const {
				a.print(out);
				out << " and ";
				b.print(out);
			}
			Variables getInputs() const {
				return combine(a.getInputs(), b.getInputs());
			}
			void addUsedInputs(const Assignment& ass, vector<Variable>& used) const {
				a.addUsedInputs(ass, used);
				if(a(ass)) { b.addUsedInputs(ass, used); }
			}
		};

		template <typename A, typename B>
		struct OrFilter : public Filter<> {
			A a;
			B b;
			OrFilter(const A& a, const B& b) : a(a), b(b) {}
			bool operator()(const Assignment& ass) const {
				return a(ass) || b(ass);
			}
			void print(std::ostream& out) const {
				a.print(out);
				out << " or ";
				b.print(out);
			}
			Variables getInputs() const {
				return combine(a.getInputs(), b.getInputs());
			}
			void addUsedInputs(const Assignment& ass, vector<Variable>& used) const {
				a.addUsedInputs(ass, used);
				if(a(ass)) { b.addUsedInputs(ass, used); }
			}
		};

		template <typename A>
		struct NegFilter : public Filter<> {
			A a;
			NegFilter(const A& a) : a(a) {}
			bool operator()(const Assignment& ass) const {
				return !a(ass);
			}
			void print(std::ostream& out) const {
				out << "!(";
				a.print(out);
				out << ")";
			}
			Variables getInputs() const {
				return a.getInputs();
			}
			void addUsedInputs(const Assignment& ass, vector<Variable>& used) const {
				a.addUsedInputs(ass, used);
			}
		};

		template <typename F, typename A, typename B>
		struct BinaryOpFilter : public Filter<> {
			F f;
			TypedVariable<A> a;
			TypedVariable<B> b;
			BinaryOpFilter(const F& f, const TypedVariable<A>& a, const TypedVariable<B>& b) : f(f), a(a), b(b) {}
			bool operator()(const Assignment& ass) const {
				return f(ass[a], ass[b]);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << "," << b << ")";
			}
			Variables getInputs() const {
				return toVector<Variable>(a, b);
			}
			void addUsedInputs(const Assignment& ass, vector<Variable>& used) const {
				used.push_back(a);
				used.push_back(b);
			}
		};

		template <typename F, typename A, typename B, typename C>
		struct TrinaryOpFilter : public Filter<> {
			F f;
			TypedVariable<A> a;
			TypedVariable<B> b;
			TypedVariable<C> c;
			TrinaryOpFilter(const F& f, const TypedVariable<A>& a, const TypedVariable<B>& b, const TypedVariable<C>& c) : f(f), a(a), b(b), c(c) {}
			bool operator()(const Assignment& ass) const {
				return f(ass[a], ass[b], ass[c]);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << "," << b << "," << c << ")";
			}
			Variables getInputs() const {
				return toVector<Variable>(a, b, c);
			}
			void addUsedInputs(const Assignment& ass, vector<Variable>& used) const {
				used.push_back(a);
				used.push_back(b);
				used.push_back(c);
			}
		};

		template <typename E, typename L>
		struct ElementOfFilter : public Filter<> {
			E e;
			TypedVariable<L> a;
			ElementOfFilter(const E& e, const TypedVariable<L>& a) : e(e), a(a) {}
			bool operator()(const Assignment& ass) const {
				typedef typename L::less_op_type less_op_type;
				static const less_op_type less_op = less_op_type();
				return less_op(e, ass[a]);
			}
			void print(std::ostream& out) const {
				out << e << " in " << a;
			}
			Variables getInputs() const {
				return toVector<Variable>(a);
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
			}
		};

		template <typename T>
		struct BiggerThanFilter : public Filter<> {
			TypedVariable<SetLattice<T>> a;
			std::size_t s;
			BiggerThanFilter(const TypedVariable<SetLattice<T>>& a, std::size_t s) : a(a), s(s) {}
			bool operator()(const Assignment& ass) const {
				return ass[a].size() > s;
			}
			void print(std::ostream& out) const {
				out << "|" << a << "| > " << s;
			}
			Variables getInputs() const {
				return toVector<Variable>(a);
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
			}
		};

		template <typename T>
		struct BiggerThanReducedFilter : public Filter<> {
			TypedVariable<SetLattice<T>> a;
			T e;
			std::size_t s;
			BiggerThanReducedFilter(const TypedVariable<SetLattice<T>>& a, const T& e, std::size_t s) : a(a), e(e), s(s) {}
			bool operator()(const Assignment& ass) const {
				auto& set = ass[a];
				return (set.size() - (contains(set, e) ? 1 : 0)) > s;
			}
			void print(std::ostream& out) const {
				out << "|" << a << " - {" << e << "}| > " << s;
			}
			Variables getInputs() const {
				return toVector<Variable>(a);
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
			}
		};


		// -------------------- Executor --------------------------------

		struct Executor {
		  protected:
			// a utility function merging sets
			template <typename meet_assign_op, typename A, typename B>
			bool addAll(const A& src, B& trg) const {
				static const meet_assign_op meet_assign = meet_assign_op();
				// compute meet operation and check for modification
				return meet_assign(trg, src);
			};

			// a utility function merging sets
			template <typename A, typename L>
			bool addAll(Assignment& ass, const A& srcSet, const TypedVariable<L>& trgSet) const {
				return addAll<typename L::meet_assign_op_type>(srcSet, ass[trgSet]);
			};

			// a utility function merging sets
			template <typename L>
			bool addAll(Assignment& ass, const TypedVariable<L>& srcSet, const TypedVariable<L>& trgSet) const {
				const Assignment& cass = ass;
				return addAll(ass, cass[srcSet], trgSet);
			};

			// a utility to check whether a certain set is a subset of another set
			template <typename less_op, typename A, typename B>
			bool isSubset(const A& a, const B& b) const {
				static const less_op less = less_op();
				return less(a, b);
			}

			// a utility to check whether a certain set is a subset of another set
			template <typename L>
			bool isSubset(Assignment& ass, const TypedVariable<L>& a, const TypedVariable<L>& b) const {
				return isSubset<typename L::less_op_type>(ass[a], ass[b]);
			}
		};

		template <typename E, typename L>
		class ElementOf : public Executor {
			E e;
			TypedVariable<L> a;

		  public:
			ElementOf(const E& e, const TypedVariable<L>& a) : e(e), a(a) {}
			const Variables& getInputs() const {
				static const Variables empty;
				return empty;
			}
			Variables getOutputs() const {
				return toVector<Variable>(a);
			}
			void print(std::ostream& out) const {
				out << e << " in " << a;
			}
			bool update(Assignment& ass) const {
				return addAll<typename L::meet_assign_op_type>(e, ass[a]);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename L::less_op_type>(e, ass[a]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << "e" << (int*)&e << " [label=\"" << e << "\"]\n";
				out << "e" << (int*)&e << " -> " << a << " " << label;
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& /*used*/) const {
				// nothing
			}
		};

		template <typename L>
		class Subset : public Executor {
			TypedVariable<L> a;
			TypedVariable<L> b;

		  public:
			Subset(const TypedVariable<L>& a, const TypedVariable<L>& b) : a(a), b(b) {
				assert_ne(a, b);
			}
			Variables getInputs() const {
				return toVector<Variable>(a);
			}
			Variables getOutputs() const {
				return toVector<Variable>(b);
			}
			void print(std::ostream& out) const {
				out << a << " sub " << b;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, a, b);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename L::less_op_type>(ass[a], ass[b]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << b << label;
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
			}
		};

		template <typename A, typename R>
		class SubsetUnary : public Executor {
			typedef typename A::value_type A_value_type;
			typedef typename R::value_type R_value_type;

			typedef std::function<R_value_type(const A_value_type&)> fun_type;

			TypedVariable<A> a;
			TypedVariable<R> r;
			fun_type f;

		  public:
			SubsetUnary(const TypedVariable<A>& a, const TypedVariable<R>& r, const fun_type& f) : a(a), r(r), f(f) {}
			Variables getInputs() const {
				return toVector<Variable>(a);
			}
			Variables getOutputs() const {
				return toVector<Variable>(r);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << ") sub " << r;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, f(ass[a]), r);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename R::less_op_type>(f(ass[a]), ass[r]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << r << label;
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
			}
		};


		template <typename A, typename B, typename R>
		class SubsetBinary : public Executor {
			typedef typename A::value_type A_value_type;
			typedef typename B::value_type B_value_type;
			typedef typename R::value_type R_value_type;

			typedef std::function<R_value_type(const A_value_type&, const B_value_type&)> fun_type;

			TypedVariable<A> a;
			TypedVariable<B> b;
			TypedVariable<R> r;
			fun_type f;

		  public:
			SubsetBinary(const TypedVariable<A>& a, const TypedVariable<B>& b, const TypedVariable<R>& r, const fun_type& f) : a(a), b(b), r(r), f(f) {}
			Variables getInputs() const {
				return toVector<Variable>(a, b);
			}
			Variables getOutputs() const {
				return toVector<Variable>(r);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << "," << b << ") sub " << r;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, f(ass[a], ass[b]), r);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename R::less_op_type>(f(ass[a], ass[b]), ass[r]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << r << label << "\n";
				out << b << " -> " << r << label;
			}
			void addUsedInputs(const Assignment& /*ass*/, vector<Variable>& used) const {
				used.push_back(a);
				used.push_back(b);
			}
		};

	} // end of details namespace

	// ----------------------------- Filter Factory Functions ------------------------------

	template <typename E, typename L>
	detail::ElementOfFilter<E, L> f_in(const E& e, const TypedVariable<L>& set) {
		return detail::ElementOfFilter<E, L>(e, set);
	}

	template <typename F1>
	typename std::enable_if<std::is_base_of<detail::Filter<false>, F1>::value, detail::NegFilter<F1>>::type operator!(const F1& f1) {
		return detail::NegFilter<F1>(f1);
	}

	template <typename F1, typename F2>
	typename std::enable_if<std::is_base_of<detail::Filter<false>, F1>::value && std::is_base_of<detail::Filter<false>, F2>::value,
	                        detail::AndFilter<F1, F2>>::type
	operator&&(const F1& f1, const F2& f2) {
		return detail::AndFilter<F1, F2>(f1, f2);
	}

	template <typename F1, typename F2>
	typename std::enable_if<std::is_base_of<detail::Filter<false>, F1>::value && std::is_base_of<detail::Filter<false>, F2>::value,
	                        detail::OrFilter<F1, F2>>::type
	operator||(const F1& f1, const F2& f2) {
		return detail::OrFilter<F1, F2>(f1, f2);
	}

	template <typename F, typename A, typename B>
	detail::BinaryOpFilter<F, A, B> f_binary(const F& filter, const TypedVariable<A>& a, const TypedVariable<B>& b) {
		return detail::BinaryOpFilter<F, A, B>(filter, a, b);
	}

	template <typename F, typename A, typename B, typename C>
	detail::TrinaryOpFilter<F, A, B, C> f_trinary(const F& filter, const TypedVariable<A>& a, const TypedVariable<B>& b, const TypedVariable<C>& c) {
		return detail::TrinaryOpFilter<F, A, B, C>(filter, a, b, c);
	}

	// ----------------------------- Executor Factory Functions ------------------------------

	template <typename E, typename L>
	detail::ElementOf<E, L> e_in(const E& e, const TypedVariable<L>& a) {
		return detail::ElementOf<E, L>(e, a);
	}

	template <typename L>
	detail::Subset<L> e_sub(const TypedVariable<L>& a, const TypedVariable<L>& b) {
		return detail::Subset<L>(a, b);
	}

	// ----------------------------- Constraint Factory Functions ------------------------------


	template <typename F, typename E>
	typename std::enable_if<std::is_base_of<detail::Filter<F::is_true>, F>::value && std::is_base_of<detail::Executor, E>::value, ConstraintPtr>::type
	combine(const F& filter, const E& executor) {
		return std::make_shared<detail::ComposedConstraint<F, E>>(filter, executor);
	}

	template <typename E>
	typename std::enable_if<std::is_base_of<detail::Executor, E>::value, ConstraintPtr>::type build(const E& executor) {
		return combine(detail::TrueFilter(), executor);
	}

	template <typename A, typename L>
	ConstraintPtr elem(const A& a, const TypedVariable<L>& b) {
		return combine(detail::TrueFilter(), e_in(a, b));
	}

	template <typename E, typename A, typename F, typename B>
	ConstraintPtr elemIf(const E& e, const TypedVariable<A>& a, const F& f, const TypedVariable<B>& b) {
		return combine(f_in(e, a), e_in(f, b));
	}

	template <typename A, typename L>
	typename std::enable_if<!std::is_base_of<TypedVariable<L>, A>::value, ConstraintPtr>::type subset(const A& a, const TypedVariable<L>& b) {
		return combine(detail::TrueFilter(), e_in(a, b));
	}

	template <typename L>
	ConstraintPtr subset(const TypedVariable<L>& a, const TypedVariable<L>& b) {
		return combine(detail::TrueFilter(), e_sub(a, b));
	}

	template <typename E, typename A, typename B>
	ConstraintPtr subsetIf(const E& e, const TypedVariable<A>& a, const TypedVariable<B>& b, const TypedVariable<B>& c) {
		return combine(f_in(e, a), e_sub(b, c));
	}

	template <typename A, typename B, typename C, typename L1, typename L2>
	ConstraintPtr subsetIf(const A& a, const TypedVariable<L1>& as, const B& b, const TypedVariable<L2>& bs, const TypedVariable<C>& in,
	                       const TypedVariable<C>& out) {
		return combine(f_in(a, as) && f_in(b, bs), e_sub(in, out));
	}

	template <typename A, typename B>
	ConstraintPtr subsetIfBigger(const TypedVariable<SetLattice<A>>& a, std::size_t s, const TypedVariable<B>& b, const TypedVariable<B>& c) {
		return combine(detail::BiggerThanFilter<A>(a, s), e_sub(b, c));
	}

	template <typename E, typename A>
	ConstraintPtr subsetIfReducedBigger(const TypedVariable<SetLattice<E>>& a, const E& e, std::size_t s, const TypedVariable<A>& b,
	                                    const TypedVariable<A>& c) {
		return combine(detail::BiggerThanReducedFilter<E>(a, e, s), e_sub(b, c));
	}

	template <typename A, typename R, typename F>
	ConstraintPtr subsetUnary(const TypedVariable<A>& in, const TypedVariable<R>& out, const F& fun) {
		return combine(detail::TrueFilter(), detail::SubsetUnary<A, R>(in, out, fun));
	}

	template <typename A, typename B, typename R, typename F>
	ConstraintPtr subsetBinary(const TypedVariable<A>& a, const TypedVariable<B>& b, const TypedVariable<R>& r, const F& fun) {
		return combine(detail::TrueFilter(), detail::SubsetBinary<A, B, R>(a, b, r, fun));
	}

	// ----------------------------- Constraint Container ------------------------------


	class Constraints : public Printable {
	  public:
		typedef vector<ConstraintPtr> data_type;
		typedef data_type::const_iterator const_iterator;

	  private:
		// think about making this a set
		data_type data;

	  public:
		Constraints() : data() {}

		Constraints(const std::initializer_list<ConstraintPtr>& list) : data(list) {}

		void add(const ConstraintPtr& constraint) {
			if(constraint) { data.push_back(constraint); }
		}

		void add(const Constraints& constraints) {
			data.insert(data.end(), constraints.data.begin(), constraints.data.end());
		}

		const vector<ConstraintPtr>& getList() const {
			return data;
		}

		const_iterator begin() const {
			return data.begin();
		}
		const_iterator end() const {
			return data.end();
		}

		std::size_t size() const {
			return data.size();
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, print<deref<ConstraintPtr>>()) << "}";
		}
	};


} // end namespace constraint
} // end namespace utils
} // end namespace insieme

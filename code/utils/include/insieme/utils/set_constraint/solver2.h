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

#include <set>
#include <map>
#include <tuple>

#include <memory>
#include <initializer_list>

#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/typed_map.h"


namespace insieme {
namespace utils {
namespace set_constraint_2 {

	// forward declaration
	class SetID;

	class Constraint;
	typedef std::shared_ptr<Constraint> ConstraintPtr;

	class Assignment;


	// ----------------------------- Set IDs ------------------------------

	class SetID : public Printable {

		int id;

	public:

		SetID(int id = -1) : id(id) { };
		SetID(const SetID& id) : id(id.id) { };

		int getID() const { return id; }

		bool operator==(const SetID& other) const { return id == other.id; }
		bool operator!=(const SetID& other) const { return !(*this == other); }
		bool operator<(const SetID& other) const { return id < other.id; }

		virtual std::ostream& printTo(std::ostream& out) const { return out << "s" << id; }
	};

	template<typename T>
	struct TypedSetID : public SetID {
		TypedSetID(int id = -1) : SetID(id) { };
		TypedSetID(const SetID& id) : SetID(id) { };
		TypedSetID(const TypedSetID<T>& id) : SetID(id) { };
	};

} // end namespace set_constraint_2
} // end namespace utils
} // end namespace insieme

namespace std {

	template<>
	struct hash<insieme::utils::set_constraint_2::SetID> {
		size_t operator()(const insieme::utils::set_constraint_2::SetID& id) const {
			return id.getID();
		}
	};

} // end namespace std

namespace insieme {
namespace utils {
namespace set_constraint_2 {


	// ----------------------------- Constraints ------------------------------

	// a common base type for all kind of constraints

	class Constraint : public VirtualPrintable {

		std::vector<SetID> inputs;
		std::vector<SetID> outputs;

	public:

		Constraint(const std::vector<SetID>& in, const std::vector<SetID>& out)
			: inputs(in), outputs(out) {}

		virtual ~Constraint() {};

		virtual void init(Assignment& ass, vector<SetID>& workList) const { };
		virtual bool update(Assignment& ass) const { return false; };
		virtual bool check(const Assignment& ass) const =0;

		virtual std::ostream& writeDotEdge(std::ostream& out) const =0;
		virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& ass) const =0;

		const std::vector<SetID>& getInputs() const { return inputs; };
		const std::vector<SetID>& getOutputs() const { return outputs; };

		virtual bool hasAssignmentDependentDependencies() const =0;
		virtual std::set<SetID> getUsedInputs(const Assignment& ass) const =0;

	protected:

		// a utility function merging sets
		template<typename A>
		bool addAll(Assignment& ass, const std::set<A>& srcSet, const TypedSetID<A>& trgSet) const {
			// get actual set
			auto& trg = ass[trgSet];

			// add values to target set
			bool newData = false;
			for(const auto& x : srcSet) {
				newData = trg.insert(x).second || newData;
			}

			// if target set has changed
			return newData;
		};

		// a utility function merging sets
		template<typename A>
		bool addAll(Assignment& ass, const TypedSetID<A>& srcSet, const TypedSetID<A>& trgSet) const {
			return addAll(ass, ass[srcSet], trgSet);
		};
	};

	namespace detail {

		typedef std::vector<SetID> SetIDs;

		inline SetIDs combine(const SetIDs& a, const SetIDs& b) {
			SetIDs res = a;
			for(auto x : b) {
				if (!contains(a, x)) {
					res.push_back(x);
				}
			}
			return res;
		}



		template<typename Filter, typename Executor>
		class ComposedConstraint : public Constraint {

			Filter filter;
			Executor executor;

		public:

			ComposedConstraint(const Filter& filter, const Executor& executor)
				: Constraint(combine(filter.getInputs(), executor.getInputs()), executor.getOutputs()), filter(filter), executor(executor) {}

			virtual void init(Assignment& ass, vector<SetID>& workList) const {
				if (update(ass)) {
					for(auto cur : getOutputs()) {
						workList.push_back(cur);
					}
				}
			}

			virtual bool update(Assignment& ass) const {
				return filter(ass) && executor.update(ass);
			}

			virtual bool check(const Assignment& ass) const {
				return !filter(ass) || executor.check(ass);
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				if (!Filter::is_true) {
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
				if (!filter(ass)) label << " style=dotted";
				label << "]\n";
				executor.writeDotEdge(out, label.str());
				return out;
			}

			virtual bool hasAssignmentDependentDependencies() const {
				return !Filter::is_true;
			}

			virtual std::set<SetID> getUsedInputs(const Assignment& ass) const {
				std::set<SetID> used;
				filter.addUsedInputs(ass, used);
				if (filter(ass)) executor.addUsedInputs(ass, used);
				return used;
			}
		};

		// -------------------- Filter --------------------------------

		template<bool isTrue = false>
		struct Filter {
			enum { is_true = isTrue };
		};

		struct TrueFilter : public Filter<true> {
			bool operator()(const Assignment& ass) const {
				return true;
			}
			void print(std::ostream& out) const {
				out << "true";
			}
			const SetIDs& getInputs() const {
				static const SetIDs empty;
				return empty;
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {}
		};

		template<typename A, typename B>
		struct AndFilter : public Filter<> {
			A a; B b;
			AndFilter(const A& a, const B& b)
				: a(a), b(b) {}
			bool operator()(const Assignment& ass) const {
				return a(ass) && b(ass);
			}
			void print(std::ostream& out) const {
				a.print(out); out << " and "; b.print(out);
			}
			SetIDs getInputs() const {
				return combine(a.getInputs(), b.getInputs());
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				a.addUsedInputs(ass, used);
				if (a(ass)) b.addUsedInputs(ass, used);
			}
		};

		template<typename T>
		struct ElementOfFilter : public Filter<>{
			T e;
			TypedSetID<T> a;
			ElementOfFilter(const T& e, const TypedSetID<T>& a)
				: e(e), a(a) {}
			bool operator()(const Assignment& ass) const {
				return contains(ass[a], e);
			}
			void print(std::ostream& out) const {
				out << e << " in " << a;
			}
			SetIDs getInputs() const {
				return toVector<SetID>(a);
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
			}
		};

		template<typename T>
		struct BiggerThanFilter : public Filter<>{
			TypedSetID<T> a;
			std::size_t s;
			BiggerThanFilter(const TypedSetID<T>& a, std::size_t s)
				: a(a), s(s) {}
			bool operator()(const Assignment& ass) const {
				return ass[a].size() > s;
			}
			void print(std::ostream& out) const {
				out << "|" << a << "| > " << s;
			}
			SetIDs getInputs() const {
				return toVector<SetID>(a);
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
			}
		};

		template<typename T>
		struct BiggerThanReducedFilter : public Filter<>{
			TypedSetID<T> a;
			T e;
			std::size_t s;
			BiggerThanReducedFilter(const TypedSetID<T>& a, const T& e, std::size_t s)
				: a(a), e(e), s(s) {}
			bool operator()(const Assignment& ass) const {
				auto& set = ass[a];
				return (set.size() - (contains(set, e)?1:0)) > s;
			}
			void print(std::ostream& out) const {
				out << "|" << a << " - {" << e << "}| > " << s;
			}
			SetIDs getInputs() const {
				return toVector<SetID>(a);
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
			}
		};


		// -------------------- Executor --------------------------------

		struct Executor {
		protected:

			// a utility function merging sets
			template<typename A>
			bool addAll(Assignment& ass, const std::set<A>& srcSet, const TypedSetID<A>& trgSet) const {
				// get actual set
				auto& trg = ass[trgSet];

				// add values to target set
				bool newData = false;
				for(const auto& x : srcSet) {
					newData = trg.insert(x).second || newData;
				}

				// if target set has changed
				return newData;
			};

			// a utility function merging sets
			template<typename A>
			bool addAll(Assignment& ass, const TypedSetID<A>& srcSet, const TypedSetID<A>& trgSet) const {
				const Assignment& cass = ass;
				return addAll(ass, cass[srcSet], trgSet);
			};
		};

		template<typename T>
		class ElementOf : public Executor {
			T e;
			TypedSetID<T> a;
		public:
			ElementOf(const T& e, const TypedSetID<T>& a)
				: e(e), a(a) {}
			const SetIDs& getInputs() const {
				static const SetIDs empty;
				return empty;
			}
			SetIDs getOutputs() const {
				return toVector<SetID>(a);
			}
			void print(std::ostream& out) const {
				out << e << " in " << a;
			}
			bool update(Assignment& ass) const {
				return ass[a].insert(e).second;
			}
			bool check(const Assignment& ass) const {
				return set::contains(ass[a], e);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << "e" << (int*)&e << " [label=\"" << e << "\"]\n";
				out << "e" << (int*)&e << " -> " << a << " " << label;
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				// nothing
			}
		};

		template<typename T>
		class Subset : public Executor {
			TypedSetID<T> a;
			TypedSetID<T> b;
		public:
			Subset(const TypedSetID<T>& a, const TypedSetID<T>& b)
				: a(a), b(b) { assert(a != b); }
			SetIDs getInputs() const {
				return toVector<SetID>(a);
			}
			SetIDs getOutputs() const {
				return toVector<SetID>(b);
			}
			void print(std::ostream& out) const {
				out << a << " sub " << b;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, a, b);
			}
			bool check(const Assignment& ass) const {
				return set::isSubset(ass[a], ass[b]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << b << label;
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
			}
		};

		template<typename A, typename R>
		class SubsetUnary : public Executor {

			typedef std::function<std::set<R>(const std::set<A>&)> fun_type;

			TypedSetID<A> a;
			TypedSetID<R> r;
			fun_type f;

		public:
			SubsetUnary(const TypedSetID<A>& a, const TypedSetID<R>& r, const fun_type& f)
				: a(a), r(r), f(f) {}
			SetIDs getInputs() const {
				return toVector<SetID>(a);
			}
			SetIDs getOutputs() const {
				return toVector<SetID>(r);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << ") sub " << r;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, f(ass[a]), r);
			}
			bool check(const Assignment& ass) const {
				return set::isSubset(f(ass[a]), ass[r]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << r << label;
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
			}
		};


		template<typename A, typename B, typename R>
		class SubsetBinary : public Executor {

			typedef std::function<std::set<R>(const std::set<A>&, const std::set<B>&)> fun_type;

			TypedSetID<A> a;
			TypedSetID<B> b;
			TypedSetID<R> r;
			fun_type f;

		public:
			SubsetBinary(const TypedSetID<A>& a, const TypedSetID<B>& b, const TypedSetID<R>& r, const fun_type& f)
				: a(a), b(b), r(r), f(f) {}
			SetIDs getInputs() const {
				return toVector<SetID>(a,b);
			}
			SetIDs getOutputs() const {
				return toVector<SetID>(r);
			}
			void print(std::ostream& out) const {
				out << "f(" << a << "," << b << ") sub " << r;
			}
			bool update(Assignment& ass) const {
				return addAll(ass, f(ass[a],ass[b]), r);
			}
			bool check(const Assignment& ass) const {
				return set::isSubset(f(ass[a],ass[b]), ass[r]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << a << " -> " << r << label << "\n";
				out << b << " -> " << r << label;
			}
			void addUsedInputs(const Assignment& ass, std::set<SetID>& used) const {
				used.insert(a);
				used.insert(b);
			}
		};

	}	// end of details namespace

	// ----------------------------- Filter Factory Functions ------------------------------

	template<typename E>
	detail::ElementOfFilter<E> f_in(const E& e, const TypedSetID<E>& set) {
		return detail::ElementOfFilter<E>(e,set);
	}

	template<typename F1, typename F2>
	typename std::enable_if<std::is_base_of<detail::Filter<false>, F1>::value && std::is_base_of<detail::Filter<false>, F2>::value, detail::AndFilter<F1, F2>>::type
	operator&&(const F1& f1, const F2& f2) {
		return detail::AndFilter<F1,F2>(f1,f2);
	}


	// ----------------------------- Executor Factory Functions ------------------------------

	template<typename E>
	detail::ElementOf<E> e_in(const E& e, const TypedSetID<E>& set) {
		return detail::ElementOf<E>(e,set);
	}

	template<typename E>
	detail::Subset<E> e_sub(const TypedSetID<E>& a, const TypedSetID<E>& b) {
		return detail::Subset<E>(a,b);
	}

	// ----------------------------- Constraint Factory Functions ------------------------------


	template<typename F, typename E>
	typename std::enable_if<std::is_base_of<detail::Filter<F::is_true>, F>::value && std::is_base_of<detail::Executor, E>::value, ConstraintPtr>::type
	combine(const F& filter, const E& executor) {
		return std::make_shared<detail::ComposedConstraint<F,E>>(filter, executor);
	}

	template<typename E>
	ConstraintPtr elem(const E& e, const TypedSetID<E>& a) {
		return combine(detail::TrueFilter(), e_in(e,a));
	}

	template<typename A, typename B>
	ConstraintPtr elemIf(const A& e, const TypedSetID<A>& a, const B& f, const TypedSetID<B>& b) {
		return combine(f_in(e,a), e_in(f,b));
	}

	template<typename A>
	ConstraintPtr subset(const TypedSetID<A>& a, const TypedSetID<A>& b) {
		return combine(detail::TrueFilter(), e_sub(a,b));
	}

	template<typename E, typename A>
	ConstraintPtr subsetIf(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return combine(f_in(e,a), e_sub(b,c));
	}

	template<typename A, typename B, typename C>
	ConstraintPtr subsetIf(const A& a, const TypedSetID<A>& as, const B& b, const TypedSetID<B>& bs, const TypedSetID<C>& in, const TypedSetID<C>& out) {
		return combine(f_in(a,as) && f_in(b,bs), e_sub(in, out));
	}

	template<typename A, typename B>
	ConstraintPtr subsetIfBigger(const TypedSetID<A>& a, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c) {
		return combine(detail::BiggerThanFilter<A>(a,s), e_sub(b,c));
	}

	template<typename E, typename A>
	ConstraintPtr subsetIfReducedBigger(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return combine(detail::BiggerThanReducedFilter<E>(a,e,s), e_sub(b,c));
	}

	template<typename A, typename B, typename F>
	ConstraintPtr subsetUnary(const TypedSetID<A>& in, const TypedSetID<B>& out, const F& fun) {
		return combine(detail::TrueFilter(), detail::SubsetUnary<A,B>(in, out, fun));
	}

	template<typename A, typename B, typename R, typename F>
	ConstraintPtr subsetBinary(const TypedSetID<A>& a, const TypedSetID<B>& b, const TypedSetID<R>& r, const F& fun) {
		return combine(detail::TrueFilter(), detail::SubsetBinary<A,B,R>(a, b, r, fun));
	}

	// ----------------------------- Constraint Container ------------------------------


	class Constraints : public Printable {

	public:

		typedef std::vector<ConstraintPtr> data_type;
		typedef typename data_type::const_iterator const_iterator;

	private:

		// think about making this a set
		data_type data;

	public:

		Constraints() : data() {}

		Constraints(const std::initializer_list<ConstraintPtr>& list) : data(list) {}

		void add(const ConstraintPtr& constraint) {
			data.push_back(constraint);
		}

		void add(const Constraints& constraints) {
			data.insert(data.end(), constraints.data.begin(), constraints.data.end());
		}

		const std::vector<ConstraintPtr>& getList() const {
			return data;
		}

		const_iterator begin() const { return data.begin(); }
		const_iterator end() const { return data.end(); }

		std::size_t size() const { return data.size(); }

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, print<deref<ConstraintPtr>>()) << "}";
		}
	};



	// ----------------------------- Assignment ------------------------------

	class Assignment : public Printable {

		struct Container : public VirtualPrintable {
			virtual ~Container() {};
			virtual void append(std::map<SetID,string>& res) const =0;
			virtual Container* copy() const =0;
		};

		template<typename T>
		class TypedContainer : public Container, public std::map<TypedSetID<T>, std::set<T>> {
			typedef std::map<TypedSetID<T>, std::set<T>> map_type;
			typedef typename map_type::value_type value_type;
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << join(",",*this, [](std::ostream& out, const value_type& cur) {
					out << cur.first << "=" << cur.second;
				});
			}
			virtual void append(std::map<SetID,string>& res) const {
				for(auto& cur : *this) {
					res.insert({cur.first, toString(cur.second)});
				}
			}
			virtual Container* copy() const {
				return new TypedContainer<T>(*this);
			}
		};

		typedef TypedMap<TypedContainer, Container> container_index_type;

		container_index_type data;

	public:

		Assignment() {};

		Assignment(const Assignment& other) : data(other.data) { }

		template<typename E>
		std::set<E>& get(const TypedSetID<E>& set) {
			return data.get<E>()[set];
		}

		template<typename E>
		const std::set<E>& get(const TypedSetID<E>& set) const {
			static const std::set<E> empty;
			auto& map = data.get<E>();
			auto pos = map.find(set);
			if (pos != map.end()) { return pos->second; }
			return empty;
		}

		template<typename E>
		std::set<E>& operator[](const TypedSetID<E>& set) {
			return get(set);
		}

		template<typename E>
		const std::set<E>& operator[](const TypedSetID<E>& set) const {
			return get(set);
		}

		bool operator==(const Assignment& other) const {
			return data == other.data;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

		std::map<SetID,string> toStringMap() const {
			std::map<SetID,string> res;
			for(auto& cur : data) {
				cur.second->append(res);
			}
			return res;
		}

	protected:

//		template<typename T>
//		TypedContainer<T>& getContainer(const TypedSetID<T>& type) {
//			auto& key = typeid(T);
//			auto pos = data.find(key);
//			if (pos != data.end()) {
//				return static_cast<TypedContainer<T>&>(*pos->second);
//			}
//
//			// create and request new instance
//			TypedContainer<T>* container = new TypedContainer<T>();
//			data[key] = container;
//			return *container;
//		}
//
//		template<typename T>
//		const TypedContainer<T>& getContainer(const TypedSetID<T>& type) const {
//			static const TypedContainer<T> empty;
//			auto& key = typeid(T);
//			auto pos = data.find(key);
//			if (pos != data.end()) {
//				return static_cast<const TypedContainer<T>&>(*pos->second);
//			}
//
//			// otherwise, return a pointer to an empty one
//			return empty;
//		}

	};


	// ----------------------------- Solver ------------------------------

	// The type of entities capable of resolving constraints.
	typedef std::function<Constraints(const std::set<SetID>&)> ConstraintResolver;

	class LazySolver {

		/**
		 * The source of lazy-generated constraints.
		 */
		ConstraintResolver resolver;

		/**
		 * The list of maintained constraints.
		 */
		Constraints constraints;

		/**
		 * The current partial solution.
		 */
		Assignment ass;

		/**
		 * The set of sets for which constraints have already been resolved.
		 */
		std::unordered_set<SetID> resolved;

		/**
		 * A lazily constructed graph of constraint dependencies.
		 */
		typedef std::unordered_map<SetID, std::set<const Constraint*>> Edges;
		Edges edges;

		/**
		 * A set of fully resolved constraints (all inputs resolved, just for performance)
		 */
		std::unordered_set<const Constraint*> resolvedConstraints;

	public:

		LazySolver(const ConstraintResolver& resolver, const Assignment& initial = Assignment())
			: resolver(resolver), ass(initial) {}

		/**
		 * Obtains an assignment including the solution of the requested set. This is an incremental
		 * approach and may be used multiple times. Previously computed results will be reused.
		 */
		const Assignment& solve(const SetID& set);

		/**
		 * Obtains an assignment including solutions for the given sets. This is an incremental
		 * approach and may be used multiple times. Previously computed results will be reused.
		 */
		const Assignment& solve(const std::set<SetID>& sets);

		/**
		 * Obtains a reference to the list of constraints maintained internally.
		 */
		const Constraints& getConstraints() const {
			return constraints;
		}

		/**
		 * Obtains a reference to the current assignment maintained internally.
		 */
		const Assignment& getAssignment() const {
			return ass;
		}

		bool isResolved(const SetID& set) const {
			return resolved.find(set) != resolved.end();
		}

	private:

		// -- internal utility functions ---

		bool hasUnresolvedInput(const Constraint& cur);

		void resolveConstraints(const Constraint& cur, vector<SetID>& worklist);

		void resolveConstraints(const std::set<SetID>& sets, vector<SetID>& worklist);

	};

	// an eager solver implementation
	Assignment solve(const Constraints& constraints, Assignment initial = Assignment());

	// a lazy solver for a single set
	Assignment solve(const SetID& set, const ConstraintResolver& resolver, Assignment initial = Assignment());

	// a lazy solver implementation
	Assignment solve(const std::set<SetID>& sets, const ConstraintResolver& resolver, Assignment initial = Assignment());

} // end namespace set_constraint_2
} // end namespace utils
} // end namespace insieme


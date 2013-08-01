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

#include <set>
#include <map>
#include <tuple>

#include <memory>
#include <initializer_list>

#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"

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


	// ----------------------------- Constraints ------------------------------

	// a common base type for all kind of constraints

	class Constraint : public Printable {

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

		const std::vector<SetID>& getInputs() const { return inputs; };
		const std::vector<SetID>& getOutputs() const { return outputs; };

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

		SetIDs combine(const SetIDs& a, const SetIDs& b) {
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
				executor.writeDotEdge(out);
				return out << "  [label=\"" << *this << "\"]\n";
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
		};

		template<typename A, typename B>
		struct AndFilter : public Filter<> {
			A a; B b;
			bool operator()(const Assignment& ass) const {
				return a(ass) && b(ass);
			}
			void print(std::ostream& out) const {
				a.print(out); out << " and "; b.print(out);
			}
			SetIDs getInputs() const {
				return combine(a.getInputs(), b.getInputs());
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
			void writeDotEdge(std::ostream& out) const {
				out << "e" << (int*)&e << " [label=\"" << e << "\"]\n";
				out << "e" << (int*)&e << " -> " << a;
			}
		};

		template<typename T>
		class Subset : public Executor {
			TypedSetID<T> a;
			TypedSetID<T> b;
		public:
			Subset(const TypedSetID<T>& a, const TypedSetID<T>& b)
				: a(a), b(b) {}
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
			void writeDotEdge(std::ostream& out) const {
				out << a << " -> " << b;
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
			void writeDotEdge(std::ostream& out) const {
				out << a << " -> " << r;
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
			void writeDotEdge(std::ostream& out) const {
				out << a << " -> " << r;
				out << b << " -> " << r;
			}
		};

	}	// end of details namespace



	// ----------------------------- Constraint Factory Functions ------------------------------


	template<typename E>
	ConstraintPtr elem(const E& e, const TypedSetID<E>& a) {
		return std::make_shared<detail::ComposedConstraint<detail::TrueFilter, detail::ElementOf<E>>>(detail::TrueFilter(), detail::ElementOf<E>(e,a));
	}

	template<typename A>
	ConstraintPtr subset(const TypedSetID<A>& a, const TypedSetID<A>& b) {
		return std::make_shared<detail::ComposedConstraint<detail::TrueFilter, detail::Subset<A>>>(detail::TrueFilter(), detail::Subset<A>(a,b));
	}

	template<typename E, typename A>
	ConstraintPtr subsetIf(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return std::make_shared<detail::ComposedConstraint<detail::ElementOfFilter<E>, detail::Subset<A>>>(detail::ElementOfFilter<E>(e,a), detail::Subset<A>(b,c));
	}

	template<typename A, typename B>
	ConstraintPtr subsetIfBigger(const TypedSetID<A>& a, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c) {
		return std::make_shared<detail::ComposedConstraint<detail::BiggerThanFilter<A>, detail::Subset<A>>>(detail::BiggerThanFilter<A>(a,s), detail::Subset<B>(b,c));
	}

	template<typename E, typename A>
	ConstraintPtr subsetIfReducedBigger(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return std::make_shared<detail::ComposedConstraint<detail::BiggerThanReducedFilter<E>, detail::Subset<A>>>(detail::BiggerThanReducedFilter<E>(a,e,s), detail::Subset<A>(b,c));
	}

	template<typename A, typename B, typename F>
	ConstraintPtr subsetUnary(const TypedSetID<A>& in, const TypedSetID<B>& out, const F& fun) {
		return std::make_shared<detail::ComposedConstraint<detail::TrueFilter, detail::SubsetUnary<A,B>>>(detail::TrueFilter(), detail::SubsetUnary<A,B>(in, out, fun));
	}

	template<typename A, typename B, typename R, typename F>
	ConstraintPtr subsetBinary(const TypedSetID<A>& a, const TypedSetID<B>& b, const TypedSetID<R>& r, const F& fun) {
		return std::make_shared<detail::ComposedConstraint<detail::TrueFilter, detail::SubsetBinary<A,B,R>>>(detail::TrueFilter(), detail::SubsetBinary<A,B,R>(a, b, r, fun));
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

		const std::vector<ConstraintPtr>& getList() const {
			return data;
		}

		const_iterator begin() const { return data.begin(); }
		const_iterator end() const { return data.end(); }

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, print<deref<ConstraintPtr>>()) << "}";
		}
	};



	// ----------------------------- Assignment ------------------------------

	class Assignment : public Printable {

		struct Container : public Printable {
			virtual ~Container() {};
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
		};

		typedef std::map<std::type_index, Container*> container_index_type;

		container_index_type data;

	public:

		~Assignment() {
			for(auto cur : data) {
				delete cur.second;
			}
		}

		template<typename E>
		std::set<E>& get(const TypedSetID<E>& set) {
			return getContainer(set)[set];
		}

		template<typename E>
		const std::set<E>& get(const TypedSetID<E>& set) const {
			static const std::set<E> empty;
			auto& map = getContainer(set);
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

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, [](std::ostream& out, const container_index_type::value_type& cur) {
				out << *cur.second;
			}) << "}";
		}

	protected:

		template<typename T>
		TypedContainer<T>& getContainer(const TypedSetID<T>& type) {
			auto& key = typeid(T);
			auto pos = data.find(key);
			if (pos != data.end()) {
				return static_cast<TypedContainer<T>&>(*pos->second);
			}

			// create and request new instance
			TypedContainer<T>* container = new TypedContainer<T>();
			data[key] = container;
			return *container;
		}

		template<typename T>
		const TypedContainer<T>& getContainer(const TypedSetID<T>& type) const {
			static const TypedContainer<T> empty;
			auto& key = typeid(T);
			auto pos = data.find(key);
			if (pos != data.end()) {
				return static_cast<const TypedContainer<T>&>(*pos->second);
			}

			// otherwise, return a pointer to an empty one
			return empty;
		}

	};


	// ----------------------------- Solver ------------------------------

	Assignment solve(const Constraints& constraints, Assignment initial = Assignment());


} // end namespace set_constraint_2
} // end namespace utils
} // end namespace insieme

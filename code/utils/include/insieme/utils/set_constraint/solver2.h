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
	template<typename ... T> class Assignment;


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
	};


	// ----------------------------- Constraints ------------------------------

	// a common base type for all kind of constraints

	template<typename ... T>
	struct Constraint : public Printable {

		std::vector<SetID> inputs;
		std::vector<SetID> outputs;

		Constraint(const std::vector<SetID>& in, const std::vector<SetID>& out)
			: inputs(in), outputs(out) {}

		virtual ~Constraint() {};

		virtual void init(Assignment<T...>& ass, vector<SetID>& workList) const { };
		virtual bool update(Assignment<T...>& ass) const { return false; };
		virtual bool check(const Assignment<T...>& ass) const =0;

		const std::vector<SetID>& getInputs() const { return inputs; };
		const std::vector<SetID>& getOutputs() const { return outputs; };

	protected:

		// a utility function merging sets
		template<typename A>
		bool addAll(Assignment<T...>& ass, const TypedSetID<A>& srcSet, const TypedSetID<A>& trgSet) const {
			// get actual set
			auto& trg = ass[trgSet];

			// add values to target set
			bool newData = false;
			for(const auto& x : ass[srcSet]) {
				newData = trg.insert(x).second || newData;
			}

			// if target set has changed
			return newData;
		};

	};


	// an element constraint:   e \in A
	template<typename E, typename ... T>
	class ElementOfConstraint : public Constraint<T...> {

		E e;
		TypedSetID<E> a;

	public:

		ElementOfConstraint(const E& e, const TypedSetID<E>& a)
			: Constraint<T...>(toVector<SetID>(), toVector<SetID>(a)), e(e), a(a) {}

		virtual void init(Assignment<T...>& ass, vector<SetID>& workList) const {
			ass[a].insert(e);
			workList.push_back(a);
		}

		virtual bool check(const Assignment<T...>& ass) const {
			return contains(ass[a], e);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << e << " in " << a;
		}
	};


	// a simple sub-set constraint:   A \subset B
	template<typename A, typename B, typename ... T>
	class SubSetConstraint : public Constraint<T...> {

		TypedSetID<A> a;
		TypedSetID<B> b;

	public:

		SubSetConstraint(const TypedSetID<A>& a, const TypedSetID<B>& b)
			: Constraint<T...>(toVector<SetID>(a), toVector<SetID>(b)), a(a), b(b) {}

		virtual bool update(Assignment<T...>& ass) const {
			return Constraint<T...>::addAll(ass,a,b);
		}

		virtual bool check(const Assignment<T...>& ass) const {
			return set::isSubset(ass[a], ass[b]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << a << " sub " << b;
		}
	};


	// e \in A  =>   B \subset C
	template<typename E, typename A, typename ... T>
	class SubSetIfConstraint : public Constraint<T...> {

		E e;
		TypedSetID<E> a;
		TypedSetID<A> b;
		TypedSetID<A> c;

	public:

		SubSetIfConstraint(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c)
			: Constraint<T...>(toVector<SetID>(a,b), toVector<SetID>(c)), e(e), a(a), b(b), c(c) {}

		virtual bool update(Assignment<T...>& ass) const {
			if (!contains(ass[a], e)) return false;
			return Constraint<T...>::addAll(ass,b,c);
		}

		virtual bool check(const Assignment<T...>& ass) const {
			return !contains(ass[a],e) || set::isSubset(ass[b], ass[c]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << e << " in " << a << " => " << b << " sub " << c;
		}
	};


	// |A| > s  =>   B \subset C
	template<typename A, typename B, typename ... T>
	class SubSetIfBiggerConstraint : public Constraint<T...> {

		std::size_t s;
		TypedSetID<A> a;
		TypedSetID<B> b;
		TypedSetID<B> c;

	public:

		SubSetIfBiggerConstraint(std::size_t s, const TypedSetID<A>& a, const TypedSetID<B>& b, const TypedSetID<B>& c)
			: Constraint<T...>(toVector<SetID>(a,b), toVector<SetID>(c)), s(s), a(a), b(b), c(c) {}

		virtual bool update(Assignment<T...>& ass) const {
			if (ass[a].size() <= s) return false;
			return Constraint<T...>::addAll(ass,b,c);
		}

		virtual bool check(const Assignment<T...>& ass) const {
			return ass[a].size() <= s || set::isSubset(ass[b], ass[c]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "|" << a << "| > " << s << " => " << b << " sub " << c;
		}
	};


	// |A \ {t} | > s  =>   B \subset C
	template<typename E, typename B, typename ... T>
	class SubSetIfReducedBiggerConstraint : public Constraint<T...> {

		std::size_t s;
		E e;
		TypedSetID<E> a;
		TypedSetID<B> b;
		TypedSetID<B> c;

	public:

		SubSetIfReducedBiggerConstraint(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c)
			: Constraint<T...>(toVector<SetID>(a,b), toVector<SetID>(c)), s(s), e(e), a(a), b(b), c(c) {}

		virtual bool update(Assignment<T...>& ass) const {
			auto& set = ass[a];
			auto n = set.size() - (contains(set, e) ? 1 : 0);
			if (n <= s) return false;
			return Constraint<T...>::addAll(ass,b,c);
		}

		virtual bool check(const Assignment<T...>& ass) const {
			return (ass[a].size() - ((contains(ass[a],e))?1:0)) <= e || set::isSubset(ass[a], ass[b]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "|" << a << " - {" << e << "}| > " << s << " => " << b << " sub " << c;
		}
	};



	// ----------------------------- Constraint Set ------------------------------


	template<typename ... T>
	class Constraints : public Printable {

		typedef std::shared_ptr<Constraint<T...>> element_type;

		std::vector<element_type> data;

	public:

		Constraints() : data() {}

		Constraints(const std::initializer_list<element_type>& list) : data(list) {}

		const std::vector<element_type>& getList() const { return data; }

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, print<deref<element_type>>()) << "}";
		}
	};


	// ----------------------------- Constraint Factory ------------------------------

	template<typename ... T>
	struct ConstraintFactory {

		typedef std::shared_ptr<Constraint<T...>> constraint_type;


		template<typename E>
		constraint_type elem(const E& e, const TypedSetID<E>& a) {
			return std::make_shared<ElementOfConstraint<E,T...>>(e,a);
		}

		template<typename A, typename B>
		constraint_type subset(const TypedSetID<A>& a, const TypedSetID<B>& b) {
			return std::make_shared<SubSetConstraint<A,B,T...>>(a,b);
		}

		template<typename E, typename A>
		constraint_type subsetIf(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c) {
			return std::make_shared<SubSetIfConstraint<E,A,T...>>(e,a,b,c);
		}

		template<typename A, typename B>
		constraint_type subsetIfBigger(const TypedSetID<A>& a, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c) {
			return std::make_shared<SubSetIfBiggerConstraint<A,B,T...>>(s,a,b,c);
		}

		template<typename E, typename A>
		constraint_type subsetIfReducedBigger(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<A>& b, const TypedSetID<A>& c) {
			return std::make_shared<SubSetIfReducedBiggerConstraint<E,A,T...>>(a,e,s,b,c);
		}

	};


	// ----------------------------- Assignment ------------------------------

	namespace detail {

		template<typename E, typename ... T> struct index_of;

		template<typename E, typename ... T>
		struct index_of<E,E,T...> {
			enum { value = 0 };
		};

		template<typename E, typename H, typename ... T>
		struct index_of<E,H,T...> {
			enum { value = 1 + index_of<E, T...>::value };
		};

	} // end namespace: detail


	template<typename ... T>
	class Assignment : public Printable {

		std::tuple<std::map<TypedSetID<T>,std::set<T>>...> data;

	public:

		template<typename E>
		std::set<E>& get(const TypedSetID<E>& set) {
			return std::get<detail::index_of<E,T...>::value>(data)[set];
		}

		template<typename E>
		const std::set<E>& get(const TypedSetID<E>& set) const {
			static const std::set<E> empty;
			auto& map = std::get<detail::index_of<E,T...>::value>(data);
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

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

	};

	template<typename ... T>
	Assignment<T...> createAssignment() {
		return Assignment<T...>();
	}


	// ----------------------------- Solver ------------------------------

	template<typename ... T>
	Assignment<T...> solve(const Constraints<T...>& constraints, Assignment<T...> initial = Assignment<T...>()) {

		// the data structure representing the graph this algorithm is based on
		typedef std::map<SetID, std::set<const Constraint<T...>*>> Edges;

		// the work-list
		std::vector<SetID> workList;

		// build data structures for the graph
		Assignment<T...> res = initial;
		Edges edges;

		// 1. create list of edges
		for(auto& cur : constraints.getList()) {
			// trigger init-routines
			cur->init(res, workList);

			// add edges
			for (auto set : cur.getInputs()) {
				edges[set].insert(&*cur);
			}
		}

		// TODO: make work list unique (there are duplicates)

		// 2. solve constraints
		while(!workList.empty()) {
			// retrieve first element
			SetID head = workList.back();
			workList.pop_back();

			// process outgoing edges
			for (const Constraint<T...>* cur : edges[head]) {
				const Constraint<T...>& cc = *cur;

				// trigger update
				bool change = cc->update(res);

				// register outputs in work-list
				if (change) {
					for (auto cur : cc.getOutputs()) {
						workList.push_back(cur);
					}
				}
			}
		}

		// done
		return res;
	};


} // end namespace set_constraint_2
} // end namespace utils
} // end namespace insieme

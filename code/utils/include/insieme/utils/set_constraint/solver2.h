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
		bool addAll(Assignment& ass, const TypedSetID<A>& srcSet, const TypedSetID<A>& trgSet) const {
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
	template<typename E>
	class ElementOfConstraint : public Constraint {

		E e;
		TypedSetID<E> a;

	public:

		ElementOfConstraint(const E& e, const TypedSetID<E>& a)
			: Constraint(toVector<SetID>(), toVector<SetID>(a)), e(e), a(a) {}

		virtual void init(Assignment& ass, vector<SetID>& workList) const {
			ass[a].insert(e);
			workList.push_back(a);
		}

		virtual bool check(const Assignment& ass) const {
			return contains(ass[a], e);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << e << " in " << a;
		}

		virtual std::ostream& writeDotEdge(std::ostream& out) const {
			return out << "e" << e << " -> " << a << " [label=\"in\"];";
		}
	};


	// a simple sub-set constraint:   A \subset B
	template<typename A, typename B>
	class SubSetConstraint : public Constraint {

		TypedSetID<A> a;
		TypedSetID<B> b;

	public:

		SubSetConstraint(const TypedSetID<A>& a, const TypedSetID<B>& b)
			: Constraint(toVector<SetID>(a), toVector<SetID>(b)), a(a), b(b) {}

		virtual bool update(Assignment& ass) const {
			return Constraint::addAll(ass,a,b);
		}

		virtual bool check(const Assignment& ass) const {
			return set::isSubset(ass[a], ass[b]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << a << " sub " << b;
		}

		virtual std::ostream& writeDotEdge(std::ostream& out) const {
			return out << a << " -> " << b << " [label=\"sub\"];";
		}
	};


	// e \in A  =>   B \subset C
	template<typename E, typename A>
	class SubSetIfConstraint : public Constraint {

		E e;
		TypedSetID<E> a;
		TypedSetID<A> b;
		TypedSetID<A> c;

	public:

		SubSetIfConstraint(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c)
			: Constraint(toVector<SetID>(a,b), toVector<SetID>(c)), e(e), a(a), b(b), c(c) {}

		virtual bool update(Assignment& ass) const {
			if (!set::contains(ass[a], e)) return false;
			return Constraint::addAll(ass,b,c);
		}

		virtual bool check(const Assignment& ass) const {
			return !set::contains(ass[a],e) || set::isSubset(ass[b], ass[c]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << e << " in " << a << " => " << b << " sub " << c;
		}

		virtual std::ostream& writeDotEdge(std::ostream& out) const {
			return out << b << " -> " << c << " [label=\"if " << e << " in " << a << "\"];";
		}

	};


	// |A| > s  =>   B \subset C
	template<typename A, typename B>
	class SubSetIfBiggerConstraint : public Constraint {

		std::size_t s;
		TypedSetID<A> a;
		TypedSetID<B> b;
		TypedSetID<B> c;

	public:

		SubSetIfBiggerConstraint(std::size_t s, const TypedSetID<A>& a, const TypedSetID<B>& b, const TypedSetID<B>& c)
			: Constraint(toVector<SetID>(a,b), toVector<SetID>(c)), s(s), a(a), b(b), c(c) {}

		virtual bool update(Assignment& ass) const {
			if (ass[a].size() <= s) return false;
			return Constraint::addAll(ass,b,c);
		}

		virtual bool check(const Assignment& ass) const {
			return ass[a].size() <= s || set::isSubset(ass[b], ass[c]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "|" << a << "| > " << s << " => " << b << " sub " << c;
		}

		virtual std::ostream& writeDotEdge(std::ostream& out) const {
			return out << b << " -> " << c << " [label=\"if |" << a << "| > " << s << "\"];";
		}
	};


	// |A \ {t} | > s  =>   B \subset C
	template<typename E, typename B>
	class SubSetIfReducedBiggerConstraint : public Constraint {

		std::size_t s;
		E e;
		TypedSetID<E> a;
		TypedSetID<B> b;
		TypedSetID<B> c;

	public:

		SubSetIfReducedBiggerConstraint(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c)
			: Constraint(toVector<SetID>(a,b), toVector<SetID>(c)), s(s), e(e), a(a), b(b), c(c) {}

		virtual bool update(Assignment& ass) const {
			auto& set = ass[a];
			auto n = set.size() - (set::contains(set, e) ? 1 : 0);
			if (n <= s) return false;
			return Constraint::addAll(ass,b,c);
		}

		virtual bool check(const Assignment& ass) const {
			return (ass[a].size() - ((set::contains(ass[a],e))?1:0)) <= s || set::isSubset(ass[b], ass[c]);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "|" << a << " - {" << e << "}| > " << s << " => " << b << " sub " << c;
		}

		virtual std::ostream& writeDotEdge(std::ostream& out) const {
			return out << b << " -> " << c << " [label=\"if |" << a << " - {" << e << "}| > " << s << "\"];";
		}
	};


	// ----------------------------- Constraint Factory Functions ------------------------------


	template<typename E>
	ConstraintPtr elem(const E& e, const TypedSetID<E>& a) {
		return std::make_shared<ElementOfConstraint<E>>(e,a);
	}

	template<typename A, typename B>
	ConstraintPtr subset(const TypedSetID<A>& a, const TypedSetID<B>& b) {
		return std::make_shared<SubSetConstraint<A,B>>(a,b);
	}

	template<typename E, typename A>
	ConstraintPtr subsetIf(const E& e, const TypedSetID<E>& a, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return std::make_shared<SubSetIfConstraint<E,A>>(e,a,b,c);
	}

	template<typename A, typename B>
	ConstraintPtr subsetIfBigger(const TypedSetID<A>& a, std::size_t s, const TypedSetID<B>& b, const TypedSetID<B>& c) {
		return std::make_shared<SubSetIfBiggerConstraint<A,B>>(s,a,b,c);
	}

	template<typename E, typename A>
	ConstraintPtr subsetIfReducedBigger(const TypedSetID<E>& a, const E& e, std::size_t s, const TypedSetID<A>& b, const TypedSetID<A>& c) {
		return std::make_shared<SubSetIfReducedBiggerConstraint<E,A>>(a,e,s,b,c);
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

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

#include <map>
#include <set>

#include "insieme/utils/graph_utils.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {

	using std::set;
	using std::map;

	typedef string Var;
	typedef int Value;

	class Node;
	class Edge;

	typedef map<Var,set<Value>> Assignment;

	class Node :
			public utils::Printable,
			public utils::Hashable {

	public:

		enum Type {
			Read, Write, Noop
		};

	private:

		std::size_t id;
		Type type;
		Var var;
		Value value;

	public:

		mutable Assignment before;
		mutable Assignment after;


		Node() : id(nextID()), type(Noop), var(), value(0) {}

		Node(const Node& other)
			: id(other.id), type(other.type), var(other.var), value(other.value),
			  before(other.before), after(other.after) {}

	private:

		Node(Type type, const Var& var, const Value& value)
			: id(nextID()), type(type), var(var), value(value) {}

	public:

		Type getType() const { return type; }
		Var  getVar() const { return var; }
		Value getValue() const { return value; }

		bool operator==(const Node& other) const {
			return id == other.id;
		}

		std::size_t hash() const {
			return id;
		}

		std::ostream& printTo(std::ostream& out) const {
			out << before << " / ";
			switch(type) {
			case Read:  out << "R: " << var; break;
			case Write: out << "W: " << var << " = " << value; break;
			case Noop:  out << "NoOp"; break;
			default:
				assert_fail() << "Unsupported type: " << type;
				break;
			}
			out << " / " << after ;
			return out;
		}

	private:

		static std::size_t nextID() {
			static std::size_t next = 0;
			return next++;
		}

	public:

		static Node read(const Var& var) {
			return Node(Read, var, 0);
		}

		static Node write(const Var& var, Value value = 0) {
			return Node(Write, var, value);
		}

		static Node noop() {
			return Node();
		}

	};

//	class Edge :
//			public utils::Printable {
//
//		map<Var,set<Value>> assignment;
//
//	public:
//
//		std::ostream& printTo(std::ostream& out) const {
//			return out << assignment;
//		}
//
//	};

//	typedef utils::graph::Graph<Node,Edge> GraphBuilder;
	typedef utils::graph::Graph<Node> Graph;

	void solve(const Graph& graph);

} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

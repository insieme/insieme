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

#include <memory>
#include <vector>
#include <set>

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/set_utils.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {

	using std::set;
	using std::vector;

	typedef set<int> WriteSet;

	template<typename ... V>
	WriteSet writeSet(V ... vs) {
		return utils::set::toSet<WriteSet>(vs...);
	}

	class RelationGraph;
	typedef std::shared_ptr<RelationGraph> Relation;


	Relation par(const Relation& a, const Relation& b);
	Relation par(const vector<Relation>& relations);

	Relation seq(const WriteSet& op);
	Relation seq(const Relation& a, const WriteSet& op);


	// implementation details


	struct RelationGraph : public utils::VirtualPrintable {

		virtual ~RelationGraph() {};

		virtual WriteSet getMostRecentWrite() const =0;

		virtual WriteSet getKilledDefinitions() const =0;

	};


	class ParallelRelationGraph : public RelationGraph {

		vector<Relation> elements;

	public:

		ParallelRelationGraph(const vector<Relation>& elements)
			: elements(elements) {}

		WriteSet getMostRecentWrite() const {
			WriteSet res;
			for(const auto& cur : elements) {
				if (cur) {
					WriteSet inner = cur->getMostRecentWrite();
					res.insert(inner.begin(), inner.end());
				}
			}

			// remove all killed definitions
			for(const auto& cur : elements) {
				if (cur) {
					WriteSet killed = cur->getKilledDefinitions();
					for(auto cur : killed) res.erase(cur);
				}
			}

			return res;
		}

		WriteSet getKilledDefinitions() const {
			return WriteSet();
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", elements, [](std::ostream& out, const Relation& cur) { out << *cur; }) << "}";
		}

	};

	class SequentialRelationGraph : public RelationGraph {

		Relation sub;

		WriteSet write;

	public:

		SequentialRelationGraph(const Relation& sub, const WriteSet& write)
			: sub(sub), write(write) {}

		WriteSet getMostRecentWrite() const {
			return write;
		}

		WriteSet getKilledDefinitions() const {
			if (!sub) return WriteSet();
			return sub->getMostRecentWrite();
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!sub) return out << write;
			return out << *sub << "->" << write;
		}
	};

	Relation par(const vector<Relation>& relations) {
		if (relations.empty()) return Relation();
		if (relations.size() == 1u) return relations[0];
		return std::make_shared<ParallelRelationGraph>(relations);
	}

	Relation par(const Relation& a, const Relation& b) {
		if (!a) return b;
		if (!b) return a;
		return par(toVector(a,b));
	}


	Relation seq(const WriteSet& set) {
		return seq(Relation(), set);
	}

	Relation seq(const Relation& a, const WriteSet& set) {
		return std::make_shared<SequentialRelationGraph>(a, set);
	}




} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	ostream& operator<<(ostream& out, const insieme::analysis::cba::prototype::Relation& relation) {
		if (!relation) return out << "{}";
		return out << *relation;
	}

}

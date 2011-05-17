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

#include "insieme/simple_backend/code_management.h"

#include <boost/graph/topological_sort.hpp>
#include <boost/graph/adjacency_list.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace simple_backend {

const CodeBuffer::IndR CodeBuffer::indR = CodeBuffer::IndR();
const CodeBuffer::IndL CodeBuffer::indL = CodeBuffer::IndL();


std::string CodeBuffer::toString() const {
	// defuglify code
	std::string retval = ss.str();
	boost::replace_all(retval, "*&", "");
	boost::replace_all(retval, ";;", ";");
	return retval;
}

std::ostream& CodeBuffer::printTo(std::ostream& out) const {
	return out << toString();
}


CodeFragmentPtr CodeFragment::createNew(const std::string& name) {
	return CodeFragmentPtr(new CodeFragment(name, false));
}

CodeFragmentPtr CodeFragment::createNewDummy(const std::string& name) {
	return CodeFragmentPtr(new CodeFragment(name, true));
}

void CodeFragment::addDependency(const CodeFragmentPtr& dep) {
	if (dep && this != &*dep) { // only add if it is not a null pointer and not self
		dependencies.push_back(dep);
	}
}

void CodeFragment::addDependencies(const std::vector<CodeFragmentPtr>& fragments) {
	// just add all dependencies
	for_each(fragments, [&](const CodeFragmentPtr& cur) {
		this->addDependency(cur);
	});
}


namespace depResolve {

	using namespace boost;

	typedef adjacency_list<vecS, vecS, directedS, property<vertex_name_t, CodeFragmentPtr>> Graph;
	typedef graph_traits<Graph>::vertex_descriptor Vertex;
	typedef std::map<CodeFragment*, Vertex> CodeVertexMap;
	typedef std::set<CodeFragment*> FragmentSet;

	void addDeps(const CodeFragmentPtr& cur, Graph& g, CodeVertexMap& vmap, FragmentSet& processed) {
		property_map<Graph, vertex_name_t>::type codePtrMap = get(vertex_name, g);
		
		// abort if node has already been processed
		auto res = processed.insert(&(*cur));
		if (!res.second) {
			return;
		}

		auto vertexGen = [&g, &vmap, &codePtrMap](const CodeFragmentPtr& ptr) -> Vertex {
			Vertex v;
			auto insertionResult = vmap.insert(std::make_pair(&(*ptr), Vertex()));
			if(insertionResult.second) {
				v = add_vertex(g);
				codePtrMap[v] = ptr;
				insertionResult.first->second = v;
			} else {
				v = insertionResult.first->second;
			}
			return v;
		};
		Vertex u = vertexGen(cur);

		for_each(cur->getDependencies(), [&](const CodeFragmentPtr& dep) {
			Vertex v = vertexGen(dep);
			add_edge(u, v, g);
			addDeps(dep, g, vmap, processed);
		});

	}

	/** Internal helper function that resolves all dependencies of the input code fragment to a flat list.
	 ** Fails in case of circular dependencies.
	 ** */
	void resolve(const CodeFragmentPtr& code, std::vector<CodeFragmentPtr>& result) {

		Graph g;
		property_map<Graph, vertex_name_t>::type codePtrMap = get(vertex_name, g);
		CodeVertexMap vmap;
		FragmentSet processed;

		addDeps(code, g, vmap, processed);
		std::vector<Vertex> vResult;

		try {
			topological_sort(g, std::back_inserter(vResult));
		}
		catch(not_a_dag e) {
			assert(0 && "Impossible to resolve cyclic dependencies.");
		}

		std::transform(vResult.cbegin(), vResult.cend(), std::back_inserter(result), [&](const Vertex& curV) {
			return codePtrMap(curV);
		});
	}
}

} // namespace simple_backend
} // namespace insieme

std::ostream& operator<<(std::ostream& os, const insieme::simple_backend::CodeFragmentPtr& cp) {
	std::vector<insieme::simple_backend::CodeFragmentPtr> flatDeps;
	insieme::simple_backend::depResolve::resolve(cp, flatDeps);
	for_each(flatDeps, [&os](const insieme::simple_backend::CodeFragmentPtr& cur) {
		if (!cur->isDummy()) {
			os << "\n// start code fragment :: " << cur->getName() << " //\n";
			os << cur->getCodeBuffer();
		}
	});
	return os;
}

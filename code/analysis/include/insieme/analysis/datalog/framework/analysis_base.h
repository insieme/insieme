/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <string>

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"

#include <souffle/SouffleInterface.h>

namespace insieme {
namespace analysis {
namespace datalog {
namespace framework {



template <typename Sf_base>
class AnalysisBase : public Sf_base, public core::IRVisitor<int> {

	int node_counter = 0;

public:
	AnalysisBase() : Sf_base(), core::IRVisitor<int>(true) {}

	//FIXME: Make it build
	using NodeType = std::string;

	int extractFacts(const core::NodePtr& rootNode) {

		dumpText(rootNode);

		return visit(rootNode);
	}

	// -- Value Nodes --

	int visitStringValue(const core::StringValuePtr& val) override {
		int id = ++node_counter;
		return id;
	}

	// -- Type Nodes --

	int visitTypeVariable(const core::TypeVariablePtr& var) override {

		// get new ID for this node
		int id = ++node_counter;

		// check whether the necessary relation is present
		auto rel = this->getRelation("TypeVariable");
		if (!rel) return id;

		// insert a tuple into the relation
		souffle::tuple tuple(rel);
		tuple << id;
		tuple << var->getVarName()->getValue();
		rel->insert(tuple);

		// return id
		return id;
	}

	int visitTupleType(const core::TupleTypePtr& tuple) override {

		// get new ID for this node
		int id = ++node_counter;

		// check whether the necessary relation is present
		auto tuple_rel = this->getRelation("TupleType");
		auto node_list_rel = this->getRelation("NodeList");
		if (!tuple_rel || !node_list_rel) return id;

		// insert a new entry into the tuple relation
		souffle::tuple entry(tuple_rel);
		entry << id;
		tuple_rel->insert(entry);

		// insert element types
		int counter = 0;
		for(const auto& cur : tuple) {
			int child_id = visit(cur);

			souffle::tuple entry(node_list_rel);
			entry << id;
			entry << counter++;
			entry << child_id;

			node_list_rel->insert(entry);
		}

		// return id
		return id;
	}

	int visitGenericType(const core::GenericTypePtr& type) override {
		int id = ++node_counter;
		return id;
	}

	// -- Expression Nodes --

	// -- Statement Nodes --

	// -- Support Nodes --


	int visitNode(const core::NodePtr& cur) override {
		assert_not_implemented() << "Unsupported node type: " << cur->getNodeType() << "\n";
		return 0;
	}

private:

	souffle::RamDomain toIndex(const std::string& str) {
		return this->symTable.lookup(str.c_str());
	}

	std::string toValue(souffle::RamDomain i) const {
		return this->symTable.resolve(i);
	}

};

} // end namespace framework
} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

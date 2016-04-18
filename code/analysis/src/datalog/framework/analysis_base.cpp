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

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include <souffle/SouffleInterface.h>

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace analysis {
namespace datalog {
namespace framework {

	namespace {

		class FactExtractor : public core::IRVisitor<int> {

			int node_counter = 0;
			souffle::Program& analysis;

		public:

			FactExtractor(souffle::Program& analysis)
				: core::IRVisitor<int>(true), analysis(analysis) {}

			int extractFacts(const core::NodePtr& rootNode) {
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

				// insert record into relation
				insert("TypeVariable", id, var->getVarName()->getValue());

				// return id
				return id;
			}

			int visitTupleType(const core::TupleTypePtr& tuple) override {

				// get new ID for this node
				int id = ++node_counter;

				// insert into tuple type relation
				insert("TupleType", id);

				// insert element types
				int counter = 0;
				for(const auto& cur : tuple) {
					insert("NodeList", id, counter++, visit(cur));
				}

				// return id
				return id;
			}

			int visitFunctionType(const core::FunctionTypePtr& fun) override {

				// get new ID for this node
				int id = ++node_counter;

				// insert into function type relation
				insert("FunctionType", id, visit(fun->getParameterTypes()), visit(fun->getReturnType()), fun->getKind(), visit(fun->getInstantiationTypes()));

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

			int visitTypes(const core::TypesPtr& types) override {
				int id = ++node_counter;

				// insert type into relation
				insert("Types", id);

				//insert element types
				int counter = 0;
				for(const auto& cur : types) {
					insert("NodeList", id, counter++, visit(cur));
				}

				// return id
				return id;
			}


			int visitNode(const core::NodePtr& cur) override {
				assert_not_implemented() << "Unsupported node type: " << cur->getNodeType() << "\n";
				return 0;
			}

		private:

			void fill(souffle::tuple&) {}

			template<typename F, typename ... Rest>
			void fill(souffle::tuple& tuple, const F& first, const Rest& ... rest) {
				tuple << first;
				fill(tuple,rest...);
			}

			template<typename ... Args>
			void insert(const std::string& relationName, const Args& ... args ) {
				// get relation
				auto rel = analysis.getRelation(relationName);
				if (!rel) return;

				// insert data
				souffle::tuple tuple(rel);
				fill(tuple, args...);
				rel->insert(tuple);
			}

		};

	}

	int extractFacts(souffle::Program& analysis, const core::NodePtr& node) {
		return FactExtractor(analysis).visit(node);
	}

} // end namespace framework
} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

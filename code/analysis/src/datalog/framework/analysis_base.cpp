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
#include "insieme/utils/logging.h"

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

			int visitUIntValue(const core::UIntValuePtr& val) override {
				int id = ++node_counter;
				insert("UintValue", id, val->getValue());
				return id;
			}

			int visitStringValue(const core::StringValuePtr& val) override {
				int id = ++node_counter;
				insert("StringValue", id, val->getValue());
				return id;
			}

			// -- Type Nodes --

			int visitGenericType(const core::GenericTypePtr& type) override {
				int id = ++node_counter;
				const std::string& name = type->getName()->getValue();
				int parents = visit(type->getParents());
				int params = visit(type->getTypeParameter()); // TODO check this again
				insert("GenericType", id, name, parents, params);
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
				int parameter_types = visit(fun->getParameterTypes());
				int return_type = visit(fun->getReturnType());
				uint kind = fun->getFunctionKind()->getValue();
				int instantiation_types = visit(fun->getInstantiationTypes());
				insert("FunctionType", id, parameter_types, return_type, kind, instantiation_types);

				// return id
				return id;
			}

			int visitTypeVariable(const core::TypeVariablePtr& var) override {

				// get new ID for this node
				int id = ++node_counter;

				// insert record into relation
				insert("TypeVariable", id, var->getVarName()->getValue());

				// return id
				return id;
			}

			int visitNumericType(const core::NumericTypePtr& var) override {
				int id = ++node_counter;
				int node = visit(var->getValue());
				insert("NumericType", id, node);
				return id;
			}

			// -- Expression Nodes --

			int visitLiteral(const core::LiteralPtr& var) override {
				int id = ++node_counter;
				int type = visit(var->getType());
				const std::string& string_value = var->getStringValue();
				insert("Literal", id, type, string_value);
				return id;
			}

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

			int visitParents(const core::ParentsPtr& parents) override {
				int id = ++node_counter;

				// insert parent into relation
				insert("Parents", id);

				//insert list of parents
				int counter = 0;
				for(const auto& cur : parents) {
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

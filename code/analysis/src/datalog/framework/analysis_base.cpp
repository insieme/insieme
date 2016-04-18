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

			//FIXME: Make it build
			using NodeType = std::string;

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

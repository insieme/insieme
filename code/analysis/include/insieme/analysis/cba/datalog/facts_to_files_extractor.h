#pragma once

#include <map>
#include <set>
#include <string>
#include <ostream>

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {


	struct TargetRelationEntry {
		using Key = std::string;
		using Value = std::string;
		using AdditionalInfo = std::pair<Key,Value>;

		TargetRelationEntry(core::NodeAddress node)
		        : id(-1), node(node) {}

		void setID(const int id) const {
			this->id = id;
		}

		int getID() const {
			return id;
		}

		TargetRelationEntry &addInfo(const Key key, const Value value) {
			auto entry = std::make_pair(key, value);
			info.push_back(entry);
			return *this;
		}

		TargetRelationEntry &addInfo(const Key &key, const int &value) {
			return addInfo(key, std::to_string(value));
		}

		const std::vector<AdditionalInfo> &getInfo() const {
			return info;
		}

		const core::NodeAddress &getNode() const {
			return node;
		}

		core::NodePtr getRootAddress() const {
			return node.getRootAddress();
		}

		bool operator ==(const core::NodeAddress &otherNode) const {
			return node == otherNode;
		}

		bool operator <(const TargetRelationEntry &other) const {
			return this->node < other.node;
		}

	private:
		mutable int id;
		const core::NodeAddress node;
		std::vector<AdditionalInfo> info;
	};

	using TargetRelations = std::map<string,std::set<TargetRelationEntry>>;

	/**
	 * Extract the facts (including 'targets') to fact files (to be used with external Soufflé)
	 * @param targets Target nodes from which we want the values (mapped to the fact file to write them in)
	 * @param edCmds some 'ed' commands that will be printed in a special file. Useful for post-processing scripts to alter generated files
	 * @param outputDir Directory where fact files are created
	 * @return true if successful
	 */
	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds = "", const std::string &outputDir = "/tmp/facts", bool debug = false, std::ostream &debugOut = std::cout);

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

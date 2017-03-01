/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/backend/name_manager.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/lang/reference.h"

#include "insieme/utils/unused.h"


namespace insieme {
namespace backend {

	using namespace insieme::core;

	namespace {

		string getAttachedName(const core::NodePtr& ptr) {
			// test whether the node has a name attached
			if(core::annotations::hasAttachedName(ptr)) {
				// => take the attached name
				return core::annotations::getAttachedName(ptr);
			}

			// no name attached
			return "";
		}
	}

	void SimpleNameManager::registerGlobalNames(const core::NodePtr& root) {
		// just collect the names of global literals (which are all references)
		core::visitDepthFirstOnce(root, [&](const core::LiteralPtr& literal) {
			if(core::lang::isReference(literal)) { globalScope.usedNames.insert(literal->getStringValue()); }
		});

		// if the main function is renamed, we need this to avoid
		// the original main function to be named main
		globalScope.usedNames.insert("main");
	}

	string SimpleNameManager::getName(const core::VariablePtr& var) {
		// test whether the name is present within the current
		const string* res = lookup(var);
		if(res) { return *res; }

		// determine name
		string name = getAttachedName(var);

		// use attached name if present
		if(name.empty() || isUsed(name)) {
			// use default name
			string prefix = string("var_") + toString(var->getId());
			name = prefix;

			int c = 1;

			while(isUsed(name)) {
				name = prefix + format("%s_%d", prefix, c++);
			}
		}

		// register name
		varScope.back().names.insert(make_pair(var, name));
		varScope.back().usedNames.insert(name);

		// return name
		return name;
	}

	string SimpleNameManager::getName(const NodePtr& ptr, const string& fragment) {
		// let variables be handled by the specialized variant
		if(ptr->getNodeType() == core::NT_Variable) { return getName(ptr.as<core::VariablePtr>()); }

		// for recursive types => use record definition
		if(auto tagType = ptr.isa<TagTypePtr>()) {
			return getName(tagType->getRecord(), ((fragment == "") ? "type" : fragment));
		}

		// test whether a name has already been picked
		auto it = globalScope.names.find(ptr);
		if(it != globalScope.names.end()) { return it->second; }

		{
			// get attached name
			string name = getAttachedName(ptr);

			// for record types -- check record name
			if (name.empty() || isUsed(name)) {
				if (auto record = ptr.isa<RecordPtr>()) {
					name = record->getName()->getValue();
				}
			}

			// for lambdas -- check reference name
			if (name.empty() || isUsed(name)) {
				if (auto lambda = ptr.isa<LambdaExprPtr>()) {
					name = lambda->getReference()->getNameAsString();
				}
			}

			// cut off everything before the last ::
			auto columnPos = name.rfind("::");
			if (columnPos != std::string::npos) {
				name = name.substr(columnPos+2);
			}

			// use attached name if present
			if(!name.empty() && name != "_" && !isUsed(name)) {
				globalScope.names.insert(make_pair(ptr, name));
				globalScope.usedNames.insert(name);
				return name;
			}
		}

		// generate a new name string
		std::stringstream name;
		name << prefix;
		if(!fragment.empty()) {
			name << fragment;
		} else {
			switch(ptr->getNodeType()) {
			case NT_BindExpr: name << "closure"; break;
			default:
				switch(ptr->getNodeCategory()) {
				case NC_IntTypeParam: name << "param"; break;
				case NC_Support: name << "supp"; break;
				case NC_Type: name << "type"; break;
				case NC_Expression:
					switch(ptr->getNodeType()) {
					case NT_LambdaExpr: name << "fun"; break;
					default: name << "expr"; break;
					};
					break;
				case NC_Statement: name << "stat"; break;
				case NC_Program: name << "prog"; break;
				case NC_Value: name << "value"; break;
				}
			}
		}

		name << "_" << num++;
		string resName = name.str();

		// register string
		globalScope.names.insert(make_pair(ptr, resName));
		globalScope.usedNames.insert(resName);

		// return result
		return resName;
	}

	void SimpleNameManager::setName(const core::NodePtr& ptr, const string& name) {
		// disabled since name collisions might happen with overloaded functions!
		// assert_true((!isUsed(name) || (lookup(ptr) && *lookup(ptr) == name))) << "Cannot bind to name already used!";

		// everything is in the global scope
		auto names = &globalScope.names;
		auto used = &globalScope.usedNames;

		// unless it is a variable
		if(ptr->getNodeType() == core::NT_Variable) {
			names = &varScope.back().names;
			used = &varScope.back().usedNames;
		}

		// insert into data structures
		__insieme_unused auto res = names->insert(make_pair(ptr, name));

		// disabled since name collisions might happen with overloaded member functions!
		// assert_true((res.second || res.first->second == name)) << "Tried to alter name after already being bound!";
		used->insert(name);
	}

	void SimpleNameManager::reserveName(const string& name) {
		globalScope.usedNames.insert(name);
	}

	void SimpleNameManager::pushVarScope(bool isolated) {
		// simply push a new scope on top of the scope stack
		varScope.push_back(Scope(!isolated));
	}

	void SimpleNameManager::popVarScope() {
		// drop the top-level scope
		assert_gt(varScope.size(), 1) << "Scope stack must not be empty!";
		varScope.pop_back();
	}

	bool SimpleNameManager::isUsed(const string& name) const {
		// test whether it is a name used by the global scope
		if(globalScope.usedNames.find(name) != globalScope.usedNames.end()) { return true; }

		// check the nested variable scope
		for(auto it = varScope.rbegin(); it != varScope.rend(); ++it) {
			if(it->usedNames.find(name) != it->usedNames.end()) { return true; }
			if(!it->extendsParentScope) {
				return false; // stop search here
			}
		}

		// not found
		return false;
	}

	const string* SimpleNameManager::lookup(const core::NodePtr& ptr) const {
		// test nested scopes first
		for(auto it = varScope.rbegin(); it != varScope.rend(); ++it) {
			auto pos = it->names.find(ptr);
			if(pos != it->names.end()) { return &(pos->second); }
			if(!it->extendsParentScope) {
				break; // stop search here
			}
		}

		// search within global scope
		auto pos = globalScope.names.find(ptr);
		if(pos != globalScope.names.end()) { return &(pos->second); }

		// not found
		return nullptr;
	}

} // end: namespace simple_backend
} // end: namespace insieme

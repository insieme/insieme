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

#include "insieme/backend/name_manager.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/annotations/c/naming.h"

#include "insieme/utils/unused.h"


namespace insieme {
namespace backend {

	using namespace insieme::core;

	namespace {

		string getAttachedName(const core::NodePtr& ptr) {

			// test whether the node has a name attached
			if (core::annotations::hasNameAttached(ptr)) {
				// => take the attached name
				return core::annotations::getAttachedName(ptr);
			}

			// test whether the node has an annotation
			if(auto cnameAnn = ptr->getAnnotation(insieme::annotations::c::CNameAnnotation::KEY)) {
				// => take original c name
				return cnameAnn->getName();
			}

			// no name attached
			return "";
		}

	}

	void SimpleNameManager::registerGlobalNames(const core::NodePtr& root) {

		// just collect the names of global literals (which are all references)
		core::visitDepthFirstOnce(root, [&](const core::LiteralPtr& literal) {
			if (literal->getType().isa<core::RefTypePtr>()) {
				globalScope.usedNames.insert(literal->getStringValue());
			}
		});

	}

	string SimpleNameManager::getName(const core::VariablePtr& var) {

		// test whether the name is present within the current
		const string* res = lookup(var);
		if (res) return *res;

		// determine name
		string name = getAttachedName(var);

		// use attached name if present
		if (name.empty() || isUsed(name)) {
			// use default name
			name = string("var_") + toString(var->getId());
		}

		// register name
		varScope.back().names.insert(make_pair(var, name));
		varScope.back().usedNames.insert(name);

		// return name
		return name;
	}



	string SimpleNameManager::getName( const NodePtr& ptr, const string& fragment) {

		// let variables be handled by the specialized variant
		if (ptr->getNodeType() == core::NT_Variable) {
			return getName(ptr.as<core::VariablePtr>());
		}


		// test whether a name has already been picked
		auto it = globalScope.names.find(ptr);
		if(it != globalScope.names.end()) return it->second;

		{
			string name = getAttachedName(ptr);

			// use attached name if present
			if (!name.empty() && !isUsed(name)) {
				globalScope.names.insert(make_pair(ptr, name));
				globalScope.usedNames.insert(name);
				return name;
			}
		}


		// generate a new name string
		std::stringstream name;
		name << prefix;
		if (!fragment.empty()) {
			name << fragment;
		} else {
			switch(ptr->getNodeType()) {
			case NT_BindExpr:
				name << "closure"; break;
			default:
				switch(ptr->getNodeCategory()) {
				case NC_IntTypeParam:
					name << "param"; break;
				case NC_Support:
					name << "supp"; break;
				case NC_Type:
					name << "type"; break;
				case NC_Expression:
					switch(ptr->getNodeType()) {
					case NT_LambdaExpr: name << "fun"; break;
					default: name << "expr"; break;
					} ; break;
				case NC_Statement:
					name << "stat"; break;
				case NC_Program:
					name << "prog"; break;
				case NC_Value:
					name << "value"; break;
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
		//assert((!isUsed(name) || (lookup(ptr) && *lookup(ptr) == name)) && "Cannot bind to name already used!");

		// everything is in the global scope
		auto names = &globalScope.names;
		auto used = &globalScope.usedNames;

		// unless it is a variable
		if (ptr->getNodeType() == core::NT_Variable) {
			names = &varScope.back().names;
			used = &varScope.back().usedNames;
		}

		// insert into data structures
		__unused auto res = names->insert(make_pair(ptr, name));

		// disabled since name collisions might happen with overloaded member functions!
		//assert((res.second || res.first->second == name) && "Tried to alter name after already being bound!");
		used->insert(name);
	}


	void SimpleNameManager::pushVarScope(bool isolated) {

		// simply push a new scope on top of the scope stack
		varScope.push_back(Scope(!isolated));

	}

	void SimpleNameManager::popVarScope() {
		// drop the top-level scope
		assert(varScope.size() > 1 && "Scope stack must not be empty!");
		varScope.pop_back();
	}

	bool SimpleNameManager::isUsed(const string& name) const {

		// test whether it is a name used by the global scope
		if (globalScope.usedNames.find(name) != globalScope.usedNames.end()) {
			return true;
		}

		// check the nested variable scope
		for(auto it=varScope.rbegin(); it != varScope.rend(); ++it) {
			if (it->usedNames.find(name) != it->usedNames.end()) {
				return true;
			}
			if (!it->extendsParentScope) {
				return false;		// stop search here
			}
		}

		// not found
		return false;
	}

	const string* SimpleNameManager::lookup(const core::NodePtr& ptr) const {

		// test nested scopes first
		for(auto it=varScope.rbegin(); it != varScope.rend(); ++it) {
			auto pos = it->names.find(ptr);
			if (pos != it->names.end()) {
				return &(pos->second);
			}
			if (!it->extendsParentScope) {
				break;		// stop search here
			}
		}

		// search within global scope
		auto pos = globalScope.names.find(ptr);
		if (pos != globalScope.names.end()) {
			return &(pos->second);
		}

		// not found
		return NULL;
	}

} // end: namespace simple_backend
} // end: namespace insieme

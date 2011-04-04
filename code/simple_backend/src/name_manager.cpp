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

#include "insieme/simple_backend/type_manager.h"

#include "insieme/core/types.h"
#include "insieme/core/expressions.h"
#include "insieme/core/statements.h"

#include "insieme/c_info/naming.h"


namespace insieme {
namespace simple_backend {

using namespace insieme::core;





string NameManager::getName( const NodePtr& ptr, const string& fragment) {

	// test whether a name has already been picked
	auto it = nameMap.find(ptr);
	if(it != nameMap.end()) return it->second;

	// test whether the node has an annotation
	if(auto cnameAnn = ptr->getAnnotation(c_info::CNameAnnotation::KEY)) {
		// => take original c name
		string name = cnameAnn->getName();
		nameMap.insert(make_pair(ptr, name));
		return name;
	}

	// special handling for variables
	if (ptr->getNodeType() == NT_Variable) {
		return string("var_") + toString(static_pointer_cast<const Variable>(ptr)->getId());
	}

	// generate a new name string
	std::stringstream name;
	name << prefix;
	if (!fragment.empty()) {
		name << fragment << "_";
	}

	switch(ptr->getNodeCategory()) {
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
	}
	name << "_" << num++;
	nameMap.insert(make_pair(ptr, name.str()));
	return getName(ptr, fragment);
}

void NameManager::setName(const core::NodePtr& ptr, const string& name) {
	auto res = nameMap.insert(make_pair(ptr, name));
	assert(res.second && "Tried to alter name after first assignment!");
}


} // end: namespace simple_backend
} // end: namespace insieme

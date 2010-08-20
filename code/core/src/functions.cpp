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

#include <algorithm>
#include <iterator>

#include <boost/functional/hash.hpp>

#include "types.h"
#include "container_utils.h"
#include "functions.h"


// ---------------------------------------------- Function ------------------------------------


FunctionTypePtr getType(TypeManager& manager, const Function::ParameterList& paramList, const TypePtr& returnType) {

	// compose argument type as tuple
	TupleType::ElementTypeList elementTypes;
	projectToSecond(paramList, elementTypes);

	// obtain argument type
	TupleTypePtr argumentType = TupleType::get(manager, elementTypes);

	// construct final result
	return FunctionType::get(manager, argumentType, manager.get(returnType));
}

std::size_t hash(const Identifier& name, const FunctionTypePtr& type) {
	std::size_t seed = 0;
	boost::hash_combine(seed, name.hash());
	boost::hash_combine(seed, type->hash());
	return seed;
}

Function::Function(const Identifier& name, const FunctionTypePtr& type,
		const vector<Identifier>& parameters, const StmtPtr& body, bool external)
	:
		name(name), type(type), parameters(parameters), external(external),
		body(body), hashCode(::hash(name, type)) { }

Function::Function(FunctionManager& manager, const Function& function)
	:
		name(function.name),
		type(manager.getTypeManager().getTypePtr(*function.type)),
		parameters(function.parameters),
		external(external),
		body(manager.getStatementManager().get(function.body)),
		hashCode(function.hashCode) { }

FunctionPtr Function::get(FunctionManager& manager, const Identifier& name, const ParameterList& paramList,
		const TypePtr& returnType, const StmtPtr& body, bool external) {

	// obtain the type from the parameters
	FunctionTypePtr type = ::getType(manager.getTypeManager(), paramList, returnType);
	auto parameters = projectToFirst(paramList);

	return manager.get(Function(name, type, parameters, body, external));
}



// ---------------------------------------------- Utilities ------------------------------------

/**
 * Integrates the hash code computation into the boost hash code framework.
 *
 * @param function the function for which a hash code should be computed.
 * @return the hash code of the given function
 */
std::size_t hash_value(const Function& function) {
	return function.hash();
}

/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Function& type) {
	out << "fun " << type.getName() << " : " << *type.getType();
	return out;
}


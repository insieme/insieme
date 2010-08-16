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
#include <cassert>

#include "container_utils.h"
#include "types.h"



// ------------------------------------- Type Manager ---------------------------------

TypePtr TypeManager::getTypePtrInternal(const Type& type) {

	// get master copy
	std::pair<TypePtr, bool> res = add(type);

	// check whether all child-nodes are within this manager
	assert( !res.second || containsAll(*(res.first->getChildren())));

	// return newly added or present node
	return res.first;
}

/**
 * Obtains a list of pointer referencing the same elements as the given list
 * of type pointers, however, those the referenced once will be managed by the
 * this manager.
 *
 * @param pointer the list of type references to be covered
 * @return  the same list of types referencing elements within the local manager
 */
vector<TypePtr> TypeManager::getTypePtr(const vector<TypePtr>& pointer) {

	// just create resulting list ...
	vector<TypePtr> res;
	res.reserve(pointer.size());

	// ... and look up all elements of the input list
	transform(pointer.cbegin(), pointer.cend(), back_inserter(res),
			[&](const TypePtr& cur) {
				return this->getTypePtr(*cur);
	});

	return res;
}


/**
 * Obtains a type pointer pointing to an identical element as the given pointer,
 * however, the referenced element will be maintained by this manager.
 *
 * @param pointer for which a local reference should be obtained
 * @return a pointer pointing to a local copy of the given pointer
 */
TypePtr TypeManager::getTypePtr(const TypePtr& pointer) {
	if (pointer.isNull()) {
		return pointer;
	}
	return this->getTypePtr(*pointer);
}


// -------------------------------- Integer Type Parameter ----------------------------

bool IntTypeParam::operator==(const IntTypeParam& param) {
	// quick check on reference
	if (this == &param) {
		return true;
	}

	// different type => different
	if (type != param.type) {
		return false;
	}

	// check type dependent content
	switch (type) {
	case VARIABLE:
		return symbol == param.symbol;
	case CONCRETE:
		return value == param.value;
	case INFINITE:
		return true;
	}
	return false;
}

/**
 * Tests whether all of the given integer type parameter are concrete.
 *
 * @param intTypeParams the list of parameters to be tested
 * @return true if all are concrete, false otherwise
 */
bool IntTypeParam::allConcrete(const vector<IntTypeParam>& intTypeParams) {
	// just use all-functionality of container utils
	return all(intTypeParams, [](const IntTypeParam& param) { return param.isConcrete(); });
}


// ---------------------------------------- Type --------------------------------

/**
 * Tests whether all of the type pointer within the given vector are concrete types.
 *
 * @param elementTypes the types to be tested
 * @return true if all are concrete types, false otherwise.
 */
bool Type::allConcrete(const vector<TypePtr>& elementTypes) {
	// just use all-functionality of container utils
	return all(elementTypes, [](const TypePtr& param) { return param->isConcrete(); });
}

// ---------------------------------------- Tuple Type --------------------------------

/**
 * A private utility method building the name of a tuple type.
 *
 * @param elementTypes	the list of element types
 * @return a string representation of the resulting tuple type
 */
string TupleType::buildNameString(const vector<TypePtr>& elementTypes) {

	// create output buffer
	std::stringstream res;
	res << "(";

	vector<string> list;
	transform(elementTypes.cbegin(), elementTypes.cend(), back_inserter(list),
			[](const TypePtr& cur) {
				return cur->getName();
	});

	res << boost::join(list, ",");

	res << ")";
	return res.str();
}

/**
 * This method provides a static factory method for this type of node. It will return
 * a tuple type pointer pointing toward a variable with the given name maintained by the
 * given manager.
 *
 * @param manager the manager to obtain the new type reference from
 * @param elementTypes the list of element types to be used to form the tuple
 */
TupleTypePtr TupleType::get(TypeManager& manager, const vector<TypePtr>& elementTypes) {
	return manager.getTypePtr(TupleType(manager.getTypePtr(elementTypes)));
}

// ---------------------------------------- Function Type ------------------------------


/**
 * This method provides a static factory method for this type of node. It will return
 * a function type pointer pointing toward a variable with the given name maintained by the
 * given manager.
 *
 * @param manager the manager to be used for handling the obtained type pointer
 * @param argumentType the argument type of the type to be obtained
 * @param returnType the type of value to be returned by the obtained function type
 * @return a pointer to a instance of the required type maintained by the given manager
 */
FunctionTypePtr FunctionType::get(TypeManager& manager, const TypePtr& argumentType, const TypePtr& returnType) {

	// obtain local references for referenced types
	TypePtr localArgumentType = manager.getTypePtr(argumentType);
	TypePtr localReturnType = manager.getTypePtr(returnType);

	// obtain reference to new element
	return manager.getTypePtr(FunctionType(localArgumentType, localReturnType));
}

// ---------------------------------------- Generic Type ------------------------------

/**
 * A private utility method building the name of a generic type.
 *
 * @param name			the name of the generic type (only prefix, generic parameters are added automatically)
 * @param typeParams 	the list of type parameters to be appended
 * @param intParams		the list of integer type parameters to be appended
 * @return a string representation of the type
 */
string GenericType::buildNameString(const string& name, const vector<TypePtr>& typeParams,
		const vector<IntTypeParam>& intParams) {

	// create output buffer
	std::stringstream res;
	res << name;

	// check whether there are type parameters
	if (!typeParams.empty() || !intParams.empty()) {

		// convert type parameters to strings ...
		vector<string> list;
		std::transform(typeParams.cbegin(), typeParams.cend(), back_inserter(list), [](const TypePtr& cur) {return cur->getName();});
		std::transform(intParams.cbegin(), intParams.cend(), back_inserter(list), [](const IntTypeParam& cur) {return cur.toString();});

		res << "<" << boost::join(list, ",") << ">";
	}
	return res.str();
}

/**
 * This method provides a static factory method for this type of node. It will return
 * a generic type pointer pointing toward a variable with the given name maintained by the
 * given manager.
 *
 * @param name 			the name of the new type (only the prefix)
 * @param typeParams	the type parameters of this type, concrete or variable
 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
 * @param baseType		the base type of this generic type
 */
GenericTypePtr GenericType::get(TypeManager& manager,
			const string& name,
			const vector<TypePtr>& typeParams,
			const vector<IntTypeParam>& intTypeParams,
			const TypePtr& baseType) {

	// get all type-parameter references from the manager
	vector<TypePtr> localTypeParams = manager.getTypePtr(typeParams);

	// get base type reference (if necessary) ...
	TypePtr localBaseType = manager.getTypePtr(baseType);

	// create resulting data element
	return manager.getTypePtr(GenericType(name, localTypeParams, intTypeParams, localBaseType));
}

// ---------------------------------------------- Utility Functions ------------------------------------

/**
 * Allows to compute the hash value of a type.
 *
 * @param type the type for which a hash value should be computed
 * @return the computed hash value
 */
std::size_t hash_value(const Type& type) {
	return type.hash();
}

/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Type& type) {
	out << type.toString();
	return out;
}

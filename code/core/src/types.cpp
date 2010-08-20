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
#include <stdexcept>

#include <boost/algorithm/string/join.hpp>
#include <boost/iterator/transform_iterator.hpp>

#include "container_utils.h"
#include "types.h"




// -------------------------------- Integer Type Parameter ----------------------------

bool IntTypeParam::operator==(const IntTypeParam& param) const {
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

// -------------------------------------- Type Variable -------------------------------



// ---------------------------------------- Tuple Type --------------------------------

/**
 * A private utility method building the name of a tuple type.
 *
 * @param elementTypes	the list of element types
 * @return a string representation of the resulting tuple type
 */
string TupleType::buildNameString(const ElementTypeList& elementTypes) {

	// create output buffer
	std::stringstream res;
	res << "(";

	vector<string> list;
	std::transform(elementTypes.cbegin(), elementTypes.cend(), back_inserter(list),
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
TupleTypePtr TupleType::get(TypeManager& manager, const ElementTypeList& elementTypes) {
	return manager.getTypePtr(TupleType(elementTypes));
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
	// obtain reference to new element
	return manager.getTypePtr(FunctionType(argumentType, returnType));
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
	vector<TypePtr> localTypeParams = manager.getAll(typeParams);

	// get base type reference (if necessary) ...
	TypePtr localBaseType = manager.get(baseType);

	// create resulting data element
	return manager.getTypePtr(GenericType(name, localTypeParams, intTypeParams, localBaseType));
}

/**
 * Retrieves the child types referenced by this generic type.
 *
 * @return the
 */
GenericType::ChildList GenericType::getChildren() const {
	auto res = makeChildList(typeParams);

	// further add base types
	if (!!baseType) {
		res->push_back(baseType);
	}

	return res;
}


// ------------------------------------ Named Composite Type ---------------------------

/**
 * Creates a new named composite type having the given name and set of entries.
 *
 * @param prefix the prefix to be used for the new type (union or struct)
 * @param entries the entries of the new type
 *
 * @throws std::invalid_argument if the same identifier is used more than once within the type
 */
NamedCompositeType::NamedCompositeType(const string& prefix, const Entries& entries) :
	Type(buildNameString(prefix, entries), allConcrete(entries)), entries(entries) {

	// get projection to first element
	auto start = boost::make_transform_iterator(entries.cbegin(), extractFirst<Entry>());
	auto end = boost::make_transform_iterator(entries.cend(), extractFirst<Entry>());

	if (hasDuplicates(start, end)) { // nice way using projections => but crashes in GCC
		throw std::invalid_argument("No duplicates within identifiers are allowed!");
	}
}

/**
 * Obtains a copy of the given entry list referencing identical elements within the
 * given manager.
 *
 * @param manager the manager to which the resulting references should point to
 * @param entries the list of entries to be looked up
 * @return a list of entries referencing identical types within the given manager
 */
NamedCompositeType::Entries NamedCompositeType::getEntriesFromManager(TypeManager& manager, Entries entries) {

	// quick check ..
	if (entries.empty()) {
		return entries;
	}

	// obtain elements by looking up one after another ...
	Entries res;
	std::transform(entries.cbegin(), entries.cend(), back_inserter(res),
		[&manager](const Entry& cur) {
			return NamedCompositeType::Entry(cur.first, manager.get(cur.second));
	});
	return res;
}

/**
 * A static utility function composing the name of this type.
 *
 * @param prefix the prefix of the type name (union or struct)
 * @param entries the list of entries forming the content of this type
 */
string NamedCompositeType::buildNameString(const string& prefix, const Entries& elements) {
	// create output buffer
	std::stringstream res;
	res << prefix << "<";

	// check whether there are type parameters
	if (!elements.empty()) {

		// convert type parameters to strings ...
		vector<string> list;
		std::transform(elements.cbegin(), elements.cend(), back_inserter(list),
			[](const Entry& cur) {
				return format("%s:%s", cur.first.getName().c_str(), cur.second->toString().c_str());
		});

		res << boost::join(list, ",");
	}
	res << ">";
	return res.str();
}

/**
 * Checks whether all types within the given list of entries are concrete types. If so, true
 * is returned, false otherwise.
 *
 * @param elements the list of elements which's types should be checked
 * @return true if all are concrete, false otherwise
 */
bool NamedCompositeType::allConcrete(const Entries& elements) {
	return all(elements,
		[](const Entry& cur) {
			return cur.second->isConcrete();
	});
}


// ------------------------------------ Struct Type ---------------------------


StructTypePtr StructType::get(TypeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.getTypePtr(StructType(NamedCompositeType::getEntriesFromManager(manager, entries)));
}

// ------------------------------------ Union Type ---------------------------


UnionTypePtr UnionType::get(TypeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.getTypePtr(UnionType(NamedCompositeType::getEntriesFromManager(manager, entries)));
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

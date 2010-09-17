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
#include "map_utils.h"
#include "types.h"


using namespace insieme::core;

enum HashSeed {
	RECURSIVE_DEFINITION
};


// -------------------------------- Integer Type Parameter ----------------------------

const IntTypeParam IntTypeParam::ZERO = IntTypeParam::getConcreteIntParam(0);
const IntTypeParam IntTypeParam::ONE  = IntTypeParam::getConcreteIntParam(1);
const IntTypeParam IntTypeParam::INF  = IntTypeParam(INFINITE);

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

IntTypeParam IntTypeParam::getVariableIntParam(char symbol) {
	return IntTypeParam(symbol);
}

IntTypeParam IntTypeParam::getConcreteIntParam(std::size_t value) {
	return IntTypeParam(value);
}

IntTypeParam IntTypeParam::getInfiniteIntParam() {
	return IntTypeParam(INFINITE);
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

TypeVariablePtr TypeVariable::get(NodeManager& manager, const string& name) {
	return manager.get(TypeVariable(name));
}

TypeVariable* TypeVariable::createCloneUsing(NodeManager&) const {
	return new TypeVariable(*this);
}

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
 * Creates a new, empty tuple type.
 */
TupleType::TupleType() : Type(buildNameString(toVector<TypePtr>()), allConcrete(toVector<TypePtr>())), elementTypes(toVector<TypePtr>()) {}

/**
 * Creates a new tuple type based on the given element type(s).
 */
TupleType::TupleType(const TypePtr& a)
	: Type(buildNameString(toVector<TypePtr>(a)), allConcrete(toVector<TypePtr>(a))), elementTypes(toVector<TypePtr>(a)) {}

/**
 * Creates a new tuple type based on the given element type(s).
 */
TupleType::TupleType(const TypePtr& a, const TypePtr& b)
	: Type(buildNameString(toVector<TypePtr>(a,b)), allConcrete(toVector<TypePtr>(a,b))), elementTypes(toVector<TypePtr>(a,b)) {}

TupleType* TupleType::createCloneUsing(NodeManager& manager) const {
	return new TupleType(migrateAllPtr(elementTypes, manager));
}

/**
 * This method provides a static factory method for this type of node. It will return
 * a tuple type pointer pointing toward a variable with the given name maintained by the
 * given manager.
 *
 * @param manager the manager to obtain the new type reference from
 * @param elementTypes the list of element types to be used to form the tuple
 */
TupleTypePtr TupleType::get(NodeManager& manager, const ElementTypeList& elementTypes) {
	return manager.get(TupleType(elementTypes));
}

Node::OptionChildList TupleType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(elementTypes.cbegin(), elementTypes.cend(), back_inserter(*res));
	return res;
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
FunctionTypePtr FunctionType::get(NodeManager& manager, const TypePtr& argumentType, const TypePtr& returnType) {
	// obtain reference to new element
	return manager.get(FunctionType(argumentType, returnType));
}

FunctionType* FunctionType::createCloneUsing(NodeManager& manager) const {
	return new FunctionType(
			migratePtr(argumentType, manager),
			migratePtr(returnType, manager)
	);
}

Node::OptionChildList FunctionType::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(argumentType);
	res->push_back(returnType);
	return res;
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
string buildNameString(const Identifier& name, const vector<TypePtr>& typeParams,
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
 * Creates an new generic type instance based on the given parameters.
 */
GenericType::GenericType(const Identifier& name,
		const vector<TypePtr>& typeParams,
		const vector<IntTypeParam>& intTypeParams,
		const TypePtr& baseType)
	:
		Type(::buildNameString(name, typeParams, intTypeParams), Type::allConcrete(typeParams) && IntTypeParam::allConcrete(intTypeParams)),
		familyName(name),
		typeParams(typeParams),
		intParams(intTypeParams),
		baseType(baseType) { }

GenericType* GenericType::createCloneUsing(NodeManager& manager) const {
	return new GenericType(familyName,
			migrateAllPtr(typeParams, manager),
			intParams,
			migratePtr(baseType, manager)
	);
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
GenericTypePtr GenericType::get(NodeManager& manager,
			const Identifier& name,
			const vector<TypePtr>& typeParams,
			const vector<IntTypeParam>& intTypeParams,
			const TypePtr& baseType) {

	// get all type-parameter references from the manager
	vector<TypePtr> localTypeParams = manager.getAll(typeParams);

	// get base type reference (if necessary) ...
	TypePtr localBaseType = manager.get(baseType);

	// create resulting data element
	return manager.get(GenericType(name, localTypeParams, intTypeParams, localBaseType));
}

/**
 * Retrieves the child types referenced by this generic type.
 *
 * @return the
 */
Node::OptionChildList GenericType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(typeParams.cbegin(), typeParams.cend(), back_inserter(*res));
	if (baseType) {
		res->push_back(baseType);
	}
	return res;
}

// ------------------------------------ Rec Definition ------------------------------

std::size_t hashRecTypeDefinition(const RecTypeDefinition::RecTypeDefs& definitions) {
	std::size_t hash = RECURSIVE_DEFINITION;
	boost::hash_combine(hash, insieme::utils::map::computeHash(definitions));
	return hash;
}

RecTypeDefinition::RecTypeDefinition(const RecTypeDefinition::RecTypeDefs& definitions)
	: Node(SUPPORT, hashRecTypeDefinition(definitions)), definitions(definitions) { };

RecTypeDefinitionPtr RecTypeDefinition::get(NodeManager& manager, const RecTypeDefinition::RecTypeDefs& definitions) {
	return manager.get(RecTypeDefinition(definitions));
}

RecTypeDefinition* RecTypeDefinition::createCloneUsing(NodeManager& manager) const {
	RecTypeDefs localDefinitions;
	std::transform(definitions.begin(), definitions.end(), inserter(localDefinitions, localDefinitions.end()),
		[&manager,this](const RecTypeDefs::value_type& cur) {
			return RecTypeDefinition::RecTypeDefs::value_type(
					this->migratePtr(cur.first, manager),
					this->migratePtr(cur.second, manager));
	});
	return new RecTypeDefinition(localDefinitions);
}

bool RecTypeDefinition::equals(const Node& other) const {
	// check type
	if (typeid(other) != typeid(RecTypeDefinition)) {
		return false;
	}

	const RecTypeDefinition& rhs = static_cast<const RecTypeDefinition&>(other);
	return insieme::utils::map::equal(definitions, rhs.definitions, equal_target<TypePtr>());
}

Node::OptionChildList RecTypeDefinition::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::for_each(definitions.begin(), definitions.end(), [&res](const RecTypeDefs::value_type& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	return res;
}

std::ostream& RecTypeDefinition::printTo(std::ostream& out) const {
	return out << "{" << join(", ", definitions, [](std::ostream& out, const RecTypeDefs::value_type& cur) {
		out << *cur.first << "=" << *cur.second;
	}) << "}";
}

const TypePtr RecTypeDefinition::getDefinitionOf(const TypeVariablePtr& variable) const {
	auto it = definitions.find(variable);
	if (it == definitions.end()) {
		return TypePtr(NULL);
	}
	return (*it).second;
}


// ---------------------------------------- Rec Type ------------------------------


string buildRecTypeName(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definition) {
	// create output buffer
	std::stringstream res;
	res << "rec " << *variable << "." << *definition;
	return res.str();
}

bool isConcreteRecType(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definition) {
	// TODO: search for unbound type variables - if there are, return false - true otherwise
	return true;
}

bool isRecFunctionType(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definitions) {
	TypePtr definition = definitions->getDefinitionOf(variable);
	assert( definition && "No definition for given variable within definitions!");
	return definition->isFunctionType();
}


// TODO: determine whether recursive type is concrete or function type ..
RecType::RecType(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definition)
	: Type(buildRecTypeName(variable, definition),
			isConcreteRecType(variable, definition),
			isRecFunctionType(variable, definition)),
			typeVariable(variable), definition(definition) { }


RecType* RecType::createCloneUsing(NodeManager& manager) const {
	return new RecType(
			migratePtr(typeVariable, manager),
			migratePtr(definition, manager));
}

Node::OptionChildList RecType::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(typeVariable);
	res->push_back(definition);
	return res;
}

RecTypePtr RecType::get(NodeManager& manager, const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition) {
	return manager.get(RecType(typeVariable, definition));
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
 * Migrates
 *
 * @param manager the manager to which the resulting references should point to
 * @param entries the list of entries to be looked up
 * @return a list of entries referencing identical types within the given manager
 */
NamedCompositeType::Entries migrateToManager(const NodeManager* src, NodeManager* target, NamedCompositeType::Entries entries) {

	// if there is no migration necessary or possible ..
	if (src == target || !target) {
		// .. return input
		return entries;
	}

	// quick check ..
	if (entries.empty()) {
		return entries;
	}

	// obtain elements by looking up one after another ...
	NamedCompositeType::Entries res;
	std::transform(entries.cbegin(), entries.cend(), back_inserter(res),
		[&src, &target](const NamedCompositeType::Entry& cur) {
			return NamedCompositeType::Entry(cur.first, migratePtr(cur.second, src, target));
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

Node::OptionChildList NamedCompositeType::getChildNodes() const {
	OptionChildList res(new ChildList());
	projectToSecond(entries.cbegin(), entries.cend(), back_inserter(*res));
	return res;
}


// ------------------------------------ Struct Type ---------------------------


StructTypePtr StructType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(StructType(::migrateToManager(NULL, &manager, entries)));
}

StructType* StructType::createCloneUsing(NodeManager& manager) const {
	return new StructType(::migrateToManager(getNodeManager(), &manager, getEntries()));
}

// ------------------------------------ Union Type ---------------------------

UnionTypePtr UnionType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(UnionType(::migrateToManager(NULL, &manager, entries)));
}

UnionType* UnionType::createCloneUsing(NodeManager& manager) const {
	return new UnionType(::migrateToManager(getNodeManager(), &manager, getEntries()));
}

// ------------------------------------ Array Type ---------------------------

ArrayType::ArrayType(const TypePtr& elementType, const IntTypeParam& dim) :
	SingleElementType("array", elementType, toVector(dim)) {}

ArrayType* ArrayType::createCloneUsing(NodeManager& manager) const {
	return new ArrayType(migratePtr(getElementType(), manager), getDimension());
}

ArrayTypePtr ArrayType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& dim) {
	return manager.get(ArrayType(elementType, dim));
}

const IntTypeParam ArrayType::getDimension() const {
	return getIntTypeParameter()[0];
}


// ------------------------------------ Vector Type ---------------------------

VectorType::VectorType(const TypePtr& elementType, const IntTypeParam& size) :
	SingleElementType("vector", elementType, toVector(size)) {}

VectorType* VectorType::createCloneUsing(NodeManager& manager) const {
	return new VectorType(migratePtr(getElementType(), manager), getIntTypeParameter()[0]);
}

VectorTypePtr VectorType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size) {
	return manager.get(VectorType(elementType, size));
}

const IntTypeParam VectorType::getSize() const {
	return getIntTypeParameter()[0];
}

// ------------------------------------ Ref Type ---------------------------

RefTypePtr RefType::get(NodeManager& manager, const TypePtr& elementType) {
	return manager.get(RefType(elementType));
}

RefType* RefType::createCloneUsing(NodeManager& manager) const {
	return new RefType(migratePtr(getElementType(), manager));
}


// ------------------------------------ Channel Type ---------------------------

ChannelType::ChannelType(const TypePtr& elementType, const IntTypeParam& size) :
	SingleElementType("channel", elementType, toVector(size)) {}

ChannelType* ChannelType::createCloneUsing(NodeManager& manager) const {
	return new ChannelType(migratePtr(getElementType(), manager), getSize());
}

ChannelTypePtr ChannelType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size) {
	return manager.get(ChannelType(elementType, size));
}

const IntTypeParam ChannelType::getSize() const {
	return getIntTypeParameter()[0];
}


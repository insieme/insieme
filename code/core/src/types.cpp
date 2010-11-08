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

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/types.h"

using namespace insieme::core;

enum HashSeed {
	RECURSIVE_DEFINITION
};


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

TypeVariablePtr TypeVariable::getFromId(NodeManager& manager, const Identifier& id) {
	return manager.get(TypeVariable(id.getName()));
}

TypeVariable* TypeVariable::createCopyUsing(NodeMapping&) const {
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
TupleType::TupleType() : Type(NT_TupleType, buildNameString(toVector<TypePtr>()), allConcrete(toVector<TypePtr>())), elementTypes(toVector<TypePtr>()) {}

/**
 * Creates a new tuple type based on the given element type(s).
 */
TupleType::TupleType(const TypePtr& a)
	: Type(NT_TupleType, buildNameString(toVector<TypePtr>(a)), allConcrete(toVector<TypePtr>(a))), elementTypes(toVector<TypePtr>(isolate(a))) {}

/**
 * Creates a new tuple type based on the given element type(s).
 */
TupleType::TupleType(const TypePtr& a, const TypePtr& b)
	: Type(NT_TupleType, buildNameString(toVector<TypePtr>(a,b)), allConcrete(toVector<TypePtr>(a,b))), elementTypes(toVector<TypePtr>(isolate(a),isolate(b))) {}

TupleType::TupleType(const ElementTypeList& elementTypes) :
	Type(NT_TupleType, buildNameString(elementTypes), allConcrete(elementTypes)), elementTypes(isolate(elementTypes)) {}


TupleType* TupleType::createCopyUsing(NodeMapping& mapper) const {
	return new TupleType(mapper.map(0, elementTypes));
}

TupleTypePtr TupleType::getEmpty(NodeManager& manager) {
	return get(manager, toVector<TypePtr>());
}

TupleTypePtr TupleType::get(NodeManager& manager, const ElementTypeList& elementTypes) {
	return manager.get(TupleType(elementTypes));
}

Node::OptionChildList TupleType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(elementTypes.cbegin(), elementTypes.cend(), back_inserter(*res));
	return res;
}

// ---------------------------------------- Function Type ------------------------------


FunctionType::FunctionType(const TupleTypePtr& argumentType, const TypePtr& returnType) :
	Type(NT_FunctionType, format("(%s->%s)", argumentType->getName().c_str(), returnType->getName().c_str()), true, true),
	argumentType(isolate(argumentType)), returnType(isolate(returnType)) {
}

FunctionTypePtr FunctionType::get(NodeManager& manager, const TupleTypePtr& argumentType, const TypePtr& returnType) {
	// obtain reference to new element
	return manager.get(FunctionType(argumentType, returnType));
}

FunctionType* FunctionType::createCopyUsing(NodeMapping& mapper) const {
	return new FunctionType(mapper.map(0, argumentType),mapper.map(1, returnType));
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
		Type(NT_GenericType, ::buildNameString(name, typeParams, intTypeParams), Type::allConcrete(typeParams) && IntTypeParam::allConcrete(intTypeParams)),
		familyName(name),
		typeParams(isolate(typeParams)),
		intParams(intTypeParams),
		baseType(isolate(baseType)) { }


GenericType::GenericType(NodeType nodeType, const Identifier& name,
		const vector<TypePtr>& typeParams,
		const vector<IntTypeParam>& intTypeParams,
		const TypePtr& baseType)
	:
		Type(nodeType, ::buildNameString(name, typeParams, intTypeParams), Type::allConcrete(typeParams) && IntTypeParam::allConcrete(intTypeParams)),
		familyName(name),
		typeParams(isolate(typeParams)),
		intParams(intTypeParams),
		baseType(isolate(baseType)) { }

GenericType* GenericType::createCopyUsing(NodeMapping& mapper) const {
	return new GenericType(familyName,
			mapper.map(0, typeParams),
			mapper.mapParam(intParams),
			mapper.map(0+typeParams.size(), baseType)
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

	// create resulting data element
	return manager.get(GenericType(name, typeParams, intTypeParams, baseType));
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

namespace { // some anonymous functions for the Rec Definition

	std::size_t hashRecTypeDefinition(const RecTypeDefinition::RecTypeDefs& definitions) {
		std::size_t hash = RECURSIVE_DEFINITION;
		boost::hash_combine(hash, insieme::utils::map::computeHash(definitions));
		return hash;
	}


	const RecTypeDefinition::RecTypeDefs& isolateRecDefinitons(const RecTypeDefinition::RecTypeDefs& definitions) {
		for_each(definitions, [](const RecTypeDefinition::RecTypeDefs::value_type& cur) {
			isolate(cur.first);
			isolate(cur.second);
		});
		return definitions;
	}

	RecTypeDefinition::RecTypeDefs copyRecDefinitonsUsing(NodeMapping& mapper, unsigned offset, const RecTypeDefinition::RecTypeDefs& definitions) {
		RecTypeDefinition::RecTypeDefs localDefinitions;
		std::transform(definitions.begin(), definitions.end(), inserter(localDefinitions, localDefinitions.end()),
			[&mapper, &offset](const RecTypeDefinition::RecTypeDefs::value_type& cur)->RecTypeDefinition::RecTypeDefs::value_type {
				auto res = RecTypeDefinition::RecTypeDefs::value_type(
						mapper.map(offset, cur.first),
						mapper.map(offset+1, cur.second));
				offset += 2;
				return res;
		});
		return localDefinitions;
	}

}

RecTypeDefinition::RecTypeDefinition(const RecTypeDefinition::RecTypeDefs& definitions)
	: Node(NT_RecTypeDefinition, NC_Support, hashRecTypeDefinition(definitions)), definitions(isolateRecDefinitons(definitions)) { };


RecTypeDefinitionPtr RecTypeDefinition::get(NodeManager& manager, const RecTypeDefinition::RecTypeDefs& definitions) {
	return manager.get(RecTypeDefinition(definitions));
}

RecTypeDefinition* RecTypeDefinition::createCopyUsing(NodeMapping& mapper) const {
	return new RecTypeDefinition(copyRecDefinitonsUsing(mapper, 0, definitions));
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
	: Type(NT_RecType, buildRecTypeName(variable, definition),
			isConcreteRecType(variable, definition),
			isRecFunctionType(variable, definition)),
			typeVariable(isolate(variable)), definition(isolate(definition)) { }


RecType* RecType::createCopyUsing(NodeMapping& mapper) const {
	return new RecType(mapper.map(0, typeVariable), mapper.map(1, definition));
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

const NamedCompositeType::Entries& isolateEntries(const NamedCompositeType::Entries& entries) {
	for_each(entries, [](const NamedCompositeType::Entry& cur) {
		isolate(cur.second);
	});
	return entries;
}

NamedCompositeType::Entries copyEntriesUsing(NodeMapping& mapper, unsigned offset, const NamedCompositeType::Entries& entries) {
	// quick check ..
	if (entries.empty()) {
		return entries;
	}

	// obtain elements by looking up one after another ...
	NamedCompositeType::Entries res;
	std::transform(entries.cbegin(), entries.cend(), back_inserter(res),
		[&mapper, &offset](const NamedCompositeType::Entry& cur) {
			return NamedCompositeType::Entry(cur.first, mapper.map(offset++, cur.second));
	});
	return res;
}


NamedCompositeType::NamedCompositeType(NodeType nodeType, const string& prefix, const Entries& entries)
	: Type(nodeType, buildNameString(prefix, entries), allConcrete(entries)), entries(isolateEntries(entries)) {

	// get projection to first element
	auto start = boost::make_transform_iterator(entries.cbegin(), extractFirst<Entry>());
	auto end = boost::make_transform_iterator(entries.cend(), extractFirst<Entry>());

	if (hasDuplicates(start, end)) { // nice way using projections => but crashes in GCC
		throw std::invalid_argument("No duplicates within identifiers are allowed!");
	}
}


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

const TypePtr NamedCompositeType::getTypeOfMember(const Identifier& member) const {
	// search for member with the given name ...
	for (auto it = entries.begin(); it != entries.end(); it++) {
		const Entry& cur = *it;
		if (cur.first == member) {
			return cur.second;
		}
	}
	return TypePtr();
}


// ------------------------------------ Struct Type ---------------------------


StructTypePtr StructType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(StructType(entries));
}

StructType* StructType::createCopyUsing(NodeMapping& mapper) const {
	return new StructType(copyEntriesUsing(mapper, 0, getEntries()));
}

// ------------------------------------ Union Type ---------------------------

UnionTypePtr UnionType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(UnionType(entries));
}

UnionType* UnionType::createCopyUsing(NodeMapping& mapper) const {
	return new UnionType(copyEntriesUsing(mapper, 0, getEntries()));
}


// ------------------------------------ Single Element Type ---------------------------

SingleElementType::SingleElementType(NodeType nodeType, const string& name,
		const TypePtr& elementType, const vector<IntTypeParam>& intTypeParams) :
	GenericType(nodeType, name, toVector(isolate(elementType)), intTypeParams) {};


// ------------------------------------ Array Type ---------------------------

ArrayType::ArrayType(const TypePtr& elementType, const IntTypeParam& dim) :
	SingleElementType(NT_ArrayType, "array", elementType, toVector(dim)) {}

ArrayType* ArrayType::createCopyUsing(NodeMapping& mapper) const {
	return new ArrayType(mapper.map(0, getElementType()), mapper.mapParam(getDimension()));
}

ArrayTypePtr ArrayType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& dim) {
	return manager.get(ArrayType(elementType, dim));
}

const IntTypeParam ArrayType::getDimension() const {
	return getIntTypeParameter()[0];
}


// ------------------------------------ Vector Type ---------------------------

VectorType::VectorType(const TypePtr& elementType, const IntTypeParam& size) :
	SingleElementType(NT_VectorType, "vector", elementType, toVector(size)) {}

VectorType* VectorType::createCopyUsing(NodeMapping& mapper) const {
	return new VectorType(mapper.map(0, getElementType()), mapper.mapParam(getSize()));
}

VectorTypePtr VectorType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size) {
	return manager.get(VectorType(elementType, size));
}

const IntTypeParam VectorType::getSize() const {
	return getIntTypeParameter()[0];
}

// ------------------------------------ Ref Type ---------------------------

RefType::RefType(const TypePtr& elementType) :
	SingleElementType(NT_RefType, "ref", elementType) {}

RefTypePtr RefType::get(NodeManager& manager, const TypePtr& elementType) {
	return manager.get(RefType(elementType));
}

RefType* RefType::createCopyUsing(NodeMapping& mapper) const {
	return new RefType(mapper.map(0, getElementType()));
}


// ------------------------------------ Channel Type ---------------------------

ChannelType::ChannelType(const TypePtr& elementType, const IntTypeParam& size) :
	SingleElementType(NT_ChannelType, "channel", elementType, toVector(size)) {}

ChannelType* ChannelType::createCopyUsing(NodeMapping& mapper) const {
	return new ChannelType(mapper.map(0, getElementType()), mapper.mapParam(getSize()));
}

ChannelTypePtr ChannelType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size) {
	return manager.get(ChannelType(elementType, size));
}

const IntTypeParam ChannelType::getSize() const {
	return getIntTypeParameter()[0];
}

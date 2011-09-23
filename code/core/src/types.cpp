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

#include "insieme/core/types.h"

#include <boost/algorithm/string/join.hpp>
#include <boost/iterator/transform_iterator.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/ast_mapper.h"

using namespace insieme::core;


// ---------------------------------------- TypeList --------------------------------

namespace {

	/**
	 * Combines the given seed with the hash values of the types within
	 * the given TypeList.
	 */
	void hashTypeList(std::size_t& seed, TypeList list) {
		insieme::utils::hashList(seed, list, deref<TypePtr>());
	}

	/**
	 * Compute a hash value for a type list.
	 */
	std::size_t hashTypeList(TypeList list) {
		std::size_t seed = 0;
		hashTypeList(seed, list);
		return seed;
	}

}


// ---------------------------------------- Type --------------------------------


bool Type::equals(const Node& other) const {
	// precondition: other must be a type
	assert( dynamic_cast<const Type*>(&other) && "Type violation by base class!" );

	// convert (statically) and check the type name
	return equalsType(static_cast<const Type&>(other));
}

// -------------------------------------- Type Variable -------------------------------

TypeVariable::TypeVariable(const string& name)
	: Type(NT_TypeVariable, utils::combineHashes(HS_TypeVariable, name)), varName(name) { }

TypeVariablePtr TypeVariable::get(NodeManager& manager, const string& name) {
	return manager.get(TypeVariable(name));
}

TypeVariablePtr TypeVariable::getFromId(NodeManager& manager, const IdentifierPtr& id) {
	return manager.get(TypeVariable(id->getName()));
}

bool TypeVariable::equalsType(const Type& type) const {
	// just compare names
	const TypeVariable& rhs = static_cast<const TypeVariable&>(type);
	return varName == rhs.varName;
}

TypeVariable* TypeVariable::createCopyUsing(NodeMapping&) const {
	return new TypeVariable(getVarName());
}

std::ostream& TypeVariable::printTypeTo(std::ostream& out) const {
	return out << "'" << varName;
}

// ---------------------------------------- Tuple Type --------------------------------

namespace {

	std::size_t hashTupleType(const TypeList& elementTypes) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_TupleType);
		hashTypeList(seed, elementTypes);
		return seed;
	}

}

TupleType::TupleType(const TypeList& elementTypes) :
	Type(NT_TupleType, hashTupleType(elementTypes)), elementTypes(isolate(elementTypes)) {}

TupleType* TupleType::createCopyUsing(NodeMapping& mapper) const {
	return new TupleType(mapper.map(0, elementTypes));
}

TupleTypePtr TupleType::getEmpty(NodeManager& manager) {
	return get(manager, toVector<TypePtr>());
}

TupleTypePtr TupleType::get(NodeManager& manager, const TypeList& elementTypes) {
	return manager.get(TupleType(elementTypes));
}

bool TupleType::equalsType(const Type& type) const {
	const TupleType& other = static_cast<const TupleType&>(type);
	return ::equals(elementTypes, other.elementTypes, equal_target<TypePtr>());
}

std::ostream& TupleType::printTypeTo(std::ostream& out) const {
	// create output buffer
	return out << '(' << join(",", elementTypes, print<deref<TypePtr>>()) << ')';
}

Node::OptionChildList TupleType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(elementTypes.cbegin(), elementTypes.cend(), back_inserter(*res));
	return res;
}


// ---------------------------------------- Function Type ------------------------------

namespace {

	std::size_t hashFunctionType(const TypeList& parameterTypes, const TypePtr& returnType, bool plain) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_FunctionType);
		boost::hash_combine(seed, plain);
		hashTypeList(seed, parameterTypes);
		boost::hash_combine(seed, *returnType);
		return seed;
	}

}

FunctionType::FunctionType(const TypeList& parameterTypes, const TypePtr& returnType, bool plain) :
		Type(NT_FunctionType, hashFunctionType(parameterTypes, returnType, plain)),
		parameterTypes(isolate(parameterTypes)), returnType(isolate(returnType)), plain(plain) { }

FunctionTypePtr FunctionType::get(NodeManager& manager, const TypePtr& paramType, const TypePtr& returnType, bool plain) {
	return get(manager, toVector(paramType), returnType, plain);
}

FunctionTypePtr FunctionType::get(NodeManager& manager, const TypeList& parameterTypes, const TypePtr& returnType, bool plain) {
	// obtain reference to new element
	return manager.get(FunctionType(parameterTypes, returnType, plain));
}

FunctionType* FunctionType::createCopyUsing(NodeMapping& mapper) const {
	return new FunctionType(mapper.map(0, parameterTypes), mapper.map(parameterTypes.size(), returnType), plain);
}

Node::OptionChildList FunctionType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(parameterTypes.begin(), parameterTypes.end(), std::back_inserter(*res));
	res->push_back(returnType);
	return res;
}

std::ostream& FunctionType::printTypeTo(std::ostream& out) const {
	out << "(";
	// add parameters
	out << "(" << join(",", parameterTypes, print<deref<TypePtr>>()) << ")";

	// add result
	return out << ((plain)?"->":"=>") << *returnType << ")";
}

bool FunctionType::equalsType(const Type& type) const {
	const FunctionType& other = static_cast<const FunctionType&>(type);

	bool res = true;
	res = res && plain == other.plain;
	res = res && ::equals(parameterTypes, other.parameterTypes, equal_target<TypePtr>());
	res = res && *returnType == *other.returnType;
	return res;
}


// ---------------------------------------- Generic Type ------------------------------

namespace {

	static std::size_t hashGenericType(std::size_t hashSeed, const string& name, const vector<TypePtr>& typeParams, const vector<IntTypeParamPtr>& intTypeParams, const TypePtr& baseType) {
		std::size_t seed = 0;
		boost::hash_combine(seed, hashSeed);
		boost::hash_combine(seed, name);
		hashPtrRange(seed, typeParams);
		hashPtrRange(seed, intTypeParams);
		if (baseType) { boost::hash_combine(seed, *baseType); }
		return seed;
	}
}

/**
 * Creates an new generic type instance based on the given parameters.
 */
GenericType::GenericType(const string& name,
		const vector<TypePtr>& typeParams,
		const vector<IntTypeParamPtr>& intTypeParams,
		const TypePtr& baseType)
	:
		Type(NT_GenericType, hashGenericType(HS_GenericType, name, typeParams, intTypeParams, baseType)),
		familyName(name),
		typeParams(isolate(typeParams)),
		intParams(isolate(intTypeParams)),
		baseType(isolate(baseType)) { }


GenericType::GenericType(NodeType nodeType, std::size_t hashSeed, const string& name,
		const vector<TypePtr>& typeParams,
		const vector<IntTypeParamPtr>& intTypeParams,
		const TypePtr& baseType)
	:
		Type(nodeType, hashGenericType(hashSeed, name, typeParams, intTypeParams, baseType)),
		familyName(name),
		typeParams(isolate(typeParams)),
		intParams(isolate(intTypeParams)),
		baseType(isolate(baseType)) { }

GenericType* GenericType::createCopyUsing(NodeMapping& mapper) const {
	return new GenericType(familyName,
			mapper.map(0, typeParams),
			mapper.map(0+typeParams.size(), intParams),
			mapper.map(0+typeParams.size()+intParams.size(), baseType)
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
			const string& name,
			const vector<TypePtr>& typeParams,
			const vector<IntTypeParamPtr>& intTypeParams,
			const TypePtr& baseType) {

	// create resulting data element
	return manager.get(GenericType(name, typeParams, intTypeParams, baseType));
}

GenericTypePtr GenericType::getFromID(NodeManager& manager,
			const IdentifierPtr& name,
			const vector<TypePtr>& typeParams,
			const vector<IntTypeParamPtr>& intTypeParams,
			const TypePtr& baseType) {

	// create result using alternative get
	return get(manager, name->getName(), typeParams, intTypeParams, baseType);
}

/**
 * Retrieves the child types referenced by this generic type.
 *
 * @return the
 */
Node::OptionChildList GenericType::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::copy(typeParams.cbegin(), typeParams.cend(), back_inserter(*res));
	std::copy(intParams.cbegin(), intParams.cend(), back_inserter(*res));
	if (baseType) {
		res->push_back(baseType);
	}
	return res;
}

std::ostream& GenericType::printTypeTo(std::ostream& out) const {
	// create output buffer
	out << familyName;

	// check whether there are type parameters
	if (!typeParams.empty() || !intParams.empty()) {
		// print parameters ...
		out << '<';
		out << join(",", typeParams, print<deref<TypePtr>>());
		if (!typeParams.empty() && !intParams.empty()) {
			out << ',';
		}
		out << join(",", intParams, print<deref<IntTypeParamPtr>>());
		out << '>';
	}
	return out;
}

bool GenericType::equalsType(const Type& type) const {
	const GenericType& other = static_cast<const GenericType&>(type);

	bool res = true;
	res = res && familyName == other.familyName;
	res = res && typeParams.size() == other.typeParams.size();
	res = res && intParams.size() == other.intParams.size();
	res = res && ((!baseType && !other.baseType) || (baseType && other.baseType && *baseType==*other.baseType));
	res = res && ::equals(typeParams, other.typeParams, equal_target<TypePtr>());
	res = res && ::equals(intParams, other.intParams, equal_target<IntTypeParamPtr>());
	return  res;
}

// ------------------------------------ Rec Definition ------------------------------

namespace { // some anonymous functions for the Rec Definition

	std::size_t hashRecTypeDefinition(const RecTypeDefinition::RecTypeDefs& definitions) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_RecTypeDefinition);
		for_each(definitions, [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {
			boost::hash_combine(seed, *cur.first);
			boost::hash_combine(seed, *cur.second);
		});
		return seed;
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

namespace {

	class RecTypeUnroller : public NodeMapping {

		NodeManager& manager;
		const RecTypeDefinition& definition;
		const RecTypeDefinition::RecTypeDefs& definitions;


	public:

		RecTypeUnroller(NodeManager& manager, const RecTypeDefinition& definition)
			: manager(manager), definition(definition), definitions(definition.getDefinitions()) { }

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// check whether it is a known variable
			if (ptr->getNodeType() == NT_TypeVariable) {
				TypeVariablePtr var = static_pointer_cast<const TypeVariable>(ptr);

				auto pos = definitions.find(var);
				if (pos != definitions.end()) {
					return RecType::get(manager, var, RecTypeDefinitionPtr(&definition));
				}
			}

			// replace recursively
			return ptr->substitute(manager, *this);
		}

		TypePtr apply(const TypePtr& node) {
			return static_pointer_cast<const Type>(node->substitute(manager, *this));
		}

	};

}

TypePtr RecTypeDefinition::unrollOnce(NodeManager& manager, const TypeVariablePtr& variable) const {
	// unroll recursive type using helper
	return RecTypeUnroller(manager, *this).apply(getDefinitionOf(variable));
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

namespace {

	static std::size_t hashRecType(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definition) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_RecType);
		boost::hash_combine(seed, *variable);
		boost::hash_combine(seed, *definition);
		return seed;
	}
}

RecType::RecType(const TypeVariablePtr& variable, const RecTypeDefinitionPtr& definition)
	: Type(NT_RecType, hashRecType(variable, definition)), typeVariable(isolate(variable)), definition(isolate(definition)) { }

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

std::ostream& RecType::printTypeTo(std::ostream& out) const {
	return out << "rec " << *typeVariable << "." << *definition;
}

bool RecType::equalsType(const Type& type) const {
	const RecType& other = static_cast<const RecType&>(type);
	return *typeVariable == *other.typeVariable && *definition == *other.definition;
}

// ------------------------------------ Named Composite Type ---------------------------

namespace {

	std::size_t hashNamedCompositeType(std::size_t hashSeed, const string& prefix, const NamedCompositeType::Entries& entries) {
		std::size_t seed = 0;
		boost::hash_combine(seed, hashSeed);
		boost::hash_combine(seed, prefix);
		for_each(entries, [&seed](const NamedCompositeType::Entry& cur) {
			boost::hash_combine(seed, *cur.first);
			boost::hash_combine(seed, *cur.second);
		});
		return seed;
	}

	const NamedCompositeType::Entries& isolateEntries(const NamedCompositeType::Entries& entries) {
		for_each(entries, [](const NamedCompositeType::Entry& cur) {
			isolate(cur.second);
		});
		return entries;
	}

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
			IdentifierPtr name = mapper.map(offset++, cur.first);
			TypePtr type = mapper.map(offset++, cur.second);
			return NamedCompositeType::Entry(name, type);
	});
	return res;
}


NamedCompositeType::NamedCompositeType(NodeType nodeType, std::size_t hashSeed, const string& prefix, const Entries& entries)
	: Type(nodeType, hashNamedCompositeType(hashSeed, prefix, entries)), entries(isolateEntries(entries)) {

	// get projection to first element
	auto start = boost::make_transform_iterator(entries.cbegin(), extractFirst<Entry>());
	auto end = boost::make_transform_iterator(entries.cend(), extractFirst<Entry>());

	if (hasDuplicates(start, end)) { // nice way using projections => but crashes in GCC
		throw std::invalid_argument("No duplicates within identifiers are allowed!");
	}
}

Node::OptionChildList NamedCompositeType::getChildNodes() const {
	OptionChildList res(new ChildList());
	for_each(entries, [&](const Entry& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	return res;
}

const TypePtr NamedCompositeType::getTypeOfMember(const IdentifierPtr& member) const {
	// search for member with the given name ...
	for (auto it = entries.begin(); it != entries.end(); it++) {
		const Entry& cur = *it;
		if (*cur.first == *member) {
			return cur.second;
		}
	}
	return TypePtr();
}

bool NamedCompositeType::equalsType(const Type& type) const {
	const NamedCompositeType& other = static_cast<const NamedCompositeType&>(type);
	return ::equals(entries, other.entries, [](const Entry& a, const Entry& b) {
		return *a.first == *b.first && *a.second == *b.second;
	});
}

std::ostream& NamedCompositeType::printTypeTo(std::ostream& out) const {
	// create output buffer
	std::stringstream res;
	out << ((getNodeType() == NT_UnionType)?"union<":"struct<");

	out << join(",", entries, [](std::ostream& out, const Entry& cur) {
		out << *cur.first << ":" << *cur.second;
	});

	return out << ">";
}

// ------------------------------------ Struct Type ---------------------------

StructType::StructType(const Entries& entries)
	: NamedCompositeType(NT_StructType, HS_StructType, "struct", entries) {}

StructTypePtr StructType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(StructType(entries));
}

StructType* StructType::createCopyUsing(NodeMapping& mapper) const {
	return new StructType(copyEntriesUsing(mapper, 0, getEntries()));
}

// ------------------------------------ Union Type ---------------------------

UnionType::UnionType(const Entries& elements)
	: NamedCompositeType(NT_UnionType, HS_UnionType, "union", elements) {}

UnionTypePtr UnionType::get(NodeManager& manager, const Entries& entries) {
	// just ask manager for new pointer
	return manager.get(UnionType(entries));
}

UnionType* UnionType::createCopyUsing(NodeMapping& mapper) const {
	return new UnionType(copyEntriesUsing(mapper, 0, getEntries()));
}


// ------------------------------------ Single Element Type ---------------------------

SingleElementType::SingleElementType(NodeType nodeType, std::size_t hashSeed, const string& name,
		const TypePtr& elementType, const vector<IntTypeParamPtr>& intTypeParams) :
	GenericType(nodeType, hashSeed, name, toVector(elementType), intTypeParams) {};


// ------------------------------------ Array Type ---------------------------

ArrayType::ArrayType(const TypePtr& elementType, const IntTypeParamPtr& dim) :
	SingleElementType(NT_ArrayType, HS_ArrayType, "array", elementType, toVector(dim)) {}

ArrayType* ArrayType::createCopyUsing(NodeMapping& mapper) const {
	return new ArrayType(mapper.map(0, getElementType()), mapper.map(1, getDimension()));
}

ArrayTypePtr ArrayType::get(NodeManager& manager, const TypePtr& elementType) {
	return manager.get(ArrayType(elementType, ConcreteIntTypeParam::get(manager, 1)));
}

ArrayTypePtr ArrayType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& dim) {
	return manager.get(ArrayType(elementType, dim));
}

const IntTypeParamPtr& ArrayType::getDimension() const {
	return getIntTypeParameter()[0];
}


// ------------------------------------ Vector Type ---------------------------

VectorType::VectorType(const TypePtr& elementType, const IntTypeParamPtr& size) :
	SingleElementType(NT_VectorType, HS_VectorType, "vector", elementType, toVector(size)) {}

VectorType* VectorType::createCopyUsing(NodeMapping& mapper) const {
	return new VectorType(mapper.map(0, getElementType()), mapper.map(1, getSize()));
}

VectorTypePtr VectorType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size) {
	return manager.get(VectorType(elementType, size));
}

const IntTypeParamPtr& VectorType::getSize() const {
	return getIntTypeParameter()[0];
}

// ------------------------------------ Ref Type ---------------------------

RefType::RefType(const TypePtr& elementType) :
	SingleElementType(NT_RefType, HS_RefType, "ref", elementType) {}

RefTypePtr RefType::get(NodeManager& manager, const TypePtr& elementType) {
	return manager.get(RefType(elementType));
}

RefType* RefType::createCopyUsing(NodeMapping& mapper) const {
	return new RefType(mapper.map(0, getElementType()));
}


// ------------------------------------ Channel Type ---------------------------

ChannelType::ChannelType(const TypePtr& elementType, const IntTypeParamPtr& size) :
	SingleElementType(NT_ChannelType, HS_ChannelType, "channel", elementType, toVector(size)) {}

ChannelType* ChannelType::createCopyUsing(NodeMapping& mapper) const {
	return new ChannelType(mapper.map(0, getElementType()), mapper.map(1, getSize()));
}

ChannelTypePtr ChannelType::get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size) {
	return manager.get(ChannelType(elementType, size));
}

const IntTypeParamPtr& ChannelType::getSize() const {
	return getIntTypeParameter()[0];
}

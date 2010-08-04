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

#pragma once

#include <algorithm>
#include <iterator>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/algorithm/string/join.hpp>

#include "stringutils.h"
#include "containerutils.h"

using std::string;
using std::vector;
using std::map;

// ---------------------------------------- A token for an abstract type ------------------------------

class Type {
	const string name;
public:
	Type(const std::string& name) : name(name) {};
	virtual bool isConcrete() const = 0;
	const string& getName() const { return name; };
	virtual string toString() const { return getName(); };
};
typedef std::shared_ptr<Type> TypeRef;


// ---------------------------------------- A token for an abstract type ------------------------------


class VariableType : public Type {
	VariableType(const string& name) : Type(format("'%s",name.c_str())){}
public:
	virtual bool isConcrete() const { return false; }
};


// ---------------------------------------- A token for an abstract type ------------------------------


class AbstractType;
typedef std::shared_ptr<AbstractType> AbstractTypeRef;

/**
 * The abstract type is a special (singleton) token representing the base class for types defined
 * in an abstract way. Hence, type definitions using this type as a base type have to be supported
 * by the code synthesizer.
 */
class AbstractType : public Type {

	/**
	 * The singleton instance of this type (shared among all type manager instances)
	 */
	static AbstractTypeRef instance;

	/**
	 * The default constructor of this type, fixing the name to "abstract".
	 */
	AbstractType() : Type("abstract") {}

public:

	/**
	 * Obtains a reference to the singleton instance of this type.
	 */
	static AbstractTypeRef getInstance() { return instance; }

	/**
	 * Overrides the implementation of this method within the parent class (where it is abstract).
	 * The abstract type is always a concrete type.
	 */
	virtual bool isConcrete() const { return true; }
};


// ---------------------------------------- Integer Type Parameters ------------------------------

class IntTypeParam {
private:
	typedef enum { VARIABLE, CONCRETE, INFINITE } Type;

	Type type;
	union {
		char symbol;
		int value;
	};

	IntTypeParam(const char symbol) : type(VARIABLE), symbol(symbol) {};
	IntTypeParam(const int value) : type(CONCRETE), value(value) {};
	IntTypeParam() : type(INFINITE) {};

public:

	const string toString() const {
		switch(type) {
		case VARIABLE: return ::toString(symbol);
		case CONCRETE: return ::toString(value);
		case INFINITE: return ::toString("Inf");
		default: throw std::runtime_error("Invalid parameter type discovered!");
		}
	}

	bool isConcrete() const {
		return type!=VARIABLE;
	}

private:
	static IntTypeParam infinite;

public:
	static IntTypeParam getVariableIntParam(char symbol) {
		return IntTypeParam(symbol);
	}

	static IntTypeParam getConcreteIntParam(int value) {
		return IntTypeParam(value);
	}

	static IntTypeParam getInfiniteIntParam() {
		return infinite;
	}

};

// ---------------------------------------- User defined Type ------------------------------


class UserType : public Type {
	static string buildNameString(const string& name, 
			const vector<TypeRef>& typeParams, const vector<IntTypeParam>& intParams) {
		
		// create output buffer
		std::stringstream res;

		// add leading name
		res << name;

		// check whether there are type parameters
		if (!typeParams.empty() || !intParams.empty()) {

			// convert type parameters to strings ...
			vector<string> list;
			std::transform(typeParams.cbegin(), typeParams.cend(), back_inserter(list), [](const TypeRef cur) { return cur->getName(); });
			std::transform(intParams.cbegin(), intParams.cend(), back_inserter(list), [](const IntTypeParam cur) { return cur.toString(); });

			// add type parameter clause
			res << "<" << boost::join(list, ",") << ">";
		}

		// return resulting string
		return res.str();
	}

	const vector<TypeRef> typeParams;
	const vector<IntTypeParam> intParams;
	const TypeRef baseType;

public:
	UserType(const string& name,
			vector<TypeRef> typeParams = vector<TypeRef>(),
			vector<IntTypeParam> intTypeParams = vector<IntTypeParam>(),
			TypeRef baseType = AbstractType::getInstance())
	   :
		Type(buildNameString(name, typeParams, intTypeParams)),
		typeParams(typeParams),
		intParams(intTypeParams),
		baseType(baseType) {}

	virtual bool isConcrete() const {
		// init result (to perform lazy evaluation)
		bool res = true;

		// check whether there is a variable type within the type-parameter list
		auto p = [](const TypeRef cur) { return cur->isConcrete(); };
		res = res && (std::find_if(typeParams.cbegin(), typeParams.cend(), p) != typeParams.end());

		// check whether there is a variable symbol within the integer-parameter list
		auto q = [](const IntTypeParam cur) { return cur.isConcrete(); };
		res = res && std::find_if(intParams.cbegin(), intParams.cend(), q) != intParams.end();

		// return result
		return res;
	}
};

class TupleType : public Type {
	const vector<TypeRef> elementTypes;
};

class FunctionType : public Type {
	const TypeRef returnType;
	const TypeRef argumentType;
};

class NamedCompositeType : public Type {
	const map<const string, const TypeRef> elements;
};
class StructType : public NamedCompositeType {
};
class UnionType : public NamedCompositeType {
};

class ArrayType : public Type {
	friend class TypeManager;
	const TypeRef elementType;
	const unsigned dimensions;

	ArrayType(const TypeRef elementType, const unsigned dim) : 
		Type(format("array<%d,%s>", dim, elementType->getName().c_str())),
			elementType(elementType), dimensions(dim)  { };

public:
	virtual bool isConcrete() const { elementType->isConcrete(); }
};
typedef const std::shared_ptr<ArrayType> ArrayTypeRef;

class VectorType : public UserType {
	VectorType(TypeRef elementType, IntTypeParam size) : UserType("vector", singleton(elementType), singleton(size)) {}
};

class ReferenceType : public UserType {
	ReferenceType(TypeRef elementType) : UserType("ref", singleton(elementType), vector<IntTypeParam>()) {}
};

class ChannelType : public Type {
	TypeRef type;
	unsigned bufferLength;
};





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

#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <memory>
#include <algorithm>
#include <iterator>
#include <boost/algorithm/string/join.hpp>

#include "stringutils.h"

using std::string;
using std::vector;
using std::map;

class Type {
	const string name;
public:
	Type(const std::string& name) : name(name) {};
	virtual bool isConcrete() const = 0;
	const string& getName() const { return name; };
	virtual string toString() const { return getName(); };
};
typedef std::shared_ptr<Type> TypeRef;

class TypeVariable : public Type {
	
};


class AbstractType;
typedef std::shared_ptr<AbstractType> AbstractTypeRef;

class AbstractType : public Type {

	static AbstractTypeRef instance;
public:
	static AbstractTypeRef getInstance() { return instance; }

	AbstractType() : Type("abstract") {}
	virtual bool isConcrete() const { return true; }
};



typedef struct IntegerParameterStruct {
	typedef enum { VARIABLE, CONCRETE, INFINITE } IntegerParamType;
	IntegerParamType type;
	union {
		char parameterName;
		int value;
	};

	const string toString() const {
		switch(type) {
		case VARIABLE: return ::toString(parameterName);
		case CONCRETE: return ::toString(value);
		case INFINITE: return ::toString("Inf");
		default: throw std::runtime_error("IntegerParameterStruct of unknown type.");
		}
	}

} IntTypeParam;


class UserType : public Type {
	static string buildNameString(const string& name, 
			const vector<TypeRef>& typeParams, const vector<IntTypeParam>& intParams) {
		
		std::stringstream res;
		res << name;

		if (!typeParams.empty() || !intParams.empty()) {

			vector<string> list;
			std::transform(typeParams.cbegin(), typeParams.cend(), back_inserter(list), [](const TypeRef cur) { return cur->getName(); });
			std::transform(intParams.cbegin(), intParams.cend(), back_inserter(list), [](const IntTypeParam cur) { return cur.toString(); });

			res << "<" << boost::join(list, ",") << ">";
		}

		return res.str();
	}

	const vector<TypeRef> typeParams;
	const vector<IntTypeParam> intParams;
	const TypeRef baseType;

public:
	UserType(string name,
			vector<TypeRef> typeParams = vector<TypeRef>(),
			vector<IntTypeParam> intTypeParams = vector<IntTypeParam>(),
			TypeRef baseType = AbstractType::getInstance())
	   :
		Type(buildNameString(name, typeParams, intTypeParams)),
		typeParams(typeParams),
		intParams(intTypeParams),
		baseType(baseType) {}

	virtual bool isConcrete() const {
		auto p = [](const TypeRef cur) { return cur->isConcrete(); };
		return std::find_if(typeParams.cbegin(), typeParams.cend(), p) != typeParams.end();
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

class VectorType : public Type {
	TypeRef elementType;
	unsigned length;
};

class ReferenceType : public Type {
	TypeRef referencedType;
};

class ChannelType : public Type {
	TypeRef type;
	unsigned bufferLength;
};





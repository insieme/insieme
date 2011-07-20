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

#include "insieme/simple_backend/backend_convert.h"

#include "insieme/core/types.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/c_info/naming.h"


namespace insieme {
namespace simple_backend {


using namespace insieme::core;


const string TypeManager::TypeInfo::UNSUPPORTED = "/* unsupported */";

TypeManager::TypeInfo toTypeInfo(string name) {
	return TypeManager::TypeInfo(name, name, name + " %s", name + " %s", CodeFragmentPtr());
}

string TypeManager::formatParamter(const CodeFragmentPtr& context, const TypePtr& paramType, const string& name, bool decl) {

	// obtain type info
	const TypeInfo& info = getTypeInfo(context, paramType);

	// format parameter
	return format((decl)?info.declPattern.c_str():info.paramPattern.c_str(), name.c_str());
}

string TypeManager::formatFunctionPointer(const CodeFragmentPtr& context, const FunctionTypePtr& funType, const string& name) {
	// construct function pointer type member
	std::stringstream buffer;
	buffer << getTypeName(context, funType->getReturnType(), false) << "(*" << name << ")(void*";
	auto params = funType->getParameterTypes();
	if (!params.empty()) {
		buffer << ", " << join(", ", params, [&](std::ostream& out, const TypePtr& cur) {
			out << getTypeName(context, cur, false);
		});
	}
	buffer << ")";
	return buffer.str();
}


const TypeManager::TypeInfo TypeManager::getTypeInfo(const CodeFragmentPtr& context, const core::TypePtr& type) {

	// resolve given type
	const TypeManager::TypeInfo& info = resolveType(type);

	// => add code dependency to definition (if necessary)
	if (info.definition) {
		context->addDependency(info.definition);
	}

	// return the obtained entry
	return info;
}

string TypeManager::getTypeName(const CodeFragmentPtr& context, const core::TypePtr& type, bool decl) {

	// obtain type entry
	TypeManager::TypeInfo info = getTypeInfo(context, type);

	// return C name of type
	return (decl)?info.lValueName:info.rValueName;
}


NameManager& TypeManager::getNameManager() const {
	return converter.getNameManager();
}


TypeManager::TypeInfo TypeManager::resolveType(const core::TypePtr& type) {

	// look up definition
	auto pos = typeDefinitions.find(type);
	if (pos != typeDefinitions.end()) {
		// found!
		return pos->second;
	}

	// resolve type
	TypeInfo res;
	switch(type->getNodeType()) {
	case NT_FunctionType:
		res = resolveFunctionType(static_pointer_cast<const FunctionType>(type)); break;
	case NT_GenericType:
		res = resolveGenericType(static_pointer_cast<const GenericType>(type)); break;
	case NT_StructType:
		res = resolveStructType(static_pointer_cast<const StructType>(type)); break;
	case NT_UnionType:
		res = resolveUnionType(static_pointer_cast<const UnionType>(type)); break;
	case NT_VectorType:
		res = resolveVectorType(static_pointer_cast<const VectorType>(type)); break;
	case NT_ArrayType:
		res = resolveArrayType(static_pointer_cast<const ArrayType>(type)); break;
	case NT_RefType:
		res = resolveRefType(static_pointer_cast<const RefType>(type)); break;
	case NT_RecType:
		res = resolveRecType(static_pointer_cast<const RecType>(type)); break;
//	case NT_ChannelType:
	default:
		// return unsupported type
		res = toTypeInfo(format("<?>%s</?>", toString(*type).c_str()));
		//assert(false && "Unsupported IR type encountered!");
	}

	// register type
	typeDefinitions.insert(std::make_pair(type, res));
	return res;
}

TypeManager::TypeInfo TypeManager::resolveGenericType(const GenericTypePtr& ptr) {
	auto& basic = ptr->getNodeManager().basic;

	// TODO: handle basic types using a map

	// check some primitive types
	if(basic.isUnit(ptr)) {
		return toTypeInfo("void");
	}
	if (basic.isAnyRef(ptr)) {
		return toTypeInfo("void*");
	}
	if(basic.isInt(ptr)) {
		string qualifier = basic.isUnsignedInt(ptr) ? "unsigned " : "";
		auto intParm = ptr->getIntTypeParameter().front();
		if(intParm->getNodeType() == NT_ConcreteIntTypeParam) {
			switch(static_pointer_cast<const ConcreteIntTypeParam>(intParm)->getValue()) {
				case 1: return toTypeInfo(qualifier + "char");
				case 2: return toTypeInfo(qualifier + "short");
				case 4: return toTypeInfo(qualifier + "int");
				case 8: return toTypeInfo(qualifier + "long"); // long long ?
				default: return toTypeInfo(ptr->toString());
			}
		}
		// TODO Warn?
		return toTypeInfo(ptr->toString());
	}
	if(basic.isBool(ptr)) {
		return toTypeInfo("bool");
	}
	if(basic.isReal(ptr)) {
		auto intParm = ptr->getIntTypeParameter().front();
		if(intParm->getNodeType() == NT_ConcreteIntTypeParam) {
			switch(static_pointer_cast<const ConcreteIntTypeParam>(intParm)->getValue()) {
				case 4: return toTypeInfo("float");
				case 8: return toTypeInfo("double");
				case 16: return toTypeInfo("long double");
				default: return toTypeInfo(ptr->toString());
			}
		}
		// TODO Warn?
		return toTypeInfo(ptr->toString());
	}
	if(basic.isString(ptr)) {
		// strings are internally managed as vectors of a certain size
		return TypeManager::TypeInfo(
//				TypeInfo::UNSUPPORTED, TypeInfo::UNSUPPORTED,
//				TypeInfo::UNSUPPORTED, TypeInfo::UNSUPPORTED,
//				"char*", "char* %s", "(%s).data");
				"char*", "char*", "char*", "char*",
				"char*", "char* %s", "%s");
	}
	if(basic.isChar(ptr)) {
		return toTypeInfo("char");
	}
	if(basic.isVarList(ptr)) {
		return toTypeInfo("...");
	}
	if(basic.isJob(ptr)) {
		return toTypeInfo("isbr_Job*");
	}
	if(basic.isThreadGroup(ptr)) {
		return toTypeInfo("isbr_ThreadGroup");
	}

	if(core::analysis::isTypeLiteralType(ptr)) {
		// this is a type literal => handle as int
		return toTypeInfo("int");
	}

	//assert(0 && "Unhandled generic type.");
	return toTypeInfo(string("[[unhandled_simple_type: ") + ptr->toString() + "]]");
}

TypeManager::TypeInfo TypeManager::resolveFunctionType(const FunctionTypePtr& ptr) {

	// lookup function definition
	FunctionTypeInfo info = getFunctionTypeInfo(ptr);

	// assemble name and dependency
	string typeName = info.closureName + "*";
	return TypeManager::TypeInfo(typeName, typeName, typeName + " %s", typeName + " %s", info.definitions);
}

TypeManager::FunctionTypeInfo TypeManager::getFunctionTypeInfo(const core::FunctionTypePtr& functionType) {

	// use cached information
	auto pos = functionTypeDefinitions.find(functionType);
	if (pos != functionTypeDefinitions.end()) {
		return pos->second;
	}

	// create new entry:

	// get name for function type
	string name = getNameManager().getName(functionType, "funType");
	string functorName = name;
	string callerName = name + "_call";
	string ctrName = name + "_ctr";
	auto params = functionType->getParameterTypes();

	CodeFragmentPtr functorAndCaller = CodeFragment::createNew("Definitions for function type: " + name);
	CodeBuffer& out = functorAndCaller->getCodeBuffer();

	// construct function pointer type member
	string callPtr = formatFunctionPointer(functorAndCaller, functionType, "call");


	// A) add abstract functor definition
	out << "//\n// -------------------- Begin of constructs for function type " << *functionType << "---------------------\n";
	out << "// Base-struct of describing closures of type " << name << " <-> " << *functionType << "\n";
	out << "typedef struct _" << functorName << " { \n";

	// add function pointer 'call'
	out << "    " << callPtr << ";\n";

	// add field for size of concrete type
	// NOTE: disabled since not used anywhere
	// out << "    const size_t size;\n";

	out << "} " << functorName << ";\n";


	// B) define caller routine - only functions without captures can be called
	out << "\n";
	out << "// Type safe function for invoking closures of type " << name << "\n";
	string resultType = getTypeName(functorAndCaller, functionType->getReturnType());
	out << "static inline " << resultType << " " << callerName << "(" << name << "* closure";
	int i = 0;
	if (!params.empty()) {
		out << ", " << join(", ", params, [&, this](std::ostream& out, const TypePtr& cur) {
			out << formatParamter(functorAndCaller, cur, format("p%d", ++i), false);
		});
	}
	out << ") { ";
	if (resultType != "void") out << "return";
	out << " closure->call(closure";
	i = 0;
	if (!params.empty()) {
		out << ", " << join(",", params, [&, this](std::ostream& out, const TypePtr& cur) {
			out << format("p%d", ++i);
		});
	}
	out << "); }\n";


	// C) define a constructor for closures of pure functions exposing this type
	out << "\n";
	out << "// A constructor for closures wrapping pure functions of type " << *functionType << "\n";
	out << "static inline " << functorName << "* " << ctrName << "(" << functorName << "* target, " << callPtr << ") {" << CodeBuffer::indR << "\n";
	out << "*target = (" << functorName << "){call};\nreturn target;";
	out << CodeBuffer::indL;
	out << "\n}\n";

	out << "// ----------------------- end of constructs for function " << *functionType << "-----------------------\n";

	// create, register and return entry
	FunctionTypeInfo info(functorName, callerName, functorAndCaller);
	functionTypeDefinitions.insert(std::make_pair(functionType, info));
	return info;
}


TypeManager::TypeInfo TypeManager::resolveRefType(const RefTypePtr& ptr) {

	// establish reference type with one additional level of indirection
	TypeInfo subType = resolveType(ptr->getElementType());
	string lvalue = subType.lValueName;
	string rvalue = subType.lValueName + "*";
	if (ptr->getElementType()->getNodeType() == NT_RefType) {
		lvalue = lvalue + "*";
	}

	// special handling of references to vectors and arrays
	string externalName = subType.externName;
	auto nodeType = ptr->getElementType()->getNodeType();
	if (nodeType != NT_ArrayType && nodeType != NT_VectorType) {
		 externalName = externalName + "*";
	}

	string externalization = "((" + externalName + ")(%s))";
	if (nodeType == NT_ArrayType || nodeType == NT_VectorType) {
		externalization = "((" + externalName + ")((*(%s)).data))";
	}

	// ---------------- add a new operator ------------------------

	CodeFragmentPtr code = CodeFragment::createNew("New operator for type " + toString(*ptr));
	code->addDependency(subType.definition);

	// add struct definition
	string name = getNameManager().getName(ptr);
	const string& result = rvalue;
	const string& value = subType.lValueName;

	code << "static inline " << result << " _ref_new_" << name << "(" << value << " value) {\n";
	code << "    " << result << " res = malloc(sizeof(" << value << "));\n";
	code << "    *res = value;\n";
	code << "    return res;\n";
	code << "}\n\n";


	return TypeManager::TypeInfo(lvalue, rvalue, lvalue + " %s", rvalue + " %s",
			externalName, externalName + " %s", externalization, subType.definition, subType.definition, code);

//	TODO: if
//	return resolveRefOrVectorOrArrayType(ptr);
}


TypeManager::TypeInfo TypeManager::resolveVectorType(const VectorTypePtr& ptr) {

	// fetch name for the vector type
	string name = getNameManager().getName(ptr);

	// look up element type info
	const TypePtr& elementType = ptr->getElementType();
	const TypeInfo& elementTypeInfo = resolveType(elementType);

	// check whether the type has been resolved while resolving the sub-type
	auto pos = typeDefinitions.find(ptr);
	if (pos != typeDefinitions.end()) {
		return pos->second;
	}

	// create a new code fragment for the struct definition
	CodeFragmentPtr code = CodeFragment::createNew("vector_type_declaration of " + name + " <=> " + toString(*ptr));
	code->addDependency(elementTypeInfo.definition);

	// add struct definition
	string size = toString(*ptr->getSize());
	code << "typedef struct _" << name << " { \n";
	code << "    " << elementTypeInfo.lValueName << " data[" << size << "];\n";
	code << "} " << name << ";\n";


	// ---------------------- add init uniform ---------------------
	code << "\n";
	code << "// A constructor initializing a vector of the type " << name << " uniformly\n";
	code << "static inline " << name << " " << name << "_init_uniform(";
	code << formatParamter(code, elementType, "value");
	code << ") {" << CodeBuffer::indR << "\n";
	code << name << " res;\n";
	code << "for (int i=0; i<" << size << ";++i) {\n";
	code << "    " << "res.data[i] = value;\n";
	code << "}\n";
	code << "return res;";
	code << CodeBuffer::indL << "\n}\n\n";


	// construct type info including external type representation (as a pointer)
	string externalName = elementTypeInfo.externName + "*";
	return TypeManager::TypeInfo(name, name, name + " %s", name + " %s",
			externalName, externalName + " %s", "(%s).data", code);

//	TODO: remove if vector-is-a-struct works out!
//	// default handling
//	TypeManager::TypeInfo res = resolveRefOrVectorOrArrayType(ptr);
//	res.rValueName = Entry::UNSUPPORTED;
//	res.paramPattern = Entry::UNSUPPORTED;
//	return res;
}

TypeManager::TypeInfo TypeManager::resolveArrayType(const ArrayTypePtr& ptr) {

	// obtain dimension of array
	unsigned dim = 0;
	const IntTypeParamPtr& dimPtr = ptr->getDimension();
	if (dimPtr->getNodeType() != NT_ConcreteIntTypeParam) {
		// non-concrete array types are not supported
		return TypeInfo();
	} else {
		dim = static_pointer_cast<const ConcreteIntTypeParam>(dimPtr)->getValue();
	}

	// otherwise: create a struct representing the array type

	// fetch name for the array type
	string name = getNameManager().getName(ptr);


	// look up element type info
	const TypeInfo& elementTypeInfo = resolveType(ptr->getElementType());

	// check whether the type has been resolved while resolving the sub-type
	auto pos = typeDefinitions.find(ptr);
	if (pos != typeDefinitions.end()) {
		return pos->second;
	}

	// create a new code fragment for the struct definition
	CodeFragmentPtr definition = CodeFragment::createNew("array type definition of " + name + " <=> " + toString(*ptr));
	definition->addDependency(elementTypeInfo.declaration);

	// see whether the size should be stored within an array
	bool useSize = converter.isSupportArrayLength();

	// add array-struct definition
	definition << "typedef struct _" << name << " { \n";
	definition << "    " << elementTypeInfo.lValueName << times("*", dim) << " data;\n";
	if (useSize) definition << "    " << "unsigned size[" << dim << "];\n";
	definition << "} " + name + ";\n";

	// ---------------------- add constructor ---------------------
	CodeFragmentPtr utils = CodeFragment::createNew("array type utils of " + name + " <=> " + toString(*ptr));
	utils->addDependency(elementTypeInfo.definition);

	utils << "// A constructor for the array type " << name << "\n";
	utils << "static inline " << name << " " << name << "_ctr(";
	for (unsigned i=0; i<dim; i++) {
		utils << "unsigned s" << (i+1);
		if (i!=dim-1) {
			definition << ",";
		}
	}
	utils << ") {\n";
	utils << "    return ((" << name << "){malloc(sizeof(" << elementTypeInfo.lValueName << ")";
	for (unsigned i=0; i<dim; i++) {
		utils << "*s" << (i+1);
	}
	utils << ")";

	if (useSize) {
		utils << ",{";
		for (unsigned i=0; i<dim; i++) {
			utils << "s" << (i+1);
			if (i!=dim-1) {
				utils << ",";
			}
		}
		utils << "}";
	}

	utils << "});\n}\n";

	string externalName = elementTypeInfo.externName + toString(times("*", dim));
	return TypeManager::TypeInfo(name, name, name + " %s", name + " %s",
			externalName, externalName + " %s", "(%s).data", definition, definition, utils);

}

TypeManager::TypeInfo TypeManager::resolveNamedCompositType(const NamedCompositeTypePtr& ptr, string prefix) {

	// fetch name for composed type
	string name = getNameManager().getName(ptr, "userdefined_type");

	// create a new code fragment for the struct definition
	CodeFragmentPtr code = CodeFragment::createNew("type_declaration_" + name);

	// add struct definition
	code << prefix << " " << name << " { \n";
	for_each(ptr->getEntries(), [&, this](const NamedCompositeType::Entry& entry) {
		code << "    " << formatParamter(code, entry.second, entry.first->getName(), true) << ";\n";
	});
	code << "};\n";

	string typeName = prefix + " " + name;
	return TypeManager::TypeInfo(typeName, typeName, typeName + " %s", typeName + " %s", code);
}

TypeManager::TypeInfo TypeManager::resolveUnionType(const UnionTypePtr& ptr) {
	return resolveNamedCompositType(ptr, "union");
}

TypeManager::TypeInfo TypeManager::resolveStructType(const StructTypePtr& ptr) {
	return resolveNamedCompositType(ptr, "struct");
}



TypeManager::TypeInfo TypeManager::resolveRecType(const core::RecTypePtr& ptr) {

	// resolve recursive type definition
	resolveRecTypeDefinition(ptr->getDefinition());

	// look up type again (now it should be known)
	return resolveType(ptr);
}

void TypeManager::resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr) {

	// create dummy code group depending on all prototype definitions
	CodeFragmentPtr group = CodeFragment::createNewDummy("Dummy fragment for recursive type group");

	NodeManager& manager = ptr->getNodeManager();
	NameManager& nameManager = getNameManager();

	// A) create prototype and add entry for each recursively defined type
	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {

		// create recursive type using current type variable
		RecTypePtr type = RecType::get(manager, cur.first, ptr);

		// create prototype
		string name = nameManager.getName(type, "userdefined_rec_type");

		switch(cur.second->getNodeType()) {
		case NT_StructType:
			name = "struct " + name; break;
		case NT_UnionType:
			name = "union " + name; break;
		default:
			assert(false && "Cannot support recursive type which isn't a struct or union!");
		}

		CodeFragmentPtr prototype = CodeFragment::createNew("Prototype of " + name);
		prototype << name << ";\n";
		group->addDependency(prototype);

		this->typeDefinitions.insert(std::make_pair(type, TypeInfo(name, name, name + " %s", name + " %s", prototype, group)));
	});



	// A) unroll types and write definitions
	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {

		// obtain unrolled type
		TypePtr unrolled = ptr->unrollOnce(manager, cur.first);

		// fix name of unrolled struct
		nameManager.setName(unrolled, nameManager.getName(RecType::get(manager, cur.first, ptr)));

		// resolve unrolled type and add dependency to group
		group->addDependency(resolveType(unrolled).definition);
	});


//	// C) update type definition map to reference entire group
//	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {
//
//		// create recursive type using current type variable
//		RecTypePtr type = RecType::get(manager, cur.first, ptr);
//
//		// ... update code pointer to reference entire group
//		this->typeDefinitions.find(type)->second.definition = group;
//	});

}

} // end: namespace simple_backend
} // end: namespace insieme

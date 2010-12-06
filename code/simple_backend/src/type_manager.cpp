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

#include "insieme/c_info/naming.h"


namespace insieme {
namespace simple_backend {


using namespace insieme::core;

TypeManager::Entry toEntry(string name) {
	return TypeManager::Entry(name, name, name + " %s", name + " %s", CodePtr());
}

string TypeManager::formatParamter(CodePtr& context, const TypePtr& paramType, const string& name, bool decl) {

	// obtain type entry
	const Entry& entry = getTypeEntry(context, paramType);

	// format parameter
	return format((decl)?entry.declPattern.c_str():entry.paramPattern.c_str(), name.c_str());
}

const TypeManager::Entry TypeManager::getTypeEntry(const CodePtr& context, const core::TypePtr& type) {

	// resolve given type
	const TypeManager::Entry& entry = resolveType(type);

	// => add code dependency to definition (if necessary)
	if (entry.definition) {
		context->addDependency(entry.definition);
	}

	// return the obtained entry
	return entry;
}

string TypeManager::getTypeName(const CodePtr& context, const core::TypePtr& type, bool decl) {

	// obtain type entry
	TypeManager::Entry entry = getTypeEntry(context, type);

	// return C name of type
	return (decl)?entry.lValueName:entry.rValueName;
}



TypeManager::Entry TypeManager::resolveType(const core::TypePtr& type) {

	// look up definition
	auto pos = typeDefinitions.find(type);
	if (pos != typeDefinitions.end()) {
		// found!
		return pos->second;
	}

	// resolve type
	Entry res;
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
		res = toEntry(format("<?>%s</?>", toString(*type).c_str()));
		//assert(false && "Unsupported IR type encountered!");
	}

	// register type
	typeDefinitions.insert(std::make_pair(type, res));
	return res;
}

TypeManager::Entry TypeManager::resolveGenericType(const GenericTypePtr& ptr) {
	auto& basic = ptr->getNodeManager().basic;

	// check some primitive types
	if(basic.isUnit(ptr)) {
		return toEntry("void");
	}
	if(basic.isInt(ptr)) {
		string qualifier = basic.isUnsignedInt(ptr) ? "unsigned " : "";
		auto intParm = ptr->getIntTypeParameter().front();
		if(intParm.isConcrete()) switch(intParm.getValue()) {
			case 1: return toEntry(qualifier + "char");
			case 2: return toEntry(qualifier + "short");
			case 4: return toEntry(qualifier + "int");
			case 8: return toEntry(qualifier + "long"); // long long ?
			default: return toEntry(ptr->getName());
		}
		// TODO Warn?
		return toEntry(ptr->getName());
	}
	if(basic.isBool(ptr)) {
		return toEntry("bool");
	}
	if(basic.isReal(ptr)) {
		auto intParm = ptr->getIntTypeParameter().front();
		if(intParm.isConcrete()) switch(intParm.getValue()) {
			case 4: return toEntry("float");
			case 8: return toEntry("double");
			case 16: return toEntry("long double");
			default: return toEntry(ptr->getName());
		}
		// TODO Warn?
		return toEntry(ptr->getName());
	}
	if(basic.isString(ptr)) {
		return toEntry("string");
	}
	if(basic.isChar(ptr)) {
		return toEntry("char");
	}
	if(basic.isVarList(ptr)) {
		return toEntry("...");
	}

	//assert(0 && "Unhandled generic type.");
	return toEntry(string("[[unhandled_simple_type: ") + ptr->getName() + "]]");
}

TypeManager::Entry TypeManager::resolveFunctionType(const FunctionTypePtr& ptr) {

	// lookup function definition
	FunctionTypeEntry entry = getFunctionTypeDetails(ptr);

	// assemble name and dependency
	string typeName = entry.functorName + "*";
	return TypeManager::Entry(typeName, typeName, typeName + " %s", typeName + " %s", entry.functorAndCaller);
}

TypeManager::FunctionTypeEntry TypeManager::getFunctionTypeDetails(const core::FunctionTypePtr& functionType) {

	// use cached information
	auto pos = functionTypeDefinitions.find(functionType);
	if (pos != functionTypeDefinitions.end()) {
		return pos->second;
	}

	// create new entry:

	// get name for function type
	string name = nameGenerator.getName(functionType, "funType");
	string functorName = "struct " + name;
	string callerName = "call" + name;

	CodePtr functorAndCaller(new CodeFragment(string("Definitions for function type: ") + name));
	CodeStream& out = functorAndCaller->getCodeStream();

	auto elementPrinter = [&](std::ostream& out, const TypePtr& cur) {
		out << getTypeName(functorAndCaller, cur, true);
	};

	// A) add abstract functor definition
	out << "// Abstract prototype for lambdas of type " << name << "\n";
	out << functorName << " { \n";

	// add function pointer 'fun'
	out << "    " << getTypeName(functorAndCaller, functionType->getReturnType()) << "(*fun)(" << "void*";
	auto arguments = functionType->getArgumentTypes();
	if (!arguments.empty()) {
		out << ", " << join(", ", arguments, elementPrinter);
	}
	out << ");\n";
	// add field for size of concrete type
	out << "    const size_t size;\n";



	// add capture variables
	auto captures = functionType->getCaptureTypes();
	if (!captures.empty()) {
		int i = 0;
		for_each(captures, [&](const TypePtr& cur) {
				out << "    " << this->formatParamter(functorAndCaller, cur, format("p%d", i++), false) << ";\n";
		});
	}

	out << "};\n";


	// B) define caller routine - only functions without captures can be called
	if (captures.empty()) {
		out << "\n";
		out << "// Type safe function for invoking lambdas of type " << name << "\n";
		out << getTypeName(functorAndCaller, functionType->getReturnType());
		out << " " << callerName << "(struct " << name << "* lambda";
		int i = 0;
		if (!arguments.empty()) {
			out << ", " << join(", ", arguments, [&, this](std::ostream& out, const TypePtr& cur) {
				out << formatParamter(functorAndCaller, cur, format("p%d", ++i), true);
			});
		}
		out << ") { return lambda->fun(lambda";
		i = 0;
		if (!arguments.empty()) {
			out << ", " << join(",", arguments, [&, this](std::ostream& out, const TypePtr& cur) {
				out << format("p%d", ++i);
			});
		}
		out << "); }\n";
	}

	// create, register and return entry
	FunctionTypeEntry entry(functorName, callerName, functorAndCaller);
	functionTypeDefinitions.insert(std::make_pair(functionType, entry));
	return entry;
}


TypeManager::Entry TypeManager::resolveRefType(const RefTypePtr& ptr) {
	auto& basic = ptr->getNodeManager().basic;

	// special handling for void* type
	if (ArrayTypePtr arrayType = dynamic_pointer_cast<const ArrayType>(ptr->getElementType())) {
		if (basic.isRefAlpha(arrayType->getElementType())) {
			return toEntry("void*");
		}
	}
	if(basic.isRefAlpha(ptr)) {
		return toEntry("void*");
	}

	return resolveRefOrVectorOrArrayType(ptr);
}


TypeManager::Entry TypeManager::resolveVectorType(const VectorTypePtr& ptr) {
	return resolveRefOrVectorOrArrayType(ptr);
}

TypeManager::Entry TypeManager::resolveArrayType(const ArrayTypePtr& ptr) {
	auto& basic = ptr->getNodeManager().basic;

	// special handling for void* type (array<ref<'a>>)
	if(ptr->getNodeType() == NT_ArrayType && basic.isRefAlpha(static_pointer_cast<const ArrayType>(ptr)->getElementType())) {
		return toEntry("void*");
	}


	return resolveRefOrVectorOrArrayType(ptr);
}


TypeManager::Entry TypeManager::resolveRefOrVectorOrArrayType(const core::TypePtr& ptr) {

	// make sure the passed type is correct
	TypePtr type = ptr;
	NodeType kind = ptr->getNodeType();
	assert (kind == NT_RefType || kind == NT_ArrayType || kind == NT_VectorType);

	// count pointers in front of arrays
	int refCount = 0;
	while(kind == NT_RefType) {
		refCount++;
		type = static_pointer_cast<const RefType>(type)->getElementType();
		kind = type->getNodeType();
	}

	// count arrays
	int arrayCount = 0;
	while(kind == NT_ArrayType) {
		ArrayTypePtr arrayType = static_pointer_cast<const ArrayType>(type);

		// check type of dimension
		IntTypeParam dim = arrayType->getDimension();
		if (dim.getType() != IntTypeParam::CONCRETE) {
			return toEntry("[[ Unsupported generic array types ]]");
		}
		arrayCount += dim.getValue();

		type = arrayType->getElementType();
		kind = type->getNodeType();

		// discard embedded reference type
		if (kind == NT_RefType) {
			type = static_pointer_cast<const RefType>(type)->getElementType();
			kind = type->getNodeType();
		}
	}

	// count vectors
	int vectorCount = 0;
	string postfix = "";
	while(kind == NT_VectorType) {
		vectorCount++;

		VectorTypePtr vectorType = static_pointer_cast<const VectorType>(type);
		postfix = postfix + "[" + toString(vectorType->getSize()) + "]";

		type = vectorType->getElementType();
		kind = type->getNodeType();

		// discard embedded reference type
		if (kind == NT_RefType) {
			type = static_pointer_cast<const RefType>(type)->getElementType();
			kind = type->getNodeType();
		}
	}

	// check for a mixed node
	if (kind == NT_ArrayType || kind == NT_RefType) {
		// mixed mode ... just finish counting and use stars
		while (kind == NT_VectorType || kind==NT_ArrayType || kind == NT_RefType) {
			refCount++;
			type = static_pointer_cast<const SingleElementType>(type)->getElementType();
			kind = type->getNodeType();
		}

		// sum up references
		refCount += arrayCount + vectorCount;

		// reset array and vector counts (if mixed, everything is done via ref)
		arrayCount = 0;
		vectorCount = 0;

		// make an assertion on the result
		assert(refCount > 0 && "RefCount should be larger than 0!");
	}


	// reduce number of references if declaring a C array (implicit in C)
//	refCount -= (decl && kind != NT_VectorType)?1:0;

	// reduce ref-count by 1 (since outermost is implicit in C) - except for pure vectors
	refCount -= (ptr->getNodeType() != NT_VectorType)?1:0;

	// create type declaration
	Entry elementType = resolveType(type);
	string prefix = elementType.lValueName;
	if (vectorCount > 0 && (refCount > 0 || arrayCount > 0)) {
		prefix += "(";
	}
	for (int i=0; i<refCount; i++) {
		prefix += "*";
	}
	for (int i=0; i<arrayCount; i++) {
		prefix += "*";
	}
	if (vectorCount > 0 && (refCount > 0 || arrayCount > 0)) {
		postfix = ")" + postfix;
	}
	//return prefix + " " + name + postfix;
	if (ptr->getNodeType() == NT_VectorType) {
		// special treatement for C vectors
		return Entry(
				prefix + "" + postfix,
				prefix + "" + postfix,
				prefix + " %s" + postfix,
				prefix + " %s" + postfix,
				elementType.definition
		);
	}

	// handling non-C-vector types
	if (vectorCount > 0 && refCount == 0 && arrayCount == 0) {
		return Entry(
				prefix + "" + postfix,
				prefix + "(*)" + postfix,
				prefix + " %s" + postfix,
				prefix + "(* %s)" + postfix,
				elementType.definition
		);
	}
	return Entry(
			prefix + "" + postfix,
			prefix + "*" + postfix,
			prefix + " %s" + postfix,
			prefix + "* %s" + postfix,
			elementType.definition
	);
}

TypeManager::Entry TypeManager::resolveNamedCompositType(const NamedCompositeTypePtr& ptr, string prefix) {

	// fetch name for composed type
	string name = nameGenerator.getName(ptr, "userdefined_type");

	// create a new code fragment for the struct definition
	CodePtr cptr(new CodeFragment(string("type_declaration_") + name));
	CodeStream& out = cptr->getCodeStream();

	// add struct definition
	out << prefix << " " << name << " { \n";
	for_each(ptr->getEntries(), [&, this](const NamedCompositeType::Entry& entry) {
		out << "    " << formatParamter(cptr, entry.second, entry.first.getName(), true) << ";\n";
	});
	out << "};\n";

	string typeName = prefix + " " + name;
	return TypeManager::Entry(typeName, typeName, typeName + " %s", typeName + " %s", cptr);
}

TypeManager::Entry TypeManager::resolveUnionType(const UnionTypePtr& ptr) {
	return resolveNamedCompositType(ptr, "union");
}

TypeManager::Entry TypeManager::resolveStructType(const StructTypePtr& ptr) {
	return resolveNamedCompositType(ptr, "struct");
}



TypeManager::Entry TypeManager::resolveRecType(const core::RecTypePtr& ptr) {

	// resolve recursive type definition
	resolveRecTypeDefinition(ptr->getDefinition());

	// look up type again (now it should be known)
	return resolveType(ptr);
}

void TypeManager::resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr) {

	// create dummy code group depending on all prototype definitions
	CodePtr group = std::make_shared<CodeFragment>("Dummy fragment for recursive type group", true);

	NodeManager& manager = ptr->getNodeManager();

	// A) create prototype and add entry for each recursively defined type
	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {

		// create recursive type using current type variable
		RecTypePtr type = RecType::get(manager, cur.first, ptr);

		// create prototype
		string name = nameGenerator.getName(type, "userdefined_rec_type");

		switch(cur.second->getNodeType()) {
		case NT_StructType:
			name = "struct " + name; break;
		case NT_UnionType:
			name = "union " + name; break;
		default:
			assert(false && "Cannot support recursive type which isn't a struct or union!");
		}

		CodePtr prototype = std::make_shared<CodeFragment>("Prototype of " + name);
		prototype->getCodeStream() << name << ";\n";

		this->typeDefinitions.insert(std::make_pair(type, Entry(name, name, name + " %s", name + " %s", prototype)));

		group->addDependency(prototype);
	});



	// A) unroll types and write definitions
	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {

		// obtain unrolled type
		TypePtr unrolled = ptr->unrollOnce(manager, cur.first);

		// fix name of unrolled struct
		nameGenerator.setName(unrolled, nameGenerator.getName(RecType::get(manager, cur.first, ptr)));

		// resolve unrolled type and add dependency to group
		group->addDependency(resolveType(unrolled).definition);
	});


	// C) update type definition map to reference entire group
	for_each(ptr->getDefinitions(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur) {

		// create recursive type using current type variable
		RecTypePtr type = RecType::get(manager, cur.first, ptr);

		// ... update code pointer to reference entire group
		this->typeDefinitions.find(type)->second.definition = group;
	});

}

} // end: namespace simple_backend
} // end: namespace insieme

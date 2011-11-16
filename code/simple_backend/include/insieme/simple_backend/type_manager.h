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

#include <boost/unordered_map.hpp>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_types.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/simple_backend/name_manager.h"
#include "insieme/simple_backend/code_management.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace simple_backend {

// a forward declaration for the backend converter
class Converter;

/**
 * Manages C type generation and lookup for IR types.
 */
class TypeManager {

	/**
	 * A reference to the converter this type manager is contributing to.
	 */
	Converter& converter;

public:

	/**
	 * A structure including all the information maintained per type.
	 */
	struct TypeInfo {

		// a named used for types that are not supported by the simple backend
		static const string UNSUPPORTED;

		// the C-name of this type if used for declaring a new variable of this type
		string lValueName;
		// the C-name of this type when being passed as an argument or returned as a value
		string rValueName;

		// the pattern to be used for declaring variables of this type - %s will replaced with the variable name
		string declPattern;
		// the pattern to be used for defining a parameters of this type - %s will replaced with the variable name
		string paramPattern;

		// the name of the type when being passed to some external function
		string externName;
		// the pattern to be used to define a parameter of this type for some external function
		string externPattern;
		// a pattern to be used when passing a value of the internal type to an external function
		string externalizingPattern;

		// the code fragment which might contain the type declaration
		CodeFragmentPtr declaration;

		// the code fragment which might contain the type definition
		CodeFragmentPtr definition;

		// a code fragment including utilities like constructors for this type
		CodeFragmentPtr utilities;

		TypeInfo()
			: lValueName(UNSUPPORTED), rValueName(UNSUPPORTED), declPattern(UNSUPPORTED), paramPattern(UNSUPPORTED),
				externName(UNSUPPORTED), externPattern(UNSUPPORTED), externalizingPattern(UNSUPPORTED), declaration(),
				definition(), utilities() { }

		TypeInfo(const string& lName, const string& rName, const string& declPattern, const string& paramPattern)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			    externName(rName), externPattern(paramPattern), externalizingPattern("%s"), declaration(),
				definition(), utilities() { }

		TypeInfo(const string& lName, const string& rName, const string& declPattern, const string& paramPattern, const CodeFragmentPtr& definition)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			  externName(rName), externPattern(paramPattern), externalizingPattern("%s"), declaration(definition),
			  definition(definition), utilities(definition) { }

		TypeInfo(const string& lName, const string& rName, const string& declPattern, const string& paramPattern, const CodeFragmentPtr& declaration, const CodeFragmentPtr& definition)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			  externName(rName), externPattern(paramPattern), externalizingPattern("%s"), declaration(declaration),
			  definition(definition), utilities(definition) { }

		TypeInfo(
				const string& lName, const string& rName, const string& declPattern, const string& paramPattern,
				const string& externName, const string& externPattern, const string& externalizingPattern)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			  externName(externName), externPattern(externPattern), externalizingPattern(externalizingPattern) { }

		TypeInfo(
				const string& lName, const string& rName, const string& declPattern, const string& paramPattern,
				const string& externName, const string& externPattern, const string& externalizingPattern,
				const CodeFragmentPtr& definition)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			  externName(externName), externPattern(externPattern), externalizingPattern(externalizingPattern),
			  declaration(definition), definition(definition), utilities(definition) { }

		TypeInfo(
				const string& lName, const string& rName, const string& declPattern, const string& paramPattern,
				const string& externName, const string& externPattern, const string& externalizingPattern,
				const CodeFragmentPtr& declaration, const CodeFragmentPtr& definition, const CodeFragmentPtr& utilities)
			: lValueName(lName), rValueName(rName), declPattern(declPattern), paramPattern(paramPattern),
			  externName(externName), externPattern(externPattern), externalizingPattern(externalizingPattern),
			  declaration(declaration), definition(definition), utilities(utilities) { }
	};

	/**
	 * A struct maintaining extra-information regarding function types.
	 */
	struct FunctionTypeInfo {

		/**
		 * The (super-) type of any closure representing a function of this type.
		 */
		const string closureName;

		/**
		 * The name of the function which can be used to invoke closures of the corresponding function type.
		 */
		const string callerName;

		/**
		 * The code fragment containing the definition of the closure and the caller.
		 */
		const CodeFragmentPtr definitions;

		FunctionTypeInfo() { }

		FunctionTypeInfo(const string& closureName, const string& callerName, const CodeFragmentPtr& definitions)
			: closureName(closureName), callerName(callerName), definitions(definitions) { }
	};

private:

	/**
	 * A map linking IR types to type definitions.
	 */
	utils::map::PointerMap<core::TypePtr, TypeInfo> typeDefinitions;

	/**
	 * A map linking a function type to the constructs defined for this type within C.
	 */
	utils::map::PointerMap<core::FunctionTypePtr, FunctionTypeInfo> functionTypeDefinitions;

public:

	TypeManager(Converter& converter) : converter(converter) { }

	/**
	 * Obtains a type name for the given type. In case the type has been used before, the same
	 * name as the last time will be returned. Otherwise a new name will be generated, the necessary
	 * type definitions will be added, dependencies will be registered and the new name will
	 * be returned.
	 *
	 * @param context the code fragment using the given type
	 * @param type the type to be resolved
	 * @param a flag allowing the user to indicate whether the type definition is required for a declaration statement or for not
	 * @return the token to be used within a C program to describe this type
	 */
	string getTypeName(const CodeFragmentPtr& context, const core::TypePtr& type, bool decl = false);

	/**
	 * Obtains a entry maintained for the given type. In case the type has been used before, the same
	 * entry as the last time will be returned. Otherwise a new entry will be resolved, the necessary
	 * type definitions will be added, dependencies will be registered and the new entry will
	 * be returned.
	 *
	 * @param context the code fragment using the given type
	 * @param type the type to be resolved
	 * @return the entry summarizing the information required for representing the given type within C
	 */
	const TypeInfo getTypeInfo(const CodeFragmentPtr& context, const core::TypePtr& type);

	/**
	 * Formats a parameter with the given type and name within the given context. This might be used
	 * for formatting function parameters and elements of a user defined type (struct and unions).
	 *
	 * @param context the context the resulting parameter is placed in
	 * @param type the type of the parameter to be formatted
	 * @param name the name of the resulting parameter
	 * @param a flag allowing the user to indicate whether the type definition is required for a declaration statement or for not
	 */
	string formatParamter(const CodeFragmentPtr& context, const core::TypePtr& type, const string& name, bool decl = false);

	/**
	 * Produces a definition of a real C-Function pointer to a function representing a valid wrapper of a closure of the given
	 * function type. The resulting declaration will declare a variable with the given name.
	 *
	 * TODO: find a better place for this function
	 */
	string formatFunctionPointer(const CodeFragmentPtr& context, const core::FunctionTypePtr& funType, const string& name);

	/**
	 * Obtains information regarding the given function type. The resulting entry contains
	 * all the code fragments included within the resulting program for the given function type.
	 *
	 * @param functionType the function type to be looked for
	 * @return a collection of information regarding the code generated for the given type
	 */
	FunctionTypeInfo getFunctionTypeInfo(const core::FunctionTypePtr& functionType);

private:

	NameManager& getNameManager() const;

	TypeInfo resolveType(const core::TypePtr& ptr);

	TypeInfo resolveGenericType(const core::GenericTypePtr& ptr);
	TypeInfo resolveStructType(const core::StructTypePtr& ptr);
	TypeInfo resolveUnionType(const core::UnionTypePtr& ptr);
	TypeInfo resolveNamedCompositType(const core::NamedCompositeTypePtr& ptr, string prefix);

	TypeInfo resolveFunctionType(const core::FunctionTypePtr& ptr);

	TypeInfo resolveArrayType(const core::ArrayTypePtr& ptr);
	TypeInfo resolveVectorType(const core::VectorTypePtr& ptr);
	TypeInfo resolveChannelType(const core::ChannelTypePtr& ptr);
	TypeInfo resolveRefType(const core::RefTypePtr& ptr);

	TypeInfo resolveRefOrVectorOrArrayType(const core::TypePtr& ptr);

	TypeInfo resolveRecType(const core::RecTypePtr& ptr);
	void resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr);
};

} // end: namespace simple_backend
} // end: namespace insieme

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

#include "insieme/core/ast_node.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/simple_backend/name_generator.h"
#include "insieme/simple_backend/code_management.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace simple_backend {

/**
 * Manages C type generation and lookup for IR types.
 */
class TypeManager {

	/**
	 * The name generator used for deriving fresh type names.
	 */
	NameGenerator& nameGenerator;

public:

	/**
	 * A pair consisting of a type name and its definition is forming an entry.
	 */
	struct Entry {
		string lValueName;
		string rValueName;
		CodePtr definition;

		Entry() : lValueName(), rValueName(), definition() { }
		Entry(const string& lName, const string& rName, const CodePtr& definition)
			: lValueName(lName), rValueName(rName), definition(definition) { }
	};

	/**
	 * The information stored regarding each function type.
	 */
	struct FunctionTypeEntry {
		string functorName;
		string callerName;
		CodePtr functorAndCaller;

		FunctionTypeEntry() { }
		FunctionTypeEntry(const string& functorName, const string& callerName, const CodePtr& functorAndCaller)
			: functorName(functorName), callerName(callerName), functorAndCaller(functorAndCaller) { }
	};

private:

	/**
	 * A map linking IR types to type definitions.
	 */
	utils::map::PointerMap<core::TypePtr, Entry> typeDefinitions;

	/**
	 * A map linking a function type to the constructs defined for this type within C.
	 */
	utils::map::PointerMap<core::FunctionTypePtr, FunctionTypeEntry> functionTypeDefinitions;

public:

	TypeManager(NameGenerator& nameGenerator) : nameGenerator(nameGenerator) { }

	/**
	 * Obtains a type name for the given type. I case the type has been used before, the same
	 * name as the last time will be returned. Otherwise a new name will be generated, the necessary
	 * type definitions will be added, dependencies will be registered and the new name will
	 * be returned.
	 *
	 * @param context the code fragment using the given type
	 * @param type the type to be resolved
	 * @param a flag allowing the user to indicate whether the type definition is required for a declaration statement or for not
	 * @return the token to be used within a C program to describe this type
	 */
	string getTypeName(const CodePtr& context, const core::TypePtr& type, bool decl = false);

	/**
	 * Formats the a parameter with the given type and name within the given context. This might be used
	 * for formatting function parameters and elements of a user defined type (struct and unions).
	 *
	 * @param context the context the resulting parameter is placed in
	 * @param type the type of the parameter to be formatted
	 * @param name the name of the resulting parameter
	 * @param a flag allowing the user to indicate whether the type definition is required for a declaration statement or for not
	 */
	string formatParamter(CodePtr& context, const core::TypePtr& type, const string& name, bool decl = false);

	/**
	 * Obtains information regarding the given function type. The resulting entry contains
	 * all the code fragments included within the resulting program for the given function type.
	 *
	 * @param functionType the function type to be looked for
	 * @return a collection of information regarding the code generated for the given type
	 */
	FunctionTypeEntry getFunctionTypeDetails(const core::FunctionTypePtr& functionType);

private:

	Entry resolveType(const core::TypePtr& ptr);

	Entry resolveGenericType(const core::GenericTypePtr& ptr);
	Entry resolveStructType(const core::StructTypePtr& ptr);
	Entry resolveUnionType(const core::UnionTypePtr& ptr);
	Entry resolveNamedCompositType(const core::NamedCompositeTypePtr& ptr, string prefix);

	Entry resolveFunctionType(const core::FunctionTypePtr& ptr);

	Entry resolveArrayType(const core::ArrayTypePtr& ptr);
	Entry resolveVectorType(const core::VectorTypePtr& ptr);
	Entry resolveChannelType(const core::ChannelTypePtr& ptr);
	Entry resolveRefType(const core::RefTypePtr& ptr);

	Entry resolveRecType(const core::RecTypePtr& ptr);
	void resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr);
};

} // end: namespace simple_backend
} // end: namespace insieme

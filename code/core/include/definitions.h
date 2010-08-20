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

#include <stdexcept>
#include <string>
#include <vector>

#include "instance_manager.h"
#include "expressions.h"
#include "statements.h"
#include "string_utils.h"
#include "types.h"
#include "types_utils.h"

using std::string;
using std::vector;


class Definition;
typedef AnnotatedPtr<const Definition> DefinitionPtr;

class DefinitionManager : public InstanceManager<DefinitionManager, Definition, AnnotatedPtr> {

	StatementManager& statementManager;
	TypeManager& typeManager;

public:

	DefinitionManager(StatementManager& statementManager) :
		statementManager(statementManager),
		typeManager(statementManager.getTypeManager())
	{};


	TypeManager& getTypeManager() const {
		return typeManager;
	}

	StatementManager& getStatementManager() const {
		return statementManager;
	}
};


class Definition {

	friend class InstanceManager<DefinitionManager, Definition, AnnotatedPtr>;

	/**
	 * The name to be defined.
	 */
	const Identifier name;

	/**
	 * The type of the newly defined element.
	 */
	const TypePtr type;

	/**
	 * A flag marking externally defined elements, hence elements which can not
	 * be altered by the Insieme Compiler.
	 */
	const bool external;

	/**
	 * An expression defining the value for the newly introduced element. In case
	 * the definition is external, this pointer may be NULL.
	 */
	const ExprPtr definition;

	/**
	 * The hash code of this immutable data element. It is evaluated during construction and based
	 * on the name and the type, since those two elements make a defined element unique.
	 */
	const std::size_t hashCode;

	Definition(const Identifier& name, const TypePtr& type, const bool& external,
			const ExprPtr& definition, const std::size_t& hashCode);

	Definition* clone(DefinitionManager& manager) const;

public:

	static DefinitionPtr get(DefinitionManager& manager, const Identifier& name, const TypePtr& type, const ExprPtr& definition, bool external);

	static DefinitionPtr get(DefinitionManager& manager, const Identifier& name, const TypePtr& type);


	const Identifier& getName() const {
		return name;
	}

	const TypePtr& getType() const {
		return type;
	}

	const ExprPtr& getDefinition() const {
		return definition;
	}

	bool isExternal() const {
		return external;
	}

	bool isAbstract() const {
		return !definition;
	}

	bool operator==(const Definition& other) const;

	const std::size_t& hash() const {
		return hashCode;
	}

};


// ---------------------------------------------- Utilities ------------------------------------

/**
 * Integrates the hash code computation into the boost hash code framework.
 *
 * @param definition the definition for which a hash code should be computed.
 * @return the hash code of the given definition
 */
std::size_t hash_value(const Definition& definition);

/**
 * Allows this definition to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Definition& type);

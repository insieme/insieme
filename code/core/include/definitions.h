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

#include "ast_node.h"
#include "instance_manager.h"
#include "expressions.h"
#include "statements.h"
#include "string_utils.h"
#include "types.h"
#include "types_utils.h"

using std::string;
using std::vector;

namespace insieme {
namespace core {

DECLARE_NODE_TYPE(Definition);


class DefinitionManager : public InstanceManager<Definition, AnnotatedPtr> {

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

/**
 * Definitions are top-level elements within an IR program. Each definition is defining
 * an element by specifying its name and type. Further, a defining expression as well as
 * a flag indicating whether it is externally defined / used may be added optionally.
 *
 * NOTE: definitions are uniquely identified by their name and type.
 */
class Definition : public Node {

	/**
	 * Allow the instance manager to access the private clone method.
	 */
	friend class InstanceManager<Definition, AnnotatedPtr>;

public:

	/**
	 * The type of instance manager to be used with this type.
	 */
	typedef DefinitionManager Manager;

private:

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
	 * A private constructor for this type creating a new element based on the given
	 * properties.
	 *
	 * @param name the name of the element to be defined by this definition
	 * @param type the type of the newly defined element.
	 * @param external a flag indicating whether this definition is externally defined / used.
	 * @param definition the actual value assigned. It has to be an expression of the specified type.
	 * @param hashCode the hash code of the new element (which will be cached). The value is accepted from
	 * 				   an external source since this constructor is private.
	 */
	Definition(const Identifier& name, const TypePtr& type, const bool& external,
			const ExprPtr& definition, const std::size_t& hashCode);

	/**
	 * Creates a clone of this definition within the given definition manager.
	 *
	 * @param manager the manager to create a clone within.
	 */
	Definition* clone(DefinitionManager& manager) const;

public:

	/**
	 * A static factory method obtaining a pointer to a definition within the given manager.
	 *
	 * NOTE: definitions are uniquely identified by their name and type.
	 *
	 * @param name the name of the element to be defined by the obtained definition.
	 * @param type the type of the element to be defined by the obtained definition.
	 * @param definition the expression providing a definition of the value, may be NULL if external.
	 * @param external a flag indicating whether the definition to be looking for is externally defined / used.
	 */
	static DefinitionPtr get(DefinitionManager& manager, const Identifier& name, const TypePtr& type, const ExprPtr& definition = NULL, bool external = false);

	/**
	 * A static factory method which will only look up a definition. The given name and type will be taken to obtain
	 * a pointer to a corresponding element within the given manager. If there is no such element, a NULL-pointer will
	 * be returned.
	 *
	 * @param manager the manager within which the element should be searched.
	 * @param type the type of element to be looking for.
	 */
	static DefinitionPtr lookup(DefinitionManager& manager, const Identifier& name, const TypePtr& type);

	/**
	 * Obtain the name of the element defined by this definition.
	 *
	 * @return the name of the defined element.
	 */
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

	bool equals(const Node& other) const;

};

} // end namespace core
} // end namespace insieme


// ---------------------------------------------- Utilities ------------------------------------

/**
 * Allows this definition to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Definition& type);

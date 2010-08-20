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

/*
 * functions.h
 *
 *  Defines the class used to represent and manage functions within the IR.
 *
 *  Created on: Aug 5, 2010
 *      Author: Herbert Jordan
 */
#pragma once

#include <stdexcept>
#include <string>
#include <vector>

#include "instance_manager.h"
#include "statements.h"
#include "string_utils.h"
#include "types.h"
#include "types_utils.h"

using std::string;
using std::vector;



class Function;
typedef AnnotatedPtr<const Function> FunctionPtr;

class FunctionManager : public InstanceManager<FunctionManager, const Function, FunctionPtr> {

	StatementManager& statementManager;
	TypeManager& typeManager;

public:

	FunctionManager(StatementManager& statementManager) :
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


class Function {

	friend class InstanceManager<FunctionManager, const Function, FunctionPtr>;

public:

	/**
	 * The type used to represent a function parameter.
	 */
	typedef std::pair<Identifier, TypePtr> Parameter;

	/**
	 * The type used to represent an entire parameter list.
	 */
	typedef vector<Parameter> ParameterList;

private:

	/**
	 * The name of this function.
	 */
	const Identifier name;

	/**
	 * The type of this function, derived from the list of parameters and the return type.
	 */
	const FunctionTypePtr type;

	/**
	 * The list of parameters accepted by this function
	 */
	const vector<Identifier> parameters;

	/**
	 * A flag determining whether this function is defined externally.
	 */
	const bool external;

	/**
	 * The statement forming the body of the represented function. If the function is external,
	 * the body might be NULL. The body of a function being marked as an external should not be altered.
	 */
	const StmtPtr body;

	/**
	 * The hash code of this immutable data element, evaluated during construction.
	 */
	const std::size_t hashCode;


	Function(const Identifier& name, const FunctionTypePtr& type, const vector<Identifier>& parameter,
			bool external=true, const StmtPtr& body=NULL);

	Function(FunctionManager& manager, const Function& function);

	Function* clone(FunctionManager& manager) const {
		return new Function(manager, *this);
	}

public:


	static FunctionPtr get(FunctionManager& manager, const Identifier& name, const ParameterList& paramList, const TypePtr& returnType);

	static FunctionPtr get(FunctionManager& manager, const Identifier& name, const ParameterList& paramList = ParameterList()) {
		return get(manager, name, paramList, UnitType::get(manager.getTypeManager()));
	}

	const Identifier& getName() const {
		return name;
	}

	const FunctionTypePtr& getType() const {
		return type;
	}

	const StmtPtr& getBody() const {
		return body;
	}

	bool isExternal() const {
		return external;
	}

	bool operator==(const Function& other) const {
		// shortcut for identical entries
		if (this == &other) {
			return true;
		}

		// shortcut for not-matching entries
		if (hashCode != other.hashCode) {
			return false;
		}

		// compare type and name - which makes a function unique
		return type == other.type && name == other.name;
	}

	const std::size_t& hash() const {
		return hashCode;
	}
};

// ---------------------------------------------- Utilities ------------------------------------

/**
 * Integrates the hash code computation into the boost hash code framework.
 *
 * @param function the function for which a hash code should be computed.
 * @return the hash code of the given function
 */
std::size_t hash_value(const Function& function);

/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Function& type);

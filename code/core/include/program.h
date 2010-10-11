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

#include <memory>
#include <ostream>
#include <unordered_set>

//#include <boost/unordered_set.hpp>

#include "ast_node.h"
#include "container_utils.h"
#include "expressions.h"
#include "functional_utils.h"
#include "set_utils.h"
#include "statements.h"
#include "types.h"

namespace insieme {
namespace core {

// forward declaration of the program class and pointer
DECLARE_NODE_TYPE(Program);


class ASTBuilder;

/**
 * This class implements an AST Node which can be used to represent entire programs. Unlike
 * other nodes, this node cannot be maintained within a node manager (invoking this will lead
 * to an assertion error).
 *
 * Programs and especially ProgramPtrs can be used like values. Generally, they are immutable
 * and can be copied / passed as an argument or return value of a function. Programs manage
 * the memory allocation of all its referenced nodes implicitly using a shared NodeManager.
 * This ensures that by passing a program between methods, no referenced AST nodes will be
 * lost.
 */
class Program : public Node {

public:

	/**
	 * The type used to represent the list of entry points.
	 */
	typedef std::unordered_set<ExpressionPtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> EntryPointSet;

private:

	/**
	 * This set contains the list of expressions to be exported to the context
	 * program. Hence, the code which can be reached starting from those points
	 * has to be considered. In case elements of this list represent functions,
	 * the signature of the corresponding structure may not be changed.
	 */
	const EntryPointSet entryPoints;

	/**
	 * Creates a new AST using a fresh AST node manager.
	 */
	Program();

	/**
	 * Creates a new AST based on the given data.
	 *
	 * @param entryPoints the list of entry points the program is consisting of.
	 */
	Program(const EntryPointSet& entryPoints);

	/**
	 * Implements the clone method defined by the Base Node class. However,
	 * invoking this method will lead to an assertion error, since programs
	 * should and cannot be migrated between managers (Programs are not maintained
	 * by managers at all).
	 *
	 * @param manager the manager this program should be cloned to
	 * @return a pointer to the new program, cloned for the new manager
	 */
	virtual Program* createCloneUsing(NodeManager& manager) const;

protected:

	/**
	 * Obtains a list of all nodes referenced by this program node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * Creates a new program node within the given manager combining the given set of entry points.
	 *
	 * @param manager the manager used to create the new node and to maintain all referenced nodes
	 * @param entryPoints the list of entry points to be included within the resulting program.
	 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
	 * 						will be bound to the given manager.
	 */
	static ProgramPtr create(NodeManager& manager, const EntryPointSet& entryPoints = EntryPointSet());

	/**
	 * Creates a new program node within the given manager which is equivalent to the given program plus the
	 * given, additional entry point.
	 *
	 * @param manager the manager used to create the new node and to maintain all referenced nodes
	 * @param program the program to be extended by an additional entry point
	 * @param point the additional entry point to be added
	 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
	 * 						will be bound to the given manager.
	 */
	static ProgramPtr addEntryPoint(NodeManager& manager, const ProgramPtr& program, const ExpressionPtr& point);

	/**
	 * Creates a new program node within the given manager which is equivalent to the given program plus the
	 * given, additional entry points.
	 *
	 * @param manager the manager used to create the new node and to maintain all referenced nodes
	 * @param program the program to be extended by additional entry points
	 * @param points the additional entry points to be added
	 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
	 * 						will be bound to the given manager.
	 */
	static ProgramPtr addEntryPoints(NodeManager& manager, const ProgramPtr& program, const EntryPointSet& points);

	/**
	 * Creates a new program node within the given manager which is equivalent to the given program except the
	 * given entry point will be removed.
	 *
	 * @param manager the manager used to create the new node and to maintain all referenced nodes
	 * @param program the program to be reduced by an entry point
	 * @param point the entry point to be removed
	 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
	 * 						will be bound to the given manager.
	 */
	static ProgramPtr remEntryPoint(NodeManager& manager, const ProgramPtr& program, const ExpressionPtr& point);

	/**
	 * Creates a new program node within the given manager which is equivalent to the given program except the
	 * given entry points will be removed.
	 *
	 * @param manager the manager used to create the new node and to maintain all referenced nodes
	 * @param program the program to be reduced by some entry points
	 * @param points the entry points to be removed
	 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
	 * 						will be bound to the given manager.
	 */
	static ProgramPtr remEntryPoints(NodeManager& manager, const ProgramPtr& program, const EntryPointSet& points);

	/**
	 * Obtains the set of entry points associated to this program node.
	 *
	 * @return the set of entry points.
	 */
	const EntryPointSet& getEntryPoints() const {
		return entryPoints;
	}

	/**
	 * Implements equals for the program type. Two programs are considered
	 * equal if they consist of the same set of definitions and entry points.
	 */
	bool equals(const Node& other) const;

	/**
	 * Implements the printing of this program into the given output stream.
	 */
	virtual std::ostream& printTo(std::ostream& out) const;

	// TODO: add consistency check routine!!
	//   - check: all names defined in scope
	//   - no duplicates in switch
	//   - no duplicates in names composite types

};

} // end namespace core
} // end namespace insieme


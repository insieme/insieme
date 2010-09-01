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
#include "definitions.h"
#include "set_utils.h"
#include "statements.h"
#include "types.h"

namespace insieme {
namespace core {

// forward declaration of the program class
class ProgramPtr;

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
	 * The type used to represent the list of top level definitions.
	 */
	typedef std::unordered_set<DefinitionPtr, hash_target<DefinitionPtr>, equal_target<DefinitionPtr>> DefinitionSet;

	/**
	 * The type used to represent the list of entry points.
	 */
	typedef std::unordered_set<ExpressionPtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> EntryPointSet;

private:

	/**
	 * The shared data manager used to maintain nodes within this AST.
	 */
	const SharedNodeManager nodeManager;

	/**
	 * The set of definitions represented by this AST. Each definition
	 * represents a root node of a tree within this AST (which is actually
	 * a forest).
	 */
	const DefinitionSet definitions;

	/**
	 * This set contains the list of expressions to be exported to the context
	 * program. Hence, the code which can be reached starting from those points
	 * has to be considered. In case elements of this list represent functions,
	 * the signature of the corresponding structure may not be changed.
	 */
	const EntryPointSet entryPoints;

	/**
	 * Creates a new AST based on the given data.
	 *
	 * @param dataManager shared data manager to be used to maintain definitions, AST nodes and types.
	 * @param definitions the list of top-level definitions the program to be represented should consist of
	 * @param entryPoints the list of entry points the program is supporting.
	 */
	Program(SharedNodeManager dataManager, const DefinitionSet& definitions, const EntryPointSet& entryPoints);

	/**
	 * Creates a new AST using a fresh AST data instance.
	 */
	Program();

	/**
	 * Implements the clone method defined by the Base Node class. However,
	 * invoking this method will lead to an assertion error, since programs
	 * should and cannot be migrated between managers (Programs are not maintained
	 * by managers at all).
	 *
	 * @param manager the manager this program should be cloned to
	 * @return a pointer to the new program, cloned for the new manager
	 */
	virtual Program* clone(NodeManager& manager) const;

public:

	/**
	 * Creates a new program based on the given definitions and entry points.
	 *
	 * @param definitions the definitions to be included within the resulting program.
	 * @param entryPoints the list of entry points to be included within the resulting program.
	 * @return a ProgramPtr referencing the resulting program. The pointer can be treated like a shared
	 * 		   pointer, hence the allocated memory will be automatically cleared as soon as the last
	 * 		   copy is gone.
	 */
	static ProgramPtr create(const DefinitionSet& definitions = DefinitionSet(), const EntryPointSet& entryPoints = EntryPointSet());

	ProgramPtr addDefinition(const DefinitionPtr& definition) const;

	ProgramPtr addDefinitions(const DefinitionSet& definitions) const;

	const DefinitionSet& getDefinitions() const {
		return definitions;
	}

	ProgramPtr remDefinition(const DefinitionPtr& definition) const;

	ProgramPtr remDefinitions(const DefinitionSet& definitions) const;


	ProgramPtr addEntryPoint(const ExpressionPtr& point) const;

	ProgramPtr addEntryPoints(const EntryPointSet& points) const;

	const EntryPointSet& getEntryPoints() const {
		return entryPoints;
	}

	ProgramPtr remEntryPoint(const ExpressionPtr& point) const;

	ProgramPtr remEntryPoints(const EntryPointSet& points) const;

	SharedNodeManager getNodeManager() const {
		return nodeManager;
	}

	ASTBuilder getASTBuilder() const;

	/**
	 * Implements equals for the program type. Two programs are considered
	 * equal if they consist of the same set of definitions and entry points.
	 */
	bool equals(const Node& other) const;

	// TODO: add consistency check routine!!
	//   - check: all names defined in scope
	//   - no duplicates in switch
	//   - no duplicates in names composite types

};

/**
 * The Program Pointer class is an extended variant of the annotated pointer.
 * It can be set up to maintain a shared pointer to its referenced program. In
 * case the last program Pointer with a shared semantic is eliminated, the
 * the referenced program will be freed.
 */
class ProgramPtr : public AnnotatedPtr<const Program> {

	/**
	 * The additional shared pointer used to maintain an optional reference count for
	 * the referenced program. If the shared-feature should not be used, this pointer
	 * will point to NULL.
	 */
	std::shared_ptr<const Program> program;

public:

	/**
	 * Creates a new program pointer referencing the given program. The boolean
	 * flag allows to determine whether the smart-pointer feature should be enabled.
	 *
	 * @param ptr the program to be referenced
	 * @param shared if set to true, the referenced program will be deleted as soon
	 * 				 as the last copy of this pointer is removed. If set to false,
	 * 				 this pointer behavior equals any other annotated pointer.
	 */
	ProgramPtr(const Program* ptr, bool shared = false)
		: AnnotatedPtr<const Program>(ptr), program((shared)?ptr:NULL) {}

	/**
	 * A implicit conversion constructor to convert a compatible annotated pointer
	 * into a program pointer. The automatic memory management of the resulting
	 * shared pointer is never enabled.
	 *
	 * @param pointer the pointer to be converted.
	 */
	ProgramPtr(const AnnotatedPtr<const Program>& pointer)
		: AnnotatedPtr<const Program>(pointer) {};
};

} // end namespace core
} // end namespace insieme


/**
 * Allows programs to be printed to an output stream.
 *
 * NOTE: this mechanism should not be used to dump / pretty print a program.
 *
 * @param out the stream to be printed to
 * @param program the program to be printed
 * @return the given output stream
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Program& program);


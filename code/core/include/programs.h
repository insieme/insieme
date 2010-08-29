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

#include "container_utils.h"
#include "expressions.h"
#include "functional_utils.h"
#include "definitions.h"
#include "set_utils.h"
#include "statements.h"
#include "types.h"


namespace insieme {
namespace core {


class Program;
typedef std::shared_ptr<Program> ProgramPtr;

class ProgramDataManager {

	TypeManager typeManager;
	StatementManager stmtManager;
	DefinitionManager definitionManager;

	// TODO: add lookup table for variables!!

public:
	ProgramDataManager() : typeManager(), stmtManager(typeManager), definitionManager(stmtManager) {}


	operator TypeManager&() {
		return typeManager;
	}

	operator StatementManager&() {
		return stmtManager;
	}

	operator DefinitionManager&() {
		return definitionManager;
	}

	TypeManager& getTypeManager() {
		return typeManager;
	}

	StatementManager& getStatementManager() {
		return stmtManager;
	}

	DefinitionManager& getDefinitionManager() {
		return definitionManager;
	}

};



class Program {

public:

	/**
	 * The type used to share statement/type nodes and information among multiple
	 * program versions.
	 */
	typedef std::shared_ptr<ProgramDataManager> SharedDataManager;

	/**
	 * The type used to represent the list of top level definitions.
	 */
	typedef std::unordered_set<DefinitionPtr, hash_target<DefinitionPtr>, equal_target<DefinitionPtr>> DefinitionSet;

	/**
	 * The type used to represent the list of entry points.
	 */
	typedef std::unordered_set<ExprPtr, hash_target<ExprPtr>, equal_target<ExprPtr>> EntryPointSet;

private:

	/**
	 * The shared data manager used to maintain nodes within this AST.
	 */
	const SharedDataManager dataManager;

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
	Program(SharedDataManager dataManager, const DefinitionSet& definitions, const EntryPointSet& entryPoints) :
		dataManager(dataManager), definitions(definitions), entryPoints(entryPoints) { };

	/**
	 * Creates a new AST using a fresh AST data instance.
	 */
	Program() : dataManager(SharedDataManager(new ProgramDataManager())) {}

public:


	static ProgramPtr createProgram(const DefinitionSet& definitions = DefinitionSet(), const EntryPointSet& entryPoints = EntryPointSet());

	ProgramPtr addDefinition(const DefinitionPtr& definition) const;

	ProgramPtr addDefinitions(const DefinitionSet& definitions) const;

	const DefinitionSet& getDefinitions() const {
		return definitions;
	}

	ProgramPtr remDefinition(const DefinitionPtr& definition) const;

	ProgramPtr remDefinitions(const DefinitionSet& definitions) const;


	ProgramPtr addEntryPoint(const ExprPtr& definition) const;

	ProgramPtr addEntryPoints(const EntryPointSet& definitions) const;

	const EntryPointSet& getEntryPoints() const {
		return entryPoints;
	}

	ProgramPtr remEntryPoint(const ExprPtr& definition) const;

	ProgramPtr remEntryPoints(const EntryPointSet& definitions) const;

	SharedDataManager getDataManager() {
		return dataManager;
	}

	// TODO: add consistency check routine!!
	//   - check: all names defined in scope
	//   - no duplicates in switch
	//   - no duplicates in names composite types

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


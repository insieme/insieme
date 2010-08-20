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

#include "expressions.h"
#include "definitions.h"
#include "statements.h"
#include "types.h"

class ASTData {

	TypeManager typeManager;
	StatementManager stmtManager;
	DefinitionManager definitionManager;

	// TODO: add lookup table for variables!!

public:
	ASTData() : typeManager(), stmtManager(typeManager), definitionManager(stmtManager) {}


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



class AST {

	/**
	 * The type used to share AST nodes and information among multiple
	 * AST versions.
	 */
	typedef shared_ptr<ASTData> SharedASTData;

	/**
	 * The type used to represent the list of top level definitions.
	 */
	typedef vector<DefinitionPtr> DefinitionList;

	/**
	 * The type used to represent the list of entry points.
	 */
	typedef vector<ExprPtr> EntryPointList;

	/**
	 * The manager used to maintain nodes within this AST.
	 */
	const SharedASTData manager;

	/**
	 * The list of definitions represented by this AST. Each definition
	 * represents a root node of a tree within this AST (which is actually
	 * a forest).
	 */
	DefinitionList definitionList;

	/**
	 * This set contains the list of expressions to be exported to the context
	 * program. Hence, the code which can be reached starting from those points
	 * has to be considered. In case elements of this list represent functions,
	 * the signature of the corresponding structure may not be changed.
	 */
	EntryPointList entryPoints;

public:

	/**
	 * Creates a new AST based on the given data.
	 *
	 * @param manager shared data manager to be used to maintain definitions, AST nodes and types.
	 * @param definitions the list of top-level definitions the program to be represented should consist of
	 * @param entryPoints the list of entry points the program is supporting.
	 */
	AST(SharedASTData& manager, const DefinitionList& definitions, const EntryPointList& entryPoints) :
		manager(manager),
		definitionList(manager->getDefinitionManager().getAll(definitions)),
		entryPoints(manager->getStatementManager().getAll(entryPoints)) {}

	/**
	 * Creates a new AST using a fresh AST data instance.
	 */
	AST() : manager(SharedASTData(new ASTData())) {}

};

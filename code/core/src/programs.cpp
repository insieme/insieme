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

#include "programs.h"

#include <algorithm>
#include "set_utils.h"

using namespace insieme::utils::set;


Program::DefinitionSet getAll(ProgramData& data, const Program::DefinitionSet& definitions) {
	Program::DefinitionSet res;
	DefinitionManager& manager = data.getDefinitionManager();
	std::for_each(definitions.cbegin(), definitions.cend(),
		[&res, &manager](const DefinitionPtr& cur) {
			res.insert(manager.get(cur));
	});
	return res;
}

Program::EntryPointSet getAll(ProgramData& data, const Program::EntryPointSet& entryPoints) {
	Program::EntryPointSet res;
	StatementManager& manager = data.getStatementManager();
	std::for_each(entryPoints.cbegin(), entryPoints.cend(),
		[&res, &manager](const ExprPtr& cur) {
			res.insert(manager.get(cur));
	});
	return res;
}


ProgramPtr Program::createProgram(const DefinitionSet& definitions, const EntryPointSet& entryPoints) {
	return ProgramPtr(new Program(SharedProgramData(new ProgramData()), definitions, entryPoints));
}

ProgramPtr Program::addDefinition(const DefinitionPtr& definition) const {
	return addDefinitions(toSet<DefinitionSet>(definition));
}

ProgramPtr Program::addDefinitions(const DefinitionSet& definitions) const {
	return ProgramPtr(new Program(sharedData, merge(this->definitions, getAll(*sharedData, definitions)), entryPoints));
}

ProgramPtr Program::remDefinition(const DefinitionPtr& definition) const {
	return remDefinitions(toSet<DefinitionSet>(definition));
}

ProgramPtr Program::remDefinitions(const DefinitionSet& definitions) const {
	return ProgramPtr(new Program(sharedData, difference(this->definitions, getAll(*sharedData, definitions)), entryPoints));
}


ProgramPtr Program::addEntryPoint(const ExprPtr& entryPoint) const {
	return addEntryPoints(toSet<EntryPointSet>(entryPoint));
}

ProgramPtr Program::addEntryPoints(const EntryPointSet& entryPoints) const {
	return ProgramPtr(new Program(sharedData, definitions, merge(this->entryPoints, getAll(*sharedData, entryPoints))));
}

ProgramPtr Program::remEntryPoint(const ExprPtr& entryPoint) const {
	return remEntryPoints(toSet<EntryPointSet>(entryPoint));
}

ProgramPtr Program::remEntryPoints(const EntryPointSet& entryPoints) const {
	return ProgramPtr(new Program(sharedData, definitions, difference(this->entryPoints, getAll(*sharedData, entryPoints))));
}


std::ostream& operator<<(std::ostream& out, const Program& program) {

	// print generic type definitions

	// print definitions

	// print entry points

}

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

#include <algorithm>
#include <vector>

#include <boost/functional/hash.hpp>

#include "program.h"

#include "container_utils.h"
#include "set_utils.h"
#include "functional_utils.h"
#include "types.h"
#include "expressions.h"
#include "ast_builder.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::utils::set;

std::size_t hash(const Program::DefinitionSet& definitions, const Program::EntryPointSet& entryPoints) {
	std::size_t seed = 0;
	boost::hash_combine(seed, insieme::utils::set::computeHash(definitions, hash_target<DefinitionPtr>()));
	boost::hash_combine(seed, insieme::utils::set::computeHash(entryPoints, hash_target<ExpressionPtr>()));
	return seed;
}

Program* Program::clone(NodeManager& manager) const {
//	return new Program(SharedNodeManager(NULL), manager.getAll(definitions), manager.getAll(entryPoints));
	assert ( false && "Programs cannot be migrated between manager!");
	return NULL;
}

/**
 * Obtains a list of all nodes referenced by this program node.
 */
Node::OptionChildList Program::getChildNodes() const {
	OptionChildList res(new ChildList());
	auto iter = inserter(*res, res->end());
	std::copy(definitions.cbegin(), definitions.cend(), iter);
	std::copy(entryPoints.cbegin(), entryPoints.cend(), iter);
	return res;
}

Program::Program(SharedNodeManager manager, const DefinitionSet& definitions, const EntryPointSet& entryPoints) :
	Node(PROGRAM, ::hash(definitions, entryPoints)), nodeManager(manager), definitions(definitions), entryPoints(entryPoints) { };

Program::Program() :
	Node(PROGRAM, ::hash(DefinitionSet(), EntryPointSet())), nodeManager(SharedNodeManager(new NodeManager())) {};

ProgramPtr Program::create(const DefinitionSet& definitions, const EntryPointSet& entryPoints) {
	return ProgramPtr(new Program(SharedNodeManager(new NodeManager()), definitions, entryPoints), true);
}

ProgramPtr Program::create(const SharedNodeManager& manager, const DefinitionSet& definitions, const EntryPointSet& entryPoints) {
	return ProgramPtr(new Program(manager, definitions, entryPoints), true);
}

ProgramPtr Program::addDefinition(const DefinitionPtr& definition) const {
	return addDefinitions(toSet<DefinitionSet>(definition));
}

ProgramPtr Program::addDefinitions(const DefinitionSet& definitions) const {
	return ProgramPtr(new Program(nodeManager, merge(this->definitions, nodeManager->getAll(definitions)), entryPoints), true);
}

ProgramPtr Program::remDefinition(const DefinitionPtr& definition) const {
	return remDefinitions(toSet<DefinitionSet>(definition));
}

ProgramPtr Program::remDefinitions(const DefinitionSet& definitions) const {
	return ProgramPtr(new Program(nodeManager, difference(this->definitions, definitions), entryPoints), true);
}


ProgramPtr Program::addEntryPoint(const ExpressionPtr& entryPoint) const {
	return addEntryPoints(toSet<EntryPointSet>(entryPoint));
}

ProgramPtr Program::addEntryPoints(const EntryPointSet& entryPoints) const {
	return ProgramPtr(new Program(nodeManager, definitions, merge(this->entryPoints, nodeManager->getAll(entryPoints))), true);
}

ProgramPtr Program::remEntryPoint(const ExpressionPtr& entryPoint) const {
	return remEntryPoints(toSet<EntryPointSet>(entryPoint));
}

ProgramPtr Program::remEntryPoints(const EntryPointSet& entryPoints) const {
	return ProgramPtr(new Program(nodeManager, definitions, difference(this->entryPoints, entryPoints)), true);
}

ASTBuilder Program::getASTBuilder() const {
	return ASTBuilder(nodeManager);
}

bool Program::equals(const Node& other) const {
	// precondition: other must be a type
	assert( dynamic_cast<const Program*>(&other) && "Type violation by base class!" );

	// convert (statically) and check the type name
	const Program& ref = static_cast<const Program&>(other);

	// compare definitions and entry points
	return
			insieme::utils::set::equal(entryPoints, ref.entryPoints) &&
			insieme::utils::set::equal(definitions, ref.definitions);
}


bool compareDefinitions(const DefinitionPtr& defA, const DefinitionPtr& defB) {
	return defA->getName() < defB->getName();
}

bool compareEntryPoints(const ExpressionPtr& exprA, const ExpressionPtr& exprB) {
	return toString(exprA) < toString(exprB);
}

bool compareTypes(const TypePtr& typeA, const TypePtr& typeB) {
	return toString(typeA) < toString(typeB);
}

std::vector<GenericTypePtr> getAllGenericTypes(const Program& program);

std::ostream& operator<<(std::ostream& out, const Program& program) {

	typedef std::vector<DefinitionPtr> DefinitionList;
	typedef std::vector<ExpressionPtr> EntryPointList;

	out << "PROGRAM { \n";

	// print generic type definitions
	out << "// Types: \n";
	out << "\n";

	// TODO: filter generic types and print them in alphabetical order

	// print definitions
	const Program::DefinitionSet& defSet = program.getDefinitions();
	DefinitionList defList;
	defList.insert(defList.end(),defSet.cbegin(), defSet.cend());
	sort(defList.begin(), defList.end(), compareDefinitions);

	out << "// Definitions:" << endl;
	for_each(defList.cbegin(), defList.cend(),
		[&out](const DefinitionPtr& cur) {
			out << *cur << endl;
	});
	out << endl;

	// print entry points
	const Program::EntryPointSet& entryPoints = program.getEntryPoints();
	EntryPointList entryList;
	entryList.insert(entryList.end(), entryPoints.cbegin(), entryPoints.cend());
	sort(entryList.begin(), entryList.end(), compareEntryPoints);

	out << "// Entry Points:" << endl;
	for_each(entryList.cbegin(), entryList.cend(),
		[&out](const ExpressionPtr& cur) {
			out << *cur << endl;
	});
	out << endl;

	out << "}" << endl;

	return out;
}



std::vector<GenericTypePtr> getAllGenericTypes(const Program& program) {

	std::vector<GenericTypePtr> res;



	return res;
}

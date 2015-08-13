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

#include "insieme/core/ir_program.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {


	ProgramPtr Program::get(NodeManager& manager, const ExpressionList& entryPoints) {
		return manager.get(Program(entryPoints));
	}

	ProgramPtr Program::addEntryPoint(NodeManager& manager, const ProgramPtr& program, const ExpressionPtr& entryPoint) {
		return addEntryPoints(manager, program, toVector<ExpressionPtr>(entryPoint));
	}

	ProgramPtr Program::addEntryPoints(NodeManager& manager, const ProgramPtr& program, const ExpressionList& entryPoints) {
		ExpressionList list(program->getEntryPoints());
		for_each(entryPoints, [&](const ExpressionPtr& cur) {
			if(!contains(list, cur, equal_target<ExpressionPtr>())) { list.push_back(cur); }
		});
		return manager.get(Program(list));
	}

	ProgramPtr Program::remEntryPoint(NodeManager& manager, const ProgramPtr& program, const ExpressionPtr& entryPoint) {
		return remEntryPoints(manager, program, toVector<ExpressionPtr>(entryPoint));
	}

	ProgramPtr Program::remEntryPoints(NodeManager& manager, const ProgramPtr& program, const ExpressionList& entryPoints) {
		ExpressionList list;
		for_each(program->getEntryPoints(), [&list, &entryPoints](const ExpressionPtr& cur) {
			if(!contains(entryPoints, cur, equal_target<ExpressionPtr>())) { list.push_back(cur); }
		});
		return manager.get(Program(list));
	}


	std::ostream& Program::printTo(std::ostream& out) const {
		out << "PROGRAM { \n";

		// print entry points
		out << "// Entry Points: \n\t";
		out << join("\n\t", getEntryPoints(), print<deref<NodePtr>>());
		out << "\n";

		return out;
	}
} // end namespace core
} // end namespace insieme

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

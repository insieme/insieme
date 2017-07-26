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

#pragma once

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {


	// ---------------------------------------- Marker Statement ------------------------------

	/**
	 * The accessor associated to the marker statement.
	 */
	IR_LIST_NODE_ACCESSOR(Program, Node, EntryPoints, Expression)
	IR_NODE_END()


	/**
	 * This class implements an IR Node which can be used to represent entire programs.
	 */
	class Program : public Node, public Accessor<Program, Program, Pointer>, public Accessor<Program, Program, Pointer>::node_helper {
	  private:
		/**
		 * Creates a new program based on the given entry points.
		 *
		 * @param entryPoints the list of entry points resulting program should be consisting of.
		 */
		Program(const ExpressionList& entryPoints)
		    : Node(NT_Program, NC_Program, convertList(entryPoints)), Accessor<Program, Program, Pointer>::node_helper(getChildNodeList()) {}

		/**
		 * Creates a new program node based on the given child list.
		 *
		 * @param children the list of children to be used within the resulting program
		 */
		Program(const NodeList& children) : Node(NT_Program, NC_Program, children), Accessor<Program, Program, Pointer>::node_helper(getChildNodeList()) {}

	  protected:
		/**
		 * The function required for the clone process.
		 */
		virtual Program* createInstanceUsing(const NodeList& children) const {
			return new Program(children);
		}

	  public:
		/**
		 * A factory method creating instances based on a child list
		 */
		static ProgramPtr get(NodeManager& manager, const NodeList& children) {
			return manager.get(Program(children));
		}

	  public:
		/**
		 * Creates a new program node within the given manager combining the given set of entry points.
		 *
		 * @param manager the manager used to create the new node and to maintain all referenced nodes
		 * @param entryPoints the list of entry points to be included within the resulting program.
		 * @return a ProgramPtr referencing the resulting program. The life time of the referenced node
		 * 						will be bound to the given manager.
		 */
		static ProgramPtr get(NodeManager& manager, const ExpressionList& entryPoints = ExpressionList());

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
		static ProgramPtr addEntryPoints(NodeManager& manager, const ProgramPtr& program, const ExpressionList& points);

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
		static ProgramPtr remEntryPoints(NodeManager& manager, const ProgramPtr& program, const ExpressionList& points);


	  protected:
		/**
		 * Implements the printing of this program into the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};

} // end namespace core
} // end namespace insieme

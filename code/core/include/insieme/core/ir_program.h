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

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {



	// ---------------------------------------- Marker Statement ------------------------------

	/**
	 * The accessor associated to the marker statement.
	 */
	IR_LIST_NODE_ACCESSOR(Program, Node, EntryPoints, Expression)
	};


	/**
	 * This class implements an IR Node which can be used to represent entire programs.
	 */
	class Program :
		public Node,
		public ProgramAccessor<Program, Pointer>,
		public ProgramAccessor<Program, Pointer>::node_helper {

	private:

		/**
		 * Creates a new program based on the given entry points.
		 *
		 * @param entryPoints the list of entry points resulting program should be consisting of.
		 */
		Program(const ExpressionList& entryPoints)
			: Node(NT_Program, NC_Program, convertList(entryPoints)),
			  ProgramAccessor<Program, Pointer>::node_helper(getChildNodeList()) {}

		/**
		 * Creates a new program node based on the given child list.
		 *
		 * @param children the list of children to be used within the resulting program
		 */
		Program(const NodeList& children)
			: Node(NT_Program, NC_Program, children),
			  ProgramAccessor<Program, Pointer>::node_helper(getChildNodeList()) {}

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


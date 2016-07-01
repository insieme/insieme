/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <iostream>
#include <functional>
#include <map>

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {
namespace printer {

	/**
	 * A plug-in interface for pretty printers.
	 */
	struct PrinterPlugin {
		virtual ~PrinterPlugin() {}

		/**
		 * A function testing whether this plug-in want's to handle the given node.
		 */
		virtual bool covers(const NodeAddress&) const = 0;

		/**
		 * A function triggered to print the given node to the given stream.
		 */
		virtual std::ostream& print(std::ostream&, const NodeAddress&, const std::function<void(const NodeAddress&)>&) const = 0;

		/**
		 * handler to perform actions after new line characters
		 */
		virtual std::ostream& afterNewLine(std::ostream& out) const {
			return out;
		}

		/**
		 * handler to perform actions when the printer ends
		 */
		virtual std::ostream& afterAllDone(std::ostream& out) const {
			return out;
		}
	};

	namespace detail {

		/**
		 * A factory for a empty plug-in.
		 */
		const PrinterPlugin& getEmptyPlugin();
	}

	/**
	 * A struct representing a pretty print of a AST subtree. Instances may be streamed
	 * into an output stream to obtain a readable version of an AST.
	 */
	struct PrettyPrinter {
		/**
		 * A list of options to adjust the print.
		 */
		enum Option {

			PRINT_DEREFS = 1 << 0,
			PRINT_CASTS = 1 << 1,
			PRINT_DEFAULT_MEMBERS = 1 << 2,
			PRINT_SINGLE_LINE = 1 << 3,
			PRINT_MARKERS = 1 << 4,
			PRINT_ANNOTATIONS = 1 << 5,
			NO_LIST_SUGAR = 1 << 7,
			PRINT_ATTRIBUTES = 1 << 8,
			NAME_CONTRACTION = 1 << 9,
			NO_EVAL_LAZY = 1 << 10,
			NO_LET_BINDINGS = 1 << 11,
			NO_LET_BOUND_FUNCTIONS = 1 << 12,
			PRINT_LITERAL_TYPES = 1 << 13,
			USE_COLOR = 1 << 14,
			PRINT_DERIVED_IMPL = 1 << 15,
			JUST_LOCAL_CONTEXT = 1 << 16,
			FULL_LITERAL_SYNTAX = 1 << 17,
			USE_VARIABLE_NAME_ANNOTATIONS = 1 << 18,
			PRINT_CALL_EXPR_TYPES = 1 << 19
		};

		/**
		 * An default setup resulting in a readable print out.
		 */
		static const unsigned OPTIONS_DEFAULT;

		/**
		 * An option to be used for more details.
		 */
		static const unsigned OPTIONS_DETAIL;

		/**
		 * An option to be used for a maximum of details.
		 */
		static const unsigned OPTIONS_MAX_DETAIL;

		/**
		 * An option to be used for a single-line print.
		 */
		static const unsigned OPTIONS_SINGLE_LINE;

		/**
		 * The root node of the sub-try to be printed
		 */
		NodePtr root;

		/**
		 * A a plug-in to be utilized for influencing IR pretty prints.
		 */
		const PrinterPlugin& plugin;

		/**
		 * The flags set for customizing the formating
		 */
		unsigned flags;

		/**
		 * The maximum number of levels to be printed
		 */
		unsigned maxDepth;

		/**
		 * Sets the tab separator
		 */
		string tabSep;

		/**
		 * Creates a new pretty print instance.
		 *
		 * @param root the root node of the IR to be printed
		 * @param flags options allowing users to customize the output
		 * @param maxDepth the maximum recursive steps the pretty printer is descending into the given AST
		 * @param tabSep the string used to print tabs
		 */
		PrettyPrinter(const NodePtr& root, unsigned flags = OPTIONS_DEFAULT, unsigned maxDepth = std::numeric_limits<unsigned>::max(), string tabSep = "    ")
		    : root(root), plugin(detail::getEmptyPlugin()), flags(flags), maxDepth(maxDepth), tabSep(tabSep) {}

		/**
		 * Creates a new pretty print instance based on the given parameters.
		 *
		 * @param root the root node of the IR to be printed
		 * @param plugin a plugin to customize the printing of nodes
		 * @param flags options allowing users to customize the output
		 * @param maxDepth the maximum recursive steps the pretty printer is descending into the given AST
		 * @param tabSep the string used to print tabs
		 */
		PrettyPrinter(const NodePtr& root, const PrinterPlugin& plugin, unsigned flags = OPTIONS_DEFAULT,
		              unsigned maxDepth = std::numeric_limits<unsigned>::max(), string tabSep = "    ")
		    : root(root), plugin(plugin), flags(flags), maxDepth(maxDepth), tabSep(tabSep) {}

		/**
		 * Tests whether a certain option is set or not.
		 *
		 * @return true if the option is set, false otherwise
		 */
		bool hasOption(Option option) const;

		/**
		 * Updates a format option for the pretty printer.
		 *
		 * @param option the option to be updated
		 * @param status the state this option should be set to
		 */
		void setOption(Option option, bool status = true);
	};

	// Represents the lineNo:columnNo pair used to identify a position in the generated output IR source file
	typedef std::pair<size_t, size_t> SourceLocation;

	// Represents a range start : end which identify the upper and lower bounds of source code generated by
	// an IR node
	typedef std::pair<SourceLocation, SourceLocation> SourceRange;

	// Map use to map code ranges of the generated IR representation to the corresponding
	// IR node. (to use NodeAddress in the future maybe)
	typedef std::map<SourceRange, NodePtr> SourceLocationMap;

	// Prints the IR to the output stream and in parallel builds the map which associates positions
	// in the generated stream to IR nodes
	SourceLocationMap printAndMap(std::ostream& out, const insieme::core::printer::PrettyPrinter& print, bool showLineNo = true, int columnWrap = -1);

	/**
	 * A utility function printing the given node using the pretty printer in a one-line-format.
	 */
	inline core::printer::PrettyPrinter printInOneLine(const core::NodePtr& node) {
		return core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::PRINT_SINGLE_LINE | core::printer::PrettyPrinter::NO_LET_BINDINGS);
	}

} // end of namespace printer
} // end of namespace core
} // end of namespace insieme

namespace std {

	/**
	 * Allows pretty prints to be directly printed into output streams.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::PrettyPrinter& print);

	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceLocation& loc);

	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceRange& range);

	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceLocationMap& srcMap);
}

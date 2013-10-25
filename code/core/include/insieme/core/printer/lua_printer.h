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

#include "insieme/core/ir_expressions.h"

#include "insieme/utils/printable.h"


namespace insieme {
namespace core {
namespace printer {

	/**
	 * The Lua printer is converting an internal IR code representation
	 * into approximately equal Lua script code. Correct semantic encoding
	 * can not be guaranteed due to the restricted support of language construts
	 * within lua.
	 */
	struct LuaPrinter : public utils::Printable {

		/**
		 * The statement to be printed by this printer.
		 */
		StatementPtr stmt;

		/**
		 * Creates a new instance of this printer converting
		 * the given statement into a equivalent Lua script.
		 *
		 * @param stmt the statement to be converted
		 */
		LuaPrinter(const StatementPtr& stmt) : stmt(stmt) {}

		/**
		 * The internal implementation realizing the actual
		 * conversion into the Lua script language.
		 *
		 * @param out the stream to be printed to
		 */
		std::ostream& printTo(std::ostream& out) const;

	};

	/**
	 * A utility function converting the given statement into a
	 * approximately equal Lua script.
	 */
	string toLuaScript(const StatementPtr& stmt);

	/**
	 * The type of exception been thrown if some construct could
	 * not be converted successfully into an equivalent Lua script.
	 */
	class LuaConversionException : public std::exception {

		/**
		 * The node which caused the problem.
		 */
		NodePtr source;

		/**
		 * A brief description of the encountered problem.
		 */
		std::string msg;

	public:

		LuaConversionException(const NodePtr& source, const string& msg = "");
		virtual ~LuaConversionException() throw() { }
		const NodePtr& getSource() const { return source; }
		virtual const char* what() const throw() { return msg.c_str(); }
	};


} // end namespace printer
} // end namespace core
} // end namespace insieme

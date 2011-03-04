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

/**
 * This include file defines some macros simplifying the specification of formats to print operators.
 */


/**
 * This macro can be used within the format specifiaction to print literals.
 * 
 * @param Literal the literal to be printed
 */
#define OUT(Literal) cStr << Literal

/**
 * This macro allows to access an argument of the call to be processed by the formatter. 
 * The indexing starts with 0. The result will be an ExpressionPtr. If there is no such argument,
 * the pointer will be null.
 * 
 * @param N the index of the argument to be obtained 
 */
#define ARG(N) insieme::simple_backend::formatting::detail::getArgument(call, N)

/**
 * A macro triggering the deceding into an argument. This will result in the printing of the code
 * required for evaluating the corresponding argument.
 * 
 * @param N the index of the argument to be printed 
 */
#define VISIT_ARG(N) insieme::simple_backend::formatting::detail::visitArgument(converter, call, N)

/**
 * Registers a new formatter instance for the given literal.
 * 
 * @param table the table to which the formatter should be added
 * @param Literal the literal for which the new formatter should be responsible for
 * @param Brackets allows to determine whether brackets should be put around the resulting code
 * @param FORMAT the function body realizing the formatting 
 */
#define ADD_FORMATTER_DETAIL(table, Literal, Brackets, FORMAT) \
			table.insert(std::make_pair(Literal, insieme::simple_backend::formatting::make_formatter([&basic](insieme::simple_backend::StmtConverter& converter, insieme::simple_backend::CodeStream& cStr, const insieme::core::CallExprPtr& call) { \
				if (Brackets) OUT("("); \
				FORMAT \
				if (Brackets) OUT(")"); \
			} )))

/**
 * A smaller version of the ADD_FORMATTER_DETAIL macro using brackets by default.
 * 
 * @param table the table to which the formatter should be added
 * @param Literal the literal for which the new formatter should be responsible for
 * @param FORMAT the function body realizing the formatting
 */
#define ADD_FORMATTER(table, Literal, FORMAT) \
			ADD_FORMATTER_DETAIL(table, Literal, true, FORMAT)

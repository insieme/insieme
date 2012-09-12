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
 * This macro file allows to define error codes for the semantic checks.
 * The following macro-definition is supported:
 * 
 * 		CODE(KIND,NAME)
 * 		
 * where
 * 		KIND ... one of TYPE, IMPERATIVE, SEMANTIC, ...
 * 		NAME ... the name of the error
 */

// ------------------------------ Structural ERRORS ---------------------------

CODE(STRUCTURE, NULL_NODE)

// ------------------------------ Typing ERRORS ---------------------------

CODE(TYPE, INVALID_NUMBER_OF_ARGUMENTS)
CODE(TYPE, INVALID_ARGUMENT_TYPE)
CODE(TYPE, INVALID_RETURN_TYPE)
CODE(TYPE, INVALID_FUNCTION_TYPE)

CODE(TYPE, INVALID_RETURN_VALUE_TYPE)
CODE(TYPE, MISSING_RETURN_STMT)

CODE(TYPE, INVALID_INITIALIZATION_EXPR)

CODE(TYPE, INVALID_CONDITION_EXPR)
CODE(TYPE, INVALID_SWITCH_EXPR)

CODE(TYPE, INVALID_ITERATOR_TYPE)
CODE(TYPE, INVALID_BOUNDARY_TYPE)

CODE(TYPE, INVALID_TYPE_OF_LITERAL)

CODE(TYPE, REF_TO_NON_REF_CAST)
CODE(TYPE, NON_REF_TO_REF_CAST)

CODE(TYPE, ILLEGAL_CAST)

CODE(TYPE, ILLEGAL_USE_OF_TYPE_KEYWORD)

CODE(TYPE, ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE)
CODE(TYPE, NO_SUCH_MEMBER)
CODE(TYPE, INVALID_TYPE_OF_MEMBER)
CODE(TYPE, INVALID_IDENTIFIER)

CODE(TYPE, INVALID_TUPLE_INDEX)
CODE(TYPE, ACCESSING_MEMBER_OF_NON_TUPLE_TYPE)

CODE(TYPE, INVALID_LAMBDA_EXPR_NO_SUCH_DEFINITION)
CODE(TYPE, INVALID_LAMBDA_EXPR_TYPE)
CODE(TYPE, INVALID_LAMBDA_REC_VAR_TYPE)
CODE(TYPE, INVALID_LAMBDA_TYPE)

CODE(TYPE, INVALID_ARRAY_VALUE)
CODE(TYPE, INVALID_ARRAY_CONTEXT)

CODE(TYPE, MALFORM_NARROW_CALL)
CODE(TYPE, MALFORM_EXPAND_CALL)

// ------------------------------ Imperative ERRORS ---------------------------

CODE(IMPERATIVE, UNDECLARED_VARIABLE_USAGE) 
CODE(IMPERATIVE, ILLEGAL_VARIABLE_REUSE)

// ------------------------------- Semantic ERRORS ----------------------------

CODE(SEMANTIC, ARRAY_INDEX_OUT_OF_RANGE)
CODE(SEMANTIC, INCORRECT_UNDEFINED)


// automatically undefine CODE macro after usage
#undef CODE

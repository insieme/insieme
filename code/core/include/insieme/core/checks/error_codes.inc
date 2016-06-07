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

GROUP(STRUCTURE, 1000)
CODE(STRUCTURE, NULL_NODE)

// ------------------------------ Typing ERRORS ---------------------------

GROUP(TYPE, 2000)
CODE(TYPE, INVALID_NUMBER_OF_ARGUMENTS)
CODE(TYPE, INVALID_ARGUMENT_TYPE)
CODE(TYPE, INVALID_RETURN_TYPE)
CODE(TYPE, INVALID_FUNCTION_TYPE)

CODE(TYPE, INVALID_RETURN_VALUE_TYPE)

CODE(TYPE, INVALID_INITIALIZATION_EXPR)
CODE(TYPE, INVALID_DECLARATION_TYPE)
CODE(TYPE, REF_DECL_TYPE_MISMATCH)

CODE(TYPE, INVALID_CONDITION_EXPR)
CODE(TYPE, INVALID_SWITCH_EXPR)

CODE(TYPE, INVALID_ITERATOR_TYPE)
CODE(TYPE, INVALID_BOUNDARY_TYPE)

CODE(TYPE, INVALID_TYPE_OF_LITERAL)
CODE(TYPE, INVALID_TYPE_OF_STRUCT_EXPR)

CODE(TYPE, REF_TO_NON_REF_CAST)
CODE(TYPE, NON_REF_TO_REF_CAST)

CODE(TYPE, ILLEGAL_CAST)
CODE(TYPE, ILLEGAL_FUNCTION_INSTANTIATION)
CODE(TYPE, ILLEGAL_GENERIC_ZERO_TYPE)

CODE(TYPE, ILLEGAL_USE_OF_TYPE_KEYWORD)

CODE(TYPE, ILLEGAL_FUNCTION_TYPE_KIND)
CODE(TYPE, ILLEGAL_OBJECT_TYPE)
CODE(TYPE, ILLEGAL_DESTRUCTOR_PARAMETERS)
CODE(TYPE, ILLEGAL_CONSTRUCTOR_RETURN_TYPE)
CODE(TYPE, ILLEGAL_DESTRUCTOR_RETURN_TYPE)

CODE(TYPE, ACCESSING_MEMBER_OF_NON_RECORD_TYPE)
CODE(TYPE, NO_SUCH_MEMBER)
CODE(TYPE, INVALID_TYPE_OF_MEMBER)
CODE(TYPE, INVALID_IDENTIFIER)

CODE(TYPE, INVALID_TUPLE_INDEX)
CODE(TYPE, ACCESSING_MEMBER_OF_NON_TUPLE_TYPE)

CODE(TYPE, INVALID_LAMBDA_EXPR_NO_SUCH_DEFINITION)
CODE(TYPE, INVALID_LAMBDA_EXPR_TYPE)
CODE(TYPE, INVALID_LAMBDA_REC_VAR_TYPE)
CODE(TYPE, INVALID_LAMBDA_TYPE)
CODE(TYPE, INVALID_LAMBDA_PARAMETER_TYPE)

CODE(TYPE, INVALID_ARRAY_VALUE)
CODE(TYPE, INVALID_ARRAY_CONTEXT)

CODE(TYPE, MALFORM_NARROW_CALL)
CODE(TYPE, MALFORM_EXPAND_CALL)

CODE(TYPE, NAMED_MEMBER_FUNCTION_COLLISION)

CODE(TYPE, INVALID_GENERIC_OPERATOR_APPLICATION)

CODE(TYPE, FREE_TAG_TYPE_REFERENCE)
CODE(TYPE, MALFORMED_TAG_TYPE)

CODE(TYPE, MALFORMED_ENUM_TYPE)
CODE(TYPE, MALFORMED_ENUM_TYPE_DEFINITION_NAME)
CODE(TYPE, MALFORMED_ENUM_ENTRY)

CODE(TYPE, INVALID_CONSTRUCTOR_TYPE)
CODE(TYPE, DUPLICATE_CONSTRUCTOR_TYPE)
CODE(TYPE, INVALID_DESTRUCTOR_TYPE)
CODE(TYPE, INVALID_MEMBER_FUNCTION_TYPE)
CODE(TYPE, DUPLICATE_MEMBER_FUNCTION)
CODE(TYPE, DUPLICATE_MEMBER_FIELD)
CODE(TYPE, MULTIPLE_DESTRUCTORS)

// ------------------------------ Imperative ERRORS ---------------------------

GROUP(IMPERATIVE, 3000)
CODE(IMPERATIVE, UNDECLARED_VARIABLE_USAGE)
CODE(IMPERATIVE, ILLEGAL_VARIABLE_REUSE)

// ------------------------------- Semantic ERRORS ----------------------------

GROUP(SEMANTIC, 4000)
CODE(SEMANTIC, ILLEGAL_REF_TO_FUN_CAST)
CODE(SEMANTIC, INCORRECT_UNDEFINED)
CODE(SEMANTIC, FREE_BREAK_INSIDE_FOR_LOOP)
CODE(SEMANTIC, MISSING_RETURN_STMT)
CODE(SEMANTIC, INVALID_INIT_MEMLOC)
CODE(SEMANTIC, ILLEGAL_NUM_CAST)
CODE(SEMANTIC, ILLEGAL_NUM_TYPE_TO_INT)

// ------------------------------ Formating ERRORS ---------------------------

GROUP(FORMAT, 5000)
CODE(FORMAT, INVALID_LITERAL)


// automatically undefine macros after usage
#undef CODE
#undef GROUP

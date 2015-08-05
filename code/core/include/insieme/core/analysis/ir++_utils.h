/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace analysis {

/**
 * A test which can be applied to the an arbitrary IR structure to determine
 * whether the represented structure includes object-oriented elements.
 *
 * @param node the IR structure to be tested
 * @return true if IR++ elements are included, false otherwise.
 */
bool isIRpp(const NodePtr& node);

/**
 * A test verifying whether the given type is a valid object type to
 * be used as an object type for a ctor / dtor / member function.
 *
 * @param type the type to be tested
 * @return true if type is a valid object type, false otherwise
 */
bool isObjectType(const TypePtr& type);

/**
 * A shortcut for the isObjectType function above obtaining the result
 * statically since any struct type is a valid object type.
 */
static inline bool isObjectType(const StructTypePtr& type) {
	return true;
}

/**
 * a Union can also have member functions, therefore it should be also in the
 * Object clasification
 */
static inline bool isObjectType(const UnionTypePtr& type) {
	return true;
}

/**
 * A shortcut for the isObjectType function above obtaining the result
 * statically since any generic type is a valid object type.
 */
static inline bool isObjectType(const GenericTypePtr& type) {
	return true;
}

/**
 * A shortcut for the isObjectType function above obtaining the result
 * statically since any type variable is a valid object type.
 */
static inline bool isObjectType(const TypeVariablePtr& type) {
	return true;
}

/**
 * A test determining whether some type is a reference to a valid object
 * type structure (hence a potential type for a this pointer).
 *
 * @param type the type to be tested
 * @return true if the given type is a reference to a valid object type.
 */
bool isObjectReferenceType(const TypePtr& type);

/**
 * A test determining whether some type is a reference to a valid object
 * type structure (hence a potential type for a this pointer).
 *
 * @param type the type to be tested
 * @return true if the given type is a reference to a valid object type.
 */
bool isObjectReferenceType(const RefTypePtr& type);

/**
 * Determines whether the given expression is representing a pure virtual function.
 */
bool isPureVirtual(const CallExprPtr& expr);

/**
 * Determines whether the given node is representing a pure virtual function.
 */
bool isPureVirtual(const NodePtr& node);


// ---------------------------- References --------------------------------------

/**
 * Checks whether the given type is a C++ reference type. A type is a C++ reference
 * type in case it has the shape
 *
 * 			struct { ref<'a> _cpp_ref; }
 *
 * @param type the type to be tested
 * @return true if it is a C++ reference type, false otherwise
 */
bool isCppRef(const TypePtr& type);

/**
 * Creates a C++ reference type referencing an instance of the given element type.
 *
 * @param elementType the type of the element to be referenced
 * @return the C++ reference type referencing an element of the given type
 */
TypePtr getCppRef(const TypePtr& elementType);

/**
 * Checks whether the given type is a const C++ reference type. A type is a const C++ reference
 * type in case it has the shape
 *
 * 			struct { ref<'a> _const_cpp_ref; }
 *
 * @param type the type to be tested
 * @return true if it is a const C++ reference type, false otherwise
 */
bool isConstCppRef(const TypePtr& type);

/**
 * Creates a const C++ reference type referencing an instance of the given element type.
 *
 * @param elementType the type of the element to be referenced
 * @return the const C++ reference type referencing an element of the given type
 */
TypePtr getConstCppRef(const TypePtr& elementType);

/**
 * Checks whether the given type is a C++ right side reference type. A type is a C++ right side reference
 * type in case it has the shape
 *
 * 			struct { ref<'a> _rval_cpp_ref; }
 *
 * @param type the type to be tested
 * @return true if it is a C++ right side reference type, false otherwise
 */
bool isRValCppRef(const TypePtr& type);

/**
 * Creates a C++ right side reference type referencing an instance of the given element type.
 *
 * @param elementType the type of the element to be referenced
 * @return the C++ right side reference type referencing an element of the given type
 */
TypePtr getRValCppRef(const TypePtr& elementType);


/**
 * Checks whether the given type is a const C++ right side reference type. A type is a const
 * C++ right side reference type in case it has the shape
 *
 * 			struct { src<'a> _const_rval_cpp_ref; }
 *
 * @param type the type to be tested
 * @return true if it is a const C++ right side reference type, false otherwise
 */
bool isConstRValCppRef(const TypePtr& type);

/**
 * Creates a const C++ right side reference type referencing an instance of the given element type.
 *
 * @param elementType the type of the element to be referenced
 * @return the const C++ right side reference type referencing an element of the given type
 */
TypePtr getConstRValCppRef(const TypePtr& elementType);

/**
 * Extracts the element type of a given (const) C++ reference type.
 *
 * @param cppRefType the type to be processed, must satisfy isCppRef(..) or isConstCppRef(..)
 * @return the element type referenced by the given type
 */
TypePtr getCppRefElementType(const TypePtr& cppRefType);

/**
 * checks whenever the underlying type is a cpp reference o const cpp reference
 * @param type the type to check
 * @return if fulfills the requirements to be a cpp ref
 */
bool isAnyCppRef(const TypePtr& type);

/*
 * To Ir ref provides a method to unwrap any cpp ref to expose the inner IR ref (and therefore be able to manipulate it)
 * @param expr, the expression to unwrap
 * @return the expression which unwraps the object
 */
ExpressionPtr unwrapCppRef(const ExpressionPtr& expr);

// --------------------------- data member pointer -----------------------------------

/**
 * queries whenever this is or not a member pointer type
 */
bool isMemberPointer(const TypePtr& type);

/**
 * constructs a member pointer type
 * @param the class of the owner object
 * @param the identifier for the
 */
TypePtr getMemberPointer(const TypePtr& classType, const TypePtr& membTy);

/**
 * initializes a value of type member pointer
 * @param
 * @param
 * @param
 */
ExpressionPtr getMemberPointerValue(const TypePtr& classType, const std::string& fieldName, const TypePtr& membTy);

/**
 * access of a member pointer
 * @param
 * @param
 */
ExpressionPtr getMemberPointerAccess(const ExpressionPtr& parent, const ExpressionPtr& expr);

/**
 * check if member pointer is not null
 * @param pointer var
 */
ExpressionPtr getMemberPointerCheck(const ExpressionPtr& expr);

// ---------------------------- Constructors --------------------------------------

/**
 * checks if a expression is a constructor call
 * @param expr, the expression to be checked
 * @return if is a call to constructor, even intercepted ones
 */
bool isConstructorCall(const core::ExpressionPtr& expr);

/**
 * Creates a default constructor for the given struct type.
 * @param type the type for which a default constructor should be created
 * @return the synthesized constructor
 */
LambdaExprPtr createDefaultConstructor(const TypePtr& type);

/**
 * A check verifying whether the given lambda is a default a default constructor
 * for its target type.
 *
 * @param lambda the lambda to be tested
 * @return true if so, false otherwise
 */
bool isDefaultConstructor(const LambdaExprPtr& lambda);

} // end namespace analysis
} // end namespace core
} // end namespace insieme

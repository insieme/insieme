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

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace analysis {

	/**
	 * Checks whether there are unbound type variables present within the given type.
	 *
	 * @param type the type to be checked
	 * @return true if there are free type variables, false otherwise
	 */
	bool hasFreeTypeVariables(const TypePtr& type);

	/**
	 * Collects all free type variables within the given type.
	 *
	 * @param type the type whose free variables shell be collected
	 * @return the set of all free type variables
	 */
	TypeVariableSet getFreeTypeVariables(const TypePtr& type);

	/**
	 * Obtains the set of type variables bound by the given function type.
	 */
	TypeVariableSet getTypeVariablesBoundBy(const FunctionTypePtr& funType);

	/**
	 * Determines the return type of a function based on its return statements.
	 *
	 * @param nodeMan NodeManager used to generate required types if necessary
	 * @param body body of the function
	 * @return the deduced return type
	 */
	TypePtr autoReturnType(NodeManager& nodeMan, const CompoundStmtPtr& body);

	/**
	 * Determines whether the given type is trivial. A trivial type is one which can be statically
	 * initialized and copied by simply copying its memory contents (without constructor/destructor/copy constructor calls).
	 *
	 * @param type the type to check
	 * @return true if the type is trivial, false otherwise
	 */
	bool isTrivial(const TypePtr& type);

	/**
	 * Determines whether the given tag type has a constructor of the given type, and optionally returns it.
	 */
	boost::optional<ExpressionPtr> hasConstructorOfType(const TagTypePtr& type, const FunctionTypePtr& funType);

	/**
	 * Determines whether the given tag type has a constructor accepting a single parameter
	 * of the given type (in addition to the this reference), and optionally returns it.
	 */
	boost::optional<ExpressionPtr> hasConstructorAccepting(const TypePtr& type, const TypePtr& paramType);

	/**
	* Determines whether the given tag type has a member function of the given name and type.
	*/
	bool hasMemberOfType(const TagTypePtr& type, const std::string& name, const FunctionTypePtr& funType);

	/**
	 * Determines whether the given tag type has a default constructor.
	 */
	bool hasDefaultConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a copy constructor.
	 */
	bool hasCopyConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a move constructor.
	 */
	bool hasMoveConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a copy assignment operator.
	 */
	bool hasCopyAssignment(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a move assignment operator.
	 */
	bool hasMoveAssignment(const TagTypePtr&);

	/**
	 * Determines whether the given constructor is one of the default generated ones for the given type.
	 *
	 * @param type the TagType to check
	 * @param ctor the ctor to check
	 */
	bool isaDefaultConstructor(const TagTypePtr& type, const ExpressionPtr& ctor);

	/**
	* Determines whether the given destructor is a default destructor for the given type.
	*
	* @param type the TagType to check
	* @param dtor the destructor to check
	*/
	bool isDefaultDestructor(const TagTypePtr& type, const ExpressionPtr& dtor);

	/**
	 * Determines whether the given TagType has the default generated destructor.
	 */
	bool hasDefaultDestructor(const TagTypePtr& type);

	/**
	 * Determines whether the given member function is one of the default generated assignment operators for the given type.
	 *
	 * @param type the TagType to check
	 * @param memberFunction the memberFunction to check
	 */
	bool isaDefaultMember(const TagTypePtr& type, const MemberFunctionPtr& memberFunction);

	/**
	* Obtains the object type this function is attached to in case it is a constructor, destructor
	* or member function. In case it is a plain or closure function type a call to this function is
	* invalid.
	*/
	template<template <typename> class Ptr, typename T>
	Ptr<const Type> getObjectType(const Ptr<const T>& type) {
		auto funType = type.template as<Ptr<const FunctionType>>();
		assert_true(funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction() || funType->isVirtualMemberFunction());
		assert_false(funType->getParameterTypes().empty());
		assert_true(analysis::isRefType(funType->getParameterType(0)));
		return funType->getParameterType(0).template as<Ptr<const GenericType>>()->getTypeParameter(0);
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme

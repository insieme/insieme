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
 *
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
	* Obtains the set of variadic type variables bound by the given function type.
	*/
	VariadicTypeVariableSet getVariadicTypeVariablesBoundBy(const FunctionTypePtr& funType);

	/**
	* Checks whether the return type of the given function is generic, and if so, whether it is potentially deducible from its arguments.
	*/
	bool isReturnTypePotentiallyDeducible(const FunctionTypePtr& funType);

	/**
	 * Normalizes the given generic type variable instance.
	 * For instance:
	 *        'a<A,'b<C>>  => 'a<_,'_<_>>
	 */
	GenericTypeVariablePtr normalize(const GenericTypeVariablePtr& var);

	/**
	 * Normalizes the given variadic generic type variable instance.
	 * For instance:
	 *        'a...<A,'b<C>>  => 'a...<_,'_<_>>
	 */
	VariadicGenericTypeVariablePtr normalize(const VariadicGenericTypeVariablePtr& var);

	/**
	 * Determines the return type of a function based on its return statements.
	 *
	 * @param nodeMan NodeManager used to generate required types if necessary
	 * @param body body of the function
	 * @return the deduced return type
	 */
	TypePtr autoReturnType(NodeManager& nodeMan, const CompoundStmtPtr& body);

	/**
	 * Determines whether the given type is trivially copyable.
	 * The only trivially copyable types are scalar types, trivially copyable classes, and arrays of such types/classes.
	 * Objects of trivially-copyable types are the only C++ objects that may be safely copied with std::memcpy or serialized to/from binary files
	 * with std::ofstream::write()/std::ifstream::read(). In general, a trivially copyable type is any type for which the underlying bytes can be
	 * copied to an array of char or unsigned char and into a new object of the same type, and the resulting object would have the same value as
	 * the original.
	 */
	bool isTriviallyCopyable(const TypePtr& type);

	/**
	 * Determines whether the given type is trivial. A trivial type is one which is trivially copyable
	 * and also has a trivial non-deleted default constructor.
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
	* @param ctor the ctor to check
	*/
	bool isaDefaultConstructor(const ExpressionPtr& ctor);

	/*
	* Determines whether the given expression is a destructor and if it is a default destructor
	*/
	bool isDefaultDestructor(const ExpressionPtr& dtor);

	/**
	 * Determines whether the given TagType has the default generated destructor.
	 */
	bool hasDefaultDestructor(const TagTypePtr&);

	/*
	* Determines whether the given Record has the default generated destructor.
	*/
	bool hasDefaultDestructor(const RecordPtr&);

	/**
	 * Determines whether the given member function is one of the default generated assignment operators for the given type.
	 *
	 * @param memberFunction the memberFunction to check
	 */
	bool isaDefaultMember(const MemberFunctionPtr& memberFunction);

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

	std::string getTypeName(const TypePtr& objTy);

} // end namespace analysis
} // end namespace core
} // end namespace insieme

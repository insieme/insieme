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

#include <map>
#include <string>

#include "insieme/core/ir.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {


	/**
	 * A struct used to represent a member function.
	 */
	class MemberFunction : public utils::Printable {

		/**
		 * The name of this member function.
		 */
		string name;

		/**
		 * The implementation of the member function.
		 */
		LambdaExprPtr lambda;

		/**
		 * Marks this member function to be virtual or not.
		 */
		bool m_virtual;

		/**
		 * Marks this member function to be const.
		 */
		bool m_const;

	public:

		/**
		 * A constructor for a member-function instance accepting
		 * the full list of parameters describing a member function.
		 *
		 * @param name the name of the member function
		 * @param lambda the implementation of the member function
		 * @param _virtual marks the resulting member function to be virtual
		 * @param _const marks the resulting member function to be const
		 */
		MemberFunction(const string& name, const LambdaExprPtr& lambda, bool _virtual = false, bool _const = false)
			: name(name), lambda(lambda), m_virtual(_virtual), m_const(_const) {}

		/**
		 * Obtains the name of this member function.
		 *
		 * @return the name of the represented member function
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Obtains the implementation of the represented member function.
		 */
		const LambdaExprPtr& getLambdaExpr() const {
			return lambda;
		}

		/**
		 * Updates the implementation of this member function.
		 */
		void setLambdaExpr(const LambdaExprPtr& newImpl) {
			lambda = newImpl;
		}

		/**
		 * Determines whether the represented member function is virtual or not.
		 */
		bool isVirtual() const {
			return m_virtual;
		}

		/**
		 * Determines whether the represented member function is const or not.
		 */
		bool isConst() const {
			return m_const;
		}

		/**
		 * A standard-implementation of an equality operator.
		 */
		bool operator==(const MemberFunction& other) const {
			return this == &other || (
					name == other.name &&
					*lambda == *other.lambda &&
					m_virtual==other.m_virtual &&
					m_const == other.m_const
			);
		}

		/**
		 * A standard-implementation of an inequality operator based on
		 * an the equality operator.
		 */
		bool operator!=(const MemberFunction& other) const {
			return !(*this == other);
		}

		/**
		 * Required to implement the printable interface.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * A class aggregating meta-information regarding class types.
	 */
	class ClassMetaInfo : public utils::Printable, public core::value_annotation::migratable {

		/**
		 * The list of constructors to be associated to the class
		 * type this meta-information is associated to.
		 */
		vector<LambdaExprPtr> constructors;

		/**
		 * The destructor associated to this class type.
		 */
		LambdaExprPtr destructor;

		/**
		 * A flag determining whether the destructor is a virtual destructor or not.
		 */
		bool virtualDestructor;

		/**
		 * The list of member functions (virtual and non-virtual member functions)
		 * attached to the class annoated by this info-collection.
		 */
		vector<MemberFunction> memberFunctions;

	public:

		/**
		 * A default constructor for this meta-info object.
		 */
		ClassMetaInfo() : virtualDestructor(false) {}

		// ---- getter and setter ---

		/**
		 * Obtains a reference to the internally maintained list of constructors.
		 */
		const vector<LambdaExprPtr>& getConstructors() const {
			return constructors;
		}

		/**
		 * Updated the internally maintained list of constructors.
		 *
		 * @param constructors the new list of constructors
		 */
		void setConstructors(const vector<LambdaExprPtr>& constructors);

		/**
		 * Adds a new constructor to this meta-info collection.
		 *
		 * @param constructor the constructor to be added
		 */
		void addConstructor(const LambdaExprPtr& constructor);

		/**
		 * Obtains a reference to the destructor implementation associated
		 * to this meta-info collection or null if no destructor has been
		 * attached.
		 */
		const LambdaExprPtr& getDestructor() const {
			return destructor;
		}

		/**
		 * Updates the destructor covered by this meta-info collection.
		 *
		 * @param destructor the new destructor
		 */
		void setDestructor(const LambdaExprPtr& destructor);

		/**
		 * Determines whether the associated destructor is marked virtual or not.
		 */
		bool isDestructorVirtual() const {
			return virtualDestructor;
		}

		/**
		 * Updated the flag determining whether the associated destructor is a
		 * virtual destructor or not
		 *
		 * @param _virtual true if it should be virtual, false otherwise
		 */
		void setDestructorVirtual(bool _virtual = true) {
			virtualDestructor = _virtual;
		}

		/**
		 * Obtains a reference to the member functions listed by this meta-info object.
		 */
		const vector<MemberFunction>& getMemberFunctions() {
			return memberFunctions;
		}

		/**
		 * Exchanges the list of member functions maintained by this meta-info object.
		 *
		 * @param functions the new list of functions
		 */
		void setMemberFunctions(const vector<MemberFunction>& functions);

		/**
		 * Adds an additional member function to the internally maintained list of
		 * member functions.
		 *
		 * @param function the function to be added
		 */
		void addMemberFunction(const MemberFunction& function);

		/**
		 * Adds an additional member function to the internally maintained list of
		 * member functions.
		 *
		 * @param name the name of the function
		 * @param impl the implementation of the member function to be added
		 * @param _virtual determines whether the function to be added should be marked virtual
		 * @param _const determines whether the function to be added should be marked const
		 */
		void addMemberFunction(const string& name, const LambdaExprPtr& impl, bool _virtual = false, bool _const = false) {
			addMemberFunction(MemberFunction(name, impl, _virtual, _const));
		}

		// ----- derived operations ------

		/**
		 * Implements the equality operator by comparing all fields of this
		 * class individually.
		 *
		 * @param other the info instance to be compared with
		 * @return true if it other is equivalent to this info object
		 */
		bool operator==(const ClassMetaInfo& other) const {
			return  this == &other || (
						virtualDestructor == other.virtualDestructor &&
						equals(constructors, constructors, equal_target<LambdaExprPtr>()) &&
						equalTarget(destructor, other.destructor) &&
						memberFunctions == other.memberFunctions
					);
		}

		/**
		 * Implements the inequality operator by obtaining the negation
		 * of the result of the equality operation.
		 */
		bool operator!=(const ClassMetaInfo& other) const {
			return !(*this == other);
		}

		/**
		 * Required to implement the printable interface.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * Required to enable instances to be migrated when cloning the
		 * carrier-object to another node manager.
		 */
		void migrateTo(const NodePtr& target) const;

		/**
		 * Obtains the object type this meta-info class is describing. The
		 * results is extracted from the internal fields and might be null
		 * if non of the fields is defined.
		 *
		 * @return the type of class type this info-class is providing extra
		 * 		information unless this can not be determined based on the
		 * 		internally stored information - in this case the result will
		 * 		be NULL.
		 */
		TypePtr getClassType() const;

	private:

		/**
		 * A utility function checking the object type of the given lambda.
		 *
		 * @return true if valid, false otherwise
		 */
		bool checkObjectType(const LambdaExprPtr& lambda) const;
	};

	/**
	 * Obtains a reference to the class-meta information currently attached to the
	 * given type.
	 *
	 * @param type the type for which the class meta-info should be obtained (must be a potential class type)
	 * @return a reference to the attached meta-info
	 */
	const ClassMetaInfo& getMetaInfo(const TypePtr& type);

	/**
	 * Updates the meta-information attached to the given type.
	 *
	 * @param type the type the meta-information should be attached to
	 * @param info the information to be attached
	 */
	void setMetaInfo(const TypePtr& type, const ClassMetaInfo& info);

} // end namespace core
} // end namespace insieme

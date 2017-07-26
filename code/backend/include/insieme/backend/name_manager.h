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
 */

#pragma once

#include <set>
#include <unordered_map>

#include "insieme/core/ir_node.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {

	/**
	 * An abstract interface for a name manager implementation. The task of the name
	 * manager is to provide names for types, functions and variables within the generated
	 * C program, thereby avoiding collisions. The abstract interface is used by the backend.
	 * Concrete implementations are free to pick an arbitrary naming schema.
	 */
	class NameManager {
	  public:
		/**
		 * A virtual destructor to support proper cleanups for sub-classes.
		 */
		virtual ~NameManager(){};

		/**
		 * This method will be invoked at the beginning of the conversion process and
		 * should be utilized to collect a list of names which shell not be assigned
		 * to any other object (e.g. the names of global variables).
		 */
		virtual void registerGlobalNames(const core::NodePtr& root) = 0;

		/**
		 * Obtains a name for the construct represented by the given node pointer.
		 *
		 * @param ptr the construct to be named
		 * @param fragment a hint for the construction of the name - may be considered or not
		 * @return a name to be used within the generated C code
		 */
		virtual string getName(const core::NodePtr& ptr, const string& fragment = "") = 0;

		/**
		 * Fixes the name of a construct within this name manager. Future getName(..) calls will
		 * return the given name for the given construct.
		 *
		 * @param ptr the construct to be named
		 * @param name the name to be assigned to the given construct
		 */
		virtual void setName(const core::NodePtr& ptr, const string& name) = 0;

		/**
		 * Reserves a given name, so that it can not be used by the original program
		 * (used e.g. for runtime function names)
		 *
		 * @param name the name to reserve
		 */
		virtual void reserveName(const string& name) = 0;

		/**
		 * Creates a new sub-scope for variables. Whenever entering a new scope in C this function
		 * shell be invoked to create a new scope. In case the isolated flag is set, the parent
		 * scope will no longer be visible - otherwise names from the parent scope will be visible.
		 */
		virtual void pushVarScope(bool isolated = false) = 0;

		/**
		 * Props a sub-scope for all variables by dropping their names. The corresponding parent scope
		 * will be visible again.
		 */
		virtual void popVarScope() = 0;
	};


	/**
	 * Generates unique names for anonymous IR nodes when required.
	 * Uses a simple counting system. Not thread safe, and won't necessarily generate the same name
	 * for the same node in different circumstances. Names will, however, stay the same for unchanged
	 * programs over multiple runs of the compiler.
	 */
	class SimpleNameManager : public NameManager {
		/**
		 * A container realizing a level of scope within this name manager.
		 */
		struct Scope {
			/**
			 * The map linking IR constructs to associated names.
			 */
			utils::map::PointerMap<core::NodePtr, string> names;

			/**
			 * The set of used names.
			 */
			std::set<string> usedNames;

			/**
			 * A flag indicating whether this scope is extending its parent scope or forming a new root-scope.
			 */
			bool extendsParentScope;

			/**
			 * Creates a new Scope optionally extending its parent scope.
			 */
			Scope(bool extendsParentScope = false) : extendsParentScope(extendsParentScope) {}
		};

		/**
		 * A counter used to generate individual names.
		 */
		unsigned long num;

		/**
		 * A prefix to be used for all kind of generated names.
		 */
		const string prefix;

		/**
		 * The global scope handling all globally defined names including function and type names.
		 */
		Scope globalScope;

		/**
		 * The list of nested scopes forming the current stack of scopes.
		 */
		vector<Scope> varScope;

	  public:
		/**
		 * The default constructor for a new name manager.
		 *
		 * @param prefix a string to be used in front of every name generated by this name manager
		 */
		SimpleNameManager(const string& prefix = "__insieme_") : num(0), prefix(prefix), varScope(toVector(Scope(false))) {}

		/**
		 * Obtains a reference to the common prefix used by this name manager.
		 */
		virtual const string& getNamePrefix() const {
			return prefix;
		};

		/**
		 * Initializes a list of used names which shell not be assigned to any construct.
		 */
		virtual void registerGlobalNames(const core::NodePtr& root);

		/**
		 * Obtains a name for the construct represented by the given node pointer.
		 *
		 * @param ptr the construct to be named
		 * @param fragment a hint for the construction of the name - may be considered or not
		 * @return a name to be used within the generated C code
		 */
		virtual string getName(const core::NodePtr& ptr, const string& fragment = "");

		/**
		 * A special variant of the method above dealing exclusively with variable names.
		 *
		 * @param var the variable to be named
		 * @return the name assigend to the given variable by this name manager
		 */
		string getName(const core::VariablePtr& var);

		/**
		 * Fixes the name of a construct within this name manager. Future getName(..) calls will
		 * return the given name for the given construct.
		 *
		 * @param ptr the construct to be named
		 * @param name the name to be assigned to the given construct
		 */
		virtual void setName(const core::NodePtr& ptr, const string& name);

		/**
		 * A special variant of the setName method dealing exclusively with variable names.
		 *
		 * @param var the variable to be named
		 * @return name the name to be assigned to the given variable within the current scope
		 */
		void setName(const core::VariablePtr& var, const string& name);

		/**
		 * Reserves a given name, so that it can not be used by the original program
		 * (used e.g. for runtime function names)
		 *
		 * @param name the name to reserve
		 */
		virtual void reserveName(const string& name);

		/**
		 * Creates a new sub-scope for variables. Whenever entering a new scope in C this function
		 * shell be invoked to create a new scope. In case the isolated flag is set, the parent
		 * scope will no longer be visible - otherwise names from the parent scope will be visible.
		 */
		virtual void pushVarScope(bool isolated = false);

		/**
		 * Props a sub-scope for all variables by dropping their names. The corresponding parent scope
		 * will be visible again.
		 */
		virtual void popVarScope();

	  private:
		// -------------- some utility functions -----------------

		/**
		 * Determines whether the given name is already used within the current scope.
		 */
		bool isUsed(const string& name) const;

		/**
		 * Looks up the given construct within the internally managed name maps to determine
		 * whether a name has been assigned before. If so, a pointer to the name will be returned.
		 * Otherwise NULL will be returned.
		 */
		const string* lookup(const core::NodePtr& ptr) const;
	};

} // end: namespace simple_backend
} // end: namespace insieme

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


#include "insieme/core/ir_pointer.h"

#include "insieme/utils/map_utils.h"

#include "insieme/simple_backend/code_management.h"

namespace insieme {
	namespace core {
		class Variable;
		typedef Pointer<const Variable> VariablePtr;
	}
}

namespace insieme {
namespace simple_backend {


	/**
	 * Defines an interface for a variable manager. The variable manager is used by the simple backend to
	 * maintain information regarding individual variables. For instance, some of those may be marked as being
	 * realized as normal variables or references to the variables they are actually representing.
	 */
	class VariableManager {

		public:

			// TODO: change those values ... STACK => DIRECT, HEAP => INDIRECT
			// TODO: change those to directly or indirectly dereferenced
			enum MemoryLocation {
				NONE, 	/* < in case the variable is not referencing a memory cell */
				STACK, 	/* < the variable references a memory cell on the stack */
				HEAP 	/* < the variable references a memory cell on the heap */

//				DIRECT, 	/* < the variable represents the corresponding memory cell directly (e.g. a local variable) */
//				INDIRECT 	/* < the variable is a pointer to the actually represented memory cell */
			};

			/**
			 * The struct containing all the information stored for each individual variable.
			 */
			struct VariableInfo {
				MemoryLocation location;
			};

		private:

			/**
			 * The map containing all the information regarding variables indexed by the corresponding
			 * variables.
			 */
			utils::map::PointerMap<core::VariablePtr, VariableInfo> variableMap;

			/**
			 * The code fragment containing the global variable declaration.
			 */
			CodeFragmentPtr globalVar;

		public:

			/**
			 * A default constructor for this manager.
			 */
			VariableManager() : variableMap(), globalVar() {};

			/**
			 * Obtains a reference to the internally maintained information associated to the
			 * given variable. The method will fail with an assertion error if there is no information
			 * maintained for this variable.
			 *
			 * @param variable the variable for which information is requested
			 * @return the information maintained for the given variable
			 */
			const VariableInfo& getInfo(const core::VariablePtr& variable) const;

			/**
			 * Adds / updates the information maintained for the given variable.
			 *
			 * @param variable the variable which's information should be updated
			 * @param info the new information to be associated to the given variable
			 */
			void addInfo(const core::VariablePtr& variable, const VariableInfo& info);

			/**
			 * Removes the information associated to the given variable.
			 *
			 * @param variable the variable which's information should be removed.
			 */
			void removeInfo(const core::VariablePtr& variable);

			/**
			 * Tests whether there is some information associated to the given variable.
			 *
			 * @param variable the variable to be tested
			 */
			bool hasInfoFor(const core::VariablePtr& variable) const;

			/**
			 * Obtains a pointer to the current global variable fragment.
			 */
			CodeFragmentPtr getGlobalVarFragment() const {
				return globalVar;
			}

			/**
			 * Sets up the global variable fragment.
			 */
			void setGlobalVarFragment(CodeFragmentPtr fragment) {
				assert(!globalVar && "Global-variable struct may only be initialized once!");
				globalVar = fragment;
			}

	};

} // end namespace simple_backend
} // end namespace insieme

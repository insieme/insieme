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

#include "insieme/simple_backend/simple_backend.h"
#include "insieme/simple_backend/code_management.h"

#include "insieme/utils/map_utils.h"

// forward declarations
namespace insieme {
	namespace core {

		template<class T> class Pointer;
		class Expression;
		typedef Pointer<const Expression> ExpressionPtr;

		class NodeManager;

		namespace lang {
			class BasicGenerator;
		}
	}
}

namespace insieme {
namespace simple_backend {

	// some forward declarations
	class StmtConverter;
	class NameManager;
	class TypeManager;
	class VariableManager;
	class FunctionManager;
	class JobManager;

	/**
	 * Stores the persistent state objects required to perform a simple_backend conversion.
	 * This includes a NameGenerator, a FunctionManager and a TypeManager.
	 */
	class Converter {

		// A list of managers required for the conversion process
		StmtConverter* stmtConverter;
		NameManager* nameManager;
		TypeManager* typeManager;
		VariableManager* variableManager;
		FunctionManager* functionManager;
		JobManager* jobManager;
		core::NodeManager* nodeManager;

	public:

		/**
		 * A default constructor for this converter. All internal managers will be set to null.
		 */
		Converter() {}

		/**
		 * A constructor allowing the explicit creation of a converter of this type. The given manager are used for
		 * the actual conversion.
		 *
		 * @param stmtConverter the actual converter implementation handling statements and expressions
		 * @param nameManager the manager used to pick and maintain names for the generated constructs (types, functions, variables, ...)
		 * @param typeManager the manager controlling the generation of types
		 * @param varManager the manager used for managing the scope of variables
		 * @param funcMan the function manager handling the creation of closures and their invocation
		 * @param nodeManager the node manager to be used for creating and maintaining intermediate IR nodes
		 */
		Converter(StmtConverter& stmtConverter, NameManager& nameManager, TypeManager& typeManager, VariableManager& varManager, FunctionManager& funcMan, core::NodeManager& nodeManager)
			: stmtConverter(&stmtConverter), nameManager(&nameManager), typeManager(&typeManager), variableManager(&varManager), functionManager(&funcMan), nodeManager(&nodeManager) { }


		/**
		 * Triggers the actual conversion of an IR program into target code.
		 *
		 * @param prog the program to be converted
		 * @return the converted target code program
		 */
		backend::TargetCodePtr convert(const core::NodePtr& prog);
		
		/**
		 * Sets the name of the primary entry point to "main"
		 */
		void setMainName(const core::NodePtr& prog);

		StmtConverter& getStmtConverter() const {
			assert(stmtConverter);
			return *stmtConverter;
		}

		void setStmtConverter(StmtConverter* converter) {
			stmtConverter = converter;
		}

		NameManager& getNameManager() const {
			assert(nameManager);
			return *nameManager;
		}

		void setNameManager(NameManager* manager) {
			nameManager = manager;
		}

		TypeManager& getTypeManager() const {
			assert(typeManager);
			return *typeManager;
		}

		void setTypeManager(TypeManager* manager) {
			typeManager = manager;
		}

		VariableManager& getVariableManager() const {
			assert(variableManager);
			return *variableManager;
		}

		void setVariableManager(VariableManager* manager) {
			variableManager = manager;
		}

		FunctionManager& getFunctionManager() const {
			assert(functionManager);
			return *functionManager;
		}

		void setFunctionManager(FunctionManager* manager) {
			functionManager = manager;
		}

		JobManager& getJobManager() const {
			assert(jobManager);
			return *jobManager;
		}

		void setJobManager(JobManager* manager) {
			jobManager = manager;
		}

		core::NodeManager& getNodeManager() const {
			assert(nodeManager);
			return *nodeManager;
		}

		void setNodeManager(core::NodeManager* manager) {
			nodeManager = manager;
		}

		const core::lang::BasicGenerator& getLangBasic() const {
			assert(nodeManager);
			return nodeManager->getLangBasic();
		}

	};

} // namespace simple_backend
} // namespace insieme

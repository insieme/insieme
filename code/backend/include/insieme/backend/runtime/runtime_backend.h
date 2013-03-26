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

#include "insieme/backend/backend.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/type_manager.h"

#include "insieme/backend/operator_converter.h"

namespace insieme {
namespace backend {
namespace runtime {

	// A forward declaration of the sequential backend implementation
	class RuntimeBackend;
	typedef std::shared_ptr<RuntimeBackend> RuntimeBackendPtr;


	/**
	 * A signature for functions to be passed to backends to influence the operator converter table.
	 * Using this mechanism, the operator table can be manipulated by an external component.
	 */
	typedef std::function<OperatorConverterTable&(core::NodeManager&, OperatorConverterTable&)> OperationTableExtender;

	/**
	 * The facade for the backend capable of generating code to be used
	 * by the runtime backend.
	 */
	class RuntimeBackend : public Backend {

		/**
		 * The table containing operator converters to be used during the
		 * conversion process.
		 */
		OperationTableExtender operationTableExtender;

		/**
		 * A flag enabling the inclusion of effort estimations within work-item tables.
		 */
		bool includeEffortEstimation;

	public:

		/**
		 * A constructor of this kind of backend accepting an operator table extender.
		 */
		RuntimeBackend(bool includeEffortEstimation, const OperationTableExtender& extender = &RuntimeBackend::unchanged)
			: operationTableExtender(extender), includeEffortEstimation(includeEffortEstimation) {}


		/**
		 * A factory method obtaining a smart pointer referencing a
		 * fresh instance of the runtime backend using the default configuration.
		 *
		 * @return a smart pointer to a fresh instance of the runtime backend
		 */
		static RuntimeBackendPtr getDefault(bool includeEffortEstimation = false);

		/**
		 * The main facade function of the runtime backend. This function converts the given
		 * IR representation into C99-target code interacting with the Insieme Runtime environment.
		 *
		 * @param source the program to be converted
		 * @return a pointer to the converted target code
		 */
		backend::TargetCodePtr convert(const core::NodePtr& source) const;

		/**
		 * Obtains a reference to the operator table this backend instance has been instantiated
		 * with.
		 */
		const OperationTableExtender& getOperatorTableExtender() const {
			return operationTableExtender;
		}

		/**
		 * Updates the operator table extender.
		 *
		 * @param extender the new extender to be used
		 */
		void setOperatorTableExtender(const OperationTableExtender& extender) {
			operationTableExtender = extender;
		}

	private:

		/**
		 * A private op-table extender leafing the operator table unchanged.
		 */
		static OperatorConverterTable& unchanged(core::NodeManager&, OperatorConverterTable& table) {
			return table;
		}
	};

	FunctionIncludeTable& addRuntimeFunctionIncludes(FunctionIncludeTable& table);

	TypeIncludeTable& addRuntimeTypeIncludes(TypeIncludeTable& table);


}
}
}

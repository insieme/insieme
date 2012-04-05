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

namespace insieme {
namespace backend {
namespace ocl_host {

	// A forward declaration of the OpenCL Host backend implementation
	class OCLHostBackend;
	typedef std::shared_ptr<OCLHostBackend> OCLHostBackendPtr;

	/**
	 * The OpenCL Host backend aims on generating pure sequential code without
	 * any dependencies to any runtime implementation.
	 */
	class OCLHostBackend : public Backend {
		// optional path to dump the binary of the kernel after preprocessing
		const std::string kernelDumpPath;
	public:
		/**
		 * default constructor
		 */
		OCLHostBackend(){}

		/**
		 * constructor setting the kernelDumpPath
		 */
		OCLHostBackend(const std::string& kernelDumpPath) : kernelDumpPath(kernelDumpPath) {}

		/**
		 * A factory method obtaining a smart pointer referencing a
		 * fresh instance of the OpenCL Host backend using the default configuration.
		 *
		 * @return a smart pointer to a fresh instance of the sequential backend
		 */
		static OCLHostBackendPtr getDefault();

		/**
		 * A factory method obtaining a smart pointer referencing a
		 * fresh instance of the OpenCL Host backend using the default configuration.
		 *
		 * @param kernelDumpPath a path to dump the binary of the kernel after preprocessing
		 * @return a smart pointer to a fresh instance of the sequential backend
		 */
		static OCLHostBackendPtr getDefault(const std::string& kernelDumpPath);

		/**
		 * The main facade function of the OpenCL Host backend. This function converts the given
		 * IR representation into pure C99-target code.
		 *
		 * @param source the program to be converted
		 * @return a pointer to the converted target code
		 */
		backend::TargetCodePtr convert(const core::NodePtr& source) const;

	};

	FunctionIncludeTable& addOpenclHostFunctionIncludes(FunctionIncludeTable& table);

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme

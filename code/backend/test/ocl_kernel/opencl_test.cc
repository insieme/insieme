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

#include <vector>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/frontend/frontend.h"
#include "insieme/utils/config.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/backend/ocl_host/host_backend.h"
#include "insieme/utils/config.h"

using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::core::annotations;
using namespace insieme::utils::set;

using namespace insieme::utils::set;
using namespace insieme::utils::log;

TEST(ocl_hostKernel, vecadd_bare) {
	NodeManager manager;
	LOG(INFO) << "Test Directory: " << std::string(OCL_KERNEL_TEST_DIR) << std::endl;
	insieme::frontend::ConversionJob job(CLANG_SRC_DIR "../../../test/ocl/vec_add/vec_add_bare.c");

	// Frontend PATH
	job.addIncludeDirectory(CLANG_SRC_DIR);								// this is for ocl_device.h in kernel.cl
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");					// this is for CL/cl.h in host.c
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/common/");	// lib_icl
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/vec_add");

	// Backend PATH
	job.addIncludeDirectory(OCL_KERNEL_TEST_DIR);
	job.setOption(insieme::frontend::ConversionJob::Lib_icl);

	// finally, do the conversion
	auto program = job.execute(manager);

	LOG(INFO) << "#01 Converted vec_add and kernel to IR, have a look:" << std::endl
			  << insieme::core::printer::PrettyPrinter(program) << std::endl << "#01 END" << std::endl << std::endl;

	auto backend = insieme::backend::ocl_host::OCLHostBackend::getDefault();
	auto converted = backend->convert(program);
	LOG(INFO) << "#02 Converted IR back to OCL C, see here:" << std::endl << *converted << "#02 END" << std::endl;
}

TEST(ocl_hostKernel, vecadd) {
	NodeManager manager;
	std::cout << "Test Directory: " << std::string(OCL_KERNEL_TEST_DIR) << std::endl;
	insieme::frontend::ConversionJob job(CLANG_SRC_DIR "../../../test/ocl/vec_add/vec_add.c");

	// Frontend PATH
	job.addIncludeDirectory(CLANG_SRC_DIR);								// this is for ocl_device.h in kernel.cl
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");					// this is for CL/cl.h in host.c
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/common/");	// lib_icl
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/vec_add");

	// Backend PATH
	job.addIncludeDirectory(OCL_KERNEL_TEST_DIR);
	job.setOption(insieme::frontend::ConversionJob::Lib_icl);

	std::cout << "Converting input program '" << string(OCL_KERNEL_TEST_DIR) << "kernel.cl" << "' to IR...\n";
	auto program = job.execute(manager);
	std::cout << "Done.\n";

	LOG(INFO) << "Starting OpenCL host code transformations";

	insieme::core::printer::PrettyPrinter pp(program); // program->getElement(0)

	std::cout << "Starting OpenCL Backend visit\n";

	auto backend = insieme::backend::ocl_host::OCLHostBackend::getDefault();
	auto converted = backend->convert(program);
	std::cout << "Converted code:\n" << *converted;
}

TEST(ocl_hostKernel, matmul) {
	NodeManager manager;
	std::cout << "Test Directory: " << std::string(OCL_KERNEL_TEST_DIR) << std::endl;
	insieme::frontend::ConversionJob job(CLANG_SRC_DIR "../../../test/ocl/mat_mul/mat_mul.c");

	// Frontend PATH
	job.addIncludeDirectory(CLANG_SRC_DIR);								// this is for ocl_device.h in kernel.cl
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");						// this is for CL/cl.h in host.c
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/common/");	// lib_icl
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/mat_mul");

	// Backend PATH
	job.addIncludeDirectory(OCL_KERNEL_TEST_DIR);
	//job.setOption(insieme::frontend::ConversionJob::OpenCL);
	job.setOption(insieme::frontend::ConversionJob::Lib_icl);

	std::cout << "Converting input program '" << string(OCL_KERNEL_TEST_DIR) << "kernel.cl" << "' to IR...\n";
	auto program = job.execute(manager);
	std::cout << "Done.\n";

	LOG(INFO) << "Starting OpenCL host code transformations";

	insieme::core::printer::PrettyPrinter pp(program); // program->getElement(0)

	std::cout << "Starting OpenCL Backend visit\n";

	auto backend = insieme::backend::ocl_host::OCLHostBackend::getDefault();
	auto converted = backend->convert(program);
	std::cout << "Converted code:\n" << *converted;
}

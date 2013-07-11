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

#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/program.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/compiler/compiler.h"


namespace insieme {
namespace frontend {

const unsigned ConversionJob::DEFAULT_FLAGS = PrintDiag;

ConversionJob::ConversionJob(const boost::filesystem::path& path)
	: files(toVector(path.string())), stdLibIncludeDirs(insieme::utils::compiler::getDefaultCppIncludePaths()), standard("c99"), flags(DEFAULT_FLAGS) {};

ConversionJob::ConversionJob(const vector<string>& files, const vector<string>& includeDirs)
	: files(files), includeDirs(includeDirs), stdLibIncludeDirs(insieme::utils::compiler::getDefaultCppIncludePaths()), standard("c99"), definitions(), flags(DEFAULT_FLAGS) {};

void ConversionJob::addDefinition(const string& name, const string& value) {
	std::stringstream def;
	def << name;
	if (!value.empty()) def << "=" << value;
	definitions.push_back(def.str());
}

core::ProgramPtr ConversionJob::execute(core::NodeManager& manager) {


	// add definitions needed by the OpenCL frontend
	ConversionJob job = *this;
	if(hasOption(OpenCL)) {
		job.addIncludeDirectory(SRC_DIR);
		job.addIncludeDirectory(SRC_DIR "inputs");
		job.addIncludeDirectory(SRC_DIR "../../../test/ocl/common/");  // lib_icl

		job.addDefinition("INSIEME");
	}

	// create a temporary manager
	core::NodeManager tmpMgr(manager);

	// create the program parser
	frontend::Program program(manager, job);

	// set up the translation units
	program.addTranslationUnits(job);

	// convert the program
	auto res = program.convert();

	// apply OpenMP sema conversion
	if (hasOption(OpenMP)) {
		res = frontend::omp::applySema(res, tmpMgr);
	}

	// apply OpenCL conversion
	if(hasOption(OpenCL)) {
		frontend::ocl::HostCompiler oclHostCompiler(res, job);
		res = oclHostCompiler.compile();
	}

	// apply Cilk conversion
	if(hasOption(Cilk)) {
		res = frontend::cilk::applySema(res, tmpMgr);
	}

	// return instance within global manager
	return manager.get(res);
}

void ConversionJob::storeAST(core::NodeManager& manager, const string& output_file) {
	// add definitions needed by the OpenCL frontend
	ConversionJob job = *this;
	if(hasOption(OpenCL)) {
		job.addIncludeDirectory(SRC_DIR);
		job.addIncludeDirectory(SRC_DIR "inputs");
		job.addIncludeDirectory(SRC_DIR "../../../test/ocl/common/");  // lib_icl

		job.addDefinition("INSIEME");
	}

	// create a temporary manager
	core::NodeManager tmpMgr(manager);

	// create the program parser
	frontend::Program program(manager, job);

	// set up the translation units
	program.addTranslationUnits(job);

    // store translation units
    program.storeTranslationUnits(output_file);
}

} // end namespace frontend
} // end namespace insieme

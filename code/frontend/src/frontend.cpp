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
#include "insieme/utils/cmd_line_utils.h"


namespace insieme {
namespace frontend {

const unsigned ConversionJob::DEFAULT_FLAGS = 0;

ConversionJob::ConversionJob(core::NodeManager& manager, const string& file)
	: manager(manager), files(toVector(file)), standard("c99"), flags(DEFAULT_FLAGS) {};

ConversionJob::ConversionJob(core::NodeManager& manager, const vector<string>& files, const vector<string>& includeDirs)
	: manager(manager), files(files), includeDirs(includeDirs), standard("c99"), definitions(), flags(DEFAULT_FLAGS) {};

void ConversionJob::addDefinition(const string& name, const string& value) {
	std::stringstream def;
	def << name << "=" << value;
	definitions.push_back(def.str());
}

core::ProgramPtr ConversionJob::execute() {

	// setup the include directories
	CommandLineOptions::IncludePaths = includeDirs;

	// setup the standard
	CommandLineOptions::STD = standard;

	// setup definition
	CommandLineOptions::Defs = definitions;

	// setup additional flags
	CommandLineOptions::OpenMP = hasOption(OpenMP);
	CommandLineOptions::OpenCL = hasOption(OpenCL);
	CommandLineOptions::Cilk = hasOption(Cilk);

	// add definitions needed by the OpenCL frontend
	if(hasOption(OpenCL)) {
		CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "inputs");
		CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR));
		CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "../../../test/ocl/common/"); // lib_icl

		CommandLineOptions::Defs.push_back("INSIEME");
	}

	// create a temporary manager
	core::NodeManager tmpMgr(manager);

	// create the program parser
	frontend::Program program(tmpMgr);

	// set up the translation units
	program.addTranslationUnits(files);

	// convert the program
	auto res = program.convert();

	// apply OpenMP sema conversion
	if (hasOption(OpenMP)) {
		res = frontend::omp::applySema(res, tmpMgr);
	}

	// apply OpenCL conversion
	if(hasOption(OpenCL)) {
		frontend::ocl::HostCompiler oclHostCompiler(res);
		res = oclHostCompiler.compile();
	}

	// apply Cilk conversion
	if(hasOption(Cilk)) {
		res = frontend::cilk::applySema(res, tmpMgr);
	}

	// return instance within global manager
	return manager.get(res);
}

} // end namespace frontend
} // end namespace insieme

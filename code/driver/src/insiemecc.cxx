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

/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

#include <string>

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/frontend/frontend.h"
#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/driver/cmd/options.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/filter/standard_filter.h"
#include "insieme/transform/rulebased/transformations.h"

using namespace std;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace be = insieme::backend;
namespace ut = insieme::utils;
namespace cp = insieme::utils::compiler;
namespace tr = insieme::transform;
namespace cmd = insieme::driver::cmd;


int main(int argc, char** argv) {
	// filter logging messages
	Logger::setLevel(ERROR);

//	// for debugging
//	std::cout << "Arguments: \n";
//	for(int i=0; i<argc; i++) {
//		std::cout << "\t" << argv[i] << "\n";
//	}
//	std::cout << "\n";

	// Step 1: parse input parameters
	//		This part is application specific and need to be customized. Within this
	//		example a few standard options are considered.
	cmd::Options options = cmd::Options::parse(argc, argv);

	if (!options.valid) return (options.help)?0:1;

	// Step 2: load input code
	//		The frontend is converting input code into the internal representation (IR).
	//		The memory management of IR nodes is realized using node manager instances.
	//		The life cycle of IR nodes is bound to the manager the have been created by.
	//      If the c flag is set only the compilation will be done and the output file
	//      will be an object file.
	co::NodeManager manager;
    if(options.job.hasOption(fe::ConversionJob::CompilationOnly)) {
        cout << "COMPILATION ONLY" << endl;
        options.job.storeAST(manager, options.outFile);
        return 0;
    }


    auto program = options.job.execute(manager);



	// Step 2: produce output code
	//		This part converts the processed code into C-99 target code using the
	//		backend producing parallel code to be executed using the Insieme runtime
	//		system. Backends targeting alternative platforms may be present in the
	//		backend modul as well.
	cout << "Creating target code ...\n";
	auto targetCode = be::runtime::RuntimeBackend::getDefault()->convert(program);


	// Step 3: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	cout << "Building binaries ...\n";
	cp::Compiler compiler = cp::Compiler::getDefaultCppCompiler();
	compiler = cp::Compiler::getRuntimeCompiler(compiler);
	bool success = cp::compileToBinary(*targetCode, options.outFile, compiler);

	// done
	return (success)?0:1;
}
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
#include <fstream>

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/driver/cmd/options.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/filter/standard_filter.h"
#include "insieme/transform/rulebased/transformations.h"

#include <boost/algorithm/string.hpp>

using namespace std;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace be = insieme::backend;
namespace cp = insieme::utils::compiler;
namespace tr = insieme::transform;
namespace cmd = insieme::driver::cmd;

namespace {

	// TODO: move to some utility header within the driver + add a unit test

	// some magic number to identify our files
	const long MAGIC_NUMBER = 42*42*42*42;


	bool isInsiemeLib(const fe::path& file) {
		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x; in >> x;

		// check magic number
		return x == MAGIC_NUMBER;
	}

	fe::tu::IRTranslationUnit loadLib(co::NodeManager& mgr, const fe::path& file) {
		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x; in >> x; assert(x == MAGIC_NUMBER);

		// load content
		return fe::tu::load(in, mgr);
	}

	void saveLib(const fe::tu::IRTranslationUnit& unit, const fe::path& file) {
		std::ofstream out(file.string(), std::ios::out | std::ios::binary );
		out << MAGIC_NUMBER;		// start with magic number
		fe::tu::dump(out, unit);	// dump the rest
	}


}


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
	bool compileOnly;
	cmd::Options options = cmd::Options::parse(argc, argv)
		// one extra parameter to limit the compiler to creating an .o file
		("compile,c", &compileOnly, "compilation only")
	;

	//indicates that a shared object files should be created
	bool createSharedObject = options.outFile.find(".so")!=std::string::npos;

	// disable cilk support for insiemecc
	options.job.setOption(fe::ConversionJob::Cilk, false);

	if (!options.valid) return (options.help)?0:1;

	// Step 2: filter input files
	vector<fe::path> inputs;
	vector<fe::path> libs;
	vector<fe::path> extLibs;

	for(const fe::path& cur : options.job.getFiles()) {
		auto ext = fs::extension(cur);
		if (ext == ".o" || ext == ".so") {
			if (isInsiemeLib(cur)) {
				libs.push_back(cur);
			} else {
				extLibs.push_back(cur);
			}
		} else {
			inputs.push_back(cur);
		}
	}

	// update input files
	options.job.setFiles(inputs);

    // Step 3: load input code
	co::NodeManager mgr;

	// load libraries
	options.job.setLibs(::transform(libs, [&](const fe::path& cur) { return loadLib(mgr, cur); }));

	// if it is compile only or if it should become an object file => save it
	if (compileOnly || createSharedObject) {
		auto res = options.job.toTranslationUnit(mgr);
		saveLib(res, options.outFile);
		return 0;		// done
	}

	// convert src file to target code
    auto program = options.job.execute(mgr);

	// Step 3: produce output code
	//		This part converts the processed code into C-99 target code using the
	//		backend producing parallel code to be executed using the Insieme runtime
	//		system. Backends targeting alternative platforms may be present in the
	//		backend modul as well.
	cout << "Creating target code ...\n";
	auto targetCode = be::runtime::RuntimeBackend::getDefault()->convert(program);


	// Step 4: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	cout << "Building binaries ...\n";
	cp::Compiler compiler = cp::Compiler::getDefaultCppCompiler();
	compiler = cp::Compiler::getRuntimeCompiler(compiler);
	for(auto cur : extLibs) compiler.addFlag(cur.string());			// TODO: add extra setter for libraries, not just a flag
	bool success = cp::compileToBinary(*targetCode, options.outFile, compiler);

	// done
	return (success)?0:1;
}

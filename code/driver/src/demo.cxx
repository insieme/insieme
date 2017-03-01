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
 *
 */
/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

#include <string>

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/frontend/frontend.h"
#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/filter/standard_filter.h"
#include "insieme/transform/rulebased/transformations.h"

using namespace std;
using namespace insieme;

namespace cp = insieme::utils::compiler;
namespace tr = insieme::transform;


int main(int argc, char** argv) {
	// Step 1: parse input parameters
	//		This part is application specific and need to be customized. Within this
	//		example a few standard options are considered.
	std::string outFile;
	unsigned unrollingFactor = 1;

	auto parser = driver::cmd::Options::getParser();
	parser.addParameter("unrolling,u", unrollingFactor, 5u,                   "The factor by which the innermost loops should be unrolled.");
	parser.addParameter("outfile,o",   outFile,         std::string("a.out"), "output file");
	auto options = parser.parse(argc, argv);

	if(!options.valid) { return (options.settings.help) ? 0 : 1; }


	// Step 2: load input code
	//		The frontend is converting input code into the internal representation (IR).
	//		The memory management of IR nodes is realized using node manager instances.
	//		The life cycle of IR nodes is bound to the manager the have been created by.
	core::NodeManager manager;
	auto program = options.job.execute(manager);


	// Step 3: process code
	//		This is the part where the actual operations on the processed input code
	//		are conducted. You may utilize whatever functionality provided by the
	//		compiler framework to analyze and manipulate the processed application.
	//		In this example we are simply unrolling all innermost loops by a factor
	//		of 5 which is always a safe transformation.

	cout << "Before Transformation:\n";
	cout << dumpPretty(program);

	// for all nodes x | if x is "innermostLoop" => unroll(x)
	auto transform = tr::makeForAll(tr::filter::innermostLoops(), tr::rulebased::makeLoopUnrolling(unrollingFactor));
	program = transform->apply(program);

	cout << "After Transformation:\n";
	cout << dumpPretty(program);


	// Step 4: produce output code
	//		This part converts the processed code into C-99 target code using the
	//		backend producing parallel code to be executed using the Insieme runtime
	//		system. Backends targeting alternative platforms may be present in the
	//		backend modul as well.
	cout << "Creating target code ...\n";
	auto targetCode = backend::runtime::RuntimeBackend::getDefault()->convert(program);


	// Step 5: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	cout << "Building binaries ...\n";
	cp::Compiler compiler = cp::Compiler::getDefaultC99Compiler();
	compiler = cp::Compiler::getOptimizedCompiler(compiler);
	compiler = cp::Compiler::getRuntimeCompiler(compiler);
	bool success = cp::compileToBinary(*targetCode, outFile, compiler);

	// done
	return (success) ? 0 : 1;
}

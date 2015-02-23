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

#include "insieme/utils/logging.h"

#include "insieme/iwir/utils.h"
#include "insieme/iwir/cmd/options.h"
#include "insieme/iwir/iwir_converter.h"
#include "insieme/iwir/iwir_builder.h"

#include <string>

using namespace insieme;

int main(int argc, char** argv) {

	std::string taskGraphFile;
	int verbosity;
	iwir::cmd::Options options = iwir::cmd::Options::parse(argc, argv)
 		("verbose,v", verbosity, 0, "vebosity level - 1 or 2")
 		("task-graph,g", taskGraphFile, string(""), "writes a dot representation of the given IWIR Workflow into the given file")
	;

	if (!options.valid) return (options.help)?0:1;

	Logger::get(std::cerr, DEBUG, verbosity);

	core::NodeManager mgr;
	std::string iwirFile = options.inFile;
	
	std::cout << "Reading input, and building IWIR AST " << iwirFile << std::endl;

	//IWR XML -> IWIR AST
	iwir::IWIRBuilder ib;
	ib.buildIWIR(iwirFile);

	std::cout << "Converting IWIR AST to INSPIRE" << std::endl;

	//IWIR AST -> INSPIRE 
	iwir::IWIRConverter iwirToIr(mgr);
	core::NodePtr ir = iwirToIr.convertIwirToIr(ib);

	// write task graph dot reprsentation 
	if (taskGraphFile != "") {
		std::cout << "Writing dot file file -- " << taskGraphFile  << std::endl;
		iwirToIr.writeTaskGraphToDot(ib, taskGraphFile);
	}

	// writing IR code
	if (options.outFile != "") {
		std::cout << "Writing output file -- " << options.outFile << std::endl;
		std::ofstream out(options.outFile);
		out << core::printer::PrettyPrinter(ir, core::printer::PrettyPrinter::PRINT_DEREFS);
	}

	return 0;
}

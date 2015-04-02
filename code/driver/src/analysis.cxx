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
#include "insieme/driver/cmd/insiemecc_options.h"

#include "insieme/analysis/cba/parallel_analysis.h"
#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/utils/cba_utils.h"

#include "insieme/utils/petri_net/petri_net_io.h"

using namespace std;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace pn = insieme::utils::petri_net;
namespace cba = insieme::analysis::cba;
namespace cmd = insieme::driver::cmd;

int main(int argc, char** argv) {
	// filter logging messages
	Logger::setLevel(ERROR);


	// Step 1: parse input parameters
	//		This part is application specific and need to be customized. Within this
	//		example a few standard options are considered.
	bool extract_execution_net = false;
	bool extract_state_graph = false;
	bool dump_equations = false;
    std::vector<std::string> arguments(argv, argv+argc);
	cmd::Options options = cmd::Options::parse(arguments)
		// register the extra flags
		("exec_net", 		'e', 	extract_execution_net, 		"extract the execution net from the program")
		("state_graph", 	's', 	extract_state_graph, 		"extract the state graph from the program")
		("dump_equations", 	'q', 	dump_equations, 			"dumps the equations utilized for computing the results")
	;
	if (!options.valid) return (options.settings.help)?0:1;


	// Step 2: load input code
	//		The frontend is converting input code into the internal representation (IR).
	//		The memory management of IR nodes is realized using node manager instances.
	//		The life cycle of IR nodes is bound to the manager the have been created by.
	co::NodeManager manager;

	std::cout << "Loading input files ...\n";
	auto main = co::StatementAddress(options.job.execute(manager)[0].as<co::LambdaExprPtr>()->getBody());

	dumpPretty(main);

	// Step 3: process code
	//		Run the requested analysis.

	// extract the execution graph
	if (extract_execution_net) {
		std::cout << "Creating Execution Net ...\n";
		std::cout << "		Running analysis ... \n";
		auto net = cba::getExecutionNet(main);
		std::cout << "		Dumping Graph ... \n";
		pn::plot(net, "exec_net.svg");
	}

	if (extract_state_graph) {
		std::cout << "Creating State Graph ...\n";
		std::cout << "		Running analysis ... \n";
		auto graph = cba::getExecutionStateGraph(main);
		std::cout << "		Dumping Graph ... \n";
		pn::plot(graph, "state_graph.svg");
	}

	const auto& analysis = cba::getCBA(main);

	if (dump_equations) {
		std::cout << "Dumping Equations ... \n";
		std::cout << "		Creating Dot file ... \n";
		{
			// open file
			std::ofstream out("solution.dot", std::ios::out );

			// write file
			analysis.plot(out);
		}

		// render file
		std::cout << "		Running dot ... \n";
		system("dot -Tsvg solution.dot -o solution.svg");
	}

	std::cout << "Done - " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints.\n";
}

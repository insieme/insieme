/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/driver/datalog/options.h"

#include <string>
#include <iostream>

#include <boost/filesystem.hpp>

namespace insieme {
namespace driver {
namespace datalog {

	using namespace std;
	namespace fs = boost::filesystem;

	bool Options::parseCommandLine(int argc, char** argv)
	{
		bpo::options_description desc("Supported parameters");

		desc.add_options()
		                ("help,h",
		                 "Show help and exit")

		                ("input-file,i", bpo::value<string>(&inputFile)->required(),
		                 "Input C code")

		                ("output-folder,o", bpo::value<string>(&outputFolder)->default_value("./datalog_facts"),
		                 "Output folder in which datalog facts should be dumped")

		                ("verbose,v", bpo::bool_switch(&verboseOutput)->default_value(false),
		                 "Output findings of IR tree traversal")

		                ("dump-ir", bpo::value<string>(&dumpIR)->implicit_value("ir_dump.txt"),
		                 "Also dump IR")

		                ("dump-json", bpo::value<string>(&dumpJSON)->implicit_value("json_dump.txt"),
		                 "Also dump JSON")
		                ;

		bpo::positional_options_description p;
		p.add("input-file", -1);

		try {
			bpo::store(bpo::command_line_parser(argc, argv).
			           options(desc).
			           positional(p).
			           run(),
			           vm);
			bpo::notify(vm);

			if (vm.count("help") == 0)
				return true;

		} catch (const bpo::error &err) {
			cerr << "Error: " << err.what() << endl << endl;
		}

		cout << "Usage: " << argv[0] << " [parameters]" << endl;
		cout << desc;

		return false;
	}

	bool Options::checkValidity()
	{
		return fs::exists(inputFile);
	}

} // namespace datalog
} // namespace driver
} // namespace insieme

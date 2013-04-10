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

#include "insieme/driver/cmd/options.h"

namespace insieme {
namespace driver {
namespace cmd {

	namespace bpo = boost::program_options;
	namespace fe = insieme::frontend;

	detail::OptionParser Options::parse(int argc, char** argv) {
		return detail::OptionParser(argc, argv);
	}

	namespace detail {

		OptionParser::OptionParser(int argc, char** argv) : argc(argc), argv(argv) {

			// -- assemble default parameters --------------------------

			// define options
			desc.add_options()
					("help,h", "produce this help message")
					("input-file,i", bpo::value<vector<string>>(), "input files - required!")
					("include-path,I", bpo::value<vector<string>>(), "include files - optional")
					("definitions,D", bpo::value<vector<string>>(), "preprocessor definitions - optional")
					("std,s", bpo::value<string>()->default_value("c99"), "determines the language standard")
					("no-omp", "disables OpenMP support")
					("no-cilk", "disables cilk support")
					("output-file,o", bpo::value<string>()->default_value("a.out"), "the output file")
			;

		}

		OptionParser::operator Options() {

			// -- parsing -------------------------------------------

			// define positional options (all options not being named)
			bpo::positional_options_description pos;
			pos.add("input-file", -1);

			// parse parameters
			bpo::variables_map map;
			bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
			bpo::notify(map);

			// -- processing -----------------------------------------

			Options res;

			// check whether help was requested
			res.help = map.count("help");
			if (res.help) {
				cout << "Usage: " << argv[0] << " [options | infile]" << "\n";
				cout << desc << "\n";
				res.valid = false;
				return res;
			}

			// build up result
			res.valid = true;

			// input files
			if (map.count("input-file")) {
				res.job.setFiles(map["input-file"].as<vector<string>>());
			} else {
				cout << "No input files provided!\n";
				res.valid = false;
				return res;
			}
			// include path
			if (map.count("include-path")) {
				res.job.setIncludeDirectories(map["include-path"].as<vector<string>>());
			}
			// preprocessor directives
			if (map.count("definitions")) {
				res.job.setDefinitions(map["definitions"].as<vector<string>>());
			}
			// insert
			res.job.setStandard("c99");
			if (map.count("std")) {
				res.job.setStandard(map["std"].as<string>());
			}

			// enable support for OpenMP and Cilk
			res.job.setOption(fe::ConversionJob::OpenMP, !map.count("no-omp"));
			res.job.setOption(fe::ConversionJob::Cilk, !map.count("no-cilk"));

			// output file (optional)
			res.outFile = "a.out";
			if (map.count("output-file")) {
				res.outFile = map["output-file"].as<string>();
			}

			// done
			return res;

		}
	}

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

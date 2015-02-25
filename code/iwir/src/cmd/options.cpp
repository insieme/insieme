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

#include "insieme/iwir/cmd/options.h"

#include <boost/algorithm/string.hpp>

namespace insieme {
namespace iwir {
namespace cmd {

	namespace bpo = boost::program_options;

	detail::OptionParser Options::parse(int argc, char** argv) {
		return detail::OptionParser(argc, argv);
	}

	namespace detail {

		OptionParser::OptionParser(int argc, char** argv) : argc(argc), argv(argv) {

			// -- assemble default parameters --------------------------

			// define options
			desc.add_options()
					("help,h", "produce this help message")
					("input-file,i", bpo::value<string>(), "input file - required!")
					("output-file,o", bpo::value<string>(), "the output file")
			;

		}

		OptionParser& OptionParser::operator()(const string& name, char symbol, bool& flag, const char* description) {

			// add flag to description
			desc.add_options()((name + "," + symbol).c_str(), description);

			// add parser step
			parser_steps.push_back([&](const bpo::variables_map& map) {
				flag = map.count(name); return true;
			});
			return *this;
		}

		OptionParser::operator Options() {

			// -- parsing -------------------------------------------

			// define positional options (all options not being named)
			bpo::positional_options_description pos;
			pos.add("input-file", -1);

			// parse parameters
			bpo::variables_map map;
			bpo::store(bpo::basic_command_line_parser<char>(argc, argv)
				.options(desc)
				.style(bpo::command_line_style::default_style | bpo::command_line_style::allow_long_disguise)
				.positional(pos)
				.allow_unregistered()
				.run(), map);


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
				res.inFile = map["input-file"].as<string>();
			} else {
				cout << "No input file provided!\n";
				res.valid = false;
				return res;
			}

			// output file (optional)
			res.outFile = "out.ir";
			if (map.count("output-file")) {
				res.outFile = map["output-file"].as<string>();
			}

			// extra flags
			for(auto cur : parser_steps) {
				res.valid = cur(map) && res.valid;
			}

			// done
			return res;

		}
	}

} // end namespace cmd
} // end namespace iwir  
} // end namespace insieme

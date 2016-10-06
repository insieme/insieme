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

#include "insieme/driver/perf_reg/options.h"

#include <string>
#include <iostream>

#include "insieme/driver/perf_reg/toolbox.h"


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	bool Options::parseCommandLine(int argc, char** argv)
	{
		bpo::options_description desc("Supported parameters");

		desc.add_options()
		                ("help,h",
		                 "Show help and exit")

		                ("keys,k", bpo::value<vector<string>>(&keys)->multitoken(),
		                 "List of keys that should be compared")

		                ("output-file,o", bpo::value<string>(&outputFile)->implicit_value("performance_regression_report.html"),
		                 "Save HTML output in a file")

		                ("input-files,i", bpo::value<vector<string>>(&inputFiles)->multitoken()->required(),
		                 "List of CSV files to be compared")

		                ("precision,p", bpo::value<unsigned>(&precision)->default_value(3),
		                 "Floating point precision")

		                ("min-data-points", bpo::value<unsigned>(&minDataPoints)->default_value(5)->notifier(check_min_dp),
		                 "Minimal number of historic logs for a test to be evaluated")

		                ("avg-over-last-x", bpo::value<unsigned>(&avgOverLastX)->default_value(10),
		                 "Consider X last records to calculate average of some key")

		                ("plot-last-x", bpo::value<unsigned>(&plotLastX)->default_value(20),
		                 "Number of data points to be plotted in the graphs")

		                ("graph-limit", bpo::value<int>(&graphLimit)->default_value(5),
		                 "Show at most X graphs for each key as they tend to make the browser slow")

		                ("id-as-x-axis", bpo::bool_switch(&idAsXAxis)->default_value(false),
		                 "Use ID instead of Timestamp for X axis (ID has to be a number!)")

		                ("create-fail-file", bpo::value<string>(&failFile)->implicit_value("performance_regressions_found.flag"),
		                 "Create (delete) given file if (no) regressions are found")
		                ;

		bpo::positional_options_description p;
		p.add("input-files", -1);

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

	void Options::check_min_dp(unsigned val)
	{
		unsigned min = 2;

		if (val < min) {
			throw bpo::validation_error(bpo::validation_error::invalid_option_value,
			                            "min-data-points needs to be at least " + to_string(min),
			                            std::to_string(val));
		}
	}

} // namespace perf_reg
} // namespace driver
} // namespace insieme

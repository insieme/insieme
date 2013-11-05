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

#include "insieme/driver/cmd/main_options.h"

#include <iostream>

#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/options_description.hpp>

#include "insieme/utils/version.h"

namespace insieme {
namespace driver {
namespace cmd {

	using namespace std;
	namespace po = boost::program_options;

	namespace {
		ostream& operator<<(ostream& out, const vector<string>& argList) {
			std::copy( argList.begin(), argList.end(), std::ostream_iterator<std::string>(cout, ", ") );
			return out;
		}
	}

	CommandLineOptions CommandLineOptions::parse(int argc, char** argv, bool debug) {

		CommandLineOptions res;

		//  --------------------- assemble program option description ---------------------------
		po::options_description cmdLineOpts("Insieme (tm) compiler:\nOptions");

		po::positional_options_description posDesc;
		posDesc.add("input-file", -1);

		#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
			(opt_name, po::value< var_type >(), var_help)
		#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
			(opt_name, po::value< int >(&res.var_name)->default_value(def_val), var_help)
		#define FLAG(opt_name, opt_id, var_name, def_val, var_help) \
			(opt_name, var_help)
		// Declare a group of options that will be allowed on the command line
		cmdLineOpts.add_options()
			#include "insieme/driver/cmd/options.def"
		;
		#undef OPTION
		#undef FLAG
		#undef INT_OPTION


		//  --------------------- parse program options ---------------------------
		po::variables_map varsMap;

		try {

			// parse the command line and stores the options on the varsMap
			po::store(po::command_line_parser(argc, argv).options(cmdLineOpts).positional(posDesc).run(), varsMap);
			po::notify(varsMap);

			// when the debug flag is enabled, the list of command line arguments are written to the std console
			if( debug ) {
				cout << "(DEBUG) Command line arguments: " << endl;
				#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
					varsMap.count(opt_id) && cout << "\t--" << opt_id << endl;
				#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
					varsMap.count(opt_id) && cout << "\t--" << opt_id << ": " << varsMap[opt_id].as< var_type >() << endl;
				#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
					varsMap.count(opt_id) && cout << "\t--" << opt_id << ": " << varsMap[opt_id].as< int >() << endl;
				#include "insieme/driver/cmd/options.def"
				#undef OPTION
				#undef FLAG
				#undef INT_OPTION
			}

			// assign the value to the class fields
			#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
				res.var_name = (varsMap.count(opt_id) ?  true : def_value);
			#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
				if(varsMap.count(opt_id)) res.var_name = varsMap[opt_id].as< var_type >();
			#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help)
			#include "insieme/driver/cmd/options.def"
			#undef OPTION
			#undef INT_OPTION
			#undef FLAG

			// Handle immediate flags (like --help or --version)
			if( res.Help ) {
				cout << cmdLineOpts << endl;
				// we exit from the compiler
				res.valid = false;
			} if( res.Version ) {
				cout << "This is the Insieme (tm) compiler version: " << INSIEME_VERSION << endl <<
						"Realized by the Distributed and Parallel Systems (DPS) group, copyright 2008-2013, " <<
						"University of Innsbruck\n";
				res.valid = false;
			}

		} catch(boost::program_options::unknown_option& ex) {
			cout << "Usage error: " << ex.what() << endl;
			cout << cmdLineOpts << endl;
			res.valid = false;
		}

		// return result
		return res;

	}

	frontend::ConversionJob CommandLineOptions::toConversionJob() const {

		// forward options
		frontend::ConversionJob job(InputFiles, IncludePaths);

		// add intercepts
		for (auto i : Interceptions) job.setInterception(i);

		// add macro definitions
		for (auto def : Defs) job.setDefinition(def);

		// update standard
		job.setStandard(frontend::ConversionSetup::Auto);

		if (Standard == "c99") {
			job.setStandard(frontend::ConversionSetup::C99);
		} else if (Standard == "c++03") {
			job.setStandard(frontend::ConversionSetup::Cxx03);
		} else if (Standard == "c++11") {
			job.setStandard(frontend::ConversionSetup::Cxx11);
		}

		// forward flags
		job.setOption(frontend::ConversionJob::OpenMP, OpenMP);
		job.setOption(frontend::ConversionJob::OpenCL, OpenCL);
		job.setOption(frontend::ConversionJob::Cilk, Cilk);
		job.setOption(frontend::ConversionJob::WinCrossCompile, WinCrossCompile);
		job.setOption(frontend::ConversionJob::TAG_MPI, MPITag);
        job.frontendPluginInit();
		// done
		return job;
	}

} // end namespace cmd_options
} // end namespace driver
} // end namespace insieme

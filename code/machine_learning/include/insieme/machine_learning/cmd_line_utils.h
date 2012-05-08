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

#pragma once

#include <vector>
#include <string>
#include <iterator>
#include <iostream>

#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/options_description.hpp>

namespace po = boost::program_options;

// current version number of the executable: todo put into a file or something else
#define VERSION_NUMBER "0.1"

enum OPTIMIZER {
	eCG,
	eQuickprop,
	eBFGS,
	eRpropPlus,
	eRpropMinus
};

namespace {
std::ostream& operator<<(std::ostream& out, const std::vector<std::string>& argList) {
	std::copy( argList.begin(), argList.end(), std::ostream_iterator<std::string>(std::cout, ", ") );
	return out;
}

}

/**
 * The TrainCmdOptions is a container for input arguments to the Insieme compiler.
 */
struct TrainCmdOptions {
#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
	static bool var_name;
#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
	static var_type var_name;
#define INT_OPTION(opt_name, opt_id, var_name, def_value, var_help) \
	static int var_name;
#define REAL_OPTION(opt_name, opt_id, var_name, def_value, var_help) \
	static double var_name;
#include "options.def"
#undef FLAG
#undef OPTION
#undef INT_OPTION
#undef REAL_OPTION
private:
	// avoid constructing instances of TrainCmdOptions
	TrainCmdOptions() { }

	static void HandleImmediateArgs(po::options_description const& desc) {
		if( TrainCmdOptions::Help ) {
			std::cout << desc << std::endl;
			// we exit from the compiler
			exit(1);
		} if( TrainCmdOptions::Version ) {
			std::cout << "This is the Insieme (tm) machine learning part: " << VERSION_NUMBER << std::endl <<
					"Realized by the Distributed and Parallel Systems (DPS) group, copyright 2011-2012, " <<
					"University of Innsbruck\n";
			exit(1);
		}
	}

public:
	/**
	 * This method reads the input arguments from the command line and parses them. The values are then stored inside
	 * the static references of the TrainCmdOptions class.
	 *
	 * The debug flags enable the Parser to print the list of parsed commands into the standard output
	 */
	static void Parse(int argc, char** argv, bool debug=false) {
		po::options_description cmdLineOpts("Insieme (tm) compiler:\nOptions");

		po::positional_options_description posDesc;
		posDesc.add("input-file", -1);

		#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
			(opt_name, po::value< var_type >(), var_help)
		#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
			(opt_name, po::value< int >(&TrainCmdOptions::var_name)->default_value(def_val), var_help)
		#define REAL_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
			(opt_name, po::value< double >(&TrainCmdOptions::var_name)->default_value(def_val), var_help)
		#define FLAG(opt_name, opt_id, var_name, def_val, var_help) \
			(opt_name, var_help)
		// Declare a group of options that will be allowed on the command line
		cmdLineOpts.add_options()
			#include "options.def"
		;
		#undef OPTION
		#undef FLAG
		#undef INT_OPTION
		#undef REAL_OPTION

		po::variables_map varsMap;

		try {
			// parse the command line and stores the options on the varsMap
			po::store(po::command_line_parser(argc, argv).options(cmdLineOpts).positional(posDesc).run(), varsMap);
			po::notify(varsMap);

			// when the debug flag is enabled, the list of command line arguments are written to the std console
			if( debug ) {
				std::cout << "(DEBUG) Command line arguments: " << std::endl;
				#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
					varsMap.count(opt_id) && std::cout << "\t--" << opt_id << std::endl;
				#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
					varsMap.count(opt_id) && std::cout << "\t--" << opt_id << ": " << varsMap[opt_id].as< var_type >() << std::endl;
				#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help)
				#define REAL_OPTION(opt_name, opt_id, var_name, def_val, var_help)
	//				std::cout << "\t--" << opt_id << ": " << varsMap[opt_id].as< int >() << std::endl;
				#include "options.def"
				#undef OPTION
				#undef FLAG
				#undef INT_OPTION
				#undef REAL_OPTION
			}

			// assign the value to the class fields
			#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
				TrainCmdOptions::var_name = (varsMap.count(opt_id) ?  true : def_value);
			#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
				if(varsMap.count(opt_id)) TrainCmdOptions::var_name = varsMap[opt_id].as< var_type >();
			#define INT_OPTION(opt_name, opt_id, var_name, var_type, var_help)
			#define REAL_OPTION(opt_name, opt_id, var_name, var_type, var_help)
			#include "options.def"
			#undef OPTION
			#undef INT_OPTION
			#undef REAL_OPTION
			#undef FLAG

			// Handle immediate flags (like --help or --version)
			HandleImmediateArgs(cmdLineOpts);

		}
		catch(boost::program_options::unknown_option& ex) {
			std::cout << "Usage error: " << ex.what() << std::endl;
			std::cout << cmdLineOpts << std::endl;
			exit(1);
		}

	}
};

// initialize the static references of the TrainCmdOptions args
#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
	bool TrainCmdOptions::var_name;
#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
	var_type TrainCmdOptions::var_name;
#define INT_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
	int TrainCmdOptions::var_name;
#define REAL_OPTION(opt_name, opt_id, var_name, def_val, var_help) \
	double TrainCmdOptions::var_name;
#include "../include/insieme/machine_learning/options.def"
#undef FLAG
#undef OPTION
#undef INT_OPTION
#undef REAL_OPTION

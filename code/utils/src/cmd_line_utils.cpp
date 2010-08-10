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

#include "cmd_line_utils.h"
#include <iostream>
#include <iterator>

#define VERSION_NUMBER "2.0.1"

using namespace std;
vector<string> CommandLineOptions::InputFiles;
vector<string> CommandLineOptions::IncludePaths;

namespace {
ostream& operator<<(ostream& out, const vector<string>& argList) {
	std::copy( argList.begin(), argList.end(), std::ostream_iterator<std::string>(cout, ", ") );
	return out;
}
}

CommandLineOptions& CommandLineOptions::Parse(int argc, char** argv, bool debug) {

	po::options_description cmdLineOpts("Insieme (tm) compiler:\nOptions");

	po::positional_options_description posDesc;
	posDesc.add("input-file", -1);

	// Declare a group of options that will be allowed on the command line
	cmdLineOpts.add_options()
		("version,v", "print version string")
		("help,h", "produce help message")
		("include-path,I", po::value< vector<string> >(), "include path(s)")
		("input-file", po::value< vector<string> >(), "input file(s)")
		//======================//
		// ADD NEW OPTIONS HERE //
		//======================//
		// ...
	;
	po::variables_map varsMap;

	try {
		// parse the command line and stores the options on the varsMap
		po::store(po::command_line_parser(argc, argv).options(cmdLineOpts).positional(posDesc).run(), varsMap);
		po::notify(varsMap);

		if( debug ) {
			cout << "(DEBUG) Command line arguments: " << endl;
			varsMap.count("help") && cout << "\t--help" << endl;
			varsMap.count("version") && cout << "\t--version" << endl;
			varsMap.count("include-path") && cout << "\tInclude path(s): " <<
					varsMap["include-path"].as< vector<string> >() << endl;
			varsMap.count("input-file") && cout << "\t"
					"Input file(s): " <<
					varsMap["input-file"].as< vector<string> >() << endl;
			// ...
		}

		// Now we check if the user wants informations (--help or --version)
		if( varsMap.count("help") ) {
			cout << cmdLineOpts << endl;
			// we exit from the compiler
			exit(1);
		} if (varsMap.count("version") ) {
			cout << "This is the Insieme (tm) compiler version: " << VERSION_NUMBER << endl <<
					"Realized by the Distributed and Parallel Systems (DPS) group, copyright 2008-2010, " <<
					"University of Innsbruck\n";
			exit(1);
		}

		// We parse the input arguments and populated the static fields of the CommandLineOptions class

		// retrieves the list of include paths
		if( varsMap.count("include-path") )
			CommandLineOptions::IncludePaths = varsMap["include-path"].as< vector<string> >();

		if( varsMap.count("input-file") )
			CommandLineOptions::InputFiles = varsMap["input-file"].as< vector<string> >();

	} catch(boost::program_options::unknown_option& ex) {
		cout << "Usage error: " << ex.what() << endl;
		cout << cmdLineOpts << endl;
		exit(1);
	}

}

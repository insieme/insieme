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

#include <boost/optional.hpp>

#include "insieme/frontend/frontend.h"

namespace insieme {
namespace driver {
namespace cmd {

	// fix an alias for the path type
	typedef frontend::path path;

	/**
	 * The CommandLineOptions is a container for input arguments to the main Insieme compiler executable.
	 */
	class CommandLineOptions {

	public:

		/**
		 * A flag indicating whether the parsed options are valid.
		 */
		bool valid;

		#define FLAG(opt_name, opt_id, var_name, def_value, var_help) \
			bool var_name;
		#define OPTION(opt_name, opt_id, var_name, var_type, var_help) \
			var_type var_name;
		#define INT_OPTION(opt_name, opt_id, var_name, def_value, var_help) \
			int var_name;

		#include "options.def"

		#undef FLAG
		#undef OPTION
		#undef INT_OPTION

		/**
		 * This method reads the input arguments from the command line and parses them. The values are then stored inside
		 * the the returned command-line-options instance.
		 *
		 * The debug flags enable the Parser to print the list of parsed commands into the standard output
		 */
		static CommandLineOptions parse(int argc, char** argv, bool debug=false);

	private:

		// avoid constructing instances of CommandLineOptions
		CommandLineOptions() : valid(true) { }

	public:

		/**
		 * Converts the command line options into a frontend conversion job.
		 */
		operator frontend::ConversionJob() const;

	};


} // end namespace cmd_options
} // end namespace driver
} // end namespace insieme

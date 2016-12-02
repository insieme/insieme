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

#pragma once

#include <string>
#include <vector>

#include "insieme/frontend/frontend.h"


namespace insieme {
namespace driver {
namespace cmd {

	// holds all settings that are not part of a ConversionJob
	struct Settings {
		#define FLAG(_name__, _id__, _description__) bool _id__;
		#define PARAMETER(_name__, _id__, _type__, _default_value__, _description__) _type__ _id__;
		#include "insieme/driver/cmd/commandline_options.def"
	};

	class OptionParser;

	/**
	 * The object holding all the standard command line options parsed.
	 */
	struct Options {
		/**
		 * A flag indicating whether the passed options are valid.
		 */
		bool valid;

		/**
		 * A flag indicating whether the executable should exit without errors.
		 * This occurs for example when requesting the version or usage.
		 */
		bool gracefulExit;

		/**
		 * Holds all general settings for setting up compilation
		 */
		Settings settings;

		/**
		 * The configuration of the frontend encapsulated into an conversion job.
		 */
		insieme::frontend::ConversionJob job;

		/**
		 * Return a commandline parser object which can be used to specify additional flags and parameters
		 */
		static OptionParser getParser();

		/**
		 * Parses the given command line options.
		 * Note that the first argument will be ignored.
		 */
		static Options parse(std::vector<std::string>& args);

		/**
		 * Parses the given command line options. This is a convenience overload to directly pass the arguments from a main function.
		 * Note that the first argument will be ignored.
		 */
		static Options parse(int argc, char** argv);

	  private:
		// allow the parser to construct instances
		friend class OptionParser;

		/**
		 * A constructor for this class.
		 */
		Options() : valid(true), gracefulExit(false), settings({}), job() {}
	};

	class OptionParser {

		/**
		 * The description of parameters to be parsed by this parser - aggregated
		 * on the fly.
		 */
		boost::program_options::options_description desc;

		/**
		 * The result we are producing here.
		 */
		Options res;

	  public:

		/**
		 * Creates a new instance of this option parser
		 */
		OptionParser();

		/**
		 * Allows to add a flag to the program options.
		 *
		 * @param name the name of the flag to be added
		 * @param target the target to be used for storing whether the flag has been set or not
		 * @param description the description of the parameter to be shown in the help message
		 */
		void addFlag(const std::string& name, bool& target, const std::string& description) {
			desc.add_options()(name.c_str(), boost::program_options::bool_switch(&target), description.c_str());
		}

		/**
		 * Allows to add a parameter to the program options.
		 *
		 * @param name the name of the parameter to be added
		 * @param target the target to be used for storing whether the flag has been set or not
		 * @param def the default value to be used if non is provided
		 * @param description the description of the parameter to be shown in the help message
		 */
		template <typename T>
		void addParameter(const std::string& name, T& target, const T& def, const std::string& description) {
			desc.add_options()(name.c_str(), boost::program_options::value<T>(&target)->default_value(def), description.c_str());
		}

		/**
		 * Parses the given command line options.
		 * Note that the first argument will be ignored.
		 */
		Options parse(std::vector<std::string>& args);

		/**
		 * Parses the given command line options. This is a convenience overload to directly pass the arguments from a main function.
		 * Note that the first argument will be ignored.
		 */
		Options parse(int argc, char** argv);
	};

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

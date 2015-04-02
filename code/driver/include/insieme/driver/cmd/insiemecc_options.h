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

#include <string>
#include <vector>
#include <cstdlib>

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/tokenizer.hpp>

#include "insieme/frontend/frontend.h"


namespace insieme {
namespace driver {
namespace cmd {

	using namespace std;

	namespace detail {
		class OptionParser;
	}

	enum BackendEnum {
		Runtime = 0,
		Sequential,
		OpenCL,
		Pthreads
	};

	struct BackendHint {
		BackendEnum backend;

		/**
		 * allow direct comparison with BackendEnum
		 */
		bool operator==(const BackendEnum e) const {
			return (this->backend == e);
		}

		friend std::ostream& operator<<(std::ostream& out, const BackendHint& b);

	};

	std::ostream& operator<<(std::ostream& out, const BackendHint& b) {
		switch(b.backend) {
			case BackendEnum::Runtime: out << "runtime"; break;
			case BackendEnum::Sequential: out << "sequential"; break;
			case BackendEnum::OpenCL: out << "opencl"; break;
			case BackendEnum::Pthreads: out << "pthreads"; break;
			default: out << "unknown";
		}
		return out;
	}

	// holds all settings that are not part of a ConversionJob
	struct Settings {

#define FLAG(_name__, _id__, _description__) \
			bool _id__;
//			#include "insieme/driver/cmd/insiemecc_options.def"
#define PARAMETER( _name__, _id__, _type__, _default_value__, _description__) \
			_type__ _id__;
#define OPTION( _name__, _id__, _type__, _default_value__, _description__) \
			_type__ _id__;
			#include "insieme/driver/cmd/insiemecc_options.def"

	};

	/**
	 * A standard command line option utility which can be extended and customized for
	 * individual needs.
	 */
	struct Options {

		/**
		 * A flag indicating whether the passed options are valid. It will not
		 * if a required option is missing or the help flag -h was used.
		 */
		bool valid;

		/**
		 * A flag indicating whether the executable should exit without errors.
		 * This occurs for example when requesting the version or usage.
		 */
		bool gracefulExit;

		/**
		 * Holds all settings for setting up compilation
		 */
		Settings settings;

		/**
		 * The configuration of the frontend encapsulated into an conversion job.
		 */
		insieme::frontend::ConversionJob job;

		/**
		 * The backend to be used if code generation is requested.
		 */
		BackendHint backendHint;

		/**
		 * Parses the given command line options.
		 */
		static detail::OptionParser parse(const std::vector<std::string>& argv);

	private:

		// allow the parser to construct instances
		friend class detail::OptionParser;

		/**
		 * A constructor for this class.
		 */
		Options(const insieme::frontend::ConversionJob& job)
			: valid(true), gracefulExit(false), settings(Settings{}), job(job) {}

	};

	namespace detail {

	typedef shared_ptr<insieme::frontend::extensions::FrontendExtension> FrontendExtensionPtr;

		class OptionParser {

			/**
			 * A function to be processed when parsing
			 */
			typedef std::function<bool(const boost::program_options::variables_map&)> parser_step;

			/**
			 * The arguments passed to the program.
			 */
            std::vector<std::string> argv;

			/**
			 * The description of parameters to be parsed by this parser - aggregated
			 * on the fly.
			 */
			boost::program_options::options_description desc;

			/**
			 * A list of extra parser steps to be conducted when parsing program options.
			 */
			vector<parser_step> parser_steps;

		public:

			/**
			 * Creates a new instance of this option parser parsing the given arguments.
			 * This
			 */
			OptionParser(const std::vector<std::string>& argv);

			/**
			 * Allows to add a flag to the program options using a convenient syntax.
			 *
			 * @param name the name of the flag to be added
			 * @param symbol the one-letter shortcut of the flag
			 * @param target the target to be used for storing whether the flag has been set or not
			 * @param description the description of the parameter to be shown in the help message
			 */
			OptionParser& operator()(const string& name, char symbol, bool& flag, const char* description);

			/**
			 * Allows to add additional program options using a convenient syntax.
			 *
			 * @param name the name of the option to be added
			 * @param target the target to store the parsed result into
			 * @param description the description of the parameter to be shown in the help message
			 */
			template<typename T>
			OptionParser& operator()(const char* name, T& target, const char* description) {
				desc.add_options()(name, boost::program_options::value<T>(&target), description);
				return *this;
			}

			/**
			 * Allows to add additional program options using a convenient syntax.
			 *
			 * @param name the name of the option to be added
			 * @param target the target to store the parsed result into
			 * @param def the default value to be used if non is provided
			 * @param description the description of the parameter to be shown in the help message
			 */
			template<typename T>
			OptionParser& operator()(const char* name, T& target, const T& def, const char* description) {
				desc.add_options()(name, boost::program_options::value<T>(&target)->default_value(def), description);
				return *this;
			}

			/**
			 * Realizes the final step by conducting the actual parsing and returning the
			 * parsed program options.
			 */
			operator Options();

		};

	}

//	class Option {
//		bool isFlag;
//		bool required;
//		string name;
//
//	};


} // end namespace cmd_options
} // end namespace driver
} // end namespace insieme


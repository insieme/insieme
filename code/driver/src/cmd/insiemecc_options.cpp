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

#include "insieme/driver/cmd/insiemecc_options.h"

#include <boost/algorithm/string.hpp>

#include <string>
#include <iostream>
#include "insieme/utils/version.h"

namespace insieme {
namespace driver {
namespace cmd {

	using namespace std;

	namespace bpo = boost::program_options;
	namespace fe = insieme::frontend;

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

	detail::OptionParser Options::parse(const std::vector<std::string>& argv) {
		return detail::OptionParser(argv);
	}

	namespace detail {

		OptionParser::OptionParser(const std::vector<std::string>& argv) : argv(argv), res() {
			// define options
			desc.add_options()
			#define FLAG(_name__, _id__, _description__) (_name__, _description__)
			#define PARAMETER(_name__, _id__, _type__, _default_value__, _description__)                                                                       \
				(_name__, bpo::value<_type__>()->default_value(_default_value__), _description__)
			#define OPTION(_name__, _id__, _type__, _default_value__, _description__)                                                                          \
				(_name__, bpo::value<_type__>(), _description__)
			#include "insieme/driver/cmd/insiemecc_options.def"
			    ;

			// get information how the plugins are activated
			res.job.registerExtensionFlags(desc);
		}


		OptionParser& OptionParser::operator()(const string& name, const string& symbol, bool& flag, const char* description) {
			// add flag to description
			if(symbol.empty()) {
				desc.add_options()(name.c_str(), description);
			} else {
				desc.add_options()((name + "," + symbol).c_str(), description);
			}

			// add parser step
			// be careful what you pass to the lambda. This is a little
			// bit dangerous because we capture the flag as reference.
			parser_steps.push_back([&flag, name](const bpo::variables_map& map) {
				(map.count(name)) ? flag = 1 : flag = 0;
				return true;
			});
			return *this;
		}

		OptionParser::operator Options() {
			// -- parsing -------------------------------------------
			// remove the first entry: this should be some string like "insiemecc"
			argv.erase(argv.begin());

			// define positional options (all options not being named)
			bpo::positional_options_description pos;
			pos.add("input-file", -1);

			// parse parameters
			bpo::variables_map map;

			bpo::parsed_options parsed =
			    bpo::basic_command_line_parser<char>(argv)
			        .options(desc)
			        .style((bpo::command_line_style::default_style | bpo::command_line_style::allow_long_disguise) ^ bpo::command_line_style::allow_guessing)
			        .allow_unregistered()
					.positional(pos)
					.run();

			bpo::store(parsed, map);
			bpo::notify(map);

			vector<string> unknown_flags = bpo::collect_unrecognized(parsed.options, bpo::exclude_positional);

			// -- processing -----------------------------------------

			std::string tempName;
			#define FLAG(_name__, _id__, _description__)                                                                                                       \
				tempName = string(_name__);                                                                                                                    \
				tempName = tempName.substr(0, tempName.find(","));                                                                                             \
				res.settings._id__ = map.count(tempName.c_str());
			#define PARAMETER(_name__, _id__, _type__, _default_value__, _description__)                                                                       \
				tempName = string(_name__);                                                                                                                    \
				tempName = tempName.substr(0, tempName.find(","));                                                                                             \
				if(map.count(tempName.c_str())) res.settings._id__ = map[tempName.c_str()].as<_type__>();
			#define OPTION(_name__, _id__, _type__, _default_value__, _description__)                                                                          \
				tempName = string(_name__);                                                                                                                    \
				tempName = tempName.substr(0, tempName.find(","));                                                                                             \
				if(map.count(tempName.c_str())) res.settings._id__ = map[tempName.c_str()].as<_type__>();
			#include "insieme/driver/cmd/insiemecc_options.def"

			// assume valid options until indicated otherwise
			res.gracefulExit = false;
			res.valid = true;

			// check whether help was requested
			if(res.settings.help) {
				std::cout << "Usage: " << argv[0] << " [options | infile]"
				          << "\n";
				std::cout << desc << "\n";
				// no non-zero exit code when requesting help!
				res.gracefulExit = true;
				return res;
			}

			if(res.settings.dumpVersion) {
				std::cout << INSIEME_VERSION << "\n";
				res.gracefulExit = true;
				return res;
			}

			// check whether version was requested
			if(res.settings.version) {
				std::cout << "This is the Insieme (tm) compiler version: " << INSIEME_VERSION << "\n"
				          << "Realized by the Distributed and Parallel Systems (DPS) group, copyright 2008-2016, "
				          << "University of Innsbruck\n"
				          << "http://www.insieme-compiler.org\n";
				// no non-zero exit code when requesting version!
				res.gracefulExit = true;
				return res;
			}

			// unparsed flags - might be used by a backend compiler
			res.job.setUnparsedOptions(unknown_flags);

			// input files
			if(!res.settings.inFiles.empty()) {
				res.job.setFiles(res.settings.inFiles);
			} else {
				std::cerr << "Error: No input files provided!\n";
				res.valid = false;
				return res;
			}

			// user include paths
			if(!res.settings.includePaths.empty()) { res.job.setIncludeDirectories(res.settings.includePaths); }

			// system include paths
			for(auto path : res.settings.systemIncludePaths) {
				res.job.addSystemHeadersDirectory(path);
			}


			// --------------- Job Settings ---------------
			res.job.setOption(fe::ConversionJob::NoWarnings, res.settings.noWarnings);
			res.job.setOption(fe::ConversionJob::NoColor, res.settings.noColor);
			res.job.setOption(fe::ConversionJob::WinCrossCompile, res.settings.winCrossCompile);
			res.job.setOption(fe::ConversionJob::NoDefaultExtensions, res.settings.noDefaultExtensions);
			res.job.setOption(fe::ConversionJob::DumpClangAST, res.settings.printClangAST);

			// check for libraries and add LD_LIBRARY_PATH entries to lib search path
			std::vector<frontend::path> ldpath;
			ldpath.push_back(boost::filesystem::current_path().string());
			if(!res.settings.libraryPaths.empty()) { ldpath = res.settings.libraryPaths; }

			if(auto ldPath = getenv("LD_LIBRARY_PATH")) {
				std::string ldvar(ldPath);
				boost::char_separator<char> sep(":");
				boost::tokenizer<boost::char_separator<char>> tokens(ldvar, sep);
				for(auto t : tokens) {
					ldpath.push_back(t);
				}
			}
			// we have to check for lib<name>.<so|a> in every library directory provided by library-path
			for(const frontend::path& s : res.settings.libraryFiles) {
				for(const frontend::path& d : ldpath) {
					frontend::path f1 = d / ("/lib" + s.string() + ".so");
					frontend::path f2 = d / ("/lib" + s.string() + ".a");
					if(boost::filesystem::is_regular_file(f1)) {
						// shared object file
						res.job.addFile(f1);
						break;
					}
					if(boost::filesystem::is_regular_file(f2)) {
						// static library
						res.job.addFile(f2);
						break;
					}
				}
			}
			// preprocessor directives
			for(auto def : res.settings.definitions) {
				res.job.setDefinition(def);
			}

			// set clang AST dump filter regex
			if(!res.settings.clangASTDumpFilter.empty()) { res.job.setClangASTDumpFilter(res.settings.clangASTDumpFilter); }

			// valid language standards, bool denotes whether accepted only for compatibility reasons with certain build systems and alike
			const std::map<string, pair<bool, frontend::ConversionSetup::Standard>> languageStandardMapping = {
				{"auto", make_pair(true, frontend::ConversionSetup::Auto)},    {"c90", make_pair(false, frontend::ConversionSetup::C99)},
				{"c99", make_pair(true, frontend::ConversionSetup::C99)},      {"c11", make_pair(false, frontend::ConversionSetup::C99)},
				{"c++98", make_pair(false, frontend::ConversionSetup::Cxx11)}, {"c++03", make_pair(false, frontend::ConversionSetup::Cxx11)},
				{"c++11", make_pair(true, frontend::ConversionSetup::Cxx11)},  {"c++14", make_pair(true, frontend::ConversionSetup::Cxx14)}};

			vector<string> supportedStandards;
			for(const auto& e : languageStandardMapping) {
				if(e.second.first) { supportedStandards.push_back(e.first); }
			}

			const std::string std = res.settings.standard.back();
			if(res.settings.standard.size() > 1) { std::cerr << "Warning: Multiple standards set. Only considering last one: " << std << "\n"; }
			if(::containsKey(languageStandardMapping, std)) {
				if(!languageStandardMapping.at(std).first) {
					std::cerr << "Warning: Standard " << std << " not explicitly supported - supported are: " << join(", ", supportedStandards) << "\n";
				}
				res.job.setStandard(languageStandardMapping.at(std).second);
			} else {
				std::cerr << "Error: Unsupported standard: " << std << " - supported are: " << join(", ", supportedStandards) << "\n";
				res.valid = false;
				return res;
			}

			if(res.settings.language != "undefined") {
				if(res.settings.language == "c++") {
					if(!res.job.isCxx()) {
						// language should be c++ but we don't
						// know what standard so lets takes c++11.
						std::cerr << "Warning: Conflicting language setting and standard flag. Set standard to c++11\n";
						res.job.setStandard(frontend::ConversionSetup::Cxx11);
					}
				} else if(res.settings.language == "c") {
					if(res.job.isCxx()) {
						// language should be c++ but we don't
						// know what standard so lets takes C99.
						std::cerr << "Warning: Conflicting language setting and standard flag. Set standard to c99\n";
						res.job.setStandard(frontend::ConversionSetup::C99);
					}
				} else {
					std::cerr << "Error: Unsupported language setting: " << res.settings.language << " - supported: c, c++\n";
					res.valid = false;
					return res;
				}
			}

			// interceptions
			if(!res.settings.interceptIncludes.empty()) { res.job.setInterceptedHeaderDirs(res.settings.interceptIncludes); }

			// f flags
			for(auto optFlag : res.settings.optimizationFlags) {
				std::string&& s = "-f" + optFlag;
				res.job.addFFlag(s);
			}

			// extra flags
			for(auto cur : parser_steps) {
				res.valid = cur(map) && res.valid;
			}

			// prepare for setting up backend
			if(res.settings.backend == "runtime" || res.settings.backend == "run") {
				res.backendHint.backend = BackendEnum::Runtime;
			} else if(res.settings.backend == "sequential" || res.settings.backend == "seq") {
				res.backendHint.backend = BackendEnum::Sequential;
			} else if(res.settings.backend == "opencl" || res.settings.backend == "ocl") {
				res.backendHint.backend = BackendEnum::OpenCL;
			} else if(res.settings.backend == "pthreads" || res.settings.backend == "pthread") {
				res.backendHint.backend = BackendEnum::Pthreads;
			} else {
				std::cerr << "Error: Unsupported backend: " << res.settings.backend << " - supported: sequential | runtime | ocl | pthreads\n";
				res.valid = false;
				return res;
			}

			if(!res.settings.dumpOclKernel.empty() && res.backendHint.backend != BackendEnum::OpenCL) {
				std::cerr << "Error: OpenCL kernel dump requires OpenCL backend (current backend selected: " << res.backendHint << ")\n";
				res.valid = false;
				return res;
			}
			// done
			return res;
		}
	}

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

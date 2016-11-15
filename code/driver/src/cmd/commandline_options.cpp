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

#include "insieme/driver/cmd/commandline_options.h"

#include <string>
#include <boost/filesystem.hpp>
#include <boost/tokenizer.hpp>

namespace insieme {
namespace driver {
namespace cmd {

	OptionParser Options::getParser() {
		return OptionParser();
	}

	Options Options::parse(std::vector<std::string>& args) {
		return getParser().parse(args);
	}

	Options Options::parse(int argc, char** argv) {
		return getParser().parse(argc, argv);
	}


	OptionParser::OptionParser() : desc({}) {
		// define options
		desc.add_options()
			#define FLAG(_name__, _id__, _description__) \
				(_name__, boost::program_options::bool_switch(&res.settings._id__), _description__)
			#define PARAMETER(_name__, _id__, _type__, _default_value__, _description__) \
				(_name__, boost::program_options::value<_type__>(&res.settings._id__)->default_value(_default_value__), _description__)
			#include "insieme/driver/cmd/commandline_options.def"
		;

		// get information how the plugins are activated
		res.job.registerExtensionFlags(desc);
	}

	Options OptionParser::parse(std::vector<std::string>& args) {
		// -- parsing -----------------------------------------
		// remove the first entry: this should be the name of the executable like "insiemecc"
		std::string programName = *args.begin();
		args.erase(args.begin());

		// define positional options (all options not being named) - the input files
		boost::program_options::positional_options_description pos;
		pos.add("input-file", -1);

		// parse parameters
		boost::program_options::variables_map map;

		boost::program_options::parsed_options parsed =
				boost::program_options::basic_command_line_parser<char>(args)
						.options(desc)
						.style((boost::program_options::command_line_style::default_style |
								boost::program_options::command_line_style::allow_long_disguise) ^ boost::program_options::command_line_style::allow_guessing)
						.allow_unregistered()
				.positional(pos)
				.run();

		boost::program_options::store(parsed, map);
		boost::program_options::notify(map);

		std::vector<std::string> unknown_flags = boost::program_options::collect_unrecognized(parsed.options, boost::program_options::exclude_positional);


		// -- processing -----------------------------------------

		// assume valid options and graceful exit until indicated otherwise
		res.gracefulExit = false;
		res.valid = true;

		// check whether help was requested
		if(res.settings.help) {
			std::cout << "Usage: " << programName << " [options | infile]\n";
			std::cout << desc << std::endl;
			// zero exit code when requesting help!
			res.gracefulExit = true;
			return res;
		}

		// check whether version was requested
		if(res.settings.version) {
			std::cout << "This is the Insieme (tm) compiler version: " << INSIEME_VERSION << "\n"
								<< "Realized by the Distributed and Parallel Systems (DPS) group, copyright 2008-2016, "
								<< "University of Innsbruck\n"
								<< "http://www.insieme-compiler.org\n";
			// zero exit code when requesting version!
			res.gracefulExit = true;
			return res;
		}
		if(res.settings.dumpVersion) {
			std::cout << INSIEME_VERSION << "\n";
			// zero exit code when requesting version!
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
		if(!res.settings.includePaths.empty()) {
			res.job.setIncludeDirectories(res.settings.includePaths);
		}

		// system include paths
		for(auto path : res.settings.systemIncludePaths) {
			res.job.addSystemHeadersDirectory(path);
		}

		// interceptions
		if(!res.settings.interceptIncludes.empty()) {
			res.job.setInterceptedHeaderDirs(res.settings.interceptIncludes);
		}

		// --------------- Job Settings ---------------
		res.job.setOption(frontend::ConversionJob::NoWarnings, res.settings.noWarnings);
		res.job.setOption(frontend::ConversionJob::NoColor, res.settings.noColor);
		res.job.setOption(frontend::ConversionJob::NoDefaultExtensions, res.settings.noDefaultExtensions);

		res.job.setOption(frontend::ConversionJob::DumpClangAST, res.settings.printClangAST);
		// set clang AST dump filter regex
		if(!res.settings.clangASTDumpFilter.empty()) {
			res.job.setClangASTDumpFilter(res.settings.clangASTDumpFilter);
		}

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

		// valid language standards, bool denotes whether accepted only for compatibility reasons with certain build systems and alike
		const std::map<string, std::pair<bool, frontend::ConversionSetup::Standard>> languageStandardMapping = {
			{"auto",  std::make_pair(true,  frontend::ConversionSetup::Auto)},
			{"c90",   std::make_pair(false, frontend::ConversionSetup::C99)},
			{"c99",   std::make_pair(true,  frontend::ConversionSetup::C99)},
			{"c11",   std::make_pair(false, frontend::ConversionSetup::C99)},
			{"c++98", std::make_pair(false, frontend::ConversionSetup::Cxx11)},
			{"c++03", std::make_pair(false, frontend::ConversionSetup::Cxx11)},
			{"c++11", std::make_pair(true,  frontend::ConversionSetup::Cxx11)},
			{"c++14", std::make_pair(true,  frontend::ConversionSetup::Cxx14)}
		};

		vector<string> supportedStandards;
		for(const auto& e : languageStandardMapping) {
			if(e.second.first) {
				supportedStandards.push_back(e.first);
			}
		}

		const std::string std = res.settings.standard.back();
		if(res.settings.standard.size() > 1) {
			std::cerr << "Warning: Multiple standards set. Only considering last one: " << std << "\n";
		}
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

		// done
		return res;
	}

	Options OptionParser::parse(int argc, char** argv) {
		std::vector<std::string> args(argv, argv + argc);
		return parse(args);
	}

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

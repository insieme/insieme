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

#include "insieme/driver/cmd/options.h"

#include <boost/algorithm/string.hpp>

namespace insieme {
namespace driver {
namespace cmd {

	namespace bpo = boost::program_options;
	namespace fe = insieme::frontend;

	detail::OptionParser Options::parse(int argc, char** argv) {
		return detail::OptionParser(argc, argv);
	}

	namespace detail {

		OptionParser::OptionParser(int argc, char** argv) : argc(argc), argv(argv) {

			// -- assemble default parameters --------------------------

			// define options
			desc.add_options()
					("help,h", "produce this help message")
					("input-file,i", bpo::value<vector<string>>(), "input files - required!")
					("library-file,l", bpo::value<vector<string>>(), "linker flags - optional")
					("library-path,L", bpo::value<vector<string>>(), "library paths - optional")
					("include-path,I", bpo::value<vector<string>>(), "include files - optional")
					("definitions,D", bpo::value<vector<string>>(), "preprocessor definitions - optional")
					("std,s", bpo::value<string>()->default_value("auto"), "determines the language standard")
					("no-omp", "disables OpenMP support")
					("no-cilk", "disables cilk support")
					("output-file,o", bpo::value<string>(), "the output file")
					("intercept-file", bpo::value<string>(), "interceptor config file")
			;

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

			Options res(frontend::ConversionJob("in.c"));

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
				res.job.setFiles(map["input-file"].as<vector<frontend::path>>());
			} else {
				cout << "No input files provided!\n";
				res.valid = false;
				return res;
			}
			// include path
			if (map.count("include-path")) {
				res.job.setIncludeDirectories(map["include-path"].as<vector<frontend::path>>());
			}
			// output file (optional)
            if (map.count("compile")) {
                    res.outFile = "";
            } else {
                res.outFile = "a.out";
            }
			if (map.count("output-file")) {
				res.outFile = map["output-file"].as<string>();
			}

            /*
			// library path
			if (map.count("library-path")) {
                for(std::string& s : map["library-path"].as<vector<string>>())
                    res.job.addStdLibIncludeDirectory(s);
			}*/
			// check for libraries and add LD_LIBRARY_PATH entries to lib search path
			std::vector<std::string> ldpath;
			ldpath.push_back(boost::filesystem::current_path().string());
			if(map.count("library-path")) {
                ldpath = map["library-path"].as<vector<string>>();
			}
			std::string ldvar(getenv("LD_LIBRARY_PATH"));
			boost::char_separator<char> sep(":");
            boost::tokenizer<boost::char_separator<char>> tokens(ldvar, sep);
            for (auto t : tokens) {
                    ldpath.push_back(t);
            }
			if (map.count("library-file")) {
                //we have to check for lib<name>.<so|a> in every library directory provided by library-path
                for(std::string s : map["library-file"].as<vector<string>>()) {
                    for(std::string d : ldpath) {
                        std::string f1 = d+"/lib"+s+".so";
                        std::string f2 = d+"/lib"+s+".a";
                        if(boost::filesystem::is_regular_file(f1)) {
                            //shared object file
                            res.job.addFile(f1);
                            break;
                        }
                        if(boost::filesystem::is_regular_file(f2)) {
                            //static library
                            res.job.addFile(f2);
                            break;
                        }
                    }
                }
			}
			// preprocessor directives
			if (map.count("definitions")) {
				for(auto def : map["definitions"].as<vector<string>>()) {
					res.job.setDefinition(def);
				}
			}

			// insert
			res.job.setStandard(frontend::ConversionSetup::Auto);
			if (map.count("std")) {
				auto selected = map["std"].as<string>();
				if (selected == "auto") {
					res.job.setStandard(frontend::ConversionSetup::Auto);
				} else if (selected == "c99") {
					res.job.setStandard(frontend::ConversionSetup::C99);
				} else if (selected == "c++03") {
					res.job.setStandard(frontend::ConversionSetup::Cxx03);
				} else {
					cout << "Unsupported standard: " << selected << " - supported: auto, c99, c++03";
					res.valid=false;
				}
			}

			// enable support for OpenMP and Cilk
			res.job.setOption(fe::ConversionJob::OpenMP, !map.count("no-omp"));
			res.job.setOption(fe::ConversionJob::Cilk, !map.count("no-cilk"));
            // interceptor file
            if (map.count("intercept-file")) {
                res.job.setIntercepterConfigFile(map["intercept-file"].as<string>());
            }

			// done
			return res;

		}
	}

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

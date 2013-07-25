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

#include "insieme/utils/library_utils.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <exception>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/lexical_cast.hpp>


namespace insieme {
namespace utils {

    LibraryUtil::LibraryUtil() {};

    LibraryUtil::~LibraryUtil() {
        for(std::string& s : directories) {
            std::stringstream command;
            command << "rm -rf " << s;
            system(command.str().c_str());
        }
    };

    /**
     * Checks if a given file is an insieme object file and returns
     * a boolean value.
     */
    bool LibraryUtil::isDeserializable(const std::string& file_name) {
        std::ifstream ifs(file_name, std::ios::in | std::ios::binary);
        ifs.seekg(0, std::ios_base::beg);
        try {
            boost::archive::text_iarchive ia(ifs);
        } catch (std::exception& e) {
            return false;
        }
        return true;
    };

    /**
     * Method extracts all files of a static library to a temporary
     * folder. Every file in the temporary folder is checked if it
     * is an insieme object file. If it is an object file it is
     * added to the inputs list. Otherwise it is added to the libs list.
     */
    void LibraryUtil::unpackStaticLibrary(const std::string& file_name,
                                          std::vector<std::string>& inputs,
                                          std::vector<std::string>& libs) {
        char tempdir[] = "/tmp/libXXXXXX";
        mkdtemp(tempdir);
        //unpack archive to tmp/XXXX
        std::stringstream command;
        //make path absolute
        boost::filesystem::path file(file_name);
        command << "cd " << tempdir << " && ar x " << boost::filesystem::canonical(file).c_str();
        std::cout << command.str() << "\n";
        std::cout << tempdir << "\n";
        int ret = system(command.str().c_str());
        //if file format not recognized push to libs
        if(ret>0) {
            libs.push_back(file_name);
            return;
        }
        //add translation units to input file list
        boost::filesystem::path dirPath(tempdir);
        boost::filesystem::directory_iterator end_iter;

        if ( boost::filesystem::exists(dirPath) && boost::filesystem::is_directory(dirPath)) {
            for(boost::filesystem::directory_iterator dir_iter(dirPath); dir_iter!=end_iter; ++dir_iter) {
                if (boost::filesystem::is_regular_file(dir_iter->status()) ) {
                    std::string file = dir_iter->path().string();
                    //std::cout << "check if " << file << " is deserializable\n";
                    if(isDeserializable(file)) {
                        //std::cout << "added " << file << " to input list\n";
                        inputs.push_back(file);
                    } else {
                        //std::cout << "added " << file << " to libs list\n";
                        libs.push_back(file);
                    }
                }
            }
        }

        //store directory path for cleanup ops
        directories.push_back(tempdir);
    };

    /**
     *  Method is used to check input file list. Input files can either be shared objects
     *  that have to be extracted and analysed or object files that can be insieme objects
     *  or any other object files. The last possibility is a normal source file.
     */
    void LibraryUtil::handleInputFiles(const std::vector<std::string>& filelist,
                                       std::vector<std::string>& inputs,
                                       std::vector<std::string>& libs) {
    	for(auto cur : filelist) {
            //if we have a library we have to unpack the archive
            //and check if the files inside are insieme objects. Insieme objects
            //are added to inputs. Non insieme objects are added to libs
            if (boost::ends_with(cur, ".a") || boost::ends_with(cur, ".so")) {
                unpackStaticLibrary(cur, inputs, libs);
            } else if (boost::ends_with(cur, ".o")) {
                //for objects check if it is an insieme file
                //and add it to the right list
                if(isDeserializable(cur)) {
                    inputs.push_back(cur);
                } else {
                    libs.push_back(cur);
                }
            } else {
                //every other file (e.g. source) is
                //added to the input file list
                inputs.push_back(cur);
            }
        }
    };

    /**
     *  Method is used to create insieme shared object files. All files that were
     *  used to create the insieme object and all other libraries, shared objects, ...
     *  are packed into an archive.
     */
    void LibraryUtil::createLibrary(const std::vector<std::string>& input_files, const std::vector<std::string>& lib_files, const std::string& outputFile) {
        //DEBUG
        std::cout << "\n###########################################################\nINPUT FILES:\n";
        for(auto s: input_files) {
            std::cout << s << "\n";
        }
        std::cout << "LIB FILES:\n";
        for(auto s: lib_files) {
            std::cout << s << "\n";
        }


        //modify file endings to .o and create so file
        //we need to store the linker flags for
        //later usage
        std::stringstream command;
        command << "ar cr " << outputFile;
        int i=0;
        for(auto s : input_files) {
            std::string raw_name = s;
            size_t lastslash = s.find_last_of("/")+1;
            size_t lastdot = s.find_last_of(".");
            if (lastdot == std::string::npos)
                raw_name = s;
            raw_name = s.substr(lastslash, lastdot-lastslash);
            raw_name += "_";
            raw_name += boost::lexical_cast<std::string>(i++);
            raw_name += ".o";
            command << " " << raw_name;
        }
        //FIXME: store non insieme shared objects as references.
        //At the moment the shared objects files that are needed
        //to create the library are copied into the insieme shared object file
        for(auto s : lib_files) {
            command << " " << s;
        }
        std::cout << "COMMAND: " << command.str() << "\n###########################################################\n";
        system(command.str().c_str());
    };

}

}

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <iostream>
#include <string>
#include <fstream>
#include <map>

#include <boost/filesystem.hpp>

#include "insieme/analysis/cba/datalog/framework/analysis_base.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	namespace {

		using namespace std;

		class Inserter {
			string folder;
			map<string,fstream> factFiles;
			bool debug = false;
			std::ostream *debugOut;

		public:
			Inserter() {}

			~Inserter() {
				for (auto &ff : factFiles)
					ff.second.close();
			}

			bool setTargetFolder(const std::string folder) {
				this->folder = folder;
				boost::filesystem::create_directories(folder);
				return true;
			}

			Inserter &setDebug(bool debug, std::ostream &debugOut) {
				this->debug = debug;
				this->debugOut = &debugOut;
				return *this;
			}

			void printDebug() {
				*debugOut << std::endl;
			}

			template <typename F, typename ... Rest>
			void printDebug(const F& first, const Rest& ...rest) {
				*debugOut << " - " << first;
				printDebug(rest...);
			}

			void printToFactFile(fstream &fs, bool) {
				fs << endl;
			}

			template <typename F, typename ... Rest>
			void printToFactFile(fstream &fs, bool first, const F& car, const Rest& ... cdr) {
				if (!first)
					fs << "\t";
				fs << car;
				printToFactFile(fs, false, cdr...);
			}

			template<typename ... Args>
			void insert(const std::string& relationName, const Args& ... args ) {

				if (debug) {
					*debugOut << "Inserting " << relationName << "  ";
					printDebug(args...);
				}

				fstream &fs = getFs(relationName);
				printToFactFile(fs, true, args...);
			}

		private:
			fstream &getFs(const string &relationName) {
				if (factFiles.find(relationName) != factFiles.end())
					return factFiles[relationName];

				string filepath(folder + "/" + relationName + ".facts");
				factFiles[relationName].open(filepath, fstream::out | fstream::trunc);
				if (!factFiles[relationName].is_open())
					cerr << "Cannot open fact file at " <<filepath<<" for writing" << endl;
				return factFiles[relationName];
			}

		};

	} // end anonymous namespace

	int extractFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer, bool debug, ostream& debugOut) {
		FactExtractor<core::Pointer,Inserter> extractor(nodeIndexer);
		if (!extractor.getInserter().setDebug(debug, debugOut).setTargetFolder(folder)) {
			cerr << "Unable to create target folder '" << folder << "'!" << endl;
			return -1;
		}
		return extractor.visit(root);
	}

	int extractAddressFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer, bool debug, ostream& debugOut) {
		FactExtractor<core::Address,Inserter> extractor(nodeIndexer);
		if (!extractor.getInserter().setDebug(debug, debugOut).setTargetFolder(folder)) {
			cerr << "Unable to create target folder '" << folder << "'!" << endl;
			return -1;
		}
		return extractor.visit(core::NodeAddress(root));
	}

	bool addFactsManually(const std::string &folder, const std::string &relationName, const std::set<string>& facts, bool debug, std::ostream &debugOut) {
		Inserter ins;
		if (!ins.setDebug(debug, debugOut).setTargetFolder(folder))
			return false;

		for (const auto &fact : facts) {
			ins.insert(relationName, fact);
		}
		return true;
	}

} // end namespace framework
} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

#include <fstream>
#include <boost/filesystem.hpp>

#include "insieme/frontend/utils/test_utils.h"

#include "insieme/frontend/frontend.h"
#include "insieme/core/analysis/ir_utils.h"


namespace insieme {
namespace frontend {
namespace utils {

	// ------- Test Utils -----


	namespace fs = boost::filesystem;

	namespace detail {

		void defaultConfig(insieme::frontend::ConversionJob& job) {
			// apply default configuration steps
			job.setStandard(insieme::frontend::ConversionSetup::Standard::Cxx14);
			job.registerDefaultExtensions();
		}

	}

	/**
	 * An enumeration of supported source language types.
	 */
	enum SrcType {
		C, CPP
	};

	/**
	 * A class managing the life-cycle of temporary source files which will only be
	 * created for the sake of unit tests.
	 */
	class Source {

		/**
		 * The path to the temporary file.
		 */
		fs::path file;

	public:

		/**
		 * The constructor creates a temporary file containing the given example code. The
		 * file will exist as long as the object is alive.
		 */
		Source(const string& code, SrcType type = CPP) {

			// create a temporary file containing the code
			switch(type) {
				case C:   file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.c"); break;
				case CPP: file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.cpp"); break;
				default: assert(false && "Invalid type selected!");
			}

			// write source to file
			std::fstream srcFile(file.string(), std::fstream::out);
			srcFile << code << "\n";
			srcFile.close();
		}

		Source(const Source&) = delete;
		Source(Source&&) = delete;

		~Source() {
			// remove temporary file
			if (fs::exists(file)) {
				fs::remove(file);
			}
		}

		const fs::path& getPath() const {
			return file;
		}

		operator fs::path() const { return getPath(); }

	};

	insieme::core::ProgramPtr parseFile(insieme::core::NodeManager& mgr, const fs::path& file, const JobConfigurator& configurator) {

		// set up the frontend conversion job
		insieme::frontend::ConversionJob job(file);

		// configure conversion job
		configurator(job);

		// run the conversion job
		return job.execute(mgr);
	}

	insieme::core::ProgramPtr parseCode(insieme::core::NodeManager& mgr, const std::string& code, const JobConfigurator& configurator) {

		// create a temporary code file
		Source src(code);

		// run the conversion
		return parseFile(mgr,src,configurator);
	}

	insieme::core::TypePtr parseType(insieme::core::NodeManager& mgr, const std::string& type, const std::string& header, const JobConfigurator& configurator) {

		// extend type to full program
		std::string code =
				"#include <string>\n"
				"#include <array>\n"
				"#include <vector>\n"
				+ header + "\n"
				"int main() { " + type + " x; }\n";

		// create a temporary code file
		Source src(code);

		// run the conversion
		auto prog = parseFile(mgr,src,configurator);

		// get type of variable declared in main
		auto body = prog->getEntryPoints()[0].as<insieme::core::LambdaExprPtr>()->getBody();
		auto var = body[0].as<insieme::core::DeclarationStmtPtr>()->getVariable();

		// get type of this variable
		return insieme::core::analysis::getReferencedType(var->getType());
	}



} // end namespace utils
} // end namespace frontend
} // end namespace insieme

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

/**
 * This utility header provides some tools to simplify the unit testing
 * of the frontend. This file is to be included within unit tests to simplify
 * the handling of example codes.
 */

#include <string>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/core/ir_program.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"

using namespace insieme::core;

namespace insieme {
namespace frontend {

	namespace fs = boost::filesystem;

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
		Source(const string& code, SrcType type = C) {

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

	// a utility to fix variable names
	NodePtr fixVariableIDs(const NodePtr& code) {
		NodeManager& mgr = code.getNodeManager();
		IRBuilder builder(mgr);

		// first, run simple normalizer
		NodePtr res = insieme::core::analysis::normalize(code);

		// now fix free variables
		std::map<VariablePtr, VariablePtr> vars;
		std::map<NodeAddress, NodePtr> replacements;
		auto freeVars = insieme::core::analysis::getFreeVariableAddresses(res);

		// if there are no free variables => done
		if (freeVars.empty()) return res;

		std::set<VariableAddress> freeVarSet(freeVars.begin(), freeVars.end());
		for (auto cur : freeVarSet) {
			VariablePtr var = cur.as<VariablePtr>();
			VariablePtr replacement;

			auto pos = vars.find(var);
			if (pos != vars.end()) {
				replacement = pos->second;
			} else {
				replacement = builder.variable(cur->getType().as<TypePtr>(), 100+vars.size());
				vars[var] = replacement;
			}
			replacements[cur] = replacement;
		}

		return transform::replaceAll(mgr, replacements);
	};

} // end namespace frontend
} // end namespace insieme


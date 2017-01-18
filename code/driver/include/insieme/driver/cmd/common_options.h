/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
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

	struct CommonOptions {

		bool compileOnly = false;
		bool checkSemaOnly = false;
		bool checkSema = false;
		frontend::path outFile;
		frontend::path dumpIR;
		frontend::path dumpTRG;
		frontend::path dumpTRGOnly;

		void addFlagsAndParameters(OptionParser& parser) {
			parser.addFlag(     "compile,c",               compileOnly,                                   "compilation only");
			parser.addFlag(     "check-sema",              checkSema,                                     "run semantic checks on the generated IR");
			parser.addFlag(     "check-sema-only",         checkSemaOnly,                                 "run semantic checks on the generated IR and stop afterwards");
			parser.addParameter("outfile,o",               outFile,           frontend::path("a.out"),    "output file");
			parser.addParameter("dump-ir",                 dumpIR,            frontend::path(),           "dump intermediate representation");
			parser.addParameter("dump-trg",                dumpTRG,           frontend::path(),           "dump target code");
			parser.addParameter("dump-trg-only",           dumpTRGOnly,       frontend::path(),           "dump target code and stop afterwards");
		}
	};

} // end namespace cmd
} // end namespace driver
} // end namespace insieme

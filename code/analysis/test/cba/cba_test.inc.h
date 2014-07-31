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

#include <fstream>
#include "insieme/analysis/cba/cba.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	namespace {

		inline void printConstraints(const CBA& analysis) {

			std::cout << "Constraints:\n";
			analysis.printConstraints();
		}

		inline void printConstraints(const NodeAddress& node) {
			// extract context and print equations
			printConstraints(getCBA(node));
		}

		inline void printSolution(const CBA& analysis) {

			std::cout << "Solution:\n";
			analysis.printSolution();
		}

		inline void printSolution(const NodeAddress& node) {
			// extract context and print equations
			printSolution(getCBA(node));
		}

		inline void createDotDump(const CBA& analysis) {
			std::cout << "Creating Dot-Dump for " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints ...\n";
			{
				// open file
				std::ofstream out("solution.dot", std::ios::out );

				// write file
				analysis.plot(out);
			}

			// create pdf
//			system("dot -Tpdf solution.dot -o solution.pdf");
//			system("dot -Tpng solution.dot -o solution.png");
			system("dot -Tsvg solution.dot -o solution.svg");
		}

		inline void createDotDump(const NodeAddress& node) {
			// extract context and dump it
			createDotDump(getCBA(node));
		}

		inline void createDotDumpRoots(const CBA& analysis) {
			std::cout << "Creating Dot-Dump for " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints ...\n";
			{
				// open file
				std::ofstream out("solution.dot", std::ios::out );

				// write file
				analysis.plotRoots(out);
			}

			// create pdf
//			system("dot -Tpdf solution.dot -o solution.pdf");
			system("dot -Tsvg solution.dot -o solution.svg");
		}

		inline void createDotDumpRoots(const NodeAddress& node) {
			// extract context and dump it
			createDotDumpRoots(getCBA(node));
		}
	}
	
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

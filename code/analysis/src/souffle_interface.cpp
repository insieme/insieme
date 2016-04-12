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

#include "insieme/analysis/datalog/souffle_interface.h"

namespace insieme {
namespace analysis {

// TODO remove as soon as proper souffle header exists
class Sf_Dummy_base {
public:
	void printAll() const {}
	void run() const {}
};

/*
 * Souffle wrapper
 */

using SouffleBase = Sf_Dummy_base;

class SouffleWrapper : public SouffleBase {
public:
	SouffleWrapper() : SouffleBase() {}

	sf_result_t getResultSet() const {

		sf_result_t res;

		#define symtab_get_num(i) (int32_t) cur[i]
		#define symtab_get_str(i) symTable.resolve(cur[i])

		// dummy
		res.emplace_back(1337);

		#undef symtab_get_str
		#undef symtab_get_num

		return res;
	}
};


/*
 * Souffle interface
 */

SouffleInterface::SouffleInterface() {
	impl = new SouffleWrapper();
}

SouffleInterface::~SouffleInterface() {
	delete impl;
}

void SouffleInterface::run() const {
	impl->run();
}

void SouffleInterface::printAll() const {
	impl->printAll();
}

sf_result_t SouffleInterface::getResultSet() const {
	return impl->getResultSet();
}

} // end namespace analysis
} // end namespace insieme

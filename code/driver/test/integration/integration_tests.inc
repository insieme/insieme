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
 * Some utilities to be shared among all integration tests.
 */

namespace insieme {

	using namespace utils::test;
	using namespace core;

	
	// --------------- Some global utilities for this test program --------------------
	
	namespace {
	
		// a node manager bound to the life cycle of the entire program
		NodeManager testGlobalManager;
	
		// a cache for already loaded Integreation tests
		std::map<IntegrationTestCase, ProgramPtr> loadedCodes;
	
		// a helper method for loading program code
		ProgramPtr load(NodeManager& manager, const IntegrationTestCase& testCase) {
	
			// check whether the code is already in the cache
			//auto pos = loadedCodes.find(testCase);
			//if (pos != loadedCodes.end()) {
			//	return manager.get(pos->second);
			//}
		
			// not loaded yet => load and cache code
			frontend::ConversionJob job(manager, testCase.getFiles(), testCase.getIncludeDirs());
			job.setOption(frontend::ConversionJob::OpenMP, testCase.isEnableOpenMP());
			core::ProgramPtr code = job.execute();

			// store the code in the global node manager 
			//loadedCodes.insert(std::make_pair(testCase, testGlobalManager.get(code)));

			return code;
		}
	
	}
	
	
	inline vector<TypePtr> getTypes(const vector<ExpressionPtr>& expressions) {
		vector<TypePtr> res;
		::transform(expressions, std::back_inserter(res), [](const ExpressionPtr& cur) { return cur->getType(); });
		return res;
	}

}

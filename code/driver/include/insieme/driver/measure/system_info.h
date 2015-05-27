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

#include "boost/lexical_cast.hpp"

#include "insieme/driver/measure/executor.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/backend/backend.h"
#include "insieme/backend/runtime/runtime_backend.h"

namespace insieme {
namespace driver {
namespace measure {


	using std::string;
	using std::map;

	class SystemInfo {
	private:

		mutable string result;

		map<string,string> env;

		map<string,string> backendCompilerDefs;

		insieme::backend::BackendPtr backend;

		insieme::utils::compiler::Compiler compiler;

		ExecutorPtr executor;

		virtual int obtainSystemInformation() const;

	public:
		SystemInfo(map<string, string> env = { {"IRT_REPORT", "1"}, { "IRT_REPORT_TO_FILE", "1" } }, map<string,string> backendCompilerDefs = { { "IRT_ENABLE_REGION_INSTRUMENTATION", "1" }, { "IRT_USE_PAPI", "1" } },
				insieme::backend::BackendPtr backend = insieme::backend::runtime::RuntimeBackend::getDefault(),
				insieme::utils::compiler::Compiler compiler = insieme::utils::compiler::Compiler::getRuntimeCompiler(),
				insieme::driver::measure::ExecutorPtr executor = insieme::driver::measure::makeLocalExecutor()) :
					result(""), env(env), backendCompilerDefs(backendCompilerDefs), backend(backend), compiler(compiler), executor(executor) { }

		virtual ~SystemInfo() { };

		virtual vector<string> queryString(const std::string regex) const;

		virtual string queryStringSingle(const std::string regex) const;

		template<typename T>
		vector<T> query(const std::string regex) const {
			const vector<string> temp = queryString(regex);
			vector<T> res;
			for(const auto& e : temp) {
				try {
					res.push_back(boost::lexical_cast<T>(e));
				} catch (boost::bad_lexical_cast& blc) {
					std::cout << "Unable to lexically cast " << e << ": " << blc.what() << "\n";
				}
			}
			return res;
		}

		template<typename T>
		T querySingle(const std::string regex) const {
			const auto temp = query<T>(regex);
			T res;
			if(!temp.empty()) {
				res = temp.front();
			}
			return res;
		}

		virtual string getFullInformation() const;
	};

} // end namespace measure
} // end namespace driver
} // end namespace insieme

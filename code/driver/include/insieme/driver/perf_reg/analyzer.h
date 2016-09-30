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

#pragma once

#include <functional>
#include <map>
#include <vector>

#include "insieme/driver/perf_reg/structures.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	class Analyzer {
	public:
		struct ArgType {
			const vector<DataPoint> &vec;
			const double &avg;
		};

	private:
		using analysisFunc = function<bool(ArgType)>;

		map<string,analysisFunc> funcs;
		analysisFunc defaultFunc = nullptr;

	public:
		void setDefault(analysisFunc func) {
			defaultFunc = func;
		}

		void mapFunction(const string &key, analysisFunc func) {
			funcs[key] = func;
		}

		analysisFunc getFunction(const string &key) {
			if (funcs.count(key))
				return funcs.at(key);

			if (!defaultFunc)
				return [](ArgType in) {
					(void) in;
					LOG(ERROR) << "Default not implemented!";
					return true;
				};

			return defaultFunc;
		}

		static Analyzer &getInstance() {
			static Analyzer instance;
			return instance;
		}

	private:
		Analyzer() {}
		Analyzer(Analyzer const&) = delete;
		Analyzer operator=(Analyzer const&) = delete;
	};

} // namespace perf_reg
} // namespace driver
} // namespace insieme

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

#include <iostream>
#include <sstream>
#include <string>

#include <gtest/gtest.h>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	class TestCaseNamePrinter {
	  public:
		template <class ParamType>
		std::string operator()(const ::testing::TestParamInfo<ParamType>& info) {
			return output(info.index, info.param.getName());
		}

		std::string operator()(const ::testing::TestParamInfo<std::string>& info) {
			return output(info.index, info.param);
		}

	  private:
		std::string output(size_t index, std::string name) {
			std::stringstream out;

			// format the index
			out << format("%03d", index);

			// format the name
			name = name.substr(0, name.find_last_of('.'));
			out << format("_%-100s", name);

			// sanitize the resulting string
			auto res = out.str();
			std::replace(res.begin(), res.end(), ' ', '_');
			std::replace(res.begin(), res.end(), '/', '_');
			std::replace(res.begin(), res.end(), '.', '_');
			std::replace(res.begin(), res.end(), '-', '_');

			return res;
		}
	};

} // end namespace utils
} // end namespace insieme

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

#pragma once

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <boost/filesystem.hpp>

#include <gtest/gtest.h>

#include "insieme/utils/assert.h"
#include "insieme/utils/container_utils.h"
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

	std::vector<std::string> collectInputFiles(const std::string& directory, const std::vector<std::string>& extensions) {
		boost::filesystem::path root(directory);
		assert_true(boost::filesystem::is_directory(root));

		std::vector<std::string> input_files;
		for(const auto& entry : boost::make_iterator_range(boost::filesystem::recursive_directory_iterator(root), {})) {
			if(containsSubString(entry.path().string(), "_disabled")) {
				continue;
			}

			if(!contains(extensions, entry.path().extension().string())) {
				continue;
			}

			auto path = entry.path().string().substr(directory.size());
			if(path[0] == '/') {
				path = path.substr(1);
			}

			input_files.push_back(path);
		}

		std::sort(input_files.begin(), input_files.end());
		return input_files;
	}

} // end namespace utils
} // end namespace insieme

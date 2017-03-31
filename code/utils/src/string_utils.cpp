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
 *
 */
#include "insieme/utils/string_utils.h"

#include <regex>

std::vector<string> split(const string& str) {
	using namespace std;
	vector<string> tokens;
	istringstream iss(str);
	copy(istream_iterator<string>(iss), istream_iterator<string>(), back_inserter<vector<string>>(tokens));
	return tokens;
}

string commonPrefix(string a, string b) {
	if(a.size() > b.size()) { std::swap(a, b); }
	return string(a.begin(), std::mismatch(a.begin(), a.end(), b.begin()).first);
}

bool containsSubString(const string& str, const string& substr) {
	return str.find(substr) != string::npos;
}

bool notContainsSubString(const string& str, const string& substr) {
	return !containsSubString(str, substr);
}

bool containsNTimesSubString(const string& str, const string& substr, const int n) {
	int count = 0;
	for(size_t offset = str.find(substr); offset != string::npos; offset = str.find(substr, offset + substr.length())) {
		count++;
	}
	return (count == n);
}

string camelcaseToUnderscore(const string input) {
	string output;

	for(char s : input) {
		if(std::isupper(s)) {
			if(!output.empty()) { output.push_back('_'); }
			output.push_back((char)tolower(s));
		} else {
			output.push_back(s);
		}
	}

	return output;
}

namespace insieme {
namespace utils {

	string removeCppStyleComments(const string& in) {
		const static std::regex comments(R"((//.*)|(/\*(?:.|\r|\n)*?\*/))");
		return std::regex_replace(in, comments, "");
	}

	string removePragmas(const string& in) {
		const static std::regex comments(R"((#.*))");
		return std::regex_replace(in, comments, "");
	}
}
}

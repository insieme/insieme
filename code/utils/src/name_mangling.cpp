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

#include "insieme/utils/name_mangling.h"

#include <vector>
#include <utility>

#include <boost/algorithm/string.hpp>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	using std::string;


	namespace {
		
		static const string manglePrefix = "IMP";
		static const string mangleLocation = "IMLOC";

		std::vector<std::pair<string, string>> replacements = {{"<", "_lt_"},
		                                                       {">", "_gt_"},
		                                                       {":", "_colon_"},
		                                                       {" ", "_space_"},
		                                                       {"(", "_lparen_"},
		                                                       {")", "_rparen_"},
		                                                       {",", "_comma_"},
		                                                       {"*", "_star_"},
		                                                       {"&", "_ampersand_"},
		                                                       {".", "_dot_"},
		                                                       {"+", "_plus_"},
		                                                       {"/", "_slash_"},
		                                                       {"-", "_minus_"},
		                                                       {"~", "_wave_"},
		                                                       {manglePrefix, "_not_really_an_imp_philipp_what_are_you_talking_about_sorry_"},
		                                                       {mangleLocation, "_not_really_mangle_location_"}};

		string applyReplacements(string in) {
			for(auto& mapping : replacements) {
				boost::replace_all(in, mapping.first, mapping.second);
			}
			return in;
		}
		
		string reverseReplacements(string in) {
			for(auto& mapping : replacements) {
				boost::replace_all(in, mapping.second, mapping.first);
			}
			return in;
		}

	}

	string mangle(string name, string file, unsigned line, unsigned column) {
		return format("%s_%s_%s_%s_%u_%u", manglePrefix, applyReplacements(name), mangleLocation, applyReplacements(file), line, column);
	}
	
	std::string mangle(std::string name) {
		return format("%s_%s", manglePrefix, applyReplacements(name));
	}
	
	string demangle(string name) {
		if(!boost::starts_with(name, manglePrefix)) return name;
		auto ret = name.substr(manglePrefix.size()+1);
		auto loc = ret.find(mangleLocation);
		if(loc != string::npos) {
			ret = ret.substr(0,	loc-1);
		}
		return reverseReplacements(ret);
	}

}
}

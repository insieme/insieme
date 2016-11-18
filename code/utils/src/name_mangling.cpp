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

#include "insieme/utils/name_mangling.h"

#include <vector>
#include <utility>

#include <boost/algorithm/string.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	using std::string;


	namespace {

		static const string manglePrefix = "IMP_";
		static const string mangleLocation = "IMLOC";
		static const string mangleEmpty = "EMPTY";

		using StringPairs = std::vector<std::pair<string, string>>;

		const StringPairs operators = {
			{"operator<<=", "_operator_lshift_assign_"},
			{"operator>>=", "_operator_rshift_assign_"},
			{"operator->*", "_operator_memberpointer_"},
			{"operator+=",  "_operator_plus_assign_"},
			{"operator-=",  "_operator_minus_assign_"},
			{"operator*=",  "_operator_mult_assign_"},
			{"operator/=",  "_operator_div_assign_"},
			{"operator%=",  "_operator_mod_assign_"},
			{"operator^=",  "_operator_xor_assign_"},
			{"operator&=",  "_operator_and_assign_"},
			{"operator|=",  "_operator_or_assign_"},
			{"operator<<",  "_operator_lshift_"},
			{"operator>>",  "_operator_rshift_"},
			{"operator==",  "_operator_eq_"},
			{"operator!=",  "_operator_neq_"},
			{"operator<=",  "_operator_le_"},
			{"operator>=",  "_operator_ge_"},
			{"operator&&",  "_operator_land_"},
			{"operator||",  "_operator_lor_"},
			{"operator++",  "_operator_inc_"},
			{"operator--",  "_operator_dec_"},
			{"operator->",  "_operator_member_"},
			{"operator()",  "_operator_call_"},
			{"operator[]",  "_operator_subscript_"},
			{"operator+",   "_operator_plus_"},
			{"operator-",   "_operator_minus_"},
			{"operator*",   "_operator_mult_"},
			{"operator/",   "_operator_div_"},
			{"operator%",   "_operator_mod_"},
			{"operator^",   "_operator_xor_"},
			{"operator&",   "_operator_and_"},
			{"operator|",   "_operator_or_"},
			{"operator~",   "_operator_complement_"},
			{"operator=",   "_operator_assign_"},
			{"operator<",   "_operator_lt_"},
			{"operator>",   "_operator_gt_"},
			{"operator!",   "_operator_not_"},
			{"operator,",   "_operator_comma_"},
			{"operator ",   "_conversion_operator_"}
		};

		const StringPairs mangling = {
			{"<", "_lt_"},
			{">", "_gt_"},
			{":", "_colon_"},
			{" ", "_space_"},
			{"(", "_lparen_"},
			{")", "_rparen_"},
			{"[", "_lbracket_"},
			{"]", "_rbracket_"},
			{",", "_comma_"},
			{"*", "_star_"},
			{"&", "_ampersand_"},
			{".", "_dot_"},
			{"+", "_plus_"},
			{"/", "_slash_"},
			{"-", "_minus_"},
			{"~", "_wave_"},
			{manglePrefix, "_not_really_an_imp_philipp_what_are_you_talking_about_sorry_"},
			{mangleLocation, "_not_really_mangle_location_"},
			{mangleEmpty, "_not_really_mangle_empty_"}
		};

		StringPairs replacements;

		const StringPairs& getReplacements() {
			if(replacements.empty()) {
				std::copy(operators.cbegin(), operators.cend(), std::back_inserter(replacements));
				std::copy(mangling.cbegin(), mangling.cend(), std::back_inserter(replacements));
			}
			return replacements;
		}

		string applyReplacements(string in) {
			for(auto& mapping : getReplacements()) {
				boost::replace_all(in, mapping.first, mapping.second);
			}
			return in;
		}

		string reverseReplacements(string in) {
			for(auto& mapping : getReplacements()) {
				boost::replace_all(in, mapping.second, mapping.first);
			}
			return in;
		}

		string removeMangled(string in, string replacement = "") {
			for(auto& mapping : mangling) {
				boost::replace_all(in, mapping.second, replacement);
			}
			return in;
		}
	}

	string mangle(string name, string file, unsigned line, unsigned column) {
		if(name.empty()) name = mangleEmpty;
		else name = applyReplacements(name);
		return format("%s%s_%s_%s_%u_%u", manglePrefix, name, mangleLocation, applyReplacements(file), line, column);
	}

	string mangle(string file, unsigned line, unsigned column) {
		return mangle("", file, line, column);
	}

	string mangle(string name) {
		return format("%s%s", manglePrefix, applyReplacements(name));
	}

	string demangle(string name, bool keepLocation) {
		if(!isMangled(name)) return name;
		auto ret = name.substr(manglePrefix.size());
		if(boost::starts_with(ret, mangleEmpty)) return "";
		if(!keepLocation) {
			auto loc = ret.find(mangleLocation);
			if(loc != string::npos) {
				ret = ret.substr(0, loc-1); // remove _ as well
			}
		}
		ret = reverseReplacements(ret);
		return ret;
	}

	bool isMangled(std::string name) {
		return boost::starts_with(name, manglePrefix);
	}

	string demangleToIdentifier(string name, bool keepLocation) {
		// special case for conversion operators
		static string convOpMangled = mangle("operator ");
		if(boost::starts_with(name, convOpMangled)) {
			return demangle(name);
		}

		// special case for all the other operators
		auto demangledName = demangle(name);
		if(::any(operators, [&](const auto& pair) { return pair.first == demangledName; })) return demangledName;

		// default case
		auto str = removeMangled(name);
		return demangle(str, keepLocation);
	}

	const string& getMangledOperatorAssignName() {
		static string result = mangle("operator=");
		return result;
	}

	const std::string& getMangledAnonymousIndicator() {
		static string result = applyReplacements("(anonymous");
		return result;
	}

	const std::string getReadableName(const std::string& name) {
		if(!isMangled(name)) return name;

		string ret = name.substr(manglePrefix.size());

		// if anon, search from back and find first /
		if(boost::contains(name, mangleLocation)) {
			auto lastSlash = boost::find_last(name, applyReplacements("/"));
			if(lastSlash.empty()) lastSlash = boost::find_last(name, mangleLocation);
			if(!lastSlash.empty()) {
				ret = string("anon_") + string(lastSlash.end(), name.end());
			}
		}

		ret = removeMangled(ret, "_");
		boost::replace_all(ret, "__", "_");

		return ret;
	}

}
}

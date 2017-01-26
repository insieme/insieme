/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/driver/integration/properties.h"

#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace driver {
namespace integration {

	using std::pair;

	const string Properties::get(const string& key, const string& category, const string& def) const {
		// check the first level
		auto p1 = data.find(key);
		if(p1 == data.end()) { return def; }

		// check the second level for a category
		auto p2 = p1->second.find(category);
		if(p2 == p1->second.end()) {
			// if the category is not present => fall-back
			if(category == "") { return def; }
			return get(key, "");
		}

		// take the value
		return p2->second;
	}

	std::set<string> Properties::getKeys() const {
		std::set<string> res;
		for(const auto& cur : data) {
			res.insert(cur.first);
		}
		return res;
	}

	PropertyView Properties::getView(const string& category) const {
		return PropertyView(*this, category);
	}

	std::ostream& Properties::printTo(std::ostream& out) const {
		// check for empty properties
		if(empty()) { return out << "Properties { }\n"; }

		// print the properties
		out << "Properties {\n\t";
		out << join("\n\t", data, [](std::ostream& out, const pair<string, map<string, string>>& cur) {
			out << join("\n\t", cur.second, [&](std::ostream& out, const pair<string, string>& p) {
				out << cur.first;
				if(p.first != "") { out << "[" << p.first << "]"; }
				out << "=" << p.second;
			});
		});
		out << "\n}\n";

		return out;
	}

	string Properties::mapVars(const string& in) const {
		auto res = in;
		for(const auto& o : data) {
			for(const auto& i : o.second) {
				if(i.first.empty()) {
					continue; // skip those for now
				}
				auto key = "${" + o.first + "[" + i.first + "]}";

				// replace key with value
				boost::replace_all(res, key, i.second);
			}

			// and the potential empty one
			auto key = "${" + o.first + "}";
			boost::replace_all(res, key, get(o.first));
		}

		return res;
	}

	Properties& Properties::operator<<=(const Properties& other) {
		Properties orig = *this;

		// merge the data
		for(const auto& o : other.data) {
			for(const auto& i : o.second) {
				// TODO: apply variable substitution
				set(o.first, i.first, orig.mapVars(i.second));
			}
		}

		// map existing keys with new info
		for(const auto& o : data) {
			for(const auto& i : o.second) {
				set(o.first, i.first, mapVars(i.second));
			}
		}

		// done
		return *this;
	}

	Properties Properties::load(std::istream& in) {
		Properties res;

		// process line by line
		string line;
		while(std::getline(in, line)) {
			// first we strip all comments
			auto commentPos = line.find('#');
			if(commentPos != string::npos) {
				line = line.substr(0, commentPos);
			}

			// then we trim all whitespaces
			boost::trim(line);

			// skip empty lines
			if(line.empty()) { continue; }

			// skip comments
			if(line[0] == '#') { continue; }

			// first split at = sign
			auto sep = line.find('=');
			if(sep == string::npos) { continue; }

			auto key = line.substr(0, sep);
			auto cat = string("");
			auto value = line.substr(sep + 1, line.size());

			// check whether key needs to cut up
			if(*key.rbegin() == ']') {
				auto csep = key.find('[');

				// sanity check
				if(csep == string::npos) { continue; }

				key = line.substr(0, csep);
				cat = line.substr(csep + 1, sep - csep - 2);
			}

			// single values are cleaned of their quotes
			if(std::count(value.begin(), value.end(), '\"') == 2 && value[0] == '\"' && value[value.size() - 1] == '\"') {
				value = value.substr(1, value.size() - 2);
			}

			// register value
			res.set(key, cat, value);
		}

		// done
		return res;
	}

	void Properties::store(std::ostream& out) const {
		// write the data to the stream
		for(const auto& o : data) {
			for(const auto& i : o.second) {
				out << o.first;
				if(!i.first.empty()) { out << "[" << i.first << "]"; }
				out << "=";
				out << i.second;
				out << "\n";
			}
		}
	}

	std::ostream& PropertyView::printTo(std::ostream& out) const {
		// special case: empty
		if(properties.empty()) { return out << properties; }

		out << "Properties {\n\t";
		out << join("\n\t", properties.getKeys(), [&](std::ostream& out, const string& key) { out << key << "=" << this->get(key); });
		out << "\n}\n";

		return out;
	}

} // end namespace integration
} // end namespace driver
} // end namespace insieme

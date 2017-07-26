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

#include <set>
#include <map>
#include <string>
#include <vector>
#include <type_traits>

#include <boost/tokenizer.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace driver {
namespace integration {

	using std::set;
	using std::map;
	using std::string;
	using std::vector;

	namespace detail {

		// -- the basic value convert functor --

		template <typename T>
		struct value_extractor {
			value_extractor() {}

			template <typename P>
			static typename std::enable_if<std::is_arithmetic<P>::value, P>::type convert(const string& value) {
				// numerical values are converted using a numeric cast
				return insieme::utils::numeric_cast<int>(value);
			}

			template <typename P>
			static typename std::enable_if<!std::is_arithmetic<P>::value, P>::type convert(const string& value) {
				return value; // default handling => implicit conversion
			}

			T operator()(const string& value) const {
				// by default we use implicit conversion (e.g. by a constructor call)
				return convert<T>(value);
			}
		};

		// -- specializations for atomic values --

		template <>
		struct value_extractor<string> {
			value_extractor() {}

			const string& operator()(const string& value) const {
				return value; // the default operation simply utilizes the implicit conversion
			}
		};

		template <>
		struct value_extractor<bool> {
			value_extractor() {}

			bool operator()(const string& value) const {
				// special handling for bool values
				return value == "1" || value == "true" || value == "t" || value == "yes" || value == "y";
			}
		};

		// -- specializations for containers values --

		template <typename T>
		struct value_extractor<vector<T>> {
			value_extractor() {}

			vector<T> operator()(const string& value) const {
				static const value_extractor<T> extract;
				static const boost::char_separator<char> sep("\",");
				vector<T> res;
				boost::tokenizer<boost::char_separator<char>> tokens(value, sep);
				for(const auto& t : tokens) {
					res.push_back(extract(t));
				}
				return res;
			}
		};

		template <typename T>
		struct value_extractor<set<T>> {
			value_extractor() {}

			set<T> operator()(const string& value) const {
				set<T> res;
				for(const auto& cur : value_extractor<vector<T>>()(value)) {
					res.insert(cur);
				}
				return res;
			}
		};
	}

	class PropertyView;

	class Properties : public utils::Printable {
		// key / category / value
		map<string, map<string, string>> data;

	  public:
		const string get(const string& key, const string& category = "", const string& def = "") const;

		template <typename T, typename R = decltype(detail::value_extractor<T>()(""))>
		R get(const string& key, const string& category = "", const string& def = "") const {
			return detail::value_extractor<T>()(get(key, category, def));
		}

		std::set<string> getKeys() const;

		void set(const string& key, const string& value) {
			set(key, "", value);
		}

		void set(const string& key, const string& category, const string& value) {
			data[key][category] = value;
		}

		const string operator[](const string& key) const {
			return get(key);
		}

		bool empty() const {
			return data.empty();
		}

		std::size_t size() const {
			return data.size();
		}

		string mapVars(const string& in) const;

		PropertyView getView(const string& category) const;

		std::ostream& printTo(std::ostream& out) const;

		bool operator==(const Properties& other) const {
			return data == other.data;
		}

		Properties& operator<<=(const Properties& other);

		Properties operator<<(const Properties& other) {
			return Properties(*this) <<= other;
		}

		/**
		 * Loads properties from the given input stream.
		 */
		static Properties load(std::istream& in);

		/**
		 * Dumps this properties to the output stream.
		 */
		void store(std::ostream& out) const;
	};


	class PropertyView : public utils::Printable {
		const Properties& properties;

		const string category;

	  public:
		PropertyView(const Properties& properties, const string& category) : properties(properties), category(category) {}

		string operator[](const string& key) const {
			return properties.get(key, category);
		}

		string get(const string& key, const string& def = "") const {
			return properties.get(key, category, def);
		}

		template <typename T, typename R = decltype(detail::value_extractor<T>()(""))>
		R get(const string& key, const string& def = "") const {
			return detail::value_extractor<T>()(get(key, def));
		}

		std::ostream& printTo(std::ostream& out) const;
	};


} // end namespace integration
} // end namespace driver
} // end namespace insieme

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

#include <map>
#include <ostream>
#include <typeindex>
#include <type_traits>

#include "insieme/utils/assert.h"

namespace insieme {
namespace utils {

	template <template <typename T> class ValueType, typename BaseType, typename ExampleElementType = int,
	          typename std::enable_if<std::is_base_of<BaseType, ValueType<ExampleElementType>>::value, int>::type = 0>
	class TypedMap {
	  public:
		typedef std::map<std::type_index, BaseType*> map_type;
		typedef typename map_type::const_iterator const_iterator;

	  private:
		map_type data;

	  public:
		TypedMap() {}

		TypedMap(const TypedMap& other) {
			for(auto cur : other.data) {
				data[cur.first] = cur.second->copy();
			}
		}

		TypedMap(TypedMap&& other) = default;

		~TypedMap() {
			for(auto cur : data) {
				delete cur.second;
			}
		}

		template <typename T>
		ValueType<T>& get() {
			auto& key = typeid(T);
			auto pos = data.find(key);
			if(pos != data.end()) { return static_cast<ValueType<T>&>(*pos->second); }

			// create, register and return a new instance
			ValueType<T>* element = new ValueType<T>();
			data[key] = element;
			return *element;
		}

		template <typename T>
		const ValueType<T>& get() const {
			static const ValueType<T> empty = ValueType<T>();

			auto& key = typeid(T);
			auto pos = data.find(key);
			if(pos != data.end()) { return static_cast<ValueType<T>&>(*pos->second); }

			return empty;
		}

		const_iterator begin() const {
			return data.begin();
		}
		const_iterator end() const {
			return data.end();
		}

		bool operator==(const TypedMap& other) const {
			return data == other.data;
		}

		bool operator!=(const TypedMap& other) const {
			return !(*this == other);
		}

		TypedMap& operator=(const TypedMap& other) {
			// delete content
			for(auto cur : data) {
				delete cur.second;
			}
			data.clear();

			// copy data
			for(auto cur : other.data) {
				data[cur.first] = cur.second->copy();
			}

			return *this;
		}
	};

	/**
	 * A container for handling heterogeneous collections of objects.
	 */
	class HeterogenousContainer {
		struct HandlerBase {
			virtual ~HandlerBase(){};
		};

		template <typename T>
		struct Handler : public HandlerBase {
			T t;
			template <typename... Args>
			Handler(const Args&... args)
			    : t(args...) {}
			operator T&() {
				return t;
			}
			operator const T&() const {
				return t;
			}
		};

		std::map<std::type_index, HandlerBase*> content;

	  public:
		~HeterogenousContainer() {
			for(auto cur : content) {
				delete cur.second;
			}
		}

		template <typename T, typename... Args>
		T& getInstance(Args... args) {
			std::type_index key = typeid(T);
			auto pos = content.find(key);
			if(pos != content.end()) { return static_cast<Handler<T>&>(*(pos->second)); }

			// insert element
			content[key] = new Handler<T>(args...);
			return getInstance<T>();
		}

		template <typename T>
		const T& getInstance() const {
			std::type_index key = typeid(T);
			auto pos = content.find(key);
			assert_true(pos != content.end());
			return static_cast<Handler<T>&>(*(pos->second));
		}

		template <typename T>
		bool contains() const {
			return content.find(typeid(T)) != content.end();
		}
	};

} // end namespace utils
} // end namespace insieme

namespace std {

	template <template <typename X> class A, typename B, typename C>
	std::ostream& operator<<(std::ostream& out, const insieme::utils::TypedMap<A, B, C>& map) {
		return out << "{"
		           << join(",", map, [](std::ostream& out, const typename insieme::utils::TypedMap<A, B, C>::map_type::value_type& cur) { out << *cur.second; })
		           << "}";
	}

} // end namespace std

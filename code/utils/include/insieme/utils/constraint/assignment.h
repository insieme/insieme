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
#pragma once

#include <string>
#include <map>

#include "insieme/utils/constraint/variables.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/typed_map.h"

namespace insieme {
namespace utils {
namespace constraint {

	using std::string;
	using std::map;

	// ----------------------------- Assignment ------------------------------

	/**
	 * An assignment is partial mapping mapping values to variables.
	 */
	class Assignment : public Printable {
		/**
		 * A base type for internal generic containers.
		 */
		struct Container : public VirtualPrintable {
			virtual ~Container(){};
			virtual void extendMap(map<Variable, string>& res) const = 0;
			virtual Container* copy() const = 0;
			virtual void clear(const Variable& value) = 0;
		};

		/**
		 * A generic internal container storing mappings of the lattice type L.
		 */
		template <typename L>
		struct TypedContainer : public Container, public map<TypedVariable<L>, typename L::value_type> {
			typedef map<TypedVariable<L>, typename L::value_type> map_type;
			typedef typename map_type::value_type value_type;

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << join(",", *this, [](std::ostream& out, const value_type& cur) { out << cur.first << "=" << cur.second; });
			}
			virtual void extendMap(map<Variable, string>& res) const {
				for(auto& cur : *this) {
					res.insert({cur.first, toString(cur.second)});
				}
			}
			virtual Container* copy() const {
				return new TypedContainer<L>(*this);
			}
			virtual void clear(const Variable& value) {
				this->erase(value);
			}
		};

		typedef TypedMap<TypedContainer, Container, SetLattice<int>> container_index_type;

		container_index_type data;

	  public:
		/**
		 * A default constructor - nothing special.
		 */
		Assignment(){};

		/**
		 * A default move constructor since the contained data container is a heavy object.
		 */
		Assignment(Assignment&& other) = default;

		/**
		 * A copy constructor for assignments.
		 */
		Assignment(const Assignment& other) = default;

		/**
		 * Obtains a reference to the value associated to the given variable by this assignment.
		 */
		template <typename L>
		typename L::value_type& get(const TypedVariable<L>& value) {
			return data.get<L>()[value];
		}

		/**
		 * Obtains the value associated to the given variable by this assignment.
		 */
		template <typename L>
		const typename L::value_type& get(const TypedVariable<L>& var) const {
			static const typename L::value_type empty;
			auto& map = data.get<L>();
			auto pos = map.find(var);
			if(pos != map.end()) { return pos->second; }
			return empty;
		}

		/**
		 * Obtains a reference to the value associated to the given variable by this assignment.
		 */
		template <typename L>
		typename L::value_type& operator[](const TypedVariable<L>& var) {
			return get(var);
		}

		/**
		 * Obtains the value associated to the given variable by this assignment.
		 */
		template <typename L>
		const typename L::value_type& operator[](const TypedVariable<L>& var) const {
			return get(var);
		}

		/**
		 * The assignment operator for this type.
		 */
		Assignment& operator=(const Assignment& other) = default;

		/**
		 * Clears the assignment to the given variable.
		 */
		void clear(const Variable& var) {
			for(auto& cur : data) {
				cur.second->clear(var);
			}
		}

		/**
		 * Enables debug-prints of this assignment.
		 */
		std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

		/**
		 * Converts this assignment into a map of variables assigned to string representations of
		 * the assigned values -- for debugging only.
		 */
		map<Variable, string> toStringMap() const {
			map<Variable, string> res;
			for(auto& cur : data) {
				cur.second->extendMap(res);
			}
			return res;
		}
	};

} // end namespace constraint
} // end namespace utils
} // end namespace insieme

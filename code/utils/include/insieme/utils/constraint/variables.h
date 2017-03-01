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

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint/lattice.h"

namespace insieme {
namespace utils {
namespace constraint {

	// ----------------------------- Basic Variable ------------------------------

	/**
	 * A variable is the entity to be constraint by constraints.
	 */
	class Variable : public Printable {
		int id;

	  public:
		Variable(int id = -1) : id(id){};
		Variable(const Variable& id) : id(id.id){};

		int getID() const {
			return id;
		}

		bool operator==(const Variable& other) const {
			return id == other.id;
		}
		bool operator!=(const Variable& other) const {
			return !(*this == other);
		}
		bool operator<(const Variable& other) const {
			return id < other.id;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "v" << id;
		}
	};


	// --------------------------- Typed Variables ------------------------------

	/**
	 * A typed variable is a variable with an associated lattice = property space.
	 *
	 * @tparam L ... the lattice defining the domain of this variable
	 */
	template <typename L>
	struct TypedVariable : public Variable {
		typedef L lattice_type;
		TypedVariable(int id = -1) : Variable(id){};
		TypedVariable(const Variable& id) : Variable(id){};
	};

	/**
	 * A typed variable that is utilizing a set lattice.
	 *
	 * @tparam E ... the element type of the set lattice defining the domain of this variable
	 */
	template <typename E>
	struct TypedSetVariable : public TypedVariable<SetLattice<E>> {
		TypedSetVariable(int id = -1) : TypedVariable<SetLattice<E>>(id){};
		TypedSetVariable(const Variable& id) : TypedVariable<SetLattice<E>>(id){};
	};

} // end namespace constraint
} // end namespace utils
} // end namespace insieme

namespace std {

	template <>
	struct hash<insieme::utils::constraint::Variable> {
		size_t operator()(const insieme::utils::constraint::Variable& id) const {
			return id.getID();
		}
	};

} // end namespace std

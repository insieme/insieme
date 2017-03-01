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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {
namespace lang {

	enum TypeCategory { INT, UINT, REAL };

	namespace detail {

		template <TypeCategory cat, int b>
		struct _base_type_map {
			enum { category = cat, bits = b };
		};
	}

	template <TypeCategory category, int bits>
	struct type_map;

	template <>
	struct type_map<INT, 1> : public detail::_base_type_map<INT, 1> {
		typedef int8_t value_type;
	};

	template <>
	struct type_map<INT, 2> : public detail::_base_type_map<INT, 2> {
		typedef int16_t value_type;
	};

	template <>
	struct type_map<INT, 4> : public detail::_base_type_map<INT, 4> {
		typedef int32_t value_type;
	};

	template <>
	struct type_map<INT, 8> : public detail::_base_type_map<INT, 8> {
		typedef int64_t value_type;
	};

	template <>
	struct type_map<UINT, 1> : public detail::_base_type_map<UINT, 1> {
		typedef uint8_t value_type;
	};

	template <>
	struct type_map<UINT, 2> : public detail::_base_type_map<UINT, 2> {
		typedef uint16_t value_type;
	};

	template <>
	struct type_map<UINT, 4> : public detail::_base_type_map<UINT, 4> {
		typedef uint32_t value_type;
	};

	template <>
	struct type_map<UINT, 8> : public detail::_base_type_map<UINT, 8> {
		typedef uint64_t value_type;
	};

	template <>
	struct type_map<REAL, 4> : public detail::_base_type_map<REAL, 4> {
		typedef float value_type;
	};

	template <>
	struct type_map<REAL, 8> : public detail::_base_type_map<REAL, 8> {
		typedef double value_type;
	};

} // end namespace lang
} // end namespace core
} // end namespace insieme

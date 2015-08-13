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

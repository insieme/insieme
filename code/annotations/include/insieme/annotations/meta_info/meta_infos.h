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

#include <tuple>

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/tuples.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/ir.h"

namespace insieme {
namespace annotations {

	namespace detail {


		// -- basic type conversion --

		template<typename T>
		struct compiler_type {
			typedef T type;
		};

		template<typename R, typename ... Ps>
		struct compiler_type<R(*)(Ps...)> {
			typedef core::ExpressionPtr type;
		};


		// -- support for comparing fields --

		template<typename T>
		struct field_comperator {
			bool operator()(const T& a, const T& b) const { return a == b; }
		};

		template<typename T>
		struct field_comperator<core::Pointer<T>> {
			bool operator()(const core::Pointer<T>& a, const core::Pointer<T>& b) const { return *a == *b; }
		};



		// -- support for cloning fields --

		template<typename T>
		struct field_cloner {
			T operator()(core::NodeManager& mgr, const T& in) const { return in; }
		};

		template<typename T>
		struct field_cloner<core::Pointer<T>> {
			core::Pointer<T> operator()(core::NodeManager& mgr, const core::Pointer<T>& in) const { return mgr.get(in); }
		};
	}

	// include the actual definitions

	#include "insieme/annotations/meta_info/generators/enum.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/struct.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/equals.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/clone.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/dump_type.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/dump_to.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/dump_from.inc"
	#include "insieme/meta_information/meta_infos.def"

	#include "insieme/annotations/meta_info/generators/clear.inc"



} // end namespace annotations
} // end namespace insieme

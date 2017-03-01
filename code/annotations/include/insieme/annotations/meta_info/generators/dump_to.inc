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
#include "clear.inc"

#define INFO_STRUCT_BEGIN(_name)                                                                                                                               \
	namespace _name##_ns {                                                                                                                                     \
		inline insieme::core::ExpressionPtr info_typeAnnotationConverter::toIR(insieme::core::NodeManager& manager,                                            \
		                                                                       const insieme::core::NodeAnnotationPtr& annotation) const {                     \
			static const string runtime_struct_name = #_name;                                                                                                  \
			typedef insieme::core::value_node_annotation<info_type>::type annotation_type;                                                                     \
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only " #_name "_info annotations supported!");                                        \
			const auto& data = static_pointer_cast<annotation_type>(annotation)->getValue();                                                                   \
		        return insieme::core::encoder::toIR(manager, tuple_type (

#define INFO_FIELD_EXT(_name, _a, _b, _type, _def) data._name,

#define INFO_STRUCT_END()                                                                                                                                      \
	    runtime_struct_name));                                                                                                                                 \
	}                                                                                                                                                          \
	}

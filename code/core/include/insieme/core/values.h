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
namespace new_core {


	/**
	 * A macro defining value nodes based on a name and the type of value to be presented.
	 */
	#define VALUE_NODE(NAME,TYPE) \
		class NAME ## Value : public ValueNode<NAME ## Value> { \
			NAME ## Value(const TYPE value) : ValueNode(NT_ ## NAME ## Value, value) {} \
		public: \
			TYPE getValue() const { \
				return boost::get<TYPE>(ValueNode::getValue()); \
			} \
			static NAME ## ValuePtr get(NodeManager& manager, const TYPE value) { \
				return manager.get(NAME ## Value(value)); \
			} \
		}

	/**
	 * The BoolValue node represents a single, boolean value.
	 */
	VALUE_NODE(Bool, bool);

	/**
	 * The CharValue node represents a character value within the IR structure.
	 */
	VALUE_NODE(Char, char);

	/**
	 * The IntValue node represents an integer value within the IR structure.
	 */
	VALUE_NODE(Int, int);

	/**
	 * The UIntValue node representing an unsigned integer value within the IR structure.
	 */
	VALUE_NODE(UInt, unsigned);

	/**
	 * The StringValue node represents a string value e.g. naming a type or an identifer within
	 * the IR structure.
	 */
	VALUE_NODE(String, string&);


	#undef VALUE_NODE

} // end namespace new_core
} // end namespace core
} // end namespace insieme

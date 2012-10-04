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

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 									Node Types
	// **********************************************************************************

	/**
	 * Defines an enumeration containing an entry for every node type. This
	 * enumeration can than be used to identify the actual type of AST nodes
	 * in case the exact type cannot be determined statically.
	 */
	#define CONCRETE(name) NT_ ## name,
	enum NodeType {
		// the necessary information is obtained from the node-definition file
		#include "insieme/core/ir_nodes.def"
	};
	#undef CONCRETE

	/**
	 * A constant defining the number of node types.
	 */
	#define CONCRETE(name) +1
	enum { NUM_CONCRETE_NODE_TYPES = 0
		// the necessary information is obtained from the node-definition file
		#include "insieme/core/ir_nodes.def"
	};
	#undef CONCRETE



	// **********************************************************************************
	// 									Node Categories
	// **********************************************************************************

	/**
	 * Defines a set of categories nodes might belong to. Every node has to belong to
	 * exactly one of the enlisted categories.
	 */
	enum NodeCategory {
		NC_Value,			// < a leaf node representing a value
		NC_IntTypeParam,	// < a node representing an int-type-param
		NC_Type,			// < a node representing a type
		NC_Expression,		// < a node representing an expression
		NC_Statement,		// < a node representing a statement
		NC_Program,			// < a node representing a program
		NC_Support			// < a utility used to realize a complex data structure
	};

	// A node trait linking a the category enumeration to the base type
	template<NodeCategory category> struct node_category_trait;
	template<> struct node_category_trait<NC_Value> 		{ typedef Value base_type; };
	template<> struct node_category_trait<NC_IntTypeParam> 	{ typedef IntTypeParam base_type; };
	template<> struct node_category_trait<NC_Type> 			{ typedef Type base_type; };
	template<> struct node_category_trait<NC_Expression> 	{ typedef Expression base_type; };
	template<> struct node_category_trait<NC_Statement> 	{ typedef Statement base_type; };
	template<> struct node_category_trait<NC_Program> 		{ typedef Program base_type; };
	template<> struct node_category_trait<NC_Support> 		{ typedef Support base_type; };


} // end namespace core
} // end namespace insieme

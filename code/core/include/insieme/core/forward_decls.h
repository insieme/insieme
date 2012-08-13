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

#include <vector>
#include <boost/variant.hpp>

#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {

	using std::string;
	using std::pair;
	using std::vector;

	// forward declaration of pointer and address templates
	template<typename T> class Pointer;
	template<typename T> class Address;

	/**
	 * Adds forward declarations for all AST node types. Further, for each
	 * type a type definition for a corresponding annotated pointer is added.
	 */
	#define NODE(NAME) \
		class NAME; \
		typedef Pointer<const NAME> NAME ## Ptr; \
		typedef Address<const NAME> NAME ## Address; \
		template<typename D, template <typename P> class P> class NAME ## Accessor;

		// take all nodes from within the definition file
		#include "insieme/core/ir_nodes.def"

	#undef NODE


	/**
	 * The union of all the values which can directly be represented using nodes. If
	 * a node represents a value, it is representing a value of this type.
	 */
	typedef boost::variant<bool,char,int,unsigned,string> NodeValue;


	// Supporting Utilities
	class IRBuilder;
	class NodeManager;
	class NodeMapping;
	class NodeAnnotation;

	namespace lang {
		class BasicGenerator;
	} // end namespace lang


	/**
	 * Typedefs for some widely used base type collections.
	 */
	typedef std::vector<NodePtr> NodeList;
	typedef std::vector<TypePtr> TypeList;
	typedef std::vector<IntTypeParamPtr> IntParamList;
	typedef std::vector<StatementPtr> StatementList;
	typedef std::vector<ExpressionPtr> ExpressionList;
	typedef std::vector<VariablePtr> VariableList;
	typedef std::vector<NamedTypePtr> NamedTypeList;
	typedef std::vector<NamedValuePtr> NamedValueList;

	typedef utils::set::PointerSet<NodePtr> NodeSet;
	typedef utils::set::PointerSet<TypePtr> TypeSet;
	typedef utils::set::PointerSet<VariablePtr> VariableSet;
	typedef utils::set::PointerSet<IntTypeParamPtr> IntParamSet;
	typedef utils::set::PointerSet<StatementPtr> StatementSet;
	typedef utils::set::PointerSet<ExpressionPtr> ExpressionSet;
	typedef utils::set::PointerSet<IntTypeParamPtr> IntTypeParamSet;
	
	typedef utils::map::PointerMap<NodePtr, NodePtr> NodeMap;
	typedef utils::map::PointerMap<TypePtr, TypePtr> TypeMap;
	typedef utils::map::PointerMap<ExpressionPtr, ExpressionPtr> ExpressionMap;
	typedef utils::map::PointerMap<StatementPtr, StatementPtr> StatementMap;
	typedef utils::map::PointerMap<VariablePtr, VariablePtr> VariableMap;
	typedef utils::map::PointerMap<VariablePtr, ExpressionPtr> VarExprMap;
	typedef utils::map::PointerMap<ExpressionPtr, VariablePtr> ExprVarMap;

} // end namespace core
} // end namespace insieme

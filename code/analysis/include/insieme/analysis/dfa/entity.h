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
#include <map>
#include <tuple>

#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/domain.h"

#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/analysis/cfg.h"

namespace insieme { 
namespace analysis { 
namespace dfa {

/** 
 * An Entity represents the unit of information examined by a dataflow problem. 
 *
 * An entity could be of various form: expressions, variables, control flow blocks, etc... 
 *
 * Not always an entity represent a physical node of the IR, the concept is more abstract. 
 * For this reason each entity should describe the domain of values which can be assumed by 
 * an instance of an entity and how this entity is extracted starting from a generic CFG.
 */
template <class... T>
class Entity {

	std::string description;

public:

	Entity(const std::string& description="") : description(description) { }

	/**
	 * Return the arity of this entity 
	 */
	constexpr static size_t arity() { return sizeof...(T); }

};

/**
 * Qualifier for entities which specifies what kind of semantics should be used during value extraction from the CFG.
 *
 * The elem<T> states that elements of this entities
 * 
 */
template <class T>
struct elem { };

template <class T>
struct dom { };



template <class... T>
struct container_type_traits;

/**
 * Type traits for defining what would be the most appropriate container type to hold the instances
 * of an entity extracted by the extractor. While for most of the types, a std::set<T> is fine, some
 * types need specialized containers (line NodePtr). 
 *
 * By default compound entities generate a cartesian-product of the entities extracted individually 
 * by the singular sub-entities. 
 */
template <class T1, class T2, class T3, class...T>
struct container_type_traits<T1,T2,T3,T...> {
	typedef CartProdSet<
				typename container_type_traits<T1>::type, 
				typename container_type_traits<T2,T3,T...>::type
			> type;
};

template <class T1, class T2>
struct container_type_traits<T1,T2> {
	typedef CartProdSet<
		typename container_type_traits<T1>::type,
		typename container_type_traits<T2>::type
	> type;
};

template <class T>
struct container_type_traits< elem<T> > {
	typedef std::set<T> type;
};

template <class T>
struct container_type_traits< elem<core::Pointer<const T>> > {
	typedef std::set<core::Pointer<const T>> type;
};

template <class T>
struct container_type_traits< elem<core::Address<const T>> > {
	typedef std::set<core::Address<const T>> type;
};



template <class T>
struct container_type_traits< dom<T> > { typedef DomainSet<T> type; };




template <class... T>
struct container_type_traits< Entity<T...> > {
	typedef typename container_type_traits<T...>::type type;
};

/**
 * Extract the set of entities existing in the given CFG. 
 *
 * Every entity needs to specialize the extract method for that kind of entity,
 * compound entityes can be extracted by combining the results of the value obtained by extracting
 * the single entities 
 */
template <class... E>
typename container_type_traits<E...>::type extract(const Entity<E...>& e, const CFG& cfg) {
	return typename container_type_traits<E...>::type(extract(Entity<E>(),cfg)...);
}

/**
 * Generic extractor for IR entities 
 *
 * IR entities (NodePtrs) can be extracted via this specialization of the extract method 
 */
template <class IRE, template <class> class Cont=core::Pointer>
typename container_type_traits< elem<Cont<const IRE>> >::type 
extract(const Entity< elem<Cont<const IRE>> >& e, const CFG& cfg) {
	
	typedef typename container_type_traits< elem<Cont<const IRE>> >::type Container;

	Container entities;

	auto collector = [&entities] (const cfg::BlockPtr& block) {

			auto visitor = core::makeLambdaVisitor(
				[&entities] (const Cont<const IRE>& var) { entities.insert( var ); }, true);

			for_each(block->stmt_begin(), block->stmt_end(), [&] (const Cont<const core::Statement>& cur) {
				auto v = makeDepthFirstVisitor( visitor );
				v.visit(cur);
			});
		};

	cfg.visitDFS(collector);

	return entities;
}

template <class T> 
DomainSet<T> extract(const Entity< dom<T> >& e, const CFG& cfg) { return DomainSet<T>(); }

} } } // end insieme::analysis::dfa

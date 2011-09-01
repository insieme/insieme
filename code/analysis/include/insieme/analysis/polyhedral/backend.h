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

#include "insieme/utils/printable.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace poly {

/**************************************************************************************************
 * Generic implementation of a the concept of a set which is natively supported by polyhedral
 * libraries. The class presents a set of operations which are possible on sets (i.e. intersect,
 * union, difference, etc...)
 *************************************************************************************************/
template <class Ctx>
struct Set : public utils::Printable {
	
	typedef Ctx ctx_type;

	std::ostream& printTo(std::ostream& out) const = 0; 

	bool isEmpty() const = 0;

	~Set() { }
private:
	Set();
};

template <class Ctx> 
struct Map : public utils::Printable {

	typedef Ctx ctx_type;

	std::ostream& printTo(std::ostream& out) const = 0; 

	bool isEmpty() const = 0;

	~Map() { }

private:
	Map();
};

template <typename Ctx>
struct SetPtr: public std::shared_ptr<Set<Ctx>> {

	SetPtr( const SetPtr<Ctx>& other ) : std::shared_ptr<Set<Ctx>>( other ) { }

	template <typename ...Args>
	SetPtr( Ctx& ctx, const Args&... args ) : 
		std::shared_ptr<Set<Ctx>>( std::make_shared<Set<Ctx>>(ctx, args...) ) { }

};

template <typename Ctx>
struct MapPtr: public std::shared_ptr<Map<Ctx>> {

	MapPtr( const MapPtr<Ctx>& other ) : std::shared_ptr<Map<Ctx>>( other ) { }

	template <typename ...Args>
	MapPtr( Ctx& ctx, const Args&... args ) : 
		std::shared_ptr<Map<Ctx>>( std::make_shared<Map<Ctx>>(ctx, args...) ) { }

};

template <typename Ctx>
SetPtr<Ctx> set_union(Ctx& ctx, const Set<Ctx>& lhs, const Set<Ctx>& rhs);

template <typename Ctx>
SetPtr<Ctx> set_intersect(Ctx& ctx, const Set<Ctx>& lhs, const Set<Ctx>& rhs);

template <typename Ctx>
MapPtr<Ctx> map_union(Ctx& ctx, const Map<Ctx>& lhs, const Map<Ctx>& rhs);

template <typename Ctx>
MapPtr<Ctx> map_intersect(Ctx& ctx, const Map<Ctx>& lhs, const Map<Ctx>& rhs);

/*
 * Intersect a map with a domain 
 */
template <typename Ctx>
MapPtr<Ctx> map_intersect_domain(Ctx& ctx, const Map<Ctx>& lhs, const Set<Ctx>& dom);

//===== Conversion Utilities ======================================================================

enum Backend { ISL };

/**
 * Defines type traits which are used to determine the type of the context for implementing backends 
 */
template <Backend B>
struct BackendTraits;

template <Backend B>
std::shared_ptr<typename BackendTraits<B>::ctx_type>
createContext() { return std::make_shared<typename BackendTraits<B>::ctx_type>(); }

template <Backend B>
SetPtr<typename BackendTraits<B>::ctx_type>
makeSet( typename BackendTraits<B>::ctx_type& ctx, 
		 const IterationDomain& domain,
		 const std::string& tuple_name = std::string())
{
	return SetPtr<typename BackendTraits<B>::ctx_type>(ctx, domain, tuple_name);
}

template <Backend B>
MapPtr<typename BackendTraits<B>::ctx_type>
makeMap( typename BackendTraits<B>::ctx_type& ctx,  
		 const AffineSystem& affSys,
		 const std::string& in_tuple_name = std::string(),
		 const std::string& out_tuple_name = std::string())
{
	return MapPtr<typename BackendTraits<B>::ctx_type>(ctx, affSys, in_tuple_name, out_tuple_name);
}

template <Backend B>
MapPtr<typename BackendTraits<B>::ctx_type>
makeEmptyMap( typename BackendTraits<B>::ctx_type& ctx, const IterationVector& iterVec)
{
	return MapPtr<typename BackendTraits<B>::ctx_type>(ctx, poly::AffineSystem(iterVec));
}

//===== Dependency analysis =======================================================================

template <typename Ctx>
struct DependenceInfo : public utils::Printable {
	MapPtr<Ctx> mustDep;
	MapPtr<Ctx> mayDep;
	MapPtr<Ctx> mustNoSource; // for now this two sets are not considered significant 
	MapPtr<Ctx> mayNoSource; //

	DependenceInfo( const MapPtr<Ctx>& mustDep, 
					const MapPtr<Ctx>& mayDep, 
					const MapPtr<Ctx>& mustNoSource, 
					const MapPtr<Ctx>& mayNoSource ): 
		mustDep(mustDep), mayDep(mayDep), mustNoSource(mustNoSource), mayNoSource(mayNoSource) { }

	bool isEmpty() const {
		return mustDep->isEmpty() && mayDep->isEmpty();
	}

	std::ostream& printTo(std::ostream& out) const;
};

template <class Ctx>
DependenceInfo<Ctx> buildDependencies( 
		Ctx&				ctx,
		const Set<Ctx>& 	domain, 
		const Map<Ctx>& 	schedule, 
		const Map<Ctx>& 	sinks, 
		const Map<Ctx>& 	must_sources, 
		const Map<Ctx>& 	may_sourcs
);
} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

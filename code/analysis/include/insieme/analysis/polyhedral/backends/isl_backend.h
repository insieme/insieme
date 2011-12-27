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

#include "insieme/core/ir_address.h"
#include "insieme/analysis/polyhedral/backend.h"

#include <string>

#include "isl/ctx.h"
#include "isl/space.h"
#include "isl/set.h"
#include "isl/union_set.h"
#include "isl/map.h"

#include <boost/utility.hpp>

#define POLY_BACKEND ISL

namespace insieme {
namespace analysis {
namespace poly {

/**************************************************************************************************
 * IslCtx
 *
 * The IslCtx contains the isl_ctx object which is created to store the polyhedral set/maps. 
 * The context object has to be unique and in order to avoid eventual accidental copy or
 * deallocation of the main ISL context, we mark the class as noncopyable and the constructor also
 * marked as explicit. 
 *************************************************************************************************/
class IslCtx : public boost::noncopyable {
	isl_ctx* ctx;
	
public:
	/** 
	 * TupleMap stores the mapping IR Address <-> String which is utilizied during the conversion of
	 * IR to polyhedral sets and relationships. This is usefull in order to reconstruct IR code from
	 * the result of polyhedral analsyis and transformations
	 */
	typedef std::map<std::string, core::NodePtr> TupleMap;

	// Build an ISL context and allocate the underlying isl_ctx object
	explicit IslCtx() : ctx( isl_ctx_alloc() ) { }

	isl_ctx* getRawContext() { return ctx; }

	TupleMap::iterator insertTuple( const TupleName& mapping ) { 
		auto&& fit = tupleMap.find( mapping.second );
		// If the mapping is already in the map, we make sure that we don't rename a mapping with a
		// new one invalidating the one already in the map
		if ( fit != tupleMap.end() ) {
			assert( mapping.second == fit->first );
			return fit;
		}
		return tupleMap.insert( std::make_pair(mapping.second, mapping.first) ).first;
	}

	const core::NodePtr& get(const std::string& name) const {
		auto&& fit = tupleMap.find(name);
		assert( fit != tupleMap.end() && "Name not in found" );
		return fit->second;
	}

	TupleMap& getTupleMap() { return tupleMap; }

	// because we do not allows copy of this class, we can safely remove the context once this
	// IslCtx goes out of scope 
	~IslCtx() { isl_ctx_free(ctx); }

private:
	TupleMap tupleMap;
};

/** Define the type traits for ISL based data structures */
template <>
struct BackendTraits<ISL> {
	typedef IslCtx ctx_type;
};

/**************************************************************************************************
 * Set<IslCtx>: is a wrapper to isl_sets, this class allows to easily convert a set of
 * constraints, represented by a constraint combiner to isl representation. Output of the isl
 * library will be represented with this same abstraction which allows for isl sets to be converted
 * back into Constraints as defined in the poly namepsace
 *************************************************************************************************/
template <>
class Set<IslCtx> : public boost::noncopyable {
	IslCtx& 		ctx;
	isl_space* 		space;
	isl_union_set* 	set;
	
public:
	Set (IslCtx& ctx, const IterationDomain& domain,const TupleName& tuple = TupleName());

	Set(IslCtx& ctx, isl_union_set* raw_set) : 
		ctx(ctx), space( isl_space_copy(isl_union_set_get_space(raw_set))), set(raw_set) { }

	std::ostream& printTo(std::ostream& out) const;

	bool isEmpty() const;

	void simplify();

	inline isl_union_set* getAsIslSet() const { return set; }

	core::ExpressionPtr getCard() const;

	~Set() { 
		isl_space_free(space);
		isl_union_set_free(set);
	}
};

/**************************************************************************************************
 * Map<IslCtx>: is the abstraction used to represent relations (or maps) in the ISL library. 
 *************************************************************************************************/
template <>
class Map<IslCtx> : public boost::noncopyable {
	IslCtx&			ctx;
	isl_space* 		space;
	isl_union_map* 	map;

public:
	Map(IslCtx& ctx, 
		const AffineSystem& affSys, 
		const TupleName& in_tuple = TupleName(), 
		const TupleName& out_tuple = TupleName());
	
	Map( IslCtx& ctx, isl_union_map* rawMap ) : 
		ctx(ctx), space( isl_space_copy(isl_union_map_get_space(rawMap)) ), map(rawMap) { }

	std::ostream& printTo(std::ostream& out) const;

	inline isl_union_map* getAsIslMap() const { return map; }

	void simplify();

	SetPtr<IslCtx> deltas() const;

	MapPtr<IslCtx> deltas_map() const;
	
	bool isEmpty() const;

	~Map() { 
		isl_space_free(space);
		isl_union_map_free(map);
	}
};

template <> 
SetPtr<IslCtx> set_union(IslCtx& ctx, const Set<IslCtx>& lhs, const Set<IslCtx>& rhs);

template <>
SetPtr<IslCtx> set_intersect(IslCtx& ctx, const Set<IslCtx>& lhs, const Set<IslCtx>& rhs);

template <> 
MapPtr<IslCtx> map_union(IslCtx& ctx, const Map<IslCtx>& lhs, const Map<IslCtx>& rhs);

template <> 
MapPtr<IslCtx> map_intersect(IslCtx& ctx, const Map<IslCtx>& lhs, const Map<IslCtx>& rhs);

template <> 
MapPtr<IslCtx> map_intersect_domain(IslCtx& ctx, const Map<IslCtx>& lhs, const Set<IslCtx>& dom);

/**************************************************************************************************
 * DEPENDENCE ANALYSIS
 *************************************************************************************************/
template <>
DependenceInfo<IslCtx> buildDependencies( 
		IslCtx&				ctx,
		const Set<IslCtx>& 	domain, 
		const Map<IslCtx>& 	schedule, 
		const Map<IslCtx>& 	sinks, 
		const Map<IslCtx>& 	must_sources,
		const Map<IslCtx>& 	may_sourcs
);

template <>
std::ostream& DependenceInfo<IslCtx>::printTo(std::ostream& out) const;

/**************************************************************************************************
 * CODE GENERATION
 *************************************************************************************************/
template <>
core::NodePtr toIR(
		core::NodeManager& mgr,
		const IterationVector& iterVec,
		IslCtx&	ctx,
		const Set<IslCtx>& 	domain, 
		const Map<IslCtx>& 	schedule
	);

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 

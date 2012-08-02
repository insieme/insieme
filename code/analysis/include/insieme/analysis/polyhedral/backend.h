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
#include "insieme/utils/constraint.h"

struct cloogoptions;

namespace insieme { namespace analysis { namespace polyhedral {

class Stmt;
typedef std::shared_ptr<Stmt> StmtPtr;

class IterationVector;
class IterationDomain;
class AffineSystem;

typedef boost::variant<
	core::NodePtr, 
	insieme::analysis::polyhedral::StmtPtr
> InfoObj;

typedef std::pair<InfoObj, std::string> TupleName;

// Defines the list of a available backends 
enum Backend { 
	NB,  // No Backend 
	ISL  // ISL Backend 
};

const Backend BACKEND = ISL;

/**
 * Yype traits which are used to determine the type of the concrete class thish implements the concept 
 * of context, set and map in a particular backend  
 */
template <Backend B>
struct BackendTraits;

template <>
struct BackendTraits<NB> {
	typedef void ctx_type;
	typedef void set_type;
	typedef void map_type;
	typedef void pw_type;
};

#define CTX_TYPE(B) typename BackendTraits<B>::ctx_type
#define SET_TYPE(B) typename BackendTraits<B>::set_type
#define MAP_TYPE(B) typename BackendTraits<B>::map_type
#define PIECEWISE_TYPE(B) typename BackendTraits<B>::pw_type

/**************************************************************************************************
 * Generic implementation of a the concept of a set which is natively supported by polyhedral
 * libraries. The class presents a set of operations which are possible on sets (i.e. intersect,
 * union, difference, etc...)
 *************************************************************************************************/
template <Backend B = BACKEND>
struct CtxPtr: public std::shared_ptr<CTX_TYPE(B)> {
	CtxPtr(): std::shared_ptr<CTX_TYPE(B)>( std::make_shared<CTX_TYPE(B)>() ) { }

	CtxPtr(const CtxPtr<B>& other) : std::shared_ptr<CTX_TYPE(B)>( other ) { }
};

template <Backend B = BACKEND>
struct SetPtr: public std::shared_ptr<SET_TYPE(B)> {

	SetPtr( const SetPtr<B>& other ) : std::shared_ptr<SET_TYPE(B)>( other ) { }

	template <typename ...Args>
	SetPtr( CTX_TYPE(B)& ctx, const Args&... args ) : 
		std::shared_ptr<SET_TYPE(B)>( std::make_shared<SET_TYPE(B)>(ctx, args...) ) { }

	SetPtr<B>& operator+=(const SetPtr<B>& other);
	SetPtr<B>& operator-=(const SetPtr<B>& other);
	SetPtr<B>& operator*=(const SetPtr<B>& other);

};

template <Backend B = BACKEND>
struct MapPtr: public std::shared_ptr<MAP_TYPE(B)> {

	MapPtr( const MapPtr<B>& other ) : std::shared_ptr<MAP_TYPE(B)>( other ) { }

	template <typename ...Args>
	MapPtr( CTX_TYPE(B)& ctx, const Args&... args ) : 
		std::shared_ptr<MAP_TYPE(B)>( std::make_shared<MAP_TYPE(B)>(ctx, args...) ) { }

	MapPtr<B>& operator+=(const MapPtr<B>& other);
	
	MapPtr<B>& operator*=(const MapPtr<B>& other);
	
	MapPtr<B>& operator*=(const SetPtr<B>& other);

	MapPtr<B> operator()(const polyhedral::MapPtr<B> other) { return (**this)(*other); }

};

template <Backend B = BACKEND>
struct PiecewisePtr: public std::shared_ptr<PIECEWISE_TYPE(B)> {

	PiecewisePtr( const PiecewisePtr<B>& other ) : std::shared_ptr<PIECEWISE_TYPE(B)>( other ) { }

	template <typename ...Args>
	PiecewisePtr( CTX_TYPE(B)& ctx, const Args&... args ) : 
		std::shared_ptr<PIECEWISE_TYPE(B)>( std::make_shared<PIECEWISE_TYPE(B)>(ctx, args...) ) { }

	PiecewisePtr<B>& operator+=(const PiecewisePtr<B>& other);

	PiecewisePtr<B>& operator*=(const SetPtr<B>& other);

};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Union operation among sets and maps is implemented using the + operator
//

/**
 * Set Union
 */
template <Backend B>
SetPtr<B> operator+(SET_TYPE(B)& lhs, const SET_TYPE(B)& rhs);

template <Backend B>
inline SetPtr<B> operator+(const SetPtr<B>& lhs, const SetPtr<B>& rhs) { return *lhs + *rhs; }

template <Backend B>
inline SetPtr<B>& SetPtr<B>::operator+=(const SetPtr<B>& other) { 
	*this = *this + other;
	return *this;
}

/**
 * Map Union
 */
template <Backend B>
MapPtr<B> operator+(MAP_TYPE(B)& lhs, const MAP_TYPE(B)& rhs);

template <Backend B>
inline MapPtr<B> operator+(const MapPtr<B>& lhs, const MapPtr<B>& rhs) {
	return (*lhs) + (*rhs);
}

template <Backend B>
MapPtr<B>& MapPtr<B>::operator+=(const MapPtr<B>& other) {
	*this = *this + other;
	return *this;
}

/**
 * Piecewise Union
 */
template <Backend B>
PiecewisePtr<B> operator+(PIECEWISE_TYPE(B)& lhs, const PIECEWISE_TYPE(B)& rhs);

template <Backend B>
inline PiecewisePtr<B> operator+(const PiecewisePtr<B>& lhs, const PiecewisePtr<B>& rhs) {
	return (*lhs) + (*rhs);
}

template <Backend B>
PiecewisePtr<B>& PiecewisePtr<B>::operator+=(const PiecewisePtr<B>& other) {
	*this = *this + other;
	return *this;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Intersect operation among sets and maps is implemented using the * operator 
//

/**
 * Set Intersection 
 */
template <Backend B>
SetPtr<B> operator*(SET_TYPE(B)& lhs, SET_TYPE(B)& rhs);

template <Backend B>
inline SetPtr<B> operator*(const SetPtr<B>& lhs, const SetPtr<B>& rhs) { return (*lhs) * (*rhs); }

template <Backend B>
PiecewisePtr<B> operator*(PIECEWISE_TYPE(B)& lhs, SET_TYPE(B)& rhs);

template <Backend B>
inline PiecewisePtr<B> operator*(const PiecewisePtr<B>& lhs, const SetPtr<B>& rhs) { return (*lhs) * (*rhs); }

template <Backend B>
SetPtr<B>& SetPtr<B>::operator*=(const SetPtr<B>& other) {
	*this = *this * other;
	return *this;
}

/**
 * Map Intersection
 */
template <Backend B>
MapPtr<B> operator*(MAP_TYPE(B)& lhs, const MAP_TYPE(B)& rhs);

template <Backend B>
inline MapPtr<B> operator*(const MapPtr<B>& lhs, const MapPtr<B>& rhs) { return (*lhs) * (*rhs); }


template <Backend B>
MapPtr<B> operator*(MAP_TYPE(B)& lhs, const SET_TYPE(B)& dom);

template <Backend B>
inline MapPtr<B> operator*(const MapPtr<B>& lhs, const SetPtr<B>& dom) { return (*lhs) * (*dom); }

template <Backend B>
MapPtr<B>& MapPtr<B>::operator*=(const MapPtr<B>& other) {
	*this = *this * other;
	return *this;
}

template <Backend B>
MapPtr<B>& MapPtr<B>::operator*=(const SetPtr<B>& other) {
	*this = *this * other;
	return *this;
}

template <Backend B>
PiecewisePtr<B>& PiecewisePtr<B>::operator*=(const SetPtr<B>& other) {
	*this = *this * other;
	return *this;
}


/**
 * Set Difference 
 */
template <Backend B>
SetPtr<B> operator-(SET_TYPE(B)& lhs, const SET_TYPE(B)& rhs);

template <Backend B>
inline SetPtr<B> operator-(const SetPtr<B>& lhs, const SetPtr<B>& rhs) { return *lhs - *rhs; }

template <Backend B>
inline SetPtr<B>& SetPtr<B>::operator-=(const SetPtr<B>& other) { 
	*this = *this - other;
	return *this;
}

// Get the range of a map
template <Backend B>
SetPtr<B> range(MAP_TYPE(B)& lhs);

template <Backend B>
inline SetPtr<B> range(const MapPtr<B>& map) { return range(*map); }

template<Backend B>
MapPtr<B> range_map(MAP_TYPE(B)& map);

template <Backend B>
inline MapPtr<B> range_map(const MapPtr<B>& map) { return range_map(*map); }

// Get the domain of a map
template <Backend B>
SetPtr<B> domain(MAP_TYPE(B)& lhs);

template <Backend B>
inline SetPtr<B> domain(const MapPtr<B>& map) { return domain(*map); }

// Map reverse 
template <Backend B>
MapPtr<B> reverse(MAP_TYPE(B)& map);

template <Backend B>
inline MapPtr<B> reverse(const MapPtr<B>& map) { return reverse(*map); }

// Get the domain map
template <Backend B>
MapPtr<B> domain_map(MAP_TYPE(B)& lhs);

template <Backend B>
inline MapPtr<B> domain_map(const MapPtr<B>& map) { return domain_map(*map); }

//===== Conversion Utilities ======================================================================

// Create a shared pointer to a context
template <Backend B = BACKEND>
inline CtxPtr<B> makeCtx() { return CtxPtr<B>(); }

// Creates a set from an IterationDomain
template <Backend B = BACKEND>
inline SetPtr<B> makeSet(CtxPtr<B>& 			ctx, 
						 const IterationDomain& domain,
						 const TupleName& 		tuple = TupleName())
{
	return SetPtr<B>(*ctx, domain, tuple);
}

template <Backend B = BACKEND>
MapPtr<B> makeMap(CtxPtr<B>& 		 ctx, 
				 const AffineSystem& affSys,
				 const TupleName& 	 in_tuple = TupleName(),
				 const TupleName& 	 out_tuple = TupleName()) 
{
	return MapPtr<B>(*ctx, affSys, in_tuple, out_tuple);
}

template <Backend B = BACKEND>
MapPtr<B> makeEmptyMap(CtxPtr<B>& ctx, const IterationVector& iterVec = polyhedral::IterationVector()) {
	return MapPtr<B>(*ctx, polyhedral::AffineSystem(iterVec));
}

template <Backend B = BACKEND>
PiecewisePtr<B> makeZeroPiecewise(CtxPtr<B>& ctx) { return PiecewisePtr<B>(*ctx); }

//template <Backend B = BACKEND>
//PiecewisePtr<B> makePiecewise(CtxPtr<B>& ctx, const utils::Piecewise<polyhedtal::AffineFunction>& pw) { 
//	return PiecewisePtr<B>(*ctx); 
//}

//===== Dependency analysis =======================================================================

template <Backend B = BACKEND>
struct DependenceInfo : public utils::Printable {
	MapPtr<B> mustDep;
	MapPtr<B> mayDep;
	MapPtr<B> mustNoSource; // for now this two sets are not considered significant 
	MapPtr<B> mayNoSource; //

	DependenceInfo(const MapPtr<B>& mustDep, 
				   const MapPtr<B>& mayDep, 
				   const MapPtr<B>& mustNoSource, 
				   const MapPtr<B>& mayNoSource 
		) : mustDep(mustDep), 
		    mayDep(mayDep), 
		    mustNoSource(mustNoSource), 
		    mayNoSource(mayNoSource) { }

	inline bool isEmpty() const { return mustDep->isEmpty() && mayDep->isEmpty(); }

	std::ostream& printTo(std::ostream& out) const;
};

template <Backend B = BACKEND>
DependenceInfo<B> buildDependencies ( 
		CTX_TYPE(B)&	ctx,
		SET_TYPE(B)& 	domain, 
		MAP_TYPE(B)& 	schedule, 
		MAP_TYPE(B)& 	sinks, 
		MAP_TYPE(B)& 	must_sources, 
		MAP_TYPE(B)& 	may_sources
);

template <Backend B = BACKEND>
inline DependenceInfo<B> buildDependencies ( 
		CtxPtr<B>&		 ctx,
		const SetPtr<B>& domain, 
		const MapPtr<B>& schedule, 
		const MapPtr<B>& sinks, 
		const MapPtr<B>& must_sources, 
		const MapPtr<B>& may_sources ) 
{
	return buildDependencies(*ctx, *domain, *schedule, *sinks, *must_sources, *may_sources);
}

//====== Code Generation ==========================================================================


struct CloogOpts {

	int optCtrlUntil;
	int optCtrlFrom;
	bool computeConvexHulls;
	int unrollFromLevel;
	bool spreadComplexEqualities;
	int spreadEqualitiesFrom;
	bool simplifyLoops;
	bool quiet;
	bool strides;

	CloogOpts() : 
		optCtrlUntil(-1), 				// optimize until the innermost
		optCtrlFrom(1),  				// optimize from the outermost
		computeConvexHulls(false), 		// do not compute convex hulls
		unrollFromLevel(-1), 			// do not perform unrolling 
		spreadComplexEqualities(true), 
		spreadEqualitiesFrom(1), 		// start spread equalities from the first iterators
		simplifyLoops(true), 			// simplify loops running once
		quiet(true), 					// suppress Cloog messages
		strides(true) 					// use non-unitary strides
	{ }

	void set(struct cloogoptions& opts) const;
};

template <Backend B = BACKEND> 
core::NodePtr toIR (core::NodeManager& 		mgr, 
					const IterationVector& 	iterVec, 
					CTX_TYPE(B)& 			ctx, 
					SET_TYPE(B)& 			domain, 
					MAP_TYPE(B)& 			schedule,
					const CloogOpts& opts = CloogOpts());

template <Backend B = BACKEND>
inline core::NodePtr toIR (core::NodeManager& 		mgr, 
							const IterationVector& 	iterVec, 
							CtxPtr<B>& 				ctx, 
							const SetPtr<B>& 		domain, 
							const MapPtr<B>& 		schedule,
							const CloogOpts& opts = CloogOpts()) 
{
	return toIR(mgr, iterVec, *ctx, *domain, *schedule, opts);
}

} } } // end insieme::analysis::polyhedral namespace


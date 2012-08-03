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
#include "insieme/analysis/polyhedral/constraint.h"
#include <string>

#include "isl/ctx.h"
#include "isl/space.h"
#include "isl/set.h"
#include "isl/map.h"
#include "isl/polynomial.h"

#include <boost/utility.hpp>

#define POLY_BACKEND ISL

namespace insieme { namespace core { namespace arithmetic {

class Formula;
class Div;

} } // end core::arithmetic namespace

namespace analysis { namespace polyhedral {

class IslCtx;
class IslSet;
class IslMap;
class IslPiecewise;

/** Define the type traits for ISL based data structures */
template <>
struct BackendTraits<ISL> {
	typedef IslCtx ctx_type;
	typedef IslSet set_type;
	typedef IslMap map_type;
	typedef IslPiecewise pw_type;
};


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
	typedef std::map<std::string, InfoObj> TupleMap;

	// Build an ISL context and allocate the underlying isl_ctx object
	explicit IslCtx() : ctx( isl_ctx_alloc() ) { }

	MapPtr<ISL> range_map(const IslMap& map);
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

	template <class T>
	inline const T& getAs(const std::string& name) const {
		auto&& fit = tupleMap.find(name);
		assert( fit != tupleMap.end() && "Name not found" );
		return boost::get<T>(fit->second);
	}

	inline TupleMap& getTupleMap() { return tupleMap; }

	// because we do not allows copy of this class, we can safely remove the context once this
	// IslCtx goes out of scope 
	~IslCtx() { isl_ctx_free(ctx); }

private:
	TupleMap tupleMap;
};

/**************************************************************************************************
 * IslObj is a base class for every wrapper to ISl objects. This class contains the IslCtx (which 
 * all Isl objects have to refer to) and the information of the space which is the iteration vector
 * on which the objects are defined.
 *************************************************************************************************/
class IslObj {

protected:
	IslCtx&  ctx;
	isl_space* 	space;

public:
	IslObj(IslCtx& ctx, isl_space* space = NULL) : ctx(ctx), space(space) { }

	IslCtx& getCtx() { return ctx; }
	const IslCtx& getCtx() const { return ctx; }

	IterationVector getIterationVector(core::NodeManager& mge) const;

	~IslObj() { 
		isl_space_free(space);
	}
};

/**************************************************************************************************
 * IslSet: is a wrapper to isl_sets, this class allows to easily convert a set of
 * constraints, represented by a constraint combiner to isl representation. Output of the isl
 * library will be represented with this same abstraction which allows for isl sets to be converted
 * back into Constraints as defined in the poly namepsace
 *************************************************************************************************/
class IslSet : public IslObj, public boost::noncopyable, public utils::Printable {
	isl_union_set* 	set;
	
public:
	IslSet (IslCtx& ctx, const IterationDomain& domain,const TupleName& tuple = TupleName());

	/** 
	 * Builds an ISL Set from and isl_union_set object. The object will take ownership of the ISL 
	 * object and it will provide for its cleanup. 
	 */
	IslSet(IslCtx& ctx, isl_union_set* raw_set) :
		IslObj(ctx, isl_union_set_get_space(raw_set)), set(raw_set) 
	{ 
		simplify();	
	}
	/** 
	 * Builds an ISL Set from a string representation using the parser provided by ISL
	 */
	IslSet(IslCtx& ctx, const std::string& set_str);

	std::ostream& printTo(std::ostream& out) const;
	/**
	 * Returns true if the underlying set is empty. The check is done transforming the set into an
	 * ISL set and checking for emptiness within the library
	 */
	bool empty() const;

	void simplify();

	// Returns a copy of the contained set 
	inline isl_union_set* getIslObj() const { return isl_union_set_copy(set); }

	bool operator==(const IslSet& other) const;

	AffineConstraintPtr toConstraint(core::NodeManager& mgr, IterationVector& iterVec) const;

	PiecewisePtr<ISL> getCard() const;

	~IslSet();
};

//==== Overloaded operators for IslSet ===========================================================
SetPtr<ISL> operator+(IslSet& lhs, const IslSet& rhs);

SetPtr<ISL> operator-(IslSet& lhs, const IslSet& rhs);

SetPtr<ISL> operator*(IslSet& lhs, const IslSet& rhs);

/**************************************************************************************************
 * IslMap: is the abstraction used to represent relations (or maps) in the ISL library. 
 *************************************************************************************************/
class IslMap : public IslObj, public boost::noncopyable, public utils::Printable {
	isl_union_map* 	map;

public:
	IslMap(IslCtx& ctx, 
		const AffineSystem& affSys, 
		const TupleName& in_tuple = TupleName(), 
		const TupleName& out_tuple = TupleName());
	
	/**
	 * Builds an ISL map from an ISL map object. The IslMap object will take ownership of the passed
	 * isl_union_map object and it will provide at deallocating it
	 */
	IslMap( IslCtx& ctx, isl_union_map* rawMap ) : 
		IslObj(ctx, isl_union_map_get_space(rawMap) ), map(rawMap) 
	{ 
		simplify();
	}

	/**
	 * Builds an ISL map from a string representation using the ISL parser
	 */
	IslMap(IslCtx& ctx, const std::string& map_str);

	// Apply operator 
	MapPtr<> operator()(const IslMap& other) const;

	std::ostream& printTo(std::ostream& out) const;

	// Returns a copy of the contained isl map object
	inline isl_union_map* getIslObj() const { return isl_union_map_copy(map); }

	void simplify();

	SetPtr<ISL> deltas() const;
	MapPtr<ISL> deltas_map() const;
	
	bool empty() const;

	PiecewisePtr<ISL> getCard() const;

	~IslMap() { 
		isl_union_map_free(map);
	}
};

//==== Overloaded operators for IslMap ============================================================
MapPtr<ISL> operator+(IslMap& lhs, const IslMap& rhs);

MapPtr<ISL> operator*(IslMap& lhs, const IslMap& rhs);

MapPtr<ISL> operator*(IslMap& lhs, const IslSet& dom);

SetPtr<ISL> range(IslMap& map);

MapPtr<ISL> range_map(IslMap& map);

SetPtr<ISL> domain(IslMap& map);

MapPtr<ISL> reverse(IslMap& map);

MapPtr<ISL> domain_map(IslMap& map);




/**************************************************************************************************
 * IslPiecewise: is the abstraction used to represent a piesewise expression ISL library. 
 *************************************************************************************************/
class IslPiecewise : public IslObj, public boost::noncopyable, public utils::Printable {
	isl_union_pw_qpolynomial* 	pw;

public:
	IslPiecewise(IslCtx& ctx);

	IslPiecewise(IslCtx& ctx, const utils::Piecewise<AffineFunction>& pw);

	IslPiecewise(IslCtx& ctx, isl_union_pw_qpolynomial* ipw) : 
		IslObj(ctx, isl_union_pw_qpolynomial_get_space(ipw)), pw(ipw) 
	{ 
		simplify();
	}

	inline isl_union_pw_qpolynomial* getIslObj() const { return isl_union_pw_qpolynomial_copy(pw); }
	
	void simplify();

	std::ostream& printTo(std::ostream& out) const;

	insieme::utils::Piecewise<insieme::core::arithmetic::Formula> toPiecewise(core::NodeManager& mgr) const;

	// Computes the upperbound of this piecewise and return its value
	//
	// The current implementation only returns a value if the result is a constant value. In any other case 
	// an assertion will be triggered 
	double upperBound() const;

	~IslPiecewise() { 
		isl_union_pw_qpolynomial_free(pw);
	}
	
};


PiecewisePtr<ISL> operator+(IslPiecewise& lhs, IslPiecewise& rhs);

PiecewisePtr<ISL> operator*(IslPiecewise& lhs, IslPiecewise& rhs);

PiecewisePtr<ISL> operator*(IslPiecewise& lhs, const IslSet& dom);



/**************************************************************************************************
 * DEPENDENCE ANALYSIS
 *************************************************************************************************/
DependenceInfo<ISL> buildDependencies(IslCtx&	ctx,
									  IslSet& 	domain, 
									  IslMap& 	schedule, 
								 	  IslMap& 	sinks, 
									  IslMap& 	must_sources,
									  IslMap&	may_sourcs);

template <>
std::ostream& DependenceInfo<ISL>::printTo(std::ostream& out) const;

/**************************************************************************************************
 * CODE GENERATION
 *************************************************************************************************/
core::NodePtr toIR(core::NodeManager& 		mgr,
				   const IterationVector& 	iterVec,
				   IslCtx&					ctx,
				   IslSet& 					domain, 
				   IslMap& 					schedule,
				   const CloogOpts&			opts);

} } } // end insieme::analysis::polyhedral namespace 

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

#include "insieme/analysis/polyhedral/backend.h"

#include "isl/ctx.h"
#include "isl/dim.h"
#include "isl/set.h"
#include "isl/map.h"

#include <boost/utility.hpp>

namespace insieme {
namespace analysis {
namespace poly {

/**************************************************************************************************
 * The IslContext contains the isl_ctx object which is created to store the polyhedral set/maps. 
 * The context object has to be unique and in order to avoid eventual accidental copy or
 * deallocation of the main ISL context, we mark the class as noncopyable and the constructor also
 * marked as explicit. 
 *************************************************************************************************/
class IslContext : public boost::noncopyable {
	isl_ctx* ctx;

public:
	// Build an ISL context and allocate the underlying isl_ctx object
	explicit IslContext() : ctx( isl_ctx_alloc() ) { }

	isl_ctx* getRawContext() { return ctx; }

	// because we do not allows copy of this class, we can safely remove the context once this
	// IslContext goes out of scope 
	~IslContext() { isl_ctx_free(ctx); }
};

template <Backend B>
struct BackendTraits {
	typedef IslContext ctx_type;
};

/**************************************************************************************************
 * IslSet: is a wrapper to isl_sets, this class allows to easily convert a set of constraints,
 * represented by a constraint combiner to isl representation. Output of the isl library will be
 * represented with this same abstraction which allows for isl sets to be converted back into
 * Constraints as defined in the poly namepsace
 *************************************************************************************************/
template <>
class Set<IslContext> : public boost::noncopyable {
	IslContext& 					ctx;
	const IterationVector& 			iterVec;
	const ConstraintCombinerPtr& 	constraint;

	isl_dim* dim;
	isl_set* set;
	
	friend class IslMap;
public:
	Set(IslContext& ctx, const IterationVector& iterVec, const ConstraintCombinerPtr& constraint);

	std::ostream& printTo(std::ostream& out) const;

	const isl_set* getAsIslSet() const { return set; }

	~Set() { 
		isl_dim_free(dim);
		isl_set_free(set);
	}
};

/**************************************************************************************************
 * IslMap: is the abstraction used to represent relations (or maps) in the ISL library. 
 *************************************************************************************************/
template <>
class Map<IslContext> {
	IslContext&				ctx;
	const AffineSystem& 	affSys;

	isl_dim* dim;
	isl_map* map;

public:
	Map(IslContext& ctx, const AffineSystem& affSys);

	std::ostream& printTo(std::ostream& out) const;

	~Map() { 
		isl_dim_free(dim);
		isl_map_free(map);
	}
};

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 

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

#include "insieme/analysis/modeling/cache.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/analysis/features/type_features.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

using namespace insieme;

namespace insieme { namespace analysis { namespace modeling {

namespace {

typedef insieme::utils::map::PointerMap<core::ExpressionPtr, size_t> ReferenceMap;

// For each reference access in this scop produce a list which contains the list of references being accessed and for
// each of those the index variable used
ReferenceMap extractReferenceInfo(const poly::Scop& scop) {

	ReferenceMap refMap;
	// go thorugh the statements of this scop and collect all the variables being accessed in the scop

	for_each(scop, [&] (const poly::StmtPtr& stmt) { 
		for_each(stmt->access_begin(), stmt->access_end(), [&](const poly::AccessInfoPtr& cur) { 
			if (cur->getRefType() != Ref::ARRAY) return;
			// Add this reference to the list of references
			refMap.insert( std::make_pair(
					cur->getExpr().getAddressedNode(), 
					cur->getAccess().size()) 
				);					
		});
	});

	return refMap;
}

poly::MapPtr<> buildCacheLineModel(poly::CtxPtr<>& ctx, size_t block_size) {
	// ADDR[i] -> BLOCK[j] : exists a = [i/block_size]: j = a
	return poly::MapPtr<>(*ctx, "{MEM[i] -> BLK[j] : j = [i/" + utils::numeric_cast<std::string>(block_size) + "]}");
}

poly::MapPtr<> buildCacheModel(poly::CtxPtr<>& ctx, size_t block_size, size_t cache_size) {

	// ADDR[i] -> BLOCK[j] : exists a = [i/block_size]: j = a
	poly::MapPtr<> addrToBlock = buildCacheLineModel(ctx, block_size);
	
	// num_blocks  = cache_size / block_size
	size_t num_blocks = cache_size / block_size;
	
	std::string num_blocks_str = utils::numeric_cast<std::string>(num_blocks);
	// BLOCK[i] -> SET[j] : exists a = [i/num_blocks] j = i - a*num_blocks and j > 0 and j < num_blocks
	poly::MapPtr<> blockToAddr(*ctx, "{BLK[i] -> SET[j] : exists a = [i/" + num_blocks_str + "] : j = i-a*" + 
								     num_blocks_str + " and j < " + num_blocks_str + " and j >= 0}");
	// Apply a map to the other 
	return addrToBlock( blockToAddr );
}

typedef std::pair<poly::MapPtr<>, poly::MapPtr<>> SchedAccessPair;


SchedAccessPair buildAccessMap(poly::CtxPtr<>& ctx, const poly::Scop& scop, const core::ExpressionPtr& reference) {

	poly::MapPtr<> schedMap = poly::makeEmptyMap(ctx);
	poly::MapPtr<> accessMap = poly::makeEmptyMap(ctx);

	size_t accID = 0;

	core::TypePtr elemType;
	core::TypePtr subType = core::analysis::getReferencedType(core::static_pointer_cast<const core::RefType>(reference->getType()));

	if (subType->getNodeType() == core::NT_ArrayType || subType->getNodeType() == core::NT_VectorType) {
		elemType = core::static_pointer_cast<const core::SingleElementType>(subType)->getElementType();
	} else if (subType->getNodeType() == core::NT_StructType) {
		// This is an access to a struct 
		elemType = core::static_pointer_cast<const core::StructType>(subType);
	} else {
		elemType = subType;
	}

	assert( elemType );
	unsigned type_size = analysis::features::getSizeInBytes(elemType);
	//LOG(DEBUG) << "SIZE FOR TYPE: " << type_size;

	for_each(scop, [&] (const poly::StmtPtr& stmt) {
	
		// Access Functions 
		std::for_each(stmt->access_begin(), stmt->access_end(), [&](const poly::AccessInfoPtr& cur) {
			// make a copy as we are going to modify this to add typing informations
			poly::AffineSystem accessInfo = cur->getAccess();
			
			if (accessInfo.empty() || (*cur->getExpr().getAddressedNode() != *reference))  return;

			assert(accessInfo.size() == 1 && "Multidimensional accesses not yet supported");

			// an access of type i+j should be converted into size * (i+j), therefore every coeff should be multiplied
			// by size
			const poly::IterationVector& iv = scop.getIterationVector();
			for(size_t cidx = 0; cidx < iv.size(); ++cidx) {
				accessInfo[0].setCoeff(iv[cidx], accessInfo[0].getCoeff(iv[cidx]) * type_size);
			}

			poly::TupleName tn(stmt->getAddr(), "acc" + utils::numeric_cast<std::string>(accID));

			poly::AffineSystem sched = stmt->getSchedule();
			poly::AffineFunction func(scop.getIterationVector());
			func.setCoeff( poly::Constant(), accID++ );
			sched.append( func );

			// compute the domain for this stmt
			poly::SetPtr<> stmtDom = makeSet(ctx, stmt->getDomain(), tn) * makeSet(ctx, cur->getDomain(), tn);

			schedMap += poly::makeMap(ctx, sched, tn) * stmtDom;
			accessMap += poly::makeMap(ctx, accessInfo, tn, poly::TupleName(cur->getExpr(), "MEM")) * stmtDom;
		});

	});
	
	return std::make_pair(schedMap, accessMap);
}

} // end anonymous namespace 


void mapCache(const core::NodePtr& root, size_t block_size, size_t cache_size) {

	boost::optional<poly::Scop> optScop = scop::ScopRegion::toScop(root);	
	if (!optScop) { return; }

	// this is a SCoP, we can perform analysis 
	const poly::Scop& scop = *optScop;

	ReferenceMap&& refMap = extractReferenceInfo(scop);

	// Make sure that all the references have 1 dimensional access. 
	// 
	// In order to support N-dimensional access we need to statically know the size of the N-1 dimensions in order to
	// perform a linearization of the access. FIXME
	if (any(refMap, [] (const ReferenceMap::value_type& cur) { return cur.second != 1; }) ) {
		throw CacheModelingError("Impossible to compute cache misses for given input code."
							"REASON: presence of multi-dimensional accesses");
	}

	auto&& ctx = poly::makeCtx();

	// Because we have no mean to determine the allocation address of each reference at compile time, we extract the
	// cache misses for each of the references in the code and then aggregate them together. 
	poly::MapPtr<> cache = buildCacheModel(ctx, block_size, cache_size);

	poly::PiecewisePtr<> pw = poly::makeZeroPiecewise(ctx);

	for_each(refMap, [&](const ReferenceMap::value_type& cur) {
		try {
			//LOG(DEBUG) << "Reference: " << *cur.first;

			SchedAccessPair&& ret = buildAccessMap(ctx, scop, cur.first);
			// accesses = map_intersect_domain(ctx, ret.first, scop.getDomain(ctx));
			// LOG(DEBUG) << "SCHED " << *ret.first;
			// LOG(DEBUG) << "REF " << *cur.first << ": " << *ret.second;
			// poly::MapPtr<> map = (*map_reverse(ctx, schedule))(*(*accesses)(*cache));
			
			poly::MapPtr<> map2(*ctx, "{[MEM[i] -> SET[j]] -> [i0,j] : i0 = [i/" + 
				utils::numeric_cast<std::string>(block_size) + "]}");
			
			poly::MapPtr<> map = poly::reverse(ret.second)( ret.second(cache) );
			map = poly::reverse(domain_map(map))( map2 );

			poly::PiecewisePtr<> misses = range(map)->getCard();
			pw += misses;
			
		} catch (features::UndefinedSize&& ex) {
			LOG(WARNING) << "Cache misses for reference '" << *cur.first 
				         << "' cannot be determined because of unknwon reference size";
		}
	});

	poly::IterationVector iv = pw->getIterationVector(root->getNodeManager());
	assert(iv.getIteratorNum() == 0);
	if (iv.getParameterNum() > 0) {
		// we have some parameters, let's set a default value = 100
		for_each(iv.param_begin(), iv.param_end(), [&](const poly::Parameter& cur) {
				poly::AffineFunction af(iv);
				af.setCoeff(cur, 1);
				af.setCoeff(poly::Constant(), -100);
				poly::IterationDomain id(
						poly::AffineConstraint( af, utils::ConstraintType::EQ ) 
					);
				pw = pw * poly::makeSet(ctx, id);
			});
	}

	return arithmetic::toFormula(pw->toFormula(mgr));

}

size_t getReuseDistance(const core::NodePtr& root, size_t block_size, size_t cache_size) {

	boost::optional<poly::Scop> optScop = scop::ScopRegion::toScop(root);	
	if (!optScop) { return 0; }

	// this is a SCoP, we can perform analysis 
	const poly::Scop& scop = *optScop;

	ReferenceMap&& refMap = extractReferenceInfo(scop);

	// Make sure that all the references have 1 dimensional access. 
	// 
	// In order to support N-dimensional access we need to statically know the size of the N-1 dimensions in order to
	// linearize the access. FIXME
	if (any(refMap, [] (const ReferenceMap::value_type& cur) { return cur.second != 1; }) ) {
		throw CacheModelingError("Impossible to compute cache misses for given input code."
							"REASON: presence of multi-dimensional accesses");
	}
	
	auto&& ctx = poly::makeCtx();

	// Because we have no mean to determine the allocation address of each reference at compile time, we extract the
	// cache misses for each of the references in the code and then aggregate them together. 
	poly::MapPtr<> C = buildCacheLineModel(ctx, block_size);
	double avg_reuse_max = 0.0;
	size_t num_refs = 0;

	for_each(refMap, [&](const ReferenceMap::value_type& cur) {
		try {
			// LOG(DEBUG) << "Reference: " << *cur.first;
			SchedAccessPair&& ret = buildAccessMap(ctx, scop, cur.first);
			
			// Schedule Map
			poly::MapPtr<> S = ret.first;
			//std::cout << "S:=" << *S << ';' << std::endl;
			//
			// Access Map
			poly::MapPtr<> A = ret.second( C );
			// std::cout << "A:=" << *A << ';' << std::endl;

			// TIME := ran S
			poly::SetPtr<> TIME = range(S);

			// LT := TIME << TIME
			poly::MapPtr<> LT 	= poly::MapPtr<>(*ctx, isl_union_set_lex_lt_union_set(TIME->getIslObj(), TIME->getIslObj()));
			// std::cout << "LT:=" << *LT << ";" << std::endl;

			// LE = TIME <<= TIME
			poly::MapPtr<> LE   = poly::MapPtr<>(*ctx, isl_union_set_lex_le_union_set(TIME->getIslObj(), TIME->getIslObj()));

			// T := ((S^-1) . A . (A^-1) . S) * LT
			poly::MapPtr<> T = (poly::reverse(S)(A)(poly::reverse(A))(S)) * LT;
			// LOG(DEBUG) << "T:=" << *T << ';' << std::endl;

			// M := lexmin T
			poly::MapPtr<> M = poly::MapPtr<>(*ctx, isl_union_map_lexmin( T->getIslObj() )); 
			// LOG(DEBUG) << "M:=" << *M << ';' << std::endl;

			// NEXT := S . M . (S^-1); # map to next access to same cache line
			poly::MapPtr<> NEXT = S ( M(poly::reverse(S)) );

			// AFTER_PREV := (NEXT^-1) . (S . LE . (S^-1));
			poly::MapPtr<> AFTER_PREV = poly::reverse(NEXT) ( S (LE) (poly::reverse(S)) );

			// BEFORE := S . (LE^-1) . (S^-1);
			poly::MapPtr<> BEFORE = S ( poly::reverse(LE) ) ( poly::reverse(S) );

			// REUSE_DIST := card ((AFTER_PREV * BEFORE) . A);
			poly::PiecewisePtr<> REUSE_DIST = (AFTER_PREV * BEFORE) (A)->getCard();
			avg_reuse_max += REUSE_DIST->upperBound();
			++num_refs;
		} catch (features::UndefinedSize&& ex) {
			LOG(WARNING) << "Cache reuse distance for reference '" << *cur.first 
				         << "' cannot be determined because of unknwon reference size";
		}

	});

	return avg_reuse_max/num_refs;
}

} } } // end insieme::analysis::modeling namespace 

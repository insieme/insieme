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

#include <iomanip>

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/analysis/features/type_features.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

using namespace insieme;
using namespace insieme::analysis::polyhedral;

namespace insieme { namespace analysis { namespace modeling {

namespace {

typedef insieme::utils::map::PointerMap<core::ExpressionPtr, size_t> ReferenceMap;

// For each reference access in this scop produce a list which contains the list of references being accessed and for
// each of those the index variable used
ReferenceMap extractReferenceInfo(const Scop& scop) {

	ReferenceMap refMap;
	// go thorugh the statements of this scop and collect all the variables being accessed in the scop

	for_each(scop, [&] (const StmtPtr& stmt) { 
		for_each(stmt->access_begin(), stmt->access_end(), [&](const AccessInfoPtr& cur) { 
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

// Builds a relationship which maps a block id to a particular set. 
// This is objetained by performing:
//
// 		BLOCK % SETS 
//
// where SETS is the number of available sets computed as:
//
// 		SETS = cache_size / block_size
//
MapPtr<> buildCacheModel(CtxPtr<>& ctx, size_t block_size, size_t cache_size, size_t associativity) {

	// num_blocks  = cache_size / block_size
	size_t num_blocks = cache_size / block_size / associativity;
	assert(num_blocks > 0 && "Parameters for cache architecture not valid");
	
	std::string num_blocks_str = utils::numeric_cast<std::string>(num_blocks);
	// BLOCK[i] -> SET[j] : exists a = [i/num_blocks] j = i - a*num_blocks and j > 0 and j < num_blocks
	MapPtr<> blockToAddr(*ctx, "{LINE[t] -> SET[s] : exists a = [t/" + num_blocks_str + "] : s = t-a*" + 
								num_blocks_str + " and s < " + num_blocks_str + " and s >= 0}");
	// Apply a map to the other 
	return blockToAddr;
}

// constains the two pieces of information required for building cache mapping later.
typedef std::pair<MapPtr<>, MapPtr<>> SchedAccessPair;

/**
 * Functions that given a particular reference builds the scheduling function for that reference and
 * the map between access to memory line being accessed. 
 */
SchedAccessPair buildAccessMap(CtxPtr<>& ctx, const Scop& scop, const core::ExpressionPtr& reference) {

	MapPtr<> schedMap = makeEmptyMap(ctx);
	MapPtr<> accessMap = makeEmptyMap(ctx);

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
	// Compute the size of elements of this reference 
	unsigned type_size = analysis::features::getSizeInBytes(elemType);
	//LOG(DEBUG) << "SIZE FOR TYPE: " << type_size;

	for_each(scop, [&] (const StmtPtr& stmt) {
	
		// Access Functions 
		std::for_each(stmt->access_begin(), stmt->access_end(), [&](const AccessInfoPtr& cur) {
			// make a copy as we are going to modify this to add typing informations
			AffineSystem accessInfo = cur->getAccess();
			
			if (accessInfo.empty() || (*cur->getExpr().getAddressedNode() != *reference))  return;

			assert(accessInfo.size() == 1 && "Multidimensional accesses not yet supported");

			// an access of type i+j should be converted into size * (i+j), therefore every coeff should be multiplied
			// by size
			const IterationVector& iv = scop.getIterationVector();
			for(size_t cidx = 0; cidx < iv.size(); ++cidx) {
				accessInfo[0].setCoeff(iv[cidx], accessInfo[0].getCoeff(iv[cidx]) * type_size);
			}

			TupleName tn(stmt->getAddr(), "acc" + utils::numeric_cast<std::string>(accID));

			AffineSystem sched = stmt->getSchedule();
			AffineFunction func(scop.getIterationVector());
			func.setCoeff( Constant(), accID++ );
			sched.append( func );

			// compute the domain for this stmt
			SetPtr<> stmtDom = makeSet(ctx, stmt->getDomain(), tn) * makeSet(ctx, cur->getDomain(), tn);

			schedMap += makeMap(ctx, sched, tn) * stmtDom;
			accessMap += makeMap(ctx, accessInfo, tn, TupleName(cur->getExpr(), "MEM")) * stmtDom;
		});

	});
	
	return std::make_pair(schedMap, accessMap);
}

std::string listOfVariables(std::string name, unsigned n) {
	std::vector<unsigned> vec(n);
	std::transform(vec.begin(), vec.end()-1, vec.begin()+1, std::bind(std::plus<unsigned>(), 1, std::placeholders::_1));

	std::ostringstream ss;
	ss << join(",", vec, [&](std::ostream& jout, const unsigned& cur) { jout << name << cur; });
	return ss.str();
}

#define PATCH

PiecewisePtr<> getCompulsoryMisses(CtxPtr<> ctx, const Scop& scop, size_t block_size, size_t cache_size, unsigned associativity) {
	using insieme::analysis::polyhedral::reverse;

	// core::NodeManager& mgr = root->getNodeManager();
	ReferenceMap&& refMap = extractReferenceInfo(scop);

	// Make sure that all the references have 1 dimensional access. 
	// 
	// In order to support N-dimensional access we need to statically know the size of the N-1 dimensions in order to
	// perform a linearization of the access. FIXME
	if (any(refMap, [] (const ReferenceMap::value_type& cur) { return cur.second != 1; }) ) {
		throw CacheModelingError("Impossible to compute cache misses for given input code."
							"REASON: presence of multi-dimensional accesses");
	}

	// Because we have no mean to determine the allocation address of each reference at compile time, we extract the
	// cache misses for each of the references in the code and then aggregate them together. 
	MapPtr<> cache = buildCacheModel(ctx, block_size, cache_size, associativity);
	VLOG(1) << "Cache model is: \nB:=" << *cache << ";";

	PiecewisePtr<>&& pw = makeZeroPiecewise(ctx);

	for_each(refMap, [&](const ReferenceMap::value_type& cur) {
		try {
			VLOG(1) << "Examining reference: " << *cur.first;

			SchedAccessPair&& ret = buildAccessMap(ctx, scop, cur.first);

			MapPtr<> map2(*ctx, "{[MEM[i] -> SET[s]] -> [t,s0] : s0=s and t = [i/" + 
				utils::numeric_cast<std::string>(block_size) + "]}"
			);
			{};

			std::string&& schedVars = listOfVariables("i", scop.schedDim()+1);
			MapPtr<> R(*ctx, "{[[" + schedVars + "] -> [o1,o2]] -> [" + schedVars + ",o1,o2] }");
			{}

			MapPtr<>&& map = polyhedral::reverse(ret.second)(ret.second)(cache);

			map = reverse(domain_map(map))( map2 );

			// These are the compulsory misses associated with the code region 
			PiecewisePtr<> comp_misses = range(map)->getCard();
			pw += comp_misses;
			
		} catch (features::UndefinedSize&& ex) {
			LOG(WARNING) << "Cache misses for reference '" << *cur.first 
				         << "' cannot be determined because of unknwon reference size";
		}
	});

	return pw;
}

PiecewisePtr<> getCapacityMisses(CtxPtr<> ctx, const Scop& scop, size_t block_size, size_t cache_size, unsigned associativity) {
	using insieme::analysis::polyhedral::reverse;

	ReferenceMap&& refMap = extractReferenceInfo(scop);

	// Make sure that all the references have 1 dimensional access. 
	// 
	// In order to support N-dimensional access we need to statically know the size of the N-1 dimensions in order to
	// perform a linearization of the access. FIXME
	if (any(refMap, [] (const ReferenceMap::value_type& cur) { return cur.second != 1; }) ) {
		throw CacheModelingError("Impossible to compute cache misses for given input code."
							"REASON: presence of multi-dimensional accesses");
	}

	// Because we have no mean to determine the allocation address of each reference at compile time, we extract the
	// cache misses for each of the references in the code and then aggregate them together. 
	MapPtr<> B = buildCacheModel(ctx, block_size, cache_size, associativity);
	VLOG(1) << "Cache model is: \nB:=" << *B << ";";

	PiecewisePtr<>&& pw = makeZeroPiecewise(ctx);

	for_each(refMap, [&](const ReferenceMap::value_type& cur) {
		try {
			LOG(DEBUG) << "Reference: " << *cur.first;

			SchedAccessPair&& ret = buildAccessMap(ctx, scop, cur.first);
			MapPtr<> SCHED = ret.first;   // this is the sceduling 
			VLOG(1) << "SCHED := " << *SCHED << ";";

			MapPtr<> M = ret.second;  // this is the mapping from scheduling to memory locations
			VLOG(1) << "M := " << *M << ";";

			SetPtr<> D = range(SCHED);	  // Domain 
			VLOG(1) << "D := " << *D << ";";

			MapPtr<> A = (polyhedral::reverse(SCHED)(M))(
				MapPtr<>(*ctx, "{MEM[x] -> LINE[([x/"+utils::numeric_cast<std::string>(block_size)+"])]}"));
			{};
			VLOG(1) << "A := " << *A << ";";

			MapPtr<> DD = MapPtr<>(*ctx, isl_union_set_lex_lt_union_set( D->getIslObj(), D->getIslObj() ));
			// S: maps iterations to later iterations that access the same memory element
			MapPtr<> S =  A(polyhedral::reverse(A)) * DD;
			VLOG(1) << "S := " << *S << ";";

			// map L, mapping an iteration I to a later access to a memory alement that is mapped to
			// the same cache set. 

			// Build Q, a map from iterations to memory lines that are mapped to the same cache set
			MapPtr<> Q = A (B) (polyhedral::reverse(B));
			VLOG(1) << "Q := " << *Q << std::endl;

			MapPtr<> T1 = polyhedral::range_map(Q) ( polyhedral::reverse(A) );
			VLOG(1) << "T1 := " << *T1 << ";";

			MapPtr<> T2 = polyhedral::domain_map(Q) (DD);
			VLOG(1) << "T2 := " << *T2 << ";";
			
			MapPtr<> T3(*ctx, isl_union_map_lexmin( (T1*T2)->getIslObj() ));
			VLOG(1) << "T3 := " << *T3 << ";";

			MapPtr<> T4 = polyhedral::reverse(polyhedral::domain_map(Q)) (T3);
			VLOG(1) << "T4 := " << *T4 << ";";

			MapPtr<> S1(*ctx, isl_union_set_identity(D->getIslObj()));
			MapPtr<> L = 
				MapPtr<>(*ctx, isl_union_map_product(S1->getIslObj(), DD->getIslObj())) * 
				MapPtr<>(*ctx, isl_union_map_from_domain_and_range(
					isl_union_map_wrap(isl_union_map_from_domain_and_range(D->getIslObj(),D->getIslObj())),
					isl_union_map_wrap(T4->getIslObj())
				));
			VLOG(1) << "L := " << *L << ";";

#ifndef PATCH
			MapPtr<> L5 = L;
			for(unsigned i=0; i<associativity; ++i) { L5 = L5(L); }
#endif
#ifdef PATCH
			isl_int val;
			isl_int_init(val);
			isl_int_set_si(val, associativity+1);
			MapPtr<> L5( *ctx, isl_union_map_fixed_power(L->getIslObj(), val) );
			isl_int_clear(val);
#endif
			VLOG(1) << "L5 := " << *L5 << ";";

			MapPtr<> R = MapPtr<>(*ctx, isl_union_map_product(isl_union_set_identity(D->getIslObj()), S->getIslObj())) * L5;
			VLOG(1) << "R := " << *R << ";";

			SetPtr<> RES = range( MapPtr<>(*ctx, isl_union_set_unwrap( range(R)->getIslObj() )) );
			VLOG(1) << "RES := " << *RES << ";";

			PiecewisePtr<> misses = RES->getCard();
			VLOG(1) << "CARD: " << *misses;

			pw += misses;
			
		} catch (features::UndefinedSize&& ex) {
			LOG(WARNING) << "Cache misses for reference '" << *cur.first 
				         << "' cannot be determined because of unknwon reference size";
		}
	});

	return pw;
}

} // end anonymous namespace 


PiecewisePtr<> getCacheMisses(CtxPtr<> ctx, const core::NodePtr& root, size_t block_size, size_t cache_size, unsigned associativity) {

	LOG(INFO) << "%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(INFO) << "% Computing cache misses for cache architecture: ";
	LOG(INFO) << "\tCache Line Size: " << block_size;
	LOG(INFO) << "\tCache Size:      " << cache_size;
	LOG(INFO) << "\tAssociativity    " << associativity;
	LOG(INFO) << "%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"; 
	boost::optional<Scop> optScop = scop::ScopRegion::toScop(root);	
	if (!optScop) { return makeZeroPiecewise(ctx); }
	// this is a SCoP, we can perform analysis 
	const Scop& scop = *optScop;
	
	auto&& compMisses = getCompulsoryMisses(ctx, scop, block_size, cache_size, associativity);
	LOG(DEBUG) << "Compulsory misses: " << *compMisses;
	auto&& capMisses = getCapacityMisses(ctx, scop, block_size, cache_size, associativity);
	LOG(DEBUG) << "Capacity Misses: " << *capMisses;

	return compMisses + capMisses;
}

size_t getReuseDistance(const core::NodePtr& root, size_t block_size) {

	using insieme::analysis::polyhedral::reverse;

	boost::optional<Scop> optScop = scop::ScopRegion::toScop(root);	
	if (!optScop) { return 0; }

	// this is a SCoP, we can perform analysis 
	const Scop& scop = *optScop;

	ReferenceMap&& refMap = extractReferenceInfo(scop);

	// Make sure that all the references have 1 dimensional access. 
	// 
	// In order to support N-dimensional access we need to statically know the size of the N-1 dimensions in order to
	// linearize the access. FIXME
	if (any(refMap, [] (const ReferenceMap::value_type& cur) { return cur.second != 1; }) ) {
		throw CacheModelingError("Impossible to compute cache misses for given input code."
							"REASON: presence of multi-dimensional accesses");
	}
	
	// auto&& ctx = makeCtx();

	// Because we have no mean to determine the allocation address of each reference at compile time, we extract the
	// cache misses for each of the references in the code and then aggregate them together. 
	//MapPtr<> C = buildCacheLineModel(ctx, block_size);
	//double avg_reuse_max = 0.0;
	//size_t num_refs = 0;

	//for_each(refMap, [&](const ReferenceMap::value_type& cur) {
		//try {
			//// LOG(DEBUG) << "Reference: " << *cur.first;
			//SchedAccessPair&& ret = buildAccessMap(ctx, scop, cur.first);
			
			//// Schedule Map
			//MapPtr<> S = ret.first;
			////std::cout << "S:=" << *S << ';' << std::endl;
			////
			//// Access Map
			//MapPtr<> A = ret.second( C );
			//// std::cout << "A:=" << *A << ';' << std::endl;

			//// TIME := ran S
			//SetPtr<> TIME = range(S);

			//// LT := TIME << TIME
			//MapPtr<> LT 	= MapPtr<>(*ctx, isl_union_set_lex_lt_union_set(TIME->getIslObj(), TIME->getIslObj()));
			//// std::cout << "LT:=" << *LT << ";" << std::endl;

			//// LE = TIME <<= TIME
			//MapPtr<> LE   = MapPtr<>(*ctx, isl_union_set_lex_le_union_set(TIME->getIslObj(), TIME->getIslObj()));

			//// T := ((S^-1) . A . (A^-1) . S) * LT
			//MapPtr<> T = (reverse(S)(A)(reverse(A))(S)) * LT;
			//// LOG(DEBUG) << "T:=" << *T << ';' << std::endl;

			//// M := lexmin T
			//MapPtr<> M = MapPtr<>(*ctx, isl_union_map_lexmin( T->getIslObj() )); 
			//// LOG(DEBUG) << "M:=" << *M << ';' << std::endl;

			//// NEXT := S . M . (S^-1); # map to next access to same cache line
			//MapPtr<> NEXT = S ( M(reverse(S)) );

			//// AFTER_PREV := (NEXT^-1) . (S . LE . (S^-1));
			//MapPtr<> AFTER_PREV = reverse(NEXT) ( S (LE) (reverse(S)) );

			//// BEFORE := S . (LE^-1) . (S^-1);
			//MapPtr<> BEFORE = S ( reverse(LE) ) ( reverse(S) );

			//// REUSE_DIST := card ((AFTER_PREV * BEFORE) . A);
			//PiecewisePtr<> REUSE_DIST = (AFTER_PREV * BEFORE) (A)->getCard();

			//// Set the value of eventual parameters to 100
			//IterationVector iv = REUSE_DIST->getIterationVector(root->getNodeManager());
			//assert(iv.getIteratorNum() == 0);
			//if (iv.getParameterNum() > 0) {
				//// we have some parameters, let's set a default value = 100
				//for_each(iv.param_begin(), iv.param_end(), [&](const Parameter& cur) {
						//REUSE_DIST *=
							//makeSet(ctx, 
								//makeVarRange(iv, cur.getExpr(), core::IRBuilder(root->getNodeManager()).intLit(100) )
							//);
					//});
			//}

			//avg_reuse_max += REUSE_DIST->upperBound();
			//++num_refs;
		//} catch (features::UndefinedSize&& ex) {
			//LOG(WARNING) << "Cache reuse distance for reference '" << *cur.first 
						 //<< "' cannot be determined because of unknwon reference size";
		//}

	//});

	//return avg_reuse_max/num_refs;
	return 0;
}

} } } // end insieme::analysis::modeling namespace 

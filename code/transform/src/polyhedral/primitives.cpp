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

#include "insieme/transform/polyhedral/primitives.h"

#include "insieme/core/ir_builder.h"

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/utils/logging.h"

namespace insieme { namespace transform { namespace polyhedral {

using namespace analysis;
using namespace analysis::polyhedral;


namespace {

UnimodularMatrix makeInterchangeMatrix(size_t size, size_t src, size_t dest) {
	Matrix<int>&& m = utils::makeIdentity<int>(size);
	m.swapRows(src, dest);
	return m;
}

std::vector<StmtPtr> getStmts(Scop& scop, const Iterator& iter, bool internal) {

	const IterationVector& iterVec = scop.getIterationVector();

	std::vector<StmtPtr> ret;
	for_each(scop, [&] (StmtPtr& cur) { 
			IntMatrix&& sched = extractFrom( cur->getSchedule() );
			int idx = iterVec.getIdx(iter);
			assert(idx != -1);

			size_t pos = 0, end = sched.rows();
			for(; pos<end && sched[pos][idx]==0; ++pos) ;
			
			if( pos!=sched.rows() && internal ) {
				ret.push_back( cur );
			} else if (pos == sched.rows() && !internal) {
				ret.push_back( cur );
			}
		} );

	return ret;

}

} // end anonymous namespace 


std::vector<StmtPtr> getLoopSubStatements(Scop& scop, const Iterator& iter) {
	// returns all the statements which have 1 in the scheduling matrix corresponding to iterator
	// 'iter'
	return getStmts(scop, iter, true);
}

void scheduleLoopBefore(Scop& scop, const Iterator& iter, const Iterator& newIter) {
	const IterationVector& iterVec = scop.getIterationVector();
	std::vector<StmtPtr>&& stmts = getLoopSubStatements(scop, iter);

	for_each(stmts, [&](StmtPtr& cur) { 
		AffineSystem& schedule = cur->getSchedule();
		size_t dim = schedule.size()+1;

		if ( dim > scop.schedDim() ) { scop.schedDim() = dim; }

		IntMatrix mat(1, iterVec.size());
		mat[0][iterVec.getIdx(newIter)] = 1;
		schedule.append( mat[0] );

		IntMatrix&& sched = extractFrom( schedule );
		
		size_t idx = sched.rows()-1;
		do {
			sched.swapRows(idx-1, idx);
			--idx;
		} while( sched[idx+1][iterVec.getIdx(iter)] != 1 );
		
		schedule.set( sched );
	} );
}

void scheduleLoopAfter(Scop& scop, const Iterator& iter, const Iterator& newIter) {
	const IterationVector& iterVec = scop.getIterationVector();
	std::vector<StmtPtr>&& stmts = getLoopSubStatements(scop, iter);

	for_each(stmts, [&](StmtPtr& cur) { 
		AffineSystem& schedule = cur->getSchedule();
		size_t dim = schedule.size()+1;

		if ( dim > scop.schedDim() ) { scop.schedDim() = dim; }

		IntMatrix mat(1, iterVec.size());
		mat[0][iterVec.getIdx(newIter)] = 1;
		schedule.append( mat[0] );

		IntMatrix&& sched = extractFrom( schedule );
		
		size_t idx = sched.rows()-1;
		while( sched[idx-1][iterVec.getIdx(iter)] != 1 ) {
			sched.swapRows(idx-1, idx);
			--idx;
			std::cout << sched << std::endl;
		}
		schedule.set( sched );
	} );

}

void addConstraint(Scop& scop, const Iterator& iter, const IterationDomain& dom) {
	
	std::vector<StmtPtr>&& stmts = getLoopSubStatements(scop, iter);

	for_each(stmts, [&](StmtPtr& cur) { cur->getDomain() &= dom; } );

}

void setZeroOtherwise(Scop& scop, const Iterator& iter) {

	const IterationVector& iterVec = scop.getIterationVector();
	std::vector<StmtPtr>&& stmts = getStmts(scop, iter, false);

	for_each(stmts, [&](StmtPtr& cur) { 
		AffineFunction func(iterVec);
		func.setCoeff( iter, 1 );

		cur->getDomain() &= IterationDomain( AffineConstraint( func, ConstraintType::EQ) ); 
	} );
}


UnimodularMatrix 
makeInterchangeMatrix(const IterationVector& 	iterVec, 
					  const core::VariablePtr& 	src, 
					  const core::VariablePtr& 	dest) 
{
	int srcIdx = iterVec.getIdx( Iterator(src) );
	int destIdx = iterVec.getIdx( Iterator(dest) );
	assert( srcIdx != -1 && destIdx != -1 && srcIdx != destIdx && "Interchange not valid");
	return makeInterchangeMatrix( iterVec.size(), srcIdx, destIdx);
}


template <>
void applyUnimodularTransformation<SCHED_ONLY>(Scop& scop, const UnimodularMatrix& trans) {
	for_each(scop, [&](StmtPtr& cur) { 
		IntMatrix&& sched = extractFrom(cur->getSchedule());
		IntMatrix&& newSched = sched * trans; 
		cur->getSchedule().set(newSched); 
	} );
}

template <>
void applyUnimodularTransformation<ACCESS_ONLY>(Scop& scop, const UnimodularMatrix& trans) {
	for_each(scop, [&](StmtPtr& cur) { 
		for_each( cur->getAccess(), [&](AccessInfoPtr& cur) { 
			IntMatrix&& access = extractFrom( cur->getAccess() );
			IntMatrix&& newAccess = access * trans;
			cur->getAccess().set( newAccess ) ;

		} );
	} );
}

template <>
void applyUnimodularTransformation<BOTH>(Scop& scop, const UnimodularMatrix& trans) {
	applyUnimodularTransformation<SCHED_ONLY>(scop, trans);
	applyUnimodularTransformation<ACCESS_ONLY>(scop, trans);
}

bool checkTransformedSchedule(Scop origin, Scop trans) {

	auto&& ctx = makeCtx();
	auto&& deps = origin.computeDeps(ctx);
	VLOG(1) << "Dependencies in the original schedule:";
	VLOG(1) << *deps;

	auto&& tSched = trans.getSchedule(ctx);
	VLOG(1) << "Transformed schedule:";
	VLOG(1) << *tSched;

	MapPtr<> umao = (polyhedral::reverse(tSched)(deps))(tSched);
	VLOG(1) << "Application of transformation to original schedule:";
	VLOG(1) << *umao;
	//	isl_union_map_apply_range(
	//		isl_union_map_apply_range( isl_union_map_reverse(tSched->getIslObj()), deps->getIslObj() ), 
	//		tSched->getIslObj() 
	//	);
	
	MapPtr<> nonValidMap(*ctx, isl_union_set_lex_gt_union_set( 
								polyhedral::range(tSched)->getIslObj(), 
								polyhedral::range(tSched)->getIslObj()));
	VLOG(1) << "Non validity map:";
	VLOG(1) << *nonValidMap;

	//isl_union_map* nonValidDom = 
	//	isl_union_set_lex_gt_union_set( 
	//			isl_union_map_range(tSched->getIslObj()), 
	//			isl_union_map_range(tSched->getIslObj()) 
	//		);
	
	// MapPtr<> map2(*ctx, isl_union_map_copy(nonValidDom));
	// LOG(INFO) << isl_union_map_is_empty(isl_union_map_intersect(umao, nonValidDom));

	MapPtr<> intersection = umao * nonValidMap;
	VLOG(1) << "Intersection: " << *intersection;

	// isl_union_map* intersection = isl_union_map_intersect( umao, nonValidDom );
	
	return intersection->empty();
}

namespace {

typedef std::vector<AffineConstraintPtr> 	ConjunctionList;
typedef std::vector<ConjunctionList> 		DisjunctionList;

std::pair<DisjunctionList, DisjunctionList> getDomainBounds(IterationVector& iterVec, 
															const core::VariablePtr oldIter,
															const core::VariablePtr& newIter, 
															const DisjunctionList& disjunctions) 
{
	DisjunctionList lbs(1), ubs(1);

	for_each(disjunctions, [&](const ConjunctionList& cur) {
		for_each(cur, [&](const AffineConstraintPtr& cur) {
			// this is either a raw or a negation 
			assert (cur->getCombinerType() == utils::CT_RAW && "Constraint not normalized");

			const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);
			const AffineConstraint& c = rc.getConstraint();

			AffineFunction f(iterVec, c.getFunction());

			int coeff = f.getCoeff(oldIter);
			if (coeff == 0) { return; }
			
			if (c.getType() == ConstraintType::EQ || c.getType() == ConstraintType::NE) {
				// stride.back().push_back( makeCombiner(AffineConstraint(f, c.getType())));
				return;
			} 

			// detect lowerbounds
			if ((coeff > 0 && (c.getType() == ConstraintType::GT || c.getType() == ConstraintType::GE)) || 
				(coeff < 0 && (c.getType() == ConstraintType::LT || c.getType() == ConstraintType::LE))) 
			{ 
				lbs.back().push_back( makeCombiner(AffineConstraint(f, c.getType()))); 
				return;
			}

			// detect upperbounds
			if ((coeff < 0 && (c.getType() == ConstraintType::GT || c.getType() == ConstraintType::GE)) || 
				(coeff > 0 && (c.getType() == ConstraintType::LT || c.getType() == ConstraintType::LE))) 
			{ 
				ubs.back().push_back( makeCombiner(AffineConstraint(f, c.getType()))); 
				return;
			}
			assert(false);
		});
		if (!lbs.back().empty()) { lbs.push_back( ConjunctionList() ); }
		if (!ubs.back().empty()) { ubs.push_back( ConjunctionList() ); }
		// if (!stride.back().empty()) { stride.push_back( ConjunctionList() ); }
	});
	
	return std::make_pair(lbs, ubs);
}

// Replace iterators 
DisjunctionList replace(DisjunctionList& disjunctions, const core::VariablePtr oldIter, const core::VariablePtr& newIter) {

	DisjunctionList ret(1);
	for_each(disjunctions, [&] (ConjunctionList& cur) {
		for_each(cur, [&](AffineConstraintPtr& cur) { 
			
			RawAffineConstraint& rc = static_cast<RawAffineConstraint&>(*cur);
			const AffineConstraint& c = rc.getConstraint();
			AffineFunction f(c.getFunction());

			int coeff = f.getCoeff(oldIter);
			f.setCoeff(newIter, coeff);
			f.setCoeff(oldIter, 0);

			ret.back().push_back( makeCombiner( AffineConstraint(f, c.getType()) ) );
		});

		if (!ret.back().empty()) { ret.push_back( ConjunctionList() ); }
	});

	return ret;
}

// Transforms a list of conjunctions into a constraint pointer which can be handled by the polyhedral backend
AffineConstraintPtr toConstraint(const DisjunctionList& disjunctions) {
	AffineConstraintPtr ret;
	
	for_each(disjunctions, [&] (const ConjunctionList& cur) {
		AffineConstraintPtr sub;
		for_each(cur, [&](const AffineConstraintPtr& cur) { sub = sub ? sub and cur : cur; });
		ret = ret ? ret or sub : sub;
	});

	return ret;
}

} // end anonymous namespace 


// Stride an iterator.
core::VariablePtr doStripMine(core::NodeManager& 		mgr,
							  Scop& 					scop, 
							  const core::VariablePtr 	iter, 
							  const IterationDomain& 	dom, 
						  	  unsigned 					tile_size)
{
	core::IRBuilder builder(mgr);

	// check whether the indexes refers to loops 
	IterationVector& iterVec = scop.getIterationVector();

	// Add a new loop and schedule it before the indexed loop 
	core::VariablePtr&& newIter = builder.variable(mgr.getLangBasic().getInt4());
	addTo(scop, newIter);

	// Add an existential variable used to created a strided domain
	core::VariablePtr&& strideIter = builder.variable(mgr.getLangBasic().getInt4());
	addTo(scop, Iterator(strideIter, true));

	// Schedule the new loop before the loop we are going to stride
	scheduleLoopBefore(scop, iter, newIter);
	
	// Set the new iterator to 0 for all the statements which are not scheduled under this loop 
	setZeroOtherwise(scop, newIter);

	// Make sure the domain is based on the current iteration vector
	AffineConstraintPtr domain = cloneConstraint(iterVec, dom.getConstraint());
	DisjunctionList&& disjunctions = getConjuctions(toDNF(domain)), lb, ub;

	boost::tie(lb,ub) = getDomainBounds(iterVec, iter, newIter, disjunctions);
	boost::tie(lb,ub) = std::make_pair(replace(lb, iter, newIter), replace(ub, iter, newIter));

	VLOG(1) << "Original domain: " << *domain;
	AffineConstraintPtr&& lbCons = toConstraint(lb);
	// stride the lowerbound
	AffineConstraintPtr&& ubCons = toConstraint(ub);

	VLOG(1) << "Extracted LB: " << *lbCons;
	VLOG(1) << "Extractd UB:  " << *ubCons; 

	DisjunctionList stride(1);
	for_each(lb, [&] (const ConjunctionList& cur) {

		for_each(cur, [&](const AffineConstraintPtr& cur) {
			// detect whether this constraint is an equality
			const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);
			AffineFunction func(iterVec, rc.getConstraint().getFunction());

			// add the stride
			func.setCoeff(strideIter, -tile_size);

			stride.back().push_back( makeCombiner( AffineConstraint(func, ConstraintType::EQ) ) );
		});
		if (!stride.back().empty()) { stride.push_back( ConjunctionList() ); }
	});

	addConstraint(scop, newIter, IterationDomain(lbCons and ubCons and toConstraint(stride)));
 
 	// Add constraint to the stripped domain which is now bounded within:
 	// 	 newIter and newIter + TileSize
 	// iter >= newIter
	AffineFunction af2(iterVec);
	af2.setCoeff(iter, 1);
	af2.setCoeff(newIter, -1);
 	
 	// iter < newIter + T ---> iter -newITer -T <= 0
 	AffineFunction af3(iterVec);
	af3.setCoeff(iter, 1);
	af3.setCoeff(newIter, -1);
 	af3.setCoeff(Constant(), -tile_size);
 
	addConstraint(scop, iter, 
			IterationDomain( AffineConstraint(af2) and AffineConstraint(af3, ConstraintType::LT)) 
		);

	return newIter;
}

} } } // end insimee::transform::polyhedral namespace 


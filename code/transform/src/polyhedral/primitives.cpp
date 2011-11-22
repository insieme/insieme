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

#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace transform {
namespace polyhedral {

using namespace analysis;
using namespace analysis::poly;

IntMatrix extractFrom(const AffineSystem& sys) {

	IntMatrix mat(sys.size(), sys.getIterationVector().size());

	size_t i=0;
	for_each (sys.begin(), sys.end(), [&](const AffineFunction& cur) {
			size_t j=0;
			std::for_each(cur.begin(), cur.end(), [&] (const AffineFunction::Term& term) {
				mat[i][j++] = term.second;
			});
			i++;
		} );

	return mat;
}

namespace {

UnimodularMatrix makeInterchangeMatrix(size_t size, size_t src, size_t dest) {
	Matrix<int>&& m = utils::makeIdentity<int>(size);
	m.swapRows(src, dest);
	return m;
}

} // end anonymous namespace 


std::vector<StmtPtr> getLoopSubStatements(Scop& scop, const Iterator& iter) {
	// returns all the statements which have 1 in the scheduling matrix corresponding to iterator
	// 'iter'
	const IterationVector& iterVec = scop.getIterationVector();

	std::vector<StmtPtr> ret;
	for_each(scop, [&] (StmtPtr& cur) { 
			IntMatrix&& sched = extractFrom( cur->getSchedule() );
			
			IntMatrix mat(iterVec.size(), 1);
			assert(iterVec.getIdx(iter) != -1);
			mat[iterVec.getIdx(iter)][0] = 1;

			IntMatrix&& ext = sched * mat;
			IntMatrix mat2(1, ext.rows(), true, 1);

			IntMatrix&& unary = mat2 * ext;
			assert(unary.cols() == 1 && unary.rows() == 1);
			if( unary[0][0] != 0 ) {
				ret.push_back( cur );
			}
		} );

	return ret;
}

void scheduleLoopBefore(Scop& scop, const Iterator& iter, const Iterator& newIter) {
	const IterationVector& iterVec = scop.getIterationVector();
	std::vector<StmtPtr>&& stmts = getLoopSubStatements(scop, iter);

	for_each(stmts, [&](StmtPtr& cur) { 
			IntMatrix mat(1, iterVec.size());
			mat[0][iterVec.getIdx(newIter)] = 1;
			cur->getSchedule().append( mat[0] );

			IntMatrix&& sched = extractFrom( cur->getSchedule() );
			
			size_t idx = sched.rows()-1;
			do {
				sched.swapRows(idx-1, idx);
				--idx;
			} while( sched[idx+1][iterVec.getIdx(iter)] != 1 );
			
			cur->getSchedule().set( sched );
		} );
}

UnimodularMatrix 
makeInterchangeMatrix(const IterationVector& 	iterVec, 
					  const core::VariablePtr& 	src, 
					  const core::VariablePtr& 	dest) 
{
	int srcIdx = iterVec.getIdx( poly::Iterator(src) );
	int destIdx = iterVec.getIdx( poly::Iterator(dest) );
	assert( srcIdx != -1 && destIdx != -1 && srcIdx != destIdx && "Interchange not valid");
	return makeInterchangeMatrix( iterVec.size(), srcIdx, destIdx);
}


template <>
void applyUnimodularTransformation<SCHED_ONLY>(Scop& scop, const UnimodularMatrix& trans) {
	for_each(scop, [&](poly::StmtPtr& cur) { 
		IntMatrix&& sched = extractFrom(cur->getSchedule());
		IntMatrix&& newSched = sched * trans; 
		cur->getSchedule().set(newSched); 
	} );
}

template <>
void applyUnimodularTransformation<ACCESS_ONLY>(Scop& scop, const UnimodularMatrix& trans) {
	for_each(scop, [&](poly::StmtPtr& cur) { 
		for_each( cur->getAccess(), [&](poly::AccessInfo& cur) { 
			IntMatrix&& access = extractFrom( cur.getAccess() );
			IntMatrix&& newAccess = access * trans;
			cur.getAccess().set( newAccess ) ;

		} );
	} );
}

template <>
void applyUnimodularTransformation<BOTH>(Scop& scop, const UnimodularMatrix& trans) {
	applyUnimodularTransformation<SCHED_ONLY>(scop, trans);
	applyUnimodularTransformation<ACCESS_ONLY>(scop, trans);
}

} // end polyhedral namespace 
} // end transform namespace 
} // end insieme namespace 

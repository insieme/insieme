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

#include "insieme/transform/polyhedral/transform.h"

#include "insieme/core/ir_builder.h"

#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/transform/pattern/irpattern.h"

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/scop.h"

namespace insieme {
namespace transform {
namespace polyhedral {

using namespace analysis;
using namespace analysis::poly;

using namespace insieme::transform::pattern;
using insieme::transform::pattern::any;

namespace {

Scop extractScopFrom(const core::NodePtr& target) {
	if (!target->hasAnnotation(scop::ScopRegion::KEY) ) {
		throw InvalidTargetException(
			"Polyhedral transformation applyied to a non Static Control Region"
		);
	}
	
	// FIXME: We need to find the larger SCoP which contains this SCoP
	
	scop::ScopRegion& region = *target->getAnnotation( scop::ScopRegion::KEY );
	if ( !region.isValid() ) {
		throw InvalidTargetException(
			"Polyhedral transformation applyied to a non Static Control Region"
		);
	}
	region.resolve();

	return region.getScop();
}

} // end anonymous namespace 

core::NodePtr LoopInterchange::apply(const core::NodePtr& target) const {

	// make a copy of the polyhedral model associated to this node so that transformations are only
	// applied to the copy and not reflected into the original region 
	Scop scop = extractScopFrom(target);

	// check whether the indexes refers to loops 
	const IterationVector& iterVec = scop.getIterationVector();

	TreePatternPtr pattern = rT ( irp::forStmt( var("iter"), any, any, any, recurse | !irp::forStmt() ) );
	auto&& match = pattern->matchPointer( target );

	auto&& matchList = match->getVarBinding("iter").getTreeList();

	if (matchList.size() < srcIdx) 
		throw InvalidTargetException("source index does not refer to a for loop");
	if (matchList.size() < srcIdx) 
		throw InvalidTargetException("destination index does not refer to a for loop");

	core::VariablePtr src = core::static_pointer_cast<const core::Variable>( 
			matchList[srcIdx]
		);

	core::VariablePtr dest = core::static_pointer_cast<const core::Variable>( 
			matchList[destIdx]
		);

	applyUnimodularTransformation<SCHED_ONLY>(scop, makeInterchangeMatrix(iterVec, src, dest));

	core::NodePtr&& transformedIR = scop.toIR( target->getNodeManager() );	
	scop::mark(transformedIR);
	return transformedIR;
}

core::NodePtr LoopStripMining::apply(const core::NodePtr& target) const {

	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// make a copy of the polyhedral model associated to this node so that transformations are only
	// applied to the copy and not reflected into the original region 
	Scop scop = extractScopFrom(target);

	// check whether the indexes refers to loops 
	const IterationVector& iterVec = scop.getIterationVector();

	TreePatternPtr pattern = 
		rT ( 
			var("loop", irp::forStmt( var("iter"), any, any, any, aT(recurse) | any) ) 
		);
	
	auto&& match = pattern->matchPointer( target );
	
	auto&& matchList = match->getVarBinding("iter").getTreeList();
	
	if (matchList.size() < loopIdx) 
		throw InvalidTargetException("loop index does not refer to a for loop");

	core::VariablePtr idx = core::static_pointer_cast<const core::Variable>( matchList[loopIdx] );

	core::ForStmtPtr forStmt = static_pointer_cast<const core::ForStmt>(
			(loopIdx == 0) ? match->getRoot() :
			match->getVarBinding("loop").getTreeList()[loopIdx]
		); 

	// Add a new loop and schedule it before the indexed loop 
	core::VariablePtr&& newIter = builder.variable(mgr.getLangBasic().getInt4());
	
	// Add an existential variable used to created a strided domain
	core::VariablePtr&& strideIter = builder.variable(mgr.getLangBasic().getInt4());

	addTo(scop, newIter);
	addTo(scop, poly::Iterator(strideIter, true));

	scheduleLoopBefore(scop, idx, newIter);

	// Set the new iterator to 0 for all the statements which are not scheduled under this loop 
	setZeroOtherwise(scop, newIter);

	// Add a constraint to strip the domain of the tiled loop index 
	AffineFunction af1(iterVec);
	af1.setCoeff(newIter, 1);
	af1.setCoeff(strideIter, -tileSize);
	af1.setCoeff(Constant(), -1);

	std::cout << af1 << std::endl;
	addConstraint(scop, newIter, poly::IterationDomain( AffineConstraint(af1, AffineConstraint::EQ) ) );

	// Add constraint to the stripped domain which is now bounded within:
	//  newIter and newIter + TileSize
	// iter >= newIter
	AffineFunction af2(iterVec);
	af2.setCoeff(idx, 1);
	af2.setCoeff(newIter, -1);
	
	// iter < newIter + T ---> iter -newITer -T <= 0
	AffineFunction af3(iterVec);
	af3.setCoeff(idx, 1);
	af3.setCoeff(newIter, -1);
	af3.setCoeff(Constant(), -tileSize);

	addConstraint(scop, idx, poly::IterationDomain( 
				AffineConstraint(af2) 						and 
				AffineConstraint(af3, AffineConstraint::LT)
			) );


	// Get the constraints for the stripped loop iterator
	poly::IterationDomain dom( iterVec, 
			forStmt->getAnnotation( scop::ScopRegion::KEY )->getDomainConstraints()
		);
	
	std::cout << *copyFromConstraint(dom.getConstraint(), poly::Iterator(idx), poly::Iterator(newIter)) << std::endl;
	addConstraint(scop, newIter, IterationDomain(
			copyFromConstraint(dom.getConstraint(), poly::Iterator(idx), poly::Iterator(newIter)))
		);

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	scop::mark(transformedIR);
	return transformedIR;
}


core::NodePtr LoopFusion::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	Scop scop = extractScopFrom( target );

	// check whether the indexes refers to loops 
	const IterationVector& iterVec = scop.getIterationVector();

	TreePatternPtr pattern = node(
			*( irp::forStmt( var("iter"), any, any, any, any ) | any )
		);
	auto&& match = pattern->matchPointer( target );

	auto&& matchList = match->getVarBinding("iter").getTreeList();
	
	if (matchList.size() < loopIdx1) 
		throw InvalidTargetException("index 1 does not refer to a for loop");
	if (matchList.size() < loopIdx2) 
		throw InvalidTargetException("index 2 does not refer to a for loop");

	core::VariablePtr idx1 = core::static_pointer_cast<const core::Variable>( 
			matchList[loopIdx1]
		);

	core::VariablePtr idx2 = core::static_pointer_cast<const core::Variable>( 
			matchList[loopIdx2]
		);
	
	// Add a new loop iterator for the fused loop 
	core::VariablePtr&& newIter = builder.variable(mgr.getLangBasic().getInt4());

	addTo(scop, newIter);

	std::vector<StmtPtr>&& loopStmt1 = getLoopSubStatements(scop, idx1);
	std::vector<StmtPtr>&& loopStmt2 = getLoopSubStatements(scop, idx2);

	AffineFunction af1(iterVec);
	af1.setCoeff(idx1, 1);
	af1.setCoeff(newIter, -1);

	AffineFunction af2(iterVec);
	af2.setCoeff(idx2, 1);
	af2.setCoeff(newIter, -1);

	addConstraint(scop, idx1, 
			IterationDomain(AffineConstraint(af1, AffineConstraint::EQ )) 
		);

	addConstraint(scop, idx2, 
			IterationDomain(AffineConstraint(af2, AffineConstraint::EQ )) 
		);

	std::vector<StmtPtr> stmts;
	std::copy(loopStmt1.begin(), loopStmt1.end(), std::back_inserter(stmts));
	std::copy(loopStmt2.begin(), loopStmt2.end(), std::back_inserter(stmts));

	// set the new iter to be equal to the two loops to fuse
	assert( stmts.size() > 0 );
	IntMatrix mat(2, iterVec.size());
	mat[0][ iterVec.getIdx(newIter) ] = 1;

	for_each(stmts, [&] (StmtPtr& curr) { 
			curr->getSchedule().set(mat); 
			mat[1][iterVec.size()-1] += 1;
			std::cout << curr->getSchedule() << std::endl;
		} );

	setZeroOtherwise(scop, newIter);

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	scop::mark(transformedIR);
	return transformedIR;
}

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 

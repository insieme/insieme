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
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/pattern/ir_pattern.h"

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace transform {
namespace polyhedral {

using namespace analysis;
using namespace analysis::poly;

using namespace insieme::transform::pattern;
using insieme::transform::pattern::any;

#define AS_EXPR(node) core::static_pointer_cast<const core::Expression>(node)

namespace {

Scop extractScopFrom(const core::NodePtr& target) {
	// Run the SCoP analysis on this node in order to determine whether is possible to apply
	// polyhedral transformations to it
	scop::mark(target);

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

bool checkTransformationValidity(Scop& orig, Scop& trans) {

	auto&& ctx = MAKE_CTX();
	auto&& deps = orig.computeDeps(ctx);

	//deps->printTo(std::cout);
	// std::cout << std::endl;
	// std::cout << "ORIGINAL SCHED: " << std::endl;
	// auto&& oSched = orig.getSchedule(ctx);

	// std::cout << std::endl;
	// oSched->printTo( std::cout );
	// std::cout << std::endl;

	// std::cout << "Transformed SCHED: " << std::endl;
	auto&& tSched = trans.getSchedule(ctx);
	// tSched->printTo(std::cout);
	// std::cout << std::endl;

	isl_union_map* umao = 
		isl_union_map_apply_range(
			isl_union_map_apply_range( 
				isl_union_map_reverse(isl_union_map_copy(tSched->getAsIslMap())), 
				isl_union_map_copy(deps->getAsIslMap())
			),
			isl_union_map_copy(tSched->getAsIslMap()) 
		);
	
	// isl_union_set* deltas = isl_union_map_deltas( isl_union_map_copy(umao) );

	// LOG(DEBUG) << "DELTAS:" << std::endl;
	// printIslSet(std::cout, ctx.getRawContext(), deltas);
	// std::cout << std::endl;
	// printIslMap(std::cout, ctx.getRawContext(), umao);
	
	// std::cout << std::endl;
	
	// std::cout << "NON MAP"<< std::endl;
	isl_union_map* nonValidDom = 
		isl_union_set_lex_gt_union_set( 
				isl_union_map_range(isl_union_map_copy(tSched->getAsIslMap())), 
				isl_union_map_range(isl_union_map_copy(tSched->getAsIslMap())) 
			);

// 	printIslMap(std::cout, ctx.getRawContext(), nonValidDom);

	// LOG(INFO) << isl_union_map_is_empty(isl_union_map_intersect(umao, nonValidDom));

	isl_union_map* intersection = isl_union_map_intersect( umao, nonValidDom );
	bool isValid = isl_union_map_is_empty(intersection);
	isl_union_map_free(intersection);
	return isValid;
}

} // end anonymous namespace 

//=================================================================================================
// Loop Interchange
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopInterchange::LoopInterchange(const parameter::Value& value)
	: Transformation(LoopInterchangeType::getInstance(), value),
	  srcIdx(parameter::getValue<unsigned>(value, 0)),
	  destIdx(parameter::getValue<unsigned>(value, 1)) {}

LoopInterchange::LoopInterchange(unsigned src, unsigned dest)
	: Transformation(LoopInterchangeType::getInstance(),
			parameter::combineValues(
					parameter::makeValue(src),
					parameter::makeValue(dest)
			)
	  ),
	  srcIdx(src), destIdx(dest) { }

core::NodePtr LoopInterchange::apply(const core::NodePtr& target) const {

	// Loop interchange which tries to interchange the same loop is not allowed, therefore we throw
	// an exception, this is an invalid transformation
	if ( srcIdx == destIdx ) {
		throw InvalidTargetException("Loop Interchange cannot be applied to the same loop");
	}

	TreePatternPtr pattern = 
		rT ( 
			irp::forStmt( var("iter"), any, any, any, aT(recurse) | aT(!irp::forStmt() ) )
		);
	auto&& match = pattern->matchPointer( target );

	if (!match || !match->isVarBound("iter")) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}
	auto&& matchList = match->getVarBinding("iter").getList();
	// check whether the indexes refers to loops 
	if (matchList.size() <= srcIdx) 
		throw InvalidTargetException("source index does not refer to a for loop");
	if (matchList.size() <= destIdx) 
		throw InvalidTargetException("destination index does not refer to a for loop");

	// We are sure the application point for this transformation is valid, therefore we proceed with
	// the extraction of the SCoP information from this target point make a copy of the polyhedral
	// model associated to this node so that transformations are only applied to the copy and not
	// reflected into the original region 
	Scop origScop = extractScopFrom(target);

	Scop transfScop(origScop.getIterationVector(), origScop.getStmts());

	const IterationVector& iterVec = origScop.getIterationVector();
	
	VLOG(1) << "@ Applying Transformation 'polyhedral.loop.interchange'";
	utils::Timer t("transform.polyhedarl.loop.interchange");

	core::VariablePtr src = core::static_pointer_cast<const core::Variable>( matchList[srcIdx] );
	core::VariablePtr dest = core::static_pointer_cast<const core::Variable>( matchList[destIdx] );

	assert( iterVec.getIdx(src) != -1 && "Index for Source Loop is invalid");
	assert( iterVec.getIdx(dest) != -1 && "Index for Destination Loop is invalid");
	applyUnimodularTransformation<SCHED_ONLY>(transfScop, makeInterchangeMatrix(iterVec, src, dest));

	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformationValidity(origScop, transfScop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}
	core::NodePtr&& transformedIR = transfScop.toIR( target->getNodeManager() );	
	
	t.stop();
	VLOG(1) << t;
	VLOG(1) << "//@ polyhedral.loop.interchange Done";
	
	// LOG(INFO) << "After interchange: " << transfScop;

	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

TransformationPtr makeLoopInterchange(size_t idx1, size_t idx2) {
	return std::make_shared<LoopInterchange>(idx1, idx2);
}

namespace {

core::VariablePtr doStripMine(core::NodeManager& 		mgr, 
							 Scop& 						scop, 
							 const core::VariablePtr& 	loopIter, 
							 const core::ExpressionPtr& begin,
							 const core::ExpressionPtr& end,
							 int 						tileSize ) 
{

	core::IRBuilder builder(mgr);
	// check whether the indexes refers to loops 
	IterationVector& iterVec = scop.getIterationVector();

	// Add a new loop and schedule it before the indexed loop 
	core::VariablePtr&& newIter = builder.variable(mgr.getLangBasic().getInt4());
	
	// Add an existential variable used to created a strided domain
	core::VariablePtr&& strideIter = builder.variable(mgr.getLangBasic().getInt4());

	addTo(scop, newIter);
	addTo(scop, poly::Iterator(strideIter, true));

	scheduleLoopBefore(scop, loopIter, newIter);

	// Set the new iterator to 0 for all the statements which are not scheduled under this loop 
	setZeroOtherwise(scop, newIter);

	try {
		// Add a constraint to strip the domain of the tiled loop index 
		AffineFunction af1(iterVec, AS_EXPR(builder.invertSign( begin )));
		af1.setCoeff(newIter, 1);
		af1.setCoeff(strideIter, -tileSize);

		AffineFunction lb(iterVec, AS_EXPR(builder.invertSign( begin ) ) );
		lb.setCoeff(newIter, 1);

		AffineFunction ub(iterVec, AS_EXPR(builder.invertSign( end ) ) );
		ub.setCoeff(newIter, 1);

		addConstraint(scop, newIter, poly::IterationDomain( 
					AffineConstraint(af1, ConstraintType::EQ) and 
					AffineConstraint(lb, ConstraintType::GE)  and
					AffineConstraint(ub, ConstraintType::LT) )
				);

	} catch (core::arithmetic::NotAFormulaException&& e) {
		throw InvalidTargetException("Loop is not a SCoP");
	}
	// Add constraint to the stripped domain which is now bounded within:
	//  newIter and newIter + TileSize
	// iter >= newIter
	AffineFunction af2(iterVec);
	af2.setCoeff(loopIter, 1);
	af2.setCoeff(newIter, -1);
	
	// iter < newIter + T ---> iter -newITer -T <= 0
	AffineFunction af3(iterVec);
	af3.setCoeff(loopIter, 1);
	af3.setCoeff(newIter, -1);
	af3.setCoeff(Constant(), -tileSize);

	addConstraint(scop, loopIter, poly::IterationDomain( 
				AffineConstraint(af2) and AffineConstraint(af3, ConstraintType::LT)
		) );

	// LOG(INFO) << "After strip mine: " << scop;

	return newIter;
}

} // end anonymous namespace 

//=================================================================================================
// Loop Strip Mining
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopStripMining::LoopStripMining(const parameter::Value& value)
	: Transformation(LoopStripMiningType::getInstance(), value),
	  loopIdx(parameter::getValue<unsigned>(value, 0)),
	  tileSize(parameter::getValue<unsigned>(value, 1)) {}

LoopStripMining::LoopStripMining(unsigned idx, unsigned tileSize)
	: Transformation(LoopInterchangeType::getInstance(),
			parameter::combineValues(
					parameter::makeValue(idx),
					parameter::makeValue(tileSize)
			)
	  ),
	  loopIdx(idx), tileSize(tileSize) { }

core::NodePtr LoopStripMining::apply(const core::NodePtr& target) const {

	if (tileSize < 2 ) {
		throw InvalidTargetException("Tile size for Strip mining must be >= 2");
	}

	TreePatternPtr pattern = 
		rT ( 
			var("loop", irp::forStmt( var("iter"), any, any, any, aT( recurse ) | any) ) 
		);

	auto&& match = pattern->matchPointer( target );
	if (!match || !match->isVarBound("iter")) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	auto&& matchList = match->getVarBinding("iter").getList();
	
	if (matchList.size() <= loopIdx) 
		throw InvalidTargetException("loop index does not refer to a for loop");

	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// make a copy of the polyhedral model associated to this node so that transformations are only
	// applied to the copy and not reflected into the original region 
	Scop scop = extractScopFrom(target);

	core::VariablePtr idx = core::static_pointer_cast<const core::Variable>( matchList[loopIdx] );

	core::ForStmtPtr forStmt = static_pointer_cast<const core::ForStmt>(
			(loopIdx == 0) ? match->getRoot() :
			match->getVarBinding("loop").getList()[loopIdx]
		); 

	assert(forStmt && "ForStmt not matched");

	if (*forStmt->getStep() != *builder.intLit(1) ) {
		throw InvalidTargetException("Cannot tile a loop with step != 1");
	}

	VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.stripmining'";
	utils::Timer t("transform.polyhedral.loop.stripmining");
	
	doStripMine(mgr, scop, idx, forStmt->getStart(), forStmt->getEnd(), tileSize);
	
	// Get the constraints for the stripped loop iterator
	//poly::IterationDomain dom( iterVec, 
	//		forStmt->getAnnotation( scop::ScopRegion::KEY )->getDomainConstraints()
	//	);
	
	// std::cout << *copyFromConstraint(dom.getConstraint(), poly::Iterator(idx), poly::Iterator(newIter)) << std::endl;
	//addConstraint(scop, newIter, IterationDomain(
	//		copyFromConstraint(dom.getConstraint(), poly::Iterator(idx), poly::Iterator(newIter)))
	//	);

	core::NodePtr&& transformedIR = scop.toIR( target->getNodeManager() );	

	t.stop();
	VLOG(1) << t;
	VLOG(1) << "//@~ polyhedral.loop.stripmining Done";

	assert( transformedIR && "Generated code for loop strip mining not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

TransformationPtr makeLoopStripMining(size_t idx, size_t tileSize) {
	return std::make_shared<LoopStripMining>(idx, tileSize);
}

//=================================================================================================
// Loop Tiling
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//core::NodePtr LoopTilingComp::apply(const core::NodePtr& target) const {

	//// make a copy of the polyhedral model associated to this node so that transformations are only
	//// applied to the copy and not reflected into the original region 
	//Scop scop = extractScopFrom(target);

	//// Match non-perfectly nested loops
	//TreePatternPtr pattern = 
		//rT ( 
			//irp::forStmt( var("iter"), any, any, any, aT(recurse) | aT(!irp::forStmt() ) )
		//);
	//LOG(DEBUG) << pattern;

	//auto&& match = pattern->matchPointer( target );
	//if (!match || !match->isVarBound("iter")) {
		//throw InvalidTargetException("Invalid application point for loop  tiling");
	//}

	//auto&& matchList = match->getVarBinding("iter").getList();
	//LOG(DEBUG) << matchList.size();
	
	//if (matchList.size() < tileSizes.size()) 
		//throw InvalidTargetException("Detected nested loop contains less loops than the provided tiling sizes");

	//VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.tiling'";
	//utils::Timer t("transform.polyhedral.loop.tiling");

	//// Build the list of transformations to perform mult-dimensional tiling to this loop stmt
	//std::vector<TransformationPtr> transList;
	//size_t pos=0;
	//for_each(tileSizes, [&] (const unsigned& cur) { 
		//transList.push_back( makeLoopStripMining( pos, cur ) );
		 //// every time we strip mine, a new loop is inserted, therefore we skip to the next one with a step 2
		//for (size_t idx=pos; idx>pos/2; --idx) {
			//transList.push_back( makeLoopInterchange( idx-1, idx ) );
		//}
		//pos+=2;
	//});

	//transform::Pipeline p(transList);
	//VLOG(1) << "Built transformtion for tiling: " << std::endl << p;

	//core::NodePtr&& transformedIR = p.apply(target);	
	
	//t.stop();
	//VLOG(1) << t;
	//VLOG(1) << "//@~ polyhedral.loop.tiling Done";

	//assert( transformedIR && "Generated code for loop fusion not valid" );
	//// std::cout << *transformedIR << std::endl;
	//return transformedIR;
//}

namespace {

	LoopTiling::TileVect extractTileVec(const parameter::Value& value) {
		const std::vector<parameter::Value> tiles = parameter::getValue< std::vector<parameter::Value> >( value );
		LoopTiling::TileVect vect;
		for_each(tiles, [&](const parameter::Value& cur) {
			vect.push_back(parameter::getValue<unsigned>(cur));
		});
		return vect;
	}

	parameter::Value encodeTileVec(const LoopTiling::TileVect& tiles) {
		vector<parameter::Value> values;
		for_each(tiles, [&](unsigned cur) {
			values.push_back(parameter::makeValue(cur));
		});
		return values;
	}

}

LoopTiling::LoopTiling(const parameter::Value& value)
	: Transformation(LoopTilingType::getInstance(), value), tileSizes(extractTileVec(value)) {}

LoopTiling::LoopTiling(const TileVect& tiles)
	: Transformation(LoopTilingType::getInstance(), encodeTileVec(tiles)), tileSizes(tiles) { }

core::NodePtr LoopTiling::apply(const core::NodePtr& target) const {

	// Match a non-perfectly nested loops
	TreePatternPtr&& pattern = 
		rT ( 
			var("loop", irp::forStmt( any, any, any, any, aT(recurse) | aT(!irp::forStmt() ) ))
		);
	auto&& match = pattern->matchPointer( target );

	if (!match || !match->isVarBound("loop")) {
		throw InvalidTargetException("Invalid application point for loop  tiling");
	}

	auto&& matchList = match->getVarBinding("loop").getList();
	
	if (matchList.size() < tileSizes.size()) 
		throw InvalidTargetException("Detected nested loop contains less loops than the provided tiling sizes");
	
	core::NodeManager& mgr = target->getNodeManager();
	
	// make a copy of the polyhedral model associated to this node so that transformations are only
	// applied to the copy and not reflected into the original region 
	Scop oScop = extractScopFrom(target);
	Scop tScop(oScop.getIterationVector(), oScop.getStmts());

	IterationVector& iterVec = tScop.getIterationVector();

	VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.tiling'";
	utils::Timer t("transform.polyhedral.loop.tiling");

	// Build the list of transformations to perform mult-dimensional tiling to this loop stmt
	core::VariableList tileIters, loopIters;
	size_t pos=0;
	for_each(tileSizes, [&] (const unsigned& cur) {

		core::ForStmtPtr forStmt = 
			static_pointer_cast<const core::ForStmt>( matchList[ pos++ ] );
		
		loopIters.push_back( forStmt->getDeclaration()->getVariable() );

		tileIters.push_back(
			doStripMine(mgr, tScop, loopIters.back(), forStmt->getStart(), forStmt->getEnd(), cur)
		);
	});

	// LOG(ERROR) << toString(tileIters) << " " << toString(loopIters);
	for(size_t pos1=1; pos1<tileIters.size(); ++pos1) {
		for(size_t pos2=pos1; pos2>0; --pos2) {
			applyUnimodularTransformation<SCHED_ONLY>(
					tScop, 
					makeInterchangeMatrix(iterVec, loopIters[pos2-1], tileIters[pos1])
				);
		}
	}

	Scop oScop2(tScop.getIterationVector(), oScop.getStmts());
	// all the introduced tiling loops must be set to zero
	for_each(tileIters, [&] (const core::VariablePtr& cur) {
		setZeroOtherwise(oScop2, cur);
	});

	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformationValidity(oScop2, tScop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	//LOG(INFO) << tScop;
	core::NodePtr&& transformedIR = tScop.toIR( mgr );	
	
	t.stop();
	VLOG(1) << t;
	VLOG(1) << "//@~ polyhedral.loop.tiling Done";

	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

namespace {

void updateScheduling(const std::vector<StmtPtr>& stmts, const core::VariablePtr& oldIter,  
	 size_t firstSched, size_t& pos) 
{
	for_each(stmts, [&] (const StmtPtr& curr) {
		AffineSystem& sys = curr->getSchedule();
		AffineSystem::iterator saveIt=sys.end(), remIt=sys.begin();
		for(AffineSystem::iterator it = sys.begin(), end = sys.end(); it != end; ++it) {
			int coeff = it->getCoeff(oldIter);
			if(coeff != 0) {
				saveIt = it+1;
				break;
			}
			remIt = it;
		}
		assert(saveIt != sys.end());
		saveIt->setCoeff(Constant(), pos++);
		if(remIt != sys.end() && remIt != saveIt) {
			remIt->setCoeff(Constant(), firstSched);	
		}

	} );
}

} // end anonymous namespace

//=================================================================================================
// Loop Fusion
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopFusion::LoopFusion(const parameter::Value& value)
	: Transformation(LoopFusionType::getInstance(), value), loopIdxs(extractTileVec(value)) {
	if (loopIdxs.empty()) {
		throw InvalidParametersException("Loop indices for fusion must not be empty!");
	}
}

LoopFusion::LoopFusion(const LoopIndexVect& idxs) : 
	Transformation(LoopFusionType::getInstance(), encodeTileVec(idxs)), loopIdxs(idxs) {
	if (loopIdxs.empty()) {
		throw InvalidParametersException("Loop indices for fusion must not be empty!");
	}
}

core::NodePtr LoopFusion::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	TreePatternPtr pattern = 
		aT(irp::compoundStmt(
			*( irp::forStmt( var("iter"), any, any, any, any ) | any )
		));

	auto&& match = pattern->matchPointer( target );
	if (!match || !match->isVarBound("iter")) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	auto&& matchList = match->getVarBinding("iter").getList();
	
	if (matchList.size() < loopIdxs.size())
		throw InvalidTargetException("Not enough loops inside compound statement");

	for_each(loopIdxs, [&](const unsigned& idx) { 
		if (matchList.size() <= idx){ 
			std::ostringstream ss;
			ss << "Could not find loop index " << idx << " inside this block";
			throw InvalidTargetException( ss.str() );
		}
	});
	
	// The application point of this transformation satisfies the preconditions, continue
	Scop scop = extractScopFrom( target );
	Scop oScop = scop;

	core::VariableList iters;
	for_each(loopIdxs, [&](const unsigned& idx) { 
		iters.push_back( core::static_pointer_cast<const core::Variable>(matchList[idx]) );
		assert( iters.back() && "Induction variable for loop with index not valid" );
	});
		
	assert( !iters.empty() );
	std::vector<StmtPtr>&& loopStmt1 = getLoopSubStatements(scop, iters[0]);

	// we schedule the fused loop at the same position of the first loop being fused (maybe this
	// could be a parameter of the transformation as the loop could be schedule at the position of
	// the second loop).
	size_t schedPos = 0;
	assert(!loopStmt1.empty() && "Trying to fuse loop containing no statements");
	AffineSystem& sys = loopStmt1.front()->getSchedule();
	AffineSystem::iterator saveIt = sys.begin();
	for(AffineSystem::iterator it = sys.begin(), end = sys.end(); it != end; ++it) {
		if(it->getCoeff(iters[0]) != 0) {
			if(saveIt != it) { schedPos = saveIt->getCoeff(Constant()); }
			break;
		}
		saveIt = it;
	}

	size_t pos = 0;
	updateScheduling(loopStmt1, iters[0], schedPos, pos);

	// Update the schedule of all the statements inside the loops selected to be fused together
	for_each(iters, [&](const core::VariablePtr& idx) {
		updateScheduling(getLoopSubStatements(scop, idx), idx, schedPos, pos);
	});

	Scop oScop2(scop.getIterationVector(), oScop.getStmts());
	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformationValidity(oScop2, scop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

//=================================================================================================
// Loop Fission
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopFission::LoopFission(const parameter::Value& value)
	: Transformation(LoopFissionType::getInstance(), value), stmtIdxs(extractTileVec(value)) {}

LoopFission::LoopFission(const StmtIndexVect& idxs) : 
	Transformation(LoopFissionType::getInstance(), encodeTileVec(idxs)), stmtIdxs(idxs) { }

core::NodePtr LoopFission::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// Exactly match a single loop statement 
	if (target->getNodeType() != core::NT_ForStmt) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	const core::ForStmtPtr& forStmt = core::static_pointer_cast<const core::ForStmt>( target );
	// The application point of this transformation satisfies the preconditions, continue
	Scop scop = extractScopFrom( forStmt );
	Scop oScop = scop;

	// chcek whether the indexes for the split refer to concrete statements inside this loop or they
	// are out of bounds 
	for_each(stmtIdxs, [&](const unsigned& idx) { 
		if ( idx > scop.size() ) {
			throw InvalidTargetException("Loop statement contains not enough statements");
		}
	});

	const core::VariablePtr& iter = forStmt->getDeclaration()->getVariable();
	std::vector<StmtPtr>&& loopStmts = getLoopSubStatements(scop, iter);

	size_t schedPos = 0;

	AffineSystem& sys = loopStmts.front()->getSchedule();
	AffineSystem::iterator saveIt = sys.begin();
	for(AffineSystem::iterator it = sys.begin(), end = sys.end(); it != end; ++it) {
		if(it->getCoeff(iter) != 0) {
			if(saveIt != it) { schedPos = saveIt->getCoeff(Constant()); }
			break;
		}
		saveIt = it;
	}

	for(size_t idx=0; idx<stmtIdxs.size(); ++idx) {
		size_t pos = 0;
		// schedule the statements between [idx, idx-1) in different loop
		for(size_t stmt=stmtIdxs[idx]; stmt < ((idx<stmtIdxs.size()-1)?stmtIdxs[idx+1]:scop.size()); stmt++) {
			AffineSystem& schedule = loopStmts[stmt]->getSchedule();
			AffineSystem::iterator saveIt = schedule.begin(), it = schedule.begin(), end = schedule.end();
			for(; it != end; ++it) {
				if(it->getCoeff(iter) != 0) {
					break;
				}
				saveIt = it;
			}

			assert( it != saveIt && saveIt != schedule.end());
			saveIt->setCoeff(poly::Constant(), ++schedPos);
			(++it)->setCoeff(poly::Constant(), ++pos);
		}
	}

	Scop oScop2(scop.getIterationVector(), oScop.getStmts());
	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformationValidity(oScop2, scop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

//=================================================================================================
// Loop Optimal 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopReschedule::LoopReschedule(const parameter::Value& value)
	: Transformation(LoopRescheduleType::getInstance(), value) {}

LoopReschedule::LoopReschedule() : Transformation(LoopRescheduleType::getInstance(), parameter::emptyValue) {}

core::NodePtr LoopReschedule::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// The application point of this transformation satisfies the preconditions, continue
	Scop scop = extractScopFrom( target );
	
	// cout << "Applying reschedule" << std::endl;
	// We add a compound statement in order to avoid wrong composition of transformations 
	core::CompoundStmtPtr&& transformedIR = 
		builder.compoundStmt( core::static_pointer_cast<const core::Statement>(scop.optimizeSchedule( mgr )) );

	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *target << std::endl;
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

//=================================================================================================
// Loop Parallelize
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopParallelize::LoopParallelize(const parameter::Value& value)
	: Transformation(LoopParallelizeType::getInstance(), value) {}

LoopParallelize::LoopParallelize() : Transformation(LoopParallelizeType::getInstance(), parameter::emptyValue) {}

core::NodePtr LoopParallelize::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// Exactly match a single loop statement 
	if (target->getNodeType() != core::NT_ForStmt) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	const core::ForStmtPtr& forStmt = core::static_pointer_cast<const core::ForStmt>( target );
	// The application point of this transformation satisfies the preconditions, continue
	Scop&& scop = extractScopFrom( forStmt );
	if (!scop.isParallel()) {
		throw InvalidTargetException("Loop carries dependencies, cannot be parallelized");
	}
	return builder.pfor(forStmt);
}

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 

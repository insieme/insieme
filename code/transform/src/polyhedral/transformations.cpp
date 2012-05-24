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

#include "insieme/transform/polyhedral/transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/transform/filter/standard_filter.h"

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/utils/timer.h"

namespace insieme { namespace transform { namespace polyhedral {

using namespace analysis;
using namespace analysis::polyhedral;

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
	return region.getScop();
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

	core::VariablePtr src = matchList[srcIdx].as<core::VariablePtr>();
	core::VariablePtr dest = matchList[destIdx].as<core::VariablePtr>();

	assert( iterVec.getIdx(src) != -1 && "Index for Source Loop is invalid");
	assert( iterVec.getIdx(dest) != -1 && "Index for Destination Loop is invalid");
	applyUnimodularTransformation<SCHED_ONLY>(transfScop, makeInterchangeMatrix(iterVec, src, dest));

	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformedSchedule(origScop, transfScop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}
	core::NodePtr&& transformedIR = transfScop.toIR( target->getNodeManager() );	
	
	t.stop();
	VLOG(1) << t;
	VLOG(1) << "//@ polyhedral.loop.interchange Done";
	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	return transformedIR;
}

TransformationPtr makeLoopInterchange(size_t idx1, size_t idx2) {
	return std::make_shared<LoopInterchange>(idx1, idx2);
}

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

	core::VariablePtr idx = matchList[loopIdx].as<core::VariablePtr>();

	core::ForStmtPtr forStmt = static_pointer_cast<const core::ForStmt>(
			(loopIdx == 0) ? match->getRoot() :
			match->getVarBinding("loop").getList()[loopIdx]
		); 

	assert(forStmt && "ForStmt not matched");

	VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.stripmining'";
	utils::Timer t("transform.polyhedral.loop.stripmining");
	
	doStripMine(mgr, scop, idx, forStmt->getAnnotation(scop::ScopRegion::KEY)->getDomainConstraints(), tileSize);

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
//core::NodePtr LoopTiling::apply(const core::NodePtr& target) const {

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

	//TransformationPtr p = makePipeline(transList);
	//VLOG(1) << "Built transformtion for tiling: " << std::endl << p;

	//core::NodePtr&& transformedIR = p->apply(target);	
	
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

LoopTiling::LoopTiling(const parameter::Value& value) : 
	Transformation(LoopTilingType::getInstance(), value), 
	tileSizes(extractTileVec( parameter::getValue<std::vector<parameter::Value>>(value,0) )),
	idxs( extractTileVec( parameter::getValue<std::vector<parameter::Value>>(value,1) ) ) 
{
	if (tileSizes.empty()) 
		throw InvalidParametersException("Tile-size vector must not be empty!");
}

LoopTiling::LoopTiling(const TileVect& tiles, const LoopIndexVect& idxs) : 
	Transformation(LoopTilingType::getInstance(), 
			parameter::combineValues(encodeTileVec(tiles), encodeTileVec(idxs))), 
	tileSizes(tiles),
	idxs(idxs)
{
	if (tileSizes.empty()) throw InvalidParametersException("Tile-size vector must not be empty!");
}

core::NodePtr LoopTiling::apply(const core::NodePtr& target) const {

	// find the application point for the transformation
	core::NodePtr trg = target;
	if (!idxs.empty()) {
		// we have to pick the loop based on the provided indexes
		std::vector<core::NodeAddress>&& f = filter::pickLoop(idxs)(target);
		if (f.empty()) {
			throw InvalidTargetException("Invalid application point for loop tiling: not such loop index");
		}
		trg = f.front().getAddressedNode();
	}
	
	// Match a non-perfectly nested loops
	TreePatternPtr&& pattern = 
		rT ( 
			var("loop", irp::forStmt( any, any, any, any, aT(recurse) | aT(!irp::forStmt() ) ))
		);
	auto&& match = pattern->matchPointer( trg );

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

	LOG(INFO) << oScop;

	Scop tScop(oScop.getIterationVector(), oScop.getStmts());

	IterationVector& iterVec = tScop.getIterationVector();

	VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.tiling'";
	utils::Timer t("transform.polyhedral.loop.tiling");

	// Build the list of transformations to perform mult-dimensional tiling to this loop stmt
	core::VariableList tileIters, loopIters;
	size_t pos=0;
	for_each(tileSizes, [&] (const unsigned& cur) {

		core::ForStmtPtr forStmt = matchList[ pos++ ].as<core::ForStmtPtr>();
		loopIters.push_back( forStmt->getDeclaration()->getVariable() );

		tileIters.push_back(
			doStripMine(mgr, tScop, loopIters.back(), forStmt->getAnnotation(scop::ScopRegion::KEY)->getDomainConstraints(), cur)
		);
	});

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
	if ( !checkTransformedSchedule(oScop2, tScop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	LOG(INFO) << tScop;
	core::NodePtr&& transformedIR = tScop.toIR( mgr );	
	
	t.stop();
	VLOG(1) << t;
	VLOG(1) << "//@~ polyhedral.loop.tiling Done";

	assert( transformedIR && "Generated code for loop fusion not valid" );
	// std::cout << *transformedIR << std::endl;
	return transformedIR;
}

//=================================================================================================
// Loop Fusion
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopFusion::LoopFusion(const parameter::Value& value)
	: Transformation(LoopFusionType::getInstance(), value), loopIdxs(extractTileVec(value)) {
	if (loopIdxs.size() <= 1u) {
		throw InvalidParametersException("Loop indices for fusion must at least include 2 elements!");
	}
}

LoopFusion::LoopFusion(const LoopIndexVect& idxs) : 
	Transformation(LoopFusionType::getInstance(), encodeTileVec(idxs)), loopIdxs(idxs) {
	if (loopIdxs.size() <= 1u) {
		throw InvalidParametersException("Loop indices for fusion must at least include 2 elements!");
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
	
	core::VariableList iters;
	for_each(loopIdxs, [&](const unsigned& idx) { 
		iters.push_back( matchList[idx].as<core::VariablePtr>() );
		assert( iters.back() && "Induction variable for loop with index not valid" );
	});

	assert( !iters.empty() );

	// The application point of this transformation satisfies the preconditions, continue
	Scop&& scop = extractScopFrom( target );

	// save the SCoP in order to check for validity of the transformation
	Scop saveScop = scop;

	// Apply fusion
	doFuse(scop, iters);

	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformedSchedule(saveScop,scop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	return transformedIR;
}

//=================================================================================================
// Loop Fission
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopFission::LoopFission(const parameter::Value& value)
	: Transformation(LoopFissionType::getInstance(), value), stmtIdxs(extractTileVec(value)) {
	if (stmtIdxs.empty()) 
		throw InvalidParametersException("Fission of loops requires at least one splitting point!");
}

LoopFission::LoopFission(const StmtIndexVect& idxs) : 
	Transformation(LoopFissionType::getInstance(), encodeTileVec(idxs)), stmtIdxs(idxs) {
	if (stmtIdxs.empty()) 
		throw InvalidParametersException("Fission of loops requires at least one splitting point!");
}

core::NodePtr LoopFission::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();
	core::IRBuilder builder(mgr);

	// Exactly match a single loop statement 
	if (target->getNodeType() != core::NT_ForStmt) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	const core::ForStmtPtr& forStmt = target.as<core::ForStmtPtr>();

	// The application point of this transformation satisfies the preconditions, continue
	Scop&& scop = extractScopFrom( forStmt );

	// Save a copy of the scop for later check the validity of the transformation
	Scop saveScop = scop;

	// chcek whether the indexes for the split refer to concrete statements inside this loop or they
	// are out of bounds 
	for_each(stmtIdxs, [&](const unsigned& idx) { 
		if ( idx > scop.size() ) {
			throw InvalidTargetException("Loop statement contains not enough statements");
		}
	});

	doSplit(scop, forStmt->getDeclaration()->getVariable(), stmtIdxs);

	// The original scop is in origScop, while the transformed one is in transScop
	if ( !checkTransformedSchedule(saveScop, scop) ) {
		throw InvalidTargetException("Dependence prevented the application of the transformation");
	}

	core::NodePtr&& transformedIR = scop.toIR( mgr );	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	return transformedIR;
}

//=================================================================================================
// Loop Reschedule
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopReschedule::LoopReschedule(const parameter::Value& value)
	: Transformation(LoopRescheduleType::getInstance(), value) {}

LoopReschedule::LoopReschedule() : 
	Transformation(LoopRescheduleType::getInstance(), parameter::emptyValue) {}

core::NodePtr LoopReschedule::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();

	// The application point of this transformation satisfies the preconditions, continue
	Scop scop = extractScopFrom( target );

	// We add a compound statement in order to avoid wrong composition of transformations 
	core::CompoundStmtPtr&& transformedIR = 
		core::IRBuilder(mgr).compoundStmt( scop.optimizeSchedule( mgr ).as<core::StatementPtr>() );

	assert( transformedIR && "Generated code for loop fusion not valid" );
	return transformedIR;
}

//=================================================================================================
// Loop Stamping
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoopStamping::LoopStamping(const parameter::Value& value)
	: Transformation(LoopStampingType::getInstance(), value),
	  tileSize(parameter::getValue<unsigned>(value,0)), 
	  idx( extractTileVec( parameter::getValue<std::vector<parameter::Value>>(value,1)) ) { }

LoopStamping::LoopStamping(const unsigned& tileSize, const LoopStamping::LoopIndexVect& index)
	: Transformation(LoopStampingType::getInstance(), 
	  parameter::combineValues(tileSize, encodeTileVec(index)) ),
	  tileSize(tileSize), idx(index) { }

core::NodePtr LoopStamping::apply(const core::NodePtr& target) const {
	core::NodeManager& mgr = target->getNodeManager();

	if (target->getNodeType() != core::NT_ForStmt) {
		throw InvalidTargetException("Invalid application point for loop stamping: outer stmt must be a loop");
	}

	core::ForStmtPtr outerStmt = target.as<core::ForStmtPtr>();

	// The application point of this transformation satisfies the preconditions, continue
	Scop scop = extractScopFrom( target );
	
	core::ForStmtPtr forStmt = outerStmt;

	if (!idx.empty()) {
		std::vector<core::NodeAddress>&& f = filter::pickLoop(idx)(target);
		if (f.empty()) {
			throw InvalidTargetException("Invalid application point for loop stamping: not such loop index");
		}
		forStmt = f.front().getAddressedNode().as<core::ForStmtPtr>();
	}
	
	assert(forStmt && "Loop stamping must be applied to a forstmt");
	core::VariablePtr iter = forStmt->getDeclaration()->getVariable();

	std::vector<StmtPtr> stampedStmts = getLoopSubStatements(scop, iter);

	core::arithmetic::Formula rangeSize = 
		core::arithmetic::toFormula(forStmt->getEnd()) - core::arithmetic::toFormula(forStmt->getStart());

	std::pair<AffineConstraintPtr, core::ExpressionPtr>&& ret = stampFor(mgr, scop, iter, rangeSize, tileSize);

	unsigned split = scop.size();
	// Duplicate all the statements in the scop
	for (size_t idx=stampedStmts.front()->getId(), end=stampedStmts.back()->getId(); idx<=end; ++idx) {
		AffineFunction f(scop.getIterationVector(), 
				-(core::arithmetic::toFormula(forStmt->getEnd())-core::arithmetic::toFormula(ret.second))
			);
		f.setCoeff(iter, 1);
		dupStmt(scop, idx, AffineConstraint(f, utils::ConstraintType::GE) and ret.first);

		scop[idx].getDomain() &= IterationDomain( AffineConstraint(f, utils::ConstraintType::LT) and ret.first );
	}

	doSplit(scop, outerStmt->getDeclaration()->getVariable(), { split });

	LOG(DEBUG) << scop;

	core::NodePtr transformedIR = scop.toIR(mgr);
	assert( transformedIR && "Generated code for loop fusion not valid" );
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

	// Exactly match a single loop statement 
	if (target->getNodeType() != core::NT_ForStmt) {
		throw InvalidTargetException("Invalid application point for loop strip mining");
	}

	const core::ForStmtPtr& forStmt = target.as<core::ForStmtPtr>();
	// The application point of this transformation satisfies the preconditions, continue
	Scop&& scop = extractScopFrom( forStmt );
	if (!scop.isParallel(mgr)) {
		throw InvalidTargetException("Loop carries dependencies, cannot be parallelized");
	}
	return core::IRBuilder(mgr).pfor(forStmt);
}


//=================================================================================================
// Region Strip Mining
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RegionStripMining::RegionStripMining(const parameter::Value& value)
	: Transformation(RegionStripMiningType::getInstance(), value),
	  tileSize(parameter::getValue<unsigned>(value)) {}

RegionStripMining::RegionStripMining(unsigned tileSize)
	: Transformation(LoopInterchangeType::getInstance(), parameter::makeValue(tileSize) ),
	  tileSize(tileSize) { }

core::NodePtr RegionStripMining::apply(const core::NodePtr& target) const {

	if (tileSize < 2 ) {
		throw InvalidTargetException("Tile size for Strip mining must be >= 2");
	}

	// We can only strip a statement which spawn an iteration domain. This is not only restricted to loops
	// but also to statements which define subranges on one or multiple variables. 
	//
	// If the strip is applied to a compound statement, then all the sub statements will be stripped and 
	// then fused into a single stripped domain
	
	// Region strip mining applied to a for stmt resolves as a normal strip mining 
	if (target->getNodeType() == core::NT_ForStmt) {
		return makeLoopStripMining(0, tileSize)->apply(target);
	}

	core::NodeManager& mgr = target->getNodeManager();

	// Now we need to make sure the thing we are handling is a SCoP... otherwise makes no sense to continue
	Scop scop = extractScopFrom( target );

	IterationVector& iv = scop.getIterationVector();
	// Region strip mining applied to something which is not a call expression which spawns a range
	// 	e.g. calls to functions for which semantic informations are provided 

	for_each(scop, [&](StmtPtr& curStmt) {

		core::StatementPtr stmt = curStmt->getAddr().getAddressedNode();

		std::vector<core::VariablePtr> iters = getOrderedIteratorsFor( curStmt->getSchedule() );

		if (iters.empty() && stmt->getNodeType() == core::NT_CallExpr) {
			core::CallExprPtr callExpr = stmt.as<core::CallExprPtr>();

			unsigned ranges = curStmt->getSubRangeNum();
			assert(ranges == 1 && "Multiple ranges not supported");
		
			// Extract the variable which should be stripped 
			core::VariablePtr var;
			for_each(curStmt->access_begin(), curStmt->access_end(), [&](const AccessInfoPtr& cur) {
				if ( cur->hasDomainInfo() ) {
					assert(!var && "Variable already set");
					var = getOrderedIteratorsFor(cur->getAccess()).front();
					curStmt->getSchedule().append( AffineFunction(iv, core::arithmetic::Formula(var)) );

					doStripMine(mgr, scop, var, curStmt->getDomain() && cur->getDomain(), tileSize);
				}
			});
			return;
		} 
		// In the other cases we need to make sure that the statment we are stripping inside a for loop 
		if (iters.empty()) {
			throw InvalidTargetException("Region contains statements which cannot be stripped");
		}
		doStripMine(mgr, scop, iters.front(), curStmt->getDomain(), tileSize);
	});
	
	LOG(INFO) << "BEFORE FUSION" << scop;

	core::NodePtr transformedIR = core::IRBuilder(mgr).compoundStmt( scop.toIR( mgr ).as<core::StatementPtr>() );	
	Scop scop2 = extractScopFrom( transformedIR );

	core::VariableList strip_iters;
	for_each(scop2, [&](StmtPtr& curStmt) {
	//			LOG(INFO) << toString(getOrderedIteratorsFor( curStmt->getSchedule() ));
	//			LOG(INFO) << *curStmt->getAddr().getAddressedNode();
				strip_iters.push_back(getOrderedIteratorsFor( curStmt->getSchedule() ).front());
			});
	
	//core::CompoundStmtPtr comp = transformedIR.as<core::CompoundStmtPtr>();
	///std::vector<unsigned> indexes(comp->getStatements().size());
	//indexes.front() = 0;
	
	//std::transform(indexes.begin(), indexes.end()-1, indexes.begin()+1, std::bind(std::plus<int>(), std::placeholders::_1, 1));

	// now apply fuse on the transformed IR 
	//TransformationPtr tr = makeLoopFusion(indexes);
	//transformedIR = tr->apply(transformedIR);
	
	doFuse(scop2, strip_iters);

	transformedIR = core::IRBuilder(mgr).compoundStmt( scop2.toIR( mgr ).as<core::StatementPtr>() );	
	assert( transformedIR && "Generated code for loop fusion not valid" );
	// LOG(DEBUG) << *transformedIR;
	return transformedIR;
}

} } } // end insieme::transform::polyhedral namespace 

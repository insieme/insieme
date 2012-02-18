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
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

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

	auto&& ctx = makeCtx();
	auto&& deps = orig.computeDeps(ctx);

	// std::cout << *deps << std::endl;
	
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
			isl_union_map_apply_range( isl_union_map_reverse(tSched->getIslObj()), deps->getIslObj() ), 
			tSched->getIslObj() 
		);
	
	// isl_union_set* deltas = isl_union_map_deltas( isl_union_map_copy(umao) );
	// SetPtr<> set(*ctx, deltas);
	// std::cout << *set<< std::endl;

	//
	// MapPtr<> map1(*ctx, isl_union_map_copy(umao));
	// std::cout << "M1 = " << *map1 << std::endl;

	// LOG(DEBUG) << "DELTAS:" << std::endl;
	// printIslSet(std::cout, ctx.getRawContext(), deltas);
	// std::cout << std::endl;
	// printIslMap(std::cout, ctx.getRawContext(), umao);
	
	// std::cout << std::endl;
	
	// std::cout << "NON MAP"<< std::endl;
	isl_union_map* nonValidDom = 
		isl_union_set_lex_gt_union_set( 
				isl_union_map_range(tSched->getIslObj()), 
				isl_union_map_range(tSched->getIslObj()) 
			);
	
	// MapPtr<> map2(*ctx, isl_union_map_copy(nonValidDom));
	// std::cout << "M2 = " << *map2 << std::endl;
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

	core::VariablePtr src = matchList[srcIdx].as<core::VariablePtr>();
	core::VariablePtr dest = matchList[destIdx].as<core::VariablePtr>();

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

// Returns the constrait type which correspond to the logic negation of a given constraint type
ConstraintType negation(const ConstraintType& c) {
	switch(c) {
		case ConstraintType::EQ: return ConstraintType::NE;
		case ConstraintType::NE: return ConstraintType::EQ;

		case ConstraintType::LT: return ConstraintType::GE;
		case ConstraintType::LE: return ConstraintType::GT;

		case ConstraintType::GT: return ConstraintType::LE;
		case ConstraintType::GE: return ConstraintType::LT;
		default:
			assert(false);
	}
}



// Analyze a generic constraint and extract the lowerbound for a specific variable
using namespace insieme::analysis::poly;

core::VariablePtr doStripMine(core::NodeManager& 			mgr, 
							 Scop& 							scop, 
							 const core::VariablePtr& 		loopIter, 
							 const poly::IterationDomain& 	dom,
							 int 							tileSize ) 
{

	LOG(INFO) << "Start strip mine";
	LOG(INFO) << scop;
	LOG(INFO) << scop.toIR(mgr);

	core::IRBuilder builder(mgr);
	// check whether the indexes refers to loops 
	IterationVector& iterVec = scop.getIterationVector();

	// Add a new loop and schedule it before the indexed loop 
	core::VariablePtr&& newIter = builder.variable(mgr.getLangBasic().getInt4());
	addTo(scop, newIter);
	// Add an existential variable used to created a strided domain
	core::VariablePtr&& strideIter = builder.variable(mgr.getLangBasic().getInt4());
	addTo(scop, poly::Iterator(strideIter, true));

	AffineConstraintPtr domain = cloneConstraint(iterVec, dom.getConstraint());

	std::vector<std::vector<AffineConstraintPtr>>&& conjunctions = getConjuctions(toDNF(domain));
	
	std::vector<std::vector<AffineConstraintPtr>> lbs(1), ubs(1);

	for_each(conjunctions, [&](const std::vector<AffineConstraintPtr>& cur) {
		for_each(cur, [&](const AffineConstraintPtr& cur) {
			// this is either a raw or a negation 
			if (cur->getCombinerType() == utils::CT_RAW) {
				const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);
				const AffineConstraint& c = rc.getConstraint();
				AffineFunction f(iterVec, c.getFunction());

				int coeff = f.getCoeff(loopIter);
				if (coeff == 0) { return; }
			
				f.setCoeff(loopIter, 0);
				f.setCoeff(newIter, coeff);

				if (c.getType() == ConstraintType::EQ || c.getType() == ConstraintType::NE) {
					lbs.back().push_back( makeCombiner(AffineConstraint(f, c.getType())));
					//ubs.back().push_back( makeCombiner(AffineConstraint(f, c.getType())));
				} else {
					if (coeff > 0) { 
						lbs.back().push_back( makeCombiner(AffineConstraint(f, c.getType()))); 
					}
					if (coeff < 0) { 
						ubs.back().push_back( makeCombiner(AffineConstraint(f, c.getType()))); 
					}
				}
				return;
			}

			if (cur->getCombinerType() == utils::CT_NEG) {
				const NegAffineConstraint& rc = static_cast<const NegAffineConstraint&>(*cur);
				const AffineConstraint& c = 
					static_cast<const RawAffineConstraint&>(*rc.getSubConstraint()).getConstraint();
				AffineFunction f(iterVec, c.getFunction());
				
				int coeff = f.getCoeff(loopIter);
				
				if (coeff == 0) { return; }
			
				f.setCoeff(loopIter, 0);
				f.setCoeff(newIter, coeff);

				if (c.getType() == ConstraintType::EQ || c.getType() == ConstraintType::NE) {
					lbs.back().push_back( makeCombiner(AffineConstraint(f, negation(c.getType()))));
					//ubs.back().push_back( makeCombiner(AffineConstraint(f, negation(c.getType()))));
				} else {
					if (coeff < 0) { lbs.back().push_back( makeCombiner(AffineConstraint(f, negation(c.getType())))); }
					if (coeff > 0) { ubs.back().push_back( makeCombiner(AffineConstraint(f, negation(c.getType())))); }
				}
				return;
			}

			assert(false);
		});
		lbs.push_back(std::vector<AffineConstraintPtr>());
		ubs.push_back(std::vector<AffineConstraintPtr>());
	});

	// EXTRACT LOWER BOUND
	LOG(INFO) << "Original" << *domain;
	


	for_each(ubs, [&] (const std::vector<AffineConstraintPtr>& cur) {
			AffineConstraintPtr sub;
			for_each(cur, [&](const AffineConstraintPtr& cur) {
					std::cout << *cur << std::endl;
				});
			std::cout << "\n";
		});

	scheduleLoopBefore(scop, loopIter, newIter);

	// Set the new iterator to 0 for all the statements which are not scheduled under this loop 
	setZeroOtherwise(scop, newIter);

	AffineConstraintPtr ub;
	AffineConstraintPtr lbb;
	AffineConstraintPtr rest;
	AffineConstraintPtr stride;

	std::vector<std::vector<AffineConstraintPtr>> lb(1);

	for_each(lbs, [&] (const std::vector<AffineConstraintPtr>& cur) {
		AffineConstraintPtr sub;

		for_each(cur, [&](const AffineConstraintPtr& cur) {
				// detect whether this constraint is an equality
				const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);
				if (rc.getConstraint().getType() != ConstraintType::EQ) {
					lb.back().push_back(cur);
				} else {
					AffineFunction f(iterVec, rc.getConstraint().getFunction());
					int coeff = f.getCoeff(newIter);
					f.setCoeff(loopIter, coeff);
					// change the stride variable
					f.setCoeff(newIter, 0);

					stride = makeCombiner( AffineConstraint(f, ConstraintType::EQ));
					return;
				}
				sub = sub ? sub and cur : cur;
			});
		lb.push_back(std::vector<AffineConstraintPtr>());
		rest = rest ? rest or sub : sub;
	});

	for_each(lb, [&] (const std::vector<AffineConstraintPtr>& cur) {
		AffineConstraintPtr sub;
		for_each(cur, [&](const AffineConstraintPtr& cur) {
				// detect whether this constraint is an equality
				const RawAffineConstraint& rc = static_cast<const RawAffineConstraint&>(*cur);
				AffineFunction func(iterVec, rc.getConstraint().getFunction());
				// add the stride
				func.setCoeff(strideIter, -tileSize);
				
				AffineConstraint c(func, ConstraintType::EQ);
				sub = sub ? sub and c : makeCombiner(c);
			});
		lbb = lbb ? lbb or sub : sub;
	});

	for_each(ubs, [&] (const std::vector<AffineConstraintPtr>& cur) {
		AffineConstraintPtr sub;

		for_each(cur, [&](const AffineConstraintPtr& cur) {

				// detect whether this constraint is an equality
				sub = sub ? sub and cur : cur;
			});

		ub = ub ? ub or sub : sub;
	});


	LOG(INFO) << *lbb;
	LOG(INFO) << *rest;
	LOG(INFO) << *ub;

	if (stride)
		LOG(INFO) << *stride;

	// Add a constraint to strip the domain of the tiled loop index 
	//AffineFunction af1(*lbf);
	//af1.setCoeff(newIter, 1);
	//af1.setCoeff(loopIter, 0);
	//af1.setCoeff(strideIter, -tileSize);

	//AffineFunction lb(*lbf );
	//lb.setCoeff(loopIter, 0);
	//lb.setCoeff(newIter, 1);

	//AffineFunction ub(iterVec, *ubf );
	//ub.setCoeff(loopIter, 0);
	//ub.setCoeff(newIter, 1);

	addConstraint(scop, newIter, poly::IterationDomain( 
				//AffineConstraint(af1, ConstraintType::EQ) and 
				lbb and rest  and ub )
			);
 

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
				AffineConstraint(af2) and AffineConstraint(af3, ConstraintType::LT) and stride
			) 
		);

	LOG(INFO) << "After strip mine: " << scop;

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

	core::VariablePtr idx = matchList[loopIdx].as<core::VariablePtr>();

	core::ForStmtPtr forStmt = static_pointer_cast<const core::ForStmt>(
			(loopIdx == 0) ? match->getRoot() :
			match->getVarBinding("loop").getList()[loopIdx]
		); 

	assert(forStmt && "ForStmt not matched");

	// RELAXED 
	// if (!core::arithmetic::toFormula(forStmt->getStep()).isOne()) {
	//	throw InvalidTargetException("Cannot tile a loop with step != 1");
	//}

	VLOG(1) << "@~~~ Applying Transformation: 'polyhedral.loop.stripmining'";
	utils::Timer t("transform.polyhedral.loop.stripmining");
	
	doStripMine(mgr, scop, idx, forStmt->getAnnotation(scop::ScopRegion::KEY)->getDomainConstraints(), tileSize);
	
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
	std::cout << *transformedIR << std::endl;
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
	: Transformation(LoopTilingType::getInstance(), value), tileSizes(extractTileVec(value)) {
	if (tileSizes.empty()) throw InvalidParametersException("Tile-size vector must not be empty!");
}

LoopTiling::LoopTiling(const TileVect& tiles)
	: Transformation(LoopTilingType::getInstance(), encodeTileVec(tiles)), tileSizes(tiles) {
	if (tileSizes.empty()) throw InvalidParametersException("Tile-size vector must not be empty!");
}

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

		core::ForStmtPtr forStmt = matchList[ pos++ ].as<core::ForStmtPtr>();
		
		loopIters.push_back( forStmt->getDeclaration()->getVariable() );

		tileIters.push_back(
			doStripMine(mgr, tScop, loopIters.back(), forStmt->getAnnotation(scop::ScopRegion::KEY)->getDomainConstraints(), cur)
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
//		LOG(INFO) << *curr->getAddr().getAddressedNode();
		AffineSystem& sys = curr->getSchedule();
//		LOG(INFO) << "Before " << sys;

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

//		LOG(INFO) << "After :" << sys;

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

	LOG(INFO) << "FUSION: " << scop;
	LOG(INFO) << *scop.toIR(mgr);

	core::VariableList iters;
	for_each(loopIdxs, [&](const unsigned& idx) { 
		iters.push_back( matchList[idx].as<core::VariablePtr>() );
		assert( iters.back() && "Induction variable for loop with index not valid" );
	});

	LOG(INFO) << toString(iters);
		
	assert( !iters.empty() );
	std::vector<StmtPtr>&& loopStmt1 = getLoopSubStatements(scop, iters[0]);

	// we schedule the fused loop at the same position of the first loop being fused (maybe this
	// could be a parameter of the transformation as the loop could be schedule at the position of
	// the second loop).
	size_t schedPos = 0;
	assert(!loopStmt1.empty() && "Trying to fuse loop containing no statements");
	AffineSystem& sys = loopStmt1.front()->getSchedule();
	LOG(INFO) << sys;
	AffineSystem::iterator saveIt = sys.begin();
	for(AffineSystem::iterator it = sys.begin(), end = sys.end(); it != end; ++it) {
		if(it->getCoeff(iters[0]) != 0) {
			if(saveIt != it) { schedPos = saveIt->getCoeff(Constant()); }
			break;
		}
		saveIt = it;
	}
	LOG(INFO) << sys;

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
	: Transformation(LoopFissionType::getInstance(), value), stmtIdxs(extractTileVec(value)) {
	if (stmtIdxs.empty()) throw InvalidParametersException("It is pointless to fuse empty list of loops!");
}

LoopFission::LoopFission(const StmtIndexVect& idxs) : 
	Transformation(LoopFissionType::getInstance(), encodeTileVec(idxs)), stmtIdxs(idxs) {
	if (stmtIdxs.empty()) throw InvalidParametersException("It is pointless to fuse empty list of loops!");
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
		builder.compoundStmt( scop.optimizeSchedule( mgr ).as<core::StatementPtr>() );

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

	const core::ForStmtPtr& forStmt = target.as<core::ForStmtPtr>();
	// The application point of this transformation satisfies the preconditions, continue
	Scop&& scop = extractScopFrom( forStmt );

	if (!scop.isParallel(mgr)) {
		throw InvalidTargetException("Loop carries dependencies, cannot be parallelized");
	}
	return builder.pfor(forStmt);
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

	LOG(INFO) << "TRG " << *target;

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

	LOG(INFO) << scop;

	// Region strip mining applied to something which is not a call expression which spawns a range
	// 	e.g. calls to functions for which semantic informations are provided 

	for_each(scop, [&](StmtPtr& curStmt) {

		core::StatementPtr stmt = curStmt->getAddr().getAddressedNode();
		LOG(INFO) << *stmt;

		std::vector<core::VariablePtr> iters = poly::getOrderedIteratorsFor( curStmt->getSchedule() );
		LOG(INFO) << toString(iters);

		if (iters.empty() && stmt->getNodeType() == core::NT_CallExpr) {
			core::CallExprPtr callExpr = stmt.as<core::CallExprPtr>();
			LOG(INFO) << *callExpr;

			unsigned ranges = curStmt->getSubRangeNum();
			LOG(INFO) << ranges;
			assert(ranges == 1 && "Multiple ranges not supported");
		
			// Extract the variable which should be stripped 
			core::VariablePtr var;
			for_each(curStmt->access_begin(), curStmt->access_end(), [&](const poly::AccessInfoPtr& cur) {
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

	core::NodePtr transformedIR = scop.toIR( mgr );	

	std::vector<unsigned> indexes(scop.size());
	indexes.front() = 0;
	
	std::transform(indexes.begin(), indexes.end(), indexes.begin()+1, std::bind(std::plus<int>(), std::placeholders::_1, 1));
	std::cout << toString(indexes) << std::endl;

	// now apply fuse on the transformed IR 
	TransformationPtr tr = makeLoopFusion(indexes);
	transformedIR = tr->apply(transformedIR);

	assert( transformedIR && "Generated code for loop fusion not valid" );
	std::cout << *transformedIR << std::endl;
	return transformedIR;

	//TreePatternPtr pattern = aT(irp::compoundStmt( *( var("stmt", any) ) ) );
	// Build a transformation sequence where strip mine is applied to each statement inside this SCoP
	//transform::TransformationPtr forAll = 
	//	transform::makeForAll( transform::filter::pattern(pattern, "stmt"), makeRegionStripMining(tileSize) );

	// return forAll->apply(target);

}




} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 

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

#include "insieme/core/forward_decls.h"
#include "insieme/utils/matrix.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"

#include "insieme/transform/catalog.h"

namespace insieme { namespace transform { namespace polyhedral {

/**************************************************************************************************
 * Polyhedral Transformations 
 *************************************************************************************************/

template <class TransTy>
struct Transformation : public transform::Transformation {

	Transformation(const TransformationType& type, const parameter::Value& params)
		: transform::Transformation(type, params) {}

	bool operator==(const transform::Transformation& other) const {
		if (const TransTy* otherPtr = dynamic_cast<const TransTy*>(&other) ) {
			return static_cast<const TransTy*>(this)->operator==(*otherPtr);
		}
		return false;
	}

	bool checkPreCondition(const core::NodePtr& target) const { 
		return true; // FIXME
	}

	bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const { 
		return true; // FIXME
	}

};

/*************************************************************************************************
 * LoopInterchange: this is the implementation of loop interchange based on the polyhedral model. 
 * The transformation is applyied from a determined loop level. The transformation will search for
 * the induction variable of the first N perfectly nested loops and will apply interchange between
 * index src and dest under the assumption that depth of this loop nest is greater than dest ( or
 * src). 
 *
 * In the case the assumption is not satisfied, an exception is thrown. 
 *
 */
class LoopInterchange : public Transformation<LoopInterchange> {

	unsigned srcIdx, destIdx;

public:

	LoopInterchange(const parameter::Value& value);
	LoopInterchange(unsigned src, unsigned dest);

	core::NodePtr apply(const core::NodePtr& target) const;
	
	bool operator==(const LoopInterchange& other) const {
		return srcIdx == other.srcIdx && destIdx == other.destIdx;
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.Interchange [" << srcIdx << "," << destIdx << "]"; 
	}
};

/**
 * Factory for the loop interchange transformation. It specifies the type and number of parameters
 * which are required by the transformations in order to be inspectable by the optimizer component
 */
TRANSFORMATION_TYPE(
	LoopInterchange,
	"Implementation of loop interchange based on the polyhedral model",
	parameter::tuple(
		parameter::atom<unsigned>("The source index of the loop being interchanged"),
		parameter::atom<unsigned>("The destination index of the loop being interchanged")
	)
);

/**
 * Utility method to create a loop interchange transformation which when applied to a loop nest
 * exchange the loops at index idx1 with the loop at idx2
 */
TransformationPtr makeLoopInterchange(size_t idx1, size_t idx2);

/**************************************************************************************************
 * LoopStripMining
 */ 
class LoopStripMining : public Transformation<LoopStripMining> {
	
	unsigned loopIdx;
	unsigned tileSize;
public:

	LoopStripMining(const parameter::Value& value);
	LoopStripMining(unsigned idx, unsigned tileSize);

	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const LoopStripMining& other) const {
		return loopIdx == other.loopIdx && tileSize == other.tileSize;
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.StripMining [" << loopIdx << "," << tileSize << "]"; 
	}
};

/**
 * The factory class is responsible to instantiate a transformation given a particular set or
 * parameters
 */
TRANSFORMATION_TYPE(
	LoopStripMining,
	"Implementation of loop strip mining based on the polyhedral model",
	parameter::tuple(
		parameter::atom<unsigned>("The index of the loop being strip minded"),
		parameter::atom<unsigned>("The tiling size")
	)
);

/**
 * Utility method creating strip mining transformation, when applied to a loop nest it strip mine
 * the loop statement at index idx with a tile size which is tileSize
 */
TransformationPtr makeLoopStripMining(size_t idx, size_t tileSize);

/**************************************************************************************************
 * LoopTiling: the loop tiling is obtained by appliend strip mining consecutevely and then
 * interchange the loops
 */

struct LoopTiling: public Transformation<LoopTiling> {

	typedef std::vector<unsigned> TileVect;
	typedef std::vector<unsigned> LoopIndexVect;

	LoopTiling(const parameter::Value& value);
	LoopTiling(const TileVect& tiles, const LoopIndexVect& idxs = LoopIndexVect());

	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const LoopTiling& other) const {
		return std::equal(tileSizes.begin(), tileSizes.end(), other.tileSizes.begin()) &&
				std::equal(idxs.begin(), idxs.end(), other.idxs.begin());
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.Tiling tiles: [" 
			<< join(", ", tileSizes,  [&](std::ostream& out, const unsigned& cur) { out << cur; }) 
			<< "]: path: {"
			<< join(", ", idxs,  [&](std::ostream& out, const unsigned& cur) { out << cur; }) 
			<< " }";
	}

private:
	TileVect tileSizes;
	LoopIndexVect idxs;
};

TRANSFORMATION_TYPE(
	LoopTiling,
	"Implementation of loop tiling based on the polyhedral model",
	parameter::tuple(
		parameter::list("The tiling size for which the statement should be stamped", parameter::atom<unsigned>()),
		parameter::list("The index of the loop to which stamping is applied", parameter::atom<unsigned>() )
	)
);

inline TransformationPtr 
makeLoopTiling(const LoopTiling::TileVect& tiles, const LoopTiling::LoopIndexVect& idxs=LoopTiling::LoopIndexVect()) {
	return std::make_shared<LoopTiling>( tiles, idxs );
}

/**
 * LoopFusion: 
 */
struct LoopFusion : public Transformation<LoopFusion> {

	typedef std::vector<unsigned> LoopIndexVect;

	LoopFusion(const parameter::Value& value);
	LoopFusion(const LoopIndexVect& idxs);

	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const LoopFusion& other) const {
		return loopIdxs.size() == other.loopIdxs.size() && 
			   std::equal( loopIdxs.begin(), loopIdxs.end(), other.loopIdxs.begin() );
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.Fusion " << loopIdxs;
	}
private:
	LoopIndexVect loopIdxs;
};

TRANSFORMATION_TYPE(
	LoopFusion,
	"Implementation of loop fusion based on the polyhedral model",
	parameter::list("The indexes of the loop to be fused", parameter::atom<unsigned>("Loop index"))
);

template <typename ...LoopIdx>
TransformationPtr makeLoopFusion(const LoopIdx&... idxs) {
	return std::make_shared<LoopFusion>( LoopFusion::LoopIndexVect({ idxs... }) );
}

inline TransformationPtr makeLoopFusion( const LoopFusion::LoopIndexVect& loops) {
	return std::make_shared<LoopFusion>( loops );
}

/**
 * LoopFusion: 
 */
struct LoopFission : public Transformation<LoopFission> {

	typedef std::vector<unsigned> StmtIndexVect;

	LoopFission(const parameter::Value& value);
	LoopFission(const StmtIndexVect& idxs);

	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const LoopFission& other) const {
		return stmtIdxs.size() == other.stmtIdxs.size() && 
			   std::equal( stmtIdxs.begin(), stmtIdxs.end(), other.stmtIdxs.begin() );
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.Fission " << stmtIdxs;
	}
private:
	StmtIndexVect stmtIdxs;
};

TRANSFORMATION_TYPE(
	LoopFission,
	"Implementation of loop fission based on the polyhedral model",
	parameter::list("The statements indexes where the split should happen", 
		parameter::atom<unsigned>("Statement indexs"))
);

template <typename ...StmtIdx>
TransformationPtr makeLoopFission(const StmtIdx&... idxs) {
	return std::make_shared<LoopFission>( LoopFission::StmtIndexVect({ idxs... }) );
}

inline TransformationPtr makeLoopFission( const LoopFission::StmtIndexVect& idxs) {
	return std::make_shared<LoopFission>( idxs );
}

/**
 * LoopStamping: 
 */
struct LoopStamping : public Transformation<LoopStamping> {
	
	typedef std::vector<unsigned> LoopIndexVect;

	LoopStamping(const parameter::Value& value);

	LoopStamping(const unsigned& tileSize, const LoopIndexVect& idx);
	
	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const LoopStamping& other) const {
		return tileSize == other.tileSize;
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Loop.Stamping(" << tileSize << ")";
	}
private:
	unsigned tileSize;
	LoopIndexVect idx;
};

TRANSFORMATION_TYPE(
	LoopStamping,
	"Implementation of loop stamping based on the polyhedral model",
	parameter::tuple(
		parameter::atom<unsigned>("The tiling size for which the statement should be stamped"),
		parameter::list("The index of the loop to which stamping is applied", parameter::atom<unsigned>() )
	)
);

template <typename ...LoopIdx>
inline TransformationPtr makeLoopStamping(const unsigned& tileSize, const LoopIdx&... idx) {
	return std::make_shared<LoopStamping>( tileSize, LoopStamping::LoopIndexVect({idx...}));
}

/**
* LoopOptimal: 
*/
struct LoopReschedule : public Transformation<LoopReschedule> {

   LoopReschedule(const parameter::Value& value);
   LoopReschedule();

   core::NodePtr apply(const core::NodePtr& target) const;

   inline bool operator==(const LoopReschedule& other) const { return true; }

   inline std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
	   return out << indent << "Polyhedral.Loop.Reschedule"; 
   }
};

TRANSFORMATION_TYPE(
   LoopReschedule,
   "Let the polyhedral model find the optimal schedule for this loop statement",
	parameter::tuple()
);

inline TransformationPtr makeLoopReschedule() {
	return std::make_shared<LoopReschedule>( );
}
/**
* LoopParallelize: 
*/
struct LoopParallelize : public Transformation<LoopParallelize> {

   LoopParallelize(const parameter::Value& value);
   LoopParallelize();

   core::NodePtr apply(const core::NodePtr& target) const;

   inline bool operator==(const LoopParallelize& other) const { return true; }

   inline std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
	   return out << indent << "Polyhedral.Loop.Parallelize"; 
   }
};

TRANSFORMATION_TYPE(
   LoopParallelize,
   "Auto-parallelization using the polyhedral model",
	parameter::tuple()
);

inline TransformationPtr makeLoopParallelize() {
	return std::make_shared<LoopParallelize>( );
}


/**
 * Takes a region (usually a compound statement) and tries to strip mine all the statements 
 * inside by the same factor. 
 */
struct RegionStripMining : public Transformation<RegionStripMining> {
	
	RegionStripMining(const parameter::Value& value);
	RegionStripMining(unsigned tileSize);

	core::NodePtr apply(const core::NodePtr& target) const;

	bool operator==(const RegionStripMining& other) const {
		return tileSize == other.tileSize;
	}

	std::ostream& printTo(std::ostream& out, const Indent& indent) const { 
		return out << indent << "Polyhedral.Region.StripMining [" << tileSize << "]"; 
	}
private:
	unsigned tileSize;
};

TRANSFORMATION_TYPE(
   RegionStripMining,
   "Strip mine a region by a constant factor",
	parameter::atom<unsigned>("The tiling size")
);


inline TransformationPtr makeRegionStripMining(unsigned tileSize) {
	return std::make_shared<RegionStripMining>( parameter::makeValue<unsigned>(tileSize) );
}

} } } // end insieme::transform::polyhedral namespace 

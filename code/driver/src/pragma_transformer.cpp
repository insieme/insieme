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

#include "insieme/driver/pragma_transformer.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/annotations/transform.h"
#include "insieme/annotations/c/location.h"

#include "insieme/transform/polyhedral/transform.h"

#include "insieme/utils/logging.h"

using namespace insieme;

namespace {

utils::SourceLocation getStartLocation(const core::NodePtr& node) {

	typedef std::shared_ptr<annotations::c::CLocAnnotation> LocationPtr;

	if (LocationPtr loc = node->getAnnotation( annotations::c::CLocAnnotation::KEY ) ) {
		return loc->getStartLoc();
	}
	return utils::SourceLocation();

}

} // end anonymous namespace
namespace insieme {
namespace driver {

core::ProgramPtr applyTransfomrations(const core::ProgramPtr& program) {
	using namespace insieme::transform;

	insieme::utils::map::PointerMap<insieme::core::NodePtr, insieme::core::NodePtr> replacements;

	typedef annotations::TransformationHint::ValueVect ValueVect;

	typedef std::shared_ptr<annotations::TransformationHint> HintPtr;

	auto&& transformer = [&]( const core::NodePtr& cur ) { 
		try {
			if( const HintPtr& hint = cur->getAnnotation( annotations::TransformationHint::KEY ) ) {
				const ValueVect& values = hint->getValues();
				TransformationPtr tr;
				switch (hint->getType()) {
			 	case annotations::TransformationHint::LOOP_INTERCHANGE:
				{
					LOG(INFO) << "Applyinig Loop Interchange (" <<  toString(values) 
							  << ") transformation hint at location: " 
							  << "[ " << getStartLocation(cur) << "]";

					assert(values.size() == 2);

					tr =  polyhedral::makeLoopInterchange(values[0], values[1]);
					break;
				}
				case annotations::TransformationHint::LOOP_TILE:
				{
					LOG(INFO) << "Applyinig Loop Tiling (" << toString(values) << ")"
							  << " transformation hint at location: [ " 
							  << getStartLocation(cur) << "]";

					tr = polyhedral::makeLoopTiling(values) ;
					break;
				}
				case annotations::TransformationHint::LOOP_FUSE:
				{
					LOG(INFO) << "Applyinig Loop Fusion (" << toString(values) << ")"
							  << " transformation hint at location: [ " 
							  << getStartLocation(cur) << "]";
				
					tr = polyhedral::makeLoopFusion( values ) ;
					break;
				}
				case annotations::TransformationHint::LOOP_SPLIT:
				{
					LOG(INFO) << "Applyinig Loop Fission (" << toString(values) << ")"
							  << " transformation hint at location: [ " 
							  << getStartLocation(cur) << "]";
				
					tr = polyhedral::makeLoopFission( values ) ;
					break;
				}
				case annotations::TransformationHint::LOOP_OPTIMIZE:
				{
					LOG(INFO) << "Applyinig Loop Optimize "
							  << " transformation hint at location: [ " 
							  << getStartLocation(cur) << "]";
				
					tr = std::make_shared<polyhedral::LoopReschedule>();
					break;
				}

				default:
					LOG(WARNING) << "TransformationHint not handled.";
				}
				replacements.insert( std::make_pair(cur, tr->apply( cur )) );

			}
			// Add more transformations here
		} catch(transform::InvalidTargetException&& e) {
			
			LOG(WARNING) << "Transformation hint from user at position" << " [" 
					  << getStartLocation(cur) << "] "
					  << "could not be applied for the following reasons: \n\t" 
					  << e.what();
		}
	};

	// FIXME filter all the for stmt
	core::visitDepthFirstOnce(program, core::makeLambdaVisitor( transformer ) );

	return static_pointer_cast<const core::ProgramPtr>( 
			core::transform::replaceAll(program->getNodeManager(), program, replacements)
		);

}

} // end driver namespace
} // end insieme namespace


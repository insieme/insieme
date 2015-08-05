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

#include "insieme/transform/transformation.h"

#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace transform {

/**
 * A performance enhancer for the transformations caching the result of transformations. This
 * utility may be used by transformation connectors internally to improve the performance of
 * code transformations.
 */
class CachedTransformation : public Transformation {

	/**
	 * The transformation to be wrapped.
	 */
	const TransformationPtr transformation;
	
	/**
	 * The transformation cache maintained for storing transformed codes.
	 */
	mutable utils::cache::PointerCache<core::NodePtr, core::NodePtr> transformed;
	
public:

	/**
	 * Creates a new instance of this transformation wrapper encapsulating the given transformation.
	 *
	 * @param transform the transformation to be wrapped
	 */
	CachedTransformation(const TransformationPtr& transform)
		: transformation(transform), transformed(fun(*transform, &Transformation::apply)) {}
		
	/**
	 * Realizes the cached processing of the underlying transformation.
	 */
	virtual core::NodePtr apply(const core::NodePtr& target) const {
		return transformed.get(target);
	}
	
};

} // end namespace transform
} // end namespace insieme

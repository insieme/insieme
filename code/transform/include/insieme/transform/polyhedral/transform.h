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

#include "insieme/transform/catalog.h"

namespace insieme {
namespace transform {
namespace polyhedral {

/**************************************************************************************************
 * Polyhedral Transformations 
 *************************************************************************************************/

/**
 * LoopInterchange: this is the implementation of loop interchange based on the polyhedral model. 
 * The transformation is applyied from a determined loop level. The transformation will search for
 * the induction variable of the first N perfectly nested loops and will apply interchange between
 * index src and dest under the assumption that depth of this loop nest is greater than dest ( or
 * src). 
 *
 * In the case the assumption is not satisfied, an exception is thrown. 
 *
 */
class LoopInterchange : public Transformation {

	unsigned srcIdx, destIdx;

public:
	LoopInterchange(unsigned src, unsigned dest) : srcIdx(src), destIdx(dest) { }

	bool checkPreCondition(const core::NodePtr& target) const { 
		return true; // FIXME
	}

	bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const { 
		return true; // FIXME
	}

	core::NodePtr apply(const core::NodePtr& target) const;

};

/**
 * Factory for the loop interchange transformation. It specifies the type and number of parameters
 * which are required by the transformations in order to be inspectable by the optimizer component
 */
struct LoopInterchangeFactory : public TransformationType {

	LoopInterchangeFactory() : 
		TransformationType (
			"Poly.Loop.Interchange", 
			"Implemenation of loop interchange based on the polyhedral model", 
			parameter::tuple( 
				parameter::atom<unsigned>("The source index of the loop being interchanged"), 
				parameter::atom<unsigned>("The destination index of the loop being interchanged") 
			) 
		) { }

	virtual TransformationPtr buildTransformation(const parameter::Value& value) const {
	 	return std::make_shared<LoopInterchange>( 
				parameter::getValue<unsigned>(value, 0), 
				parameter::getValue<unsigned>(value, 1) 
			);
	}
};

/**
 * LoopStripMining: 
 */ 
class LoopStripMining : public Transformation {

};

} // end poly namespace 
} // end transform namespace 
} // end insieme namespac

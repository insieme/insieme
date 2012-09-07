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

#include "insieme/transform/sequential/constant_folding.h"

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/lang/basic.h"

namespace insieme {
namespace transform {
namespace sequential {


	namespace {

		class ConstantFolder : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;

			const core::lang::BasicGenerator& basic;

		public:

			ConstantFolder(core::NodeManager& manager)
				: manager(manager), basic(manager.getLangBasic()) {}

			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// skip everything that isn't a expression
				core::ExpressionPtr expr = dynamic_pointer_cast<core::ExpressionPtr>(ptr);
				if (!expr) {
					return ptr.substitute(manager, *this);
				}

				// check type
				if (basic.isInt(expr->getType())) {
					// => try to reduce integer expression
					try {
						return core::arithmetic::toIR(manager, core::arithmetic::toPiecewise(expr));
					} catch (const core::arithmetic::NotAPiecewiseException& npe) {
						// so, it cannot be simplified => skip it
						return expr;
					}
				}

				// TODO: add support for boolean constraints
				// TODO: reduce for / while / if in cases where constraints are fixed

				// nothing else to do => solve recursive
				return expr.substitute(manager, *this);
			}

		};

	}



	core::NodePtr foldConstants(core::NodeManager& manager, const core::NodePtr& node) {
		// as always, the constant folder is doing the job
		return ConstantFolder(manager).map(node);
	}


} // end namespace sequential
} // end namespace transform
} // end namespace insieme

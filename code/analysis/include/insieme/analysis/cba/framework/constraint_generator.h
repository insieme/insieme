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

#include <type_traits>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/constraint/solver.h"

namespace insieme {
namespace analysis {
namespace cba {

	// forward declaration
	class CBA;

	namespace sc = insieme::utils::constraint;


	// -------------------- Constraint Generator ---------------------------

	// the type used for lists of constraints
	typedef utils::constraint::Constraints Constraints;

	/**
	 * A generic base class for generators capable of resolving value constraints for individual values.
	 */
	struct ConstraintGenerator {

		/**
		 * A virtual destructor since only derived classes of this type will be utilized.
		 */
		virtual ~ConstraintGenerator() {}

		/**
		 * The main interface function conducting the actual resolution of contstraints for the given
		 * value by utilizing the given cba context.
		 *
		 * @param cba the context within which the resolution of the constraints should occur
		 * @param value the value for which constraints should be obtained
		 * @param constraints the constraint set to be extended
		 */
		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) =0;

		/**
		 * Requests this constraint generator to print a human readable description of the given
		 * value ID to the given output stream (to be utilized within debug prints and dot plots).
		 *
		 * @param out the stream to be printing to
		 * @param cba the context within which the given value is utilized
		 * @param value the value to be formated
		 */
		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const =0;

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

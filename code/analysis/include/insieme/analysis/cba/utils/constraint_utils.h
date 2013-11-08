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

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/constraint/solver.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace {

		template<typename LatticeA, typename LatticeB, typename LatticeC, typename Op>
		struct binary_connector_op {

			typedef typename LatticeA::value_type value_a_type;
			typedef typename LatticeA::base_lattice::value_type set_a_type;

			typedef typename LatticeB::value_type value_b_type;
			typedef typename LatticeB::base_lattice::value_type set_b_type;

			typedef typename LatticeC::manager_type mgr_type;
			typedef typename LatticeC::base_lattice::value_type set_c_type;

			typedef typename LatticeC::value_type value_type;

			mgr_type& mgr;
			Op op;

			binary_connector_op(mgr_type& mgr, const Op& op) : mgr(mgr), op(op) {}

			value_type operator()(const value_a_type& a, const value_b_type& b) const {

				// convert to set
				const set_a_type& set_a = a;
				const set_b_type& set_b = b;

				// compute result
				set_c_type res;
				for(const auto& a : set_a) {
					for(const auto& b : set_b) {
						res.insert(op(a, b));
					}
				}

				// build result set
				return mgr.atomic(res);
			}
		};

	}

	/**
	 * A utility building a constraint connecting two set-based data value lattices (standard data flow analysis).
	 *
	 * @param mgr the manager to be used for resulting data values
	 * @param a the value of the first input
	 * @param b the value of the second input
	 * @param res the value to be updated
	 * @param op the combination operation
	 */
	template<typename DataValueA, typename DataValueB, typename DataValueC, typename Op>
	utils::constraint::ConstraintPtr
	combine(typename DataValueC::lattice_type::manager_type& mgr, const DataValueA& a, const DataValueB& b, const DataValueC& res, const Op& op) {
		typedef typename DataValueA::lattice_type lattice_a;
		typedef typename DataValueB::lattice_type lattice_b;
		typedef typename DataValueC::lattice_type lattice_c;
		return subsetBinary(a, b, res, binary_connector_op<lattice_a, lattice_b, lattice_c, Op>(mgr, op));
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

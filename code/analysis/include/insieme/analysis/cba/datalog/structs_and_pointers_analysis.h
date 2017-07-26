/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <utility>
#include <map>
#include <string>

#include "insieme/core/ir_address.h"

#include "insieme/analysis/cba/datalog/context.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	/**
	 * Result type from pointer analysis
	 */
	using PointerResult = std::pair<bool,std::string>;
	using PointerResults = std::map<core::NodeAddress,PointerResult>;


	/**
	 * Run a pointer analysis: To what value does a pointer point to?
	 * @param target Return value for this expression
	 * @return a 'PointerResult' pair in the form of <analysisSuccessful?, value>
	 */
	PointerResult runPointerAnalysis(Context &context, const core::ExpressionAddress &target);


	/**
	 * Run a pointer analysis: To what value does a pointer point to?
	 * @param targets Return values for these expressions
	 * @return a 'PointerResults' map, where the node addresses are mapped to <analysisSuccessful?, value>
	 */
	PointerResults runPointerAnalysis(Context &context, const std::set<core::ExpressionAddress>& targets);


} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

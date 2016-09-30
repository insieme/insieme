/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/common/interface_tools.h"

#include "insieme/utils/assert.h"
#include "insieme/core/ir_address.h"

#include "insieme/analysis/common/arithmetic_set.h"
#include "insieme/analysis/common/integer_set.h"
#include "insieme/analysis/common/memory_location_set.h"

namespace insieme {
namespace analysis {

	/**
	 * Each of the following declarations creates a function of the following signatures:
	 *
	 * 		template<typename Backend>
	 * 		<res> <name>(<args ...>) { .. };
	 *
	 * To be utilized, one of the backend interface headers has to be included, and the corresponding
	 * backend has to be passed as a template parameter to the given function.
	 */


	// --- Reference Analysis ---

	//                | Name                        | Res              | Argument               |
	declare_analysis_1( getReferencedMemoryLocations, MemoryLocationSet, core::ExpressionAddress);


	// --- Alias Analysis ---

    //                | Name    | Res | Arguments                                        |
	declare_analysis_2( areAlias, bool, core::ExpressionAddress, core::ExpressionAddress );
	declare_analysis_2( mayAlias, bool, core::ExpressionAddress, core::ExpressionAddress );
	declare_analysis_2( notAlias, bool, core::ExpressionAddress, core::ExpressionAddress );


	// --- Boolean Analysis ---

	//                | Name       | Res | Argument              |
	declare_analysis_1( isTrue,     bool, core::ExpressionAddress)
	declare_analysis_1( isFalse,    bool, core::ExpressionAddress)
	declare_analysis_1( mayBeTrue,  bool, core::ExpressionAddress)
	declare_analysis_1( mayBeFalse, bool, core::ExpressionAddress)


	// --- Simple Integer Analysis ---

	//                | Name                  | Res       | Argument             |
	declare_analysis_1( getIntegerValues,     IntegerSet, core::ExpressionAddress);
	declare_analysis_1( isIntegerConstant,    bool,       core::ExpressionAddress);

	//                | Name                | Res | Arguments                                       |
	declare_analysis_2( areEqualInteger,      bool, core::ExpressionAddress, core::ExpressionAddress);
	declare_analysis_2( areNotEqualInteger,   bool, core::ExpressionAddress, core::ExpressionAddress);
	declare_analysis_2( mayBeEqualInteger,    bool, core::ExpressionAddress, core::ExpressionAddress);
	declare_analysis_2( mayBeNotEqualInteger, bool, core::ExpressionAddress, core::ExpressionAddress);


	// --- Symbolic Integer Analysis ---

	//                | Name                  | Res          | Arguments             |
	declare_analysis_1( getArithmeticValue,   ArithmeticSet, core::ExpressionAddress );


} // end namespace analysis
} // end namespace insieme

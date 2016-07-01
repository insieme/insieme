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

#include "insieme/analysis/datalog/alias_analysis.h"
#include "insieme/analysis/datalog/boolean_analysis.h"
#include "insieme/analysis/datalog/code_properties.h"
#include "insieme/analysis/datalog/integer_analysis.h"

namespace insieme {
namespace analysis {

	/*
	 * Create a type for this backend
	 */
	struct datalogEngine {};

	/*
	 * List of CBAs that this backend provides
	 */
	add_cba_implementation(datalog, areAlias)
	add_cba_implementation(datalog, mayAlias)
	add_cba_implementation(datalog, notAlias)

	add_cba_implementation(datalog, getDefinitionPoint)

	add_cba_implementation(datalog, isTrue)
	add_cba_implementation(datalog, isFalse)
	add_cba_implementation(datalog, mayBeTrue)
	add_cba_implementation(datalog, mayBeFalse)

	add_cba_implementation(datalog, getIntegerValues)
	add_cba_implementation(datalog, isIntegerConstant)

} // end namespace analysis
} // end namespace insieme

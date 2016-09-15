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

#include "insieme/analysis/interface.h"
#include "insieme/analysis/haskell/context.h"

#include "insieme/analysis/haskell/alias_analysis.h"
#include "insieme/analysis/haskell/arithmetic_analysis.h"
#include "insieme/analysis/haskell/boolean_analysis.h"

namespace insieme {
namespace analysis {

	/*
	 * Create a type for this backend.
	 */
	struct HaskellEngine : public analysis_engine<haskell::Context> {};

	// --- Alias Analysis ---

	register_analysis_implementation(HaskellEngine, areAlias, haskell::areAlias);
	register_analysis_implementation(HaskellEngine, mayAlias, haskell::mayAlias);
	register_analysis_implementation(HaskellEngine, notAlias, haskell::notAlias);


	// --- Boolean Analysis ---

	register_analysis_implementation(HaskellEngine , isTrue,     haskell::isTrue    );
	register_analysis_implementation(HaskellEngine , isFalse,    haskell::isFalse   );
	register_analysis_implementation(HaskellEngine , mayBeTrue,  haskell::mayBeTrue );
	register_analysis_implementation(HaskellEngine , mayBeFalse, haskell::mayBeFalse);


	// --- Symbolic Integer Analysis ---

	register_analysis_implementation(HaskellEngine , getArithmeticValue, haskell::getArithmeticValue);


} // end namespace analysis
} // end namespace insieme

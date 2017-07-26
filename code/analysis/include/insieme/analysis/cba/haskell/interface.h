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

#include "insieme/analysis/cba/interface.h"
#include "insieme/analysis/cba/haskell/context.h"

#include "insieme/analysis/cba/haskell/alias_analysis.h"
#include "insieme/analysis/cba/haskell/arithmetic_analysis.h"
#include "insieme/analysis/cba/haskell/boolean_analysis.h"
#include "insieme/analysis/cba/haskell/reference_analysis.h"

namespace insieme {
namespace analysis {
namespace cba {

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


	// --- Reference Analysis ---

	register_analysis_implementation(HaskellEngine, isNull,                       haskell::isNull                      );
	register_analysis_implementation(HaskellEngine, mayBeNull,                    haskell::mayBeNull                   );
	register_analysis_implementation(HaskellEngine, notNull,                      haskell::notNull                     );
	register_analysis_implementation(HaskellEngine, isExtern,                     haskell::isExtern                    );
	register_analysis_implementation(HaskellEngine, mayBeExtern,                  haskell::mayBeExtern                 );
	register_analysis_implementation(HaskellEngine, notExtern,                    haskell::notExtern                   );
	register_analysis_implementation(HaskellEngine, getReferencedMemoryLocations, haskell::getReferencedMemoryLocations);


	// --- Symbolic Integer Analysis ---

	register_analysis_implementation(HaskellEngine , getArithmeticValue, haskell::getArithmeticValue);


} //'end namespace cba
} // end namespace analysis
} // end namespace insieme

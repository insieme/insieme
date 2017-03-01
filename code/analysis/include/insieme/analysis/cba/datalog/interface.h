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
 *
 */
#pragma once

#include "insieme/analysis/cba/interface.h"
#include "insieme/analysis/cba/datalog/context.h"
#include "insieme/analysis/cba/datalog/alias_analysis.h"
#include "insieme/analysis/cba/datalog/boolean_analysis.h"
#include "insieme/analysis/cba/datalog/code_properties.h"
#include "insieme/analysis/cba/datalog/integer_analysis.h"

namespace insieme {
namespace analysis {
namespace cba {

	/*
	 * Create a type for this backend
	 */
	struct DatalogEngine : public analysis_engine<datalog::Context> {};


	// --- Alias Analysis ---

	register_analysis_implementation( DatalogEngine , areAlias, datalog::areAlias );
	register_analysis_implementation( DatalogEngine , mayAlias, datalog::mayAlias );
	register_analysis_implementation( DatalogEngine , notAlias, datalog::notAlias );


	// --- Boolean Analysis ---

	register_analysis_implementation( DatalogEngine , isTrue,     datalog::isTrue     );
	register_analysis_implementation( DatalogEngine , isFalse,    datalog::isFalse    );
	register_analysis_implementation( DatalogEngine , mayBeTrue,  datalog::mayBeTrue  );
	register_analysis_implementation( DatalogEngine , mayBeFalse, datalog::mayBeFalse );


	// --- Simple Integer Analysis ---

	register_analysis_implementation( DatalogEngine , getIntegerValues,  datalog::getIntegerValues  );
	register_analysis_implementation( DatalogEngine , isIntegerConstant, datalog::isIntegerConstant );

	register_analysis_implementation( DatalogEngine , areEqualInteger,      datalog::integer::areEqual    );
	register_analysis_implementation( DatalogEngine , areNotEqualInteger,   datalog::integer::areNotEqual );
	register_analysis_implementation( DatalogEngine , mayBeEqualInteger,    datalog::integer::mayEqual    );
	register_analysis_implementation( DatalogEngine , mayBeNotEqualInteger, datalog::integer::mayNotEqual );


} //'end namespace cba
} // end namespace analysis
} // end namespace insieme

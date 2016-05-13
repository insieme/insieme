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

#include <boost/optional.hpp>
#include "insieme/utils/assert.h"
#include "insieme/core/ir_address.h"
#include "insieme/analysis/datalog/code_properties.h"
#include "insieme/analysis/haskell//dataflow.h"

namespace insieme {
namespace analysis {

	/**
	 * Enumeration of available backends. Should be used as a template parameter
	 * in the functions below.
	 */
	enum class Backend { DATALOG, HASKELL };

	/**
	 * Get the definition point for a certain variable, if there is one.
	 *
	 * @param var the VariableAddress of the root node whose subtree will be searched
	 */
	template <Backend B>
	boost::optional<core::VariableAddress> getDefinitionPoint(const core::VariableAddress& var) {
		switch(B) {
		case Backend::DATALOG: return datalog::getDefinitionPoint(var);
		case Backend::HASKELL: return haskell::getDefinitionPoint(var);
		default: assert_not_implemented() << "Backend not implemented!";
		}
		return boost::optional<core::VariableAddress>();
	}

	/*
	 * Usage example:
	 * getDefinitionPoint<Backend::Datalog>(root);
	 */


//	template<typename Engine>
//	core::VariableAddress getDefinitionPoint(const core::VariableAddress& var) {
//		return Engine::getDefinitionPoint(var);
//	}


} // end namespace analysis
} // end namespace insieme

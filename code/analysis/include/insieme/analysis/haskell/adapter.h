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

#include "insieme/analysis/common/arithmetic_set.h"

#include "insieme/core/ir_address.h"

#include <memory>

namespace insieme {
namespace analysis {
namespace haskell {

	#include "alias_analysis.h"
	#include "boolean_analysis.h"

	typedef void* StablePtr;

	class Context {
		StablePtr context_hs;

		core::NodePtr root;

		std::map<const core::NodeAddress, StablePtr> addresses;

	  public:
		explicit Context(core::NodePtr root);
		~Context();

		// move semantics
		Context(Context&& other);
		Context& operator=(Context&& other);

		core::NodePtr getRoot() const;

		// analysis
		core::VariableAddress getDefinitionPoint(const core::VariableAddress& var);
		BooleanAnalysisResult checkBoolean(const core::ExpressionAddress& expr);
		AliasAnalysisResult checkAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y);
		ArithmeticSet arithmeticValue(const core::ExpressionAddress& expr);

	  private:
		StablePtr addNodeAddress(const core::NodeAddress& addr);
		core::NodeAddress getNodeAddress(StablePtr addr) const;
	};

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

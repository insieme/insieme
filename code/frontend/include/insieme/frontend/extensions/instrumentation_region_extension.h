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

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/backend/runtime/runtime_extensions.h"

using namespace insieme::frontend;
using namespace insieme::frontend::pragma;

namespace insieme {
namespace frontend {
namespace extensions {

/**
 * This extension allows for the manual instrumentation of marked regions
 * using "#pragma instrumentation region id(N)"
 */
class InstrumentationRegionExtension : public FrontendExtension {
public:
	InstrumentationRegionExtension() : FrontendExtension() {
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>("instrumentation", "region", 
			pragma::kwd("id") >> tok::l_paren >> tok::expr["id"] >> tok::r_paren >> tok::eod,
			[](MatchObject match, stmtutils::StmtWrapper node) {
				try {
					// Get id number
					core::ExpressionPtr	idClause = match.getSingleExpr("id");
					auto idFormula = core::arithmetic::toFormula(idClause);
					if(!idFormula.isConstant() || !idFormula.isInteger()) throw core::arithmetic::NotAFormulaException(idClause);
					auto id = idFormula.getIntegerValue();

					// Build instrumentation calls 
					auto& manager = node[0]->getNodeManager();
					auto& basic = manager.getLangBasic();
					auto& ext = manager.getLangExtension<insieme::backend::runtime::Extensions>();
					core::IRBuilder builder(manager);
					auto startCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionStart, builder.uintLit(id));
					auto endCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionEnd, builder.uintLit(id));

					// Attach instrumentation calls
					node.insert(node.begin(), startCall);
					node.push_back(endCall);

				} catch(const core::arithmetic::NotAFormulaException& nafe) {
					// TODO: use Diagnosis tools
					std::cerr << "Instrumentation region error: id not a statically computed number";
				}

				return node;
		}));
	}	
};

} // extensions
} // frontend
} // insieme

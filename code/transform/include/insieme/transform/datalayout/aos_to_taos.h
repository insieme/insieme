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

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {

namespace analysis {
typedef std::map<core::VariableAddress, core::CompoundStmtAddress> VariableScopeMap;
}

namespace transform {
namespace datalayout {

class AosToTaos : public AosToSoa {
	core::IntTypeParamPtr genericTileSize;

	virtual core::StructTypePtr createNewType(core::StructTypePtr oldType);

	virtual core::StatementList generateNewDecl(const core::ExpressionMap& varReplacements, const core::DeclarationStmtAddress& decl,
			const core::VariablePtr& newVar, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType);

	virtual core::StatementList generateNewAssigns(const core::ExpressionMap& varReplacements, const core::CallExprAddress& call,
			const core::ExpressionPtr& newVar, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType);

	virtual core::StatementPtr generateMarshalling(const core::ExpressionPtr& oldVar, const core::ExpressionPtr& newVar, const core::ExpressionPtr& start,
			const core::ExpressionPtr& end, const core::StructTypePtr& structType);

	virtual core::StatementPtr generateUnmarshalling(const core::ExpressionPtr& oldVar, const core::ExpressionPtr& newVar, const core::ExpressionPtr& start,
			const core::ExpressionPtr& end, const core::StructTypePtr& structType);

	virtual core::StatementList generateDel(const core::StatementAddress& stmt, const core::ExpressionPtr& oldVar, const core::ExpressionPtr& newVar,
			const core::StructTypePtr& newStructType);

	virtual core::ExpressionPtr generateNewAccesses(const core::ExpressionPtr& oldVar, const core::ExpressionPtr& newVar, const core::StringValuePtr& member,
			const core::ExpressionPtr& index, const core::ExpressionPtr& structAccess);

	virtual core::ExpressionPtr generateByValueAccesses(const core::ExpressionPtr& oldVar, const core::ExpressionPtr& newVar, const core::StructTypePtr& newStructType,
			const core::ExpressionPtr& index, const core::ExpressionPtr& oldStructAccess);

public:
	AosToTaos(core::NodePtr& toTransform);

	virtual void transform();
};

} // datalayout
} // transform
} // insieme

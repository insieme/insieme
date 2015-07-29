/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/transform/datalayout/aos_to_taos.h"
#include "insieme/transform/datalayout/aos_to_soa.h"
#include "insieme/core/forward_decls.h"

namespace insieme {

namespace analysis {
typedef std::map<core::VariableAddress, core::CompoundStmtAddress> VariableScopeMap;
}

namespace transform {
namespace datalayout {

template<class Baseclass>
class ParSecTransform : public Baseclass {
protected:
	ExprAddressMap varReplacements;
	ExprAddressMap& varsToPropagate;
	std::map<core::NodeAddress, core::NodePtr>& replacements;
	const core::StructTypePtr& newStructType;
	const core::StructTypePtr& oldStructType;
	std::vector<core::LambdaExprAddress> globalLambdas;

	virtual core::NodeMap generateTypeReplacements(const core::TypePtr& oldStructType, const core::TypePtr& newStructType);

	virtual ExprAddressStructTypeMap findCandidates(const core::NodeAddress& toTransform);

	virtual core::StatementList generateNewDecl(const ExprAddressMap& varReplacements, const core::DeclarationStmtAddress& decl,
			const core::StatementPtr& newVar, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType,
			const core::ExpressionPtr& nElems);

	virtual core::StatementList generateNewAssigns(const ExprAddressMap& varReplacements, const core::CallExprAddress& call,
			const core::StatementPtr& newVar, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType,
			const core::ExpressionPtr& nElems = core::ExpressionPtr());
public:
	ParSecTransform(core::NodePtr& toTransform, ExprAddressMap& varsToPropagate, std::map<core::NodeAddress, core::NodePtr>& replacements,
			const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType);
//	virtual ~ParSecAtt() {}

	virtual void transform();

	const ExprAddressMap getKernelVarReplacements() { return varReplacements; }
};

template class ParSecTransform<DatalayoutTransformer>;
template class ParSecTransform<AosToTaos>;
template class ParSecTransform<AosToSoa>;
} // datalayout
} // transform
} // insieme


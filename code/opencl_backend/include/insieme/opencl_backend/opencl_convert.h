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

#include "insieme/backend/backend.h"

#include "insieme/simple_backend/statement_converter.h"
#include "insieme/simple_backend/function_manager.h"

namespace insieme {
namespace backend {
namespace ocl {

class OpenCLBackend;
typedef std::shared_ptr<OpenCLBackend> OpenCLBackendPtr;

class OpenCLBackend : public backend::Backend {
public:
	static OpenCLBackendPtr getDefault();
	backend::TargetCodePtr convert(const core::NodePtr& source) const;
};

class OclFunctionManager : public simple_backend::FunctionManager {
public:
	OclFunctionManager(simple_backend::Converter& conversionContext);

	virtual void appendFunctionParameter(const simple_backend::CodeFragmentPtr& fragment, const core::VariablePtr& param);
	virtual void addFunctionPrefix(const simple_backend::CodeFragmentPtr& fragment, const core::LambdaPtr& lambda);
};

typedef std::map<unsigned, unsigned> varNameMapType;
typedef std::map<unsigned, core::VariablePtr> qualifierMapType;

class OclStmtConvert : public simple_backend::StmtConverter {
	
	// Memory Qualifier Map
	qualifierMapType qualifierMap;
	// Variable Name Map
	varNameMapType backwardVarNameMap;
	varNameMapType forwardVarNameMap;
	
public:
	OclStmtConvert(simple_backend::Converter& conversionContext, const simple_backend::formatting::FormatTable& formats);
	void visitLambdaExpr(const core::LambdaExprPtr& ptr);
	void visitDeclarationStmt(const core::DeclarationStmtPtr& ptr);
	void visitCallExpr(const core::CallExprPtr& ptr);
	virtual vector<string> getHeaderDefinitions() { return vector<string>(); }
};

} // namespace ocl
} // namespace backend
} // namespace insieme

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

#include "insieme/opencl_backend/opencl_convert.h"
#include "insieme/frontend/ocl/ocl_annotations.h"
#include "insieme/core/ast_builder.h"
#include "insieme/utils/logging.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace backend {
namespace ocl {

namespace frontOpencl = insieme::frontend::ocl;
using namespace insieme::core;
using namespace insieme::simple_backend;


TargetCodePtr convert(const ProgramPtr& source) {

	// create and set up the converter
	Converter converter;

	// Prepare managers
	NodeManager& nodeManager = source->getNodeManager();
	converter.setNodeManager(&nodeManager);

	OclStmtConvert stmtConverter(converter);
	converter.setStmtConverter(&stmtConverter);

	NameManager nameManager;
	converter.setNameManager(&nameManager);

	TypeManager typeManager(nameManager);
	converter.setTypeManager(&typeManager);

	VariableManager variableManager;
	converter.setVariableManager(&variableManager);

	FunctionManager functionManager(converter);
	converter.setFunctionManager(&functionManager);

	// conduct conversion
	return converter.convert(source);
}

OclStmtConvert::OclStmtConvert(Converter& conversionContext) : simple_backend::StmtConverter(conversionContext) { }

void OclStmtConvert::visitLambdaExpr(const core::LambdaExprPtr& ptr) {
	ASTBuilder builder(ptr->getNodeManager());
	if(ptr->hasAnnotation(frontOpencl::BaseAnnotation::KEY)) {
		std::cout << "Function with some Opencl Annotation...\n";
		frontOpencl::BaseAnnotationPtr annotations = ptr->getAnnotation(frontOpencl::BaseAnnotation::KEY);
		assert(annotations && "BaseAnnotation is empty");
		for(frontOpencl::BaseAnnotation::AnnotationList::const_iterator iter = annotations->getAnnotationListBegin();
			iter < annotations->getAnnotationListEnd(); ++iter) {
			if(frontOpencl::KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<frontOpencl::KernelFctAnnotation>(*iter)) {
				std::cout << "Function with kernel annotation...\n";
				const Lambda::ParamList& oldParams = ptr->getParameterList();
				const CompoundStmtPtr& oldBody = dynamic_pointer_cast<const CompoundStmt>(ptr->getBody());
				const FunctionTypePtr& oldFuncType = dynamic_pointer_cast<const FunctionType>(ptr->getType());
				
				// new paramList
				Lambda::ParamList newParams;
				for (uint i = 0; i < oldParams.size()-2; i++){
					newParams.push_back(oldParams.at(i));
				}
				
				// new functionType
				const core::TypeList& oldArgs = oldFuncType->getArgumentTypes();
				const core::TypePtr& retType = oldFuncType->getReturnType();
				assert(retType->getName() == "unit" && "Return type of kernel functions must be void.");
				TypeList newArgs;
				for (uint i = 0; i < oldArgs.size()-2; i++){
					newArgs.push_back(oldArgs.at(i));
				}
				const FunctionTypePtr newFuncType = builder.functionType(newArgs, retType);
				
				// reverse
				const vector<StatementPtr>& bodyCompoundStmt = oldBody->getStatements();
				
				const CallExprPtr& globalParallel = dynamic_pointer_cast<const CallExpr>(bodyCompoundStmt.back());
				
				const vector<ExpressionPtr>& globalExpr = globalParallel->getArguments();
				
				const JobExprPtr& globalJob = dynamic_pointer_cast<const JobExpr>(globalExpr.back());
				
				const CaptureInitExprPtr& globalCapture =  dynamic_pointer_cast<const CaptureInitExpr>(globalJob->getDefaultStmt());
				
				const LambdaExprPtr& globalParFct = dynamic_pointer_cast<const LambdaExpr>(globalCapture->getLambda());
				
				const CompoundStmtPtr& globalParBody = dynamic_pointer_cast<const CompoundStmt>(globalParFct->getBody());
				
				const vector<StatementPtr>& globalBodyStmts = globalParBody->getStatements();
				
				const CallExprPtr& localParallel =  dynamic_pointer_cast<const CallExpr>(globalBodyStmts.front());
				
				const vector<ExpressionPtr>& localExpr = localParallel->getArguments();
				
				const JobExprPtr& localJob = dynamic_pointer_cast<const JobExpr>(localExpr.back());
				
				const CaptureInitExprPtr& localCapture =  dynamic_pointer_cast<const CaptureInitExpr>(localJob->getDefaultStmt());
				
				const LambdaExprPtr& localParFct = dynamic_pointer_cast<const LambdaExpr>(localCapture->getLambda());
				
				const CompoundStmtPtr& localParBody = dynamic_pointer_cast<const CompoundStmt>(localParFct->getBody());
				
				//LOG(INFO) << core::printer::PrettyPrinter(localParBody);
				
				LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, newParams, localParBody);
				
				FunctionManager& funManager = cc.getFunctionManager();
				getCodeStream() << funManager.getFunctionName(defCodePtr, newFunc);
			}
		}
	} 
	else {
		FunctionManager& funManager = cc.getFunctionManager();
		getCodeStream() << funManager.getFunctionName(defCodePtr, ptr);
	}
}

} // namespace ocl
} // namespace backend
} // namespace insieme

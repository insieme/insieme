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
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace backend {
namespace ocl {

using namespace insieme::ocl;
using namespace insieme::ocl;
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

unsigned initialVarName(std::map<unsigned, unsigned>& varNameMap, unsigned value){
	for (auto fit = varNameMap.find(value); fit != varNameMap.end(); fit = varNameMap.find(value)){
		value = fit->second;
	}
	return value;
}

void addQualifier(std::map<unsigned, VariablePtr>& qualifierMap, unsigned value, 
					enum AddressSpaceAnnotation::addressSpace as) {

}

void OclStmtConvert::visitLambdaExpr(const core::LambdaExprPtr& ptr) {
	ASTBuilder builder(ptr->getNodeManager());
	if(ptr->hasAnnotation(BaseAnnotation::KEY)) {
		std::cout << "Function with some Opencl Annotation...\n";
		BaseAnnotationPtr annotations = ptr->getAnnotation(BaseAnnotation::KEY);
		assert(annotations && "BaseAnnotation is empty");
		for(BaseAnnotation::AnnotationList::const_iterator iter = annotations->getAnnotationListBegin();
			iter < annotations->getAnnotationListEnd(); ++iter) {
			if(KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<KernelFctAnnotation>(*iter)) {
				std::cout << "Function with kernel annotation...\n";
				const Lambda::ParamList& oldParams = ptr->getParameterList();
				const CompoundStmtPtr& oldBody = dynamic_pointer_cast<const CompoundStmt>(ptr->getBody());
				const FunctionTypePtr& oldFuncType = dynamic_pointer_cast<const FunctionType>(ptr->getType());
				
				// Memory Qualifier Map
				std::map<unsigned, VariablePtr> qualifierMap;
				// Variable Name Map
				std::map<unsigned, unsigned> varNameMap;
				
				// new paramList (case IR created from OpenCL frontend)
				Lambda::ParamList newParams;
				for (uint i = 0; i < oldParams.size()-2; i++){
					VariablePtr tmpVar = oldParams.at(i);
					newParams.push_back(tmpVar);
					qualifierMap.insert(std::make_pair(tmpVar->getId(), tmpVar));
				}
				
				// TEST
				/*
				std::map<unsigned, VariablePtr>::const_iterator fit = qualifierMap.find(24);
				assert(fit != qualifierMap.end() && "WTF -> element not in the map!");
				const VariablePtr& temp = fit->second;
				
				AddressSpaceAnnotationPtr globalMem(new AddressSpaceAnnotation(AddressSpaceAnnotation::addressSpace::GLOBAL));
				AddressSpaceAnnotationPtr localMem(new AddressSpaceAnnotation(AddressSpaceAnnotation::addressSpace::LOCAL));
				
				temp->addAnnotation(globalMem);
				
				if(temp->hasAnnotation(AddressSpaceAnnotation::KEY)) {
				}
				*/
				// END TEST
				
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
				
				// Check for global variables
				const vector<DeclarationStmtPtr>& globalJobDecls = globalJob->getLocalDecls();
				for_each(globalJobDecls, [&](const DeclarationStmtPtr& curDecl) {
					unsigned newValue = (curDecl->getVariable())->getId();
					unsigned  oldValue = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
					varNameMap.insert(std::make_pair(newValue, oldValue));
					unsigned firstVal = initialVarName(varNameMap, oldValue);
					// Add global qualifier
					addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::GLOBAL);
				});
				
				const CaptureInitExprPtr& globalCapture =  dynamic_pointer_cast<const CaptureInitExpr>(globalJob->getDefaultStmt());
				
				const std::vector<ExpressionPtr> globalNewValues = globalCapture->getValues();			
				
				const LambdaExprPtr& globalParFct = dynamic_pointer_cast<const LambdaExpr>(globalCapture->getLambda());
				
				const std::vector<VariablePtr> globalOldValues = globalParFct->getCaptureList();
				
				// check for local variables
				auto iter2 = globalNewValues.begin();
				for (auto iter = globalOldValues.begin(); iter != globalOldValues.end(); ++iter, ++iter2){
					unsigned newValue = (*iter)->getId();
					unsigned  oldValue = (dynamic_pointer_cast<const Variable>(*iter2))->getId();
					
					varNameMap.insert(std::make_pair(newValue, oldValue));
					unsigned firstVal = initialVarName(varNameMap, oldValue);
					// Add local qualifier
					addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::LOCAL);
				}
				
				const CompoundStmtPtr& globalParBody = dynamic_pointer_cast<const CompoundStmt>(globalParFct->getBody());
				
				const vector<StatementPtr>& globalBodyStmts = globalParBody->getStatements();
				
				const CallExprPtr& localParallel =  dynamic_pointer_cast<const CallExpr>(globalBodyStmts.front());
				
				const vector<ExpressionPtr>& localExpr = localParallel->getArguments();
				
				const JobExprPtr& localJob = dynamic_pointer_cast<const JobExpr>(localExpr.back());
				
				const vector<DeclarationStmtPtr>& localJobDecls = localJob->getLocalDecls();
				for_each(localJobDecls, [&](const DeclarationStmtPtr& curDecl) {
					varNameMap.insert(std::make_pair((curDecl->getVariable())->getId(), 
							(dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId()));
				});
				
				const CaptureInitExprPtr& localCapture =  dynamic_pointer_cast<const CaptureInitExpr>(localJob->getDefaultStmt());
				
				const std::vector<ExpressionPtr> localNewValues = localCapture->getValues();
				
				const LambdaExprPtr& localParFct = dynamic_pointer_cast<const LambdaExpr>(localCapture->getLambda());
				
				const std::vector<VariablePtr> localOldValues = localParFct->getCaptureList();
				
				iter2 = localNewValues.begin();
				for (auto iter = localOldValues.begin(); iter != localOldValues.end(); ++iter, ++iter2){
					varNameMap.insert(std::make_pair((*iter)->getId(), 
							(dynamic_pointer_cast<const Variable>(*iter2))->getId()));
				}
				
				const CompoundStmtPtr& localParBody = dynamic_pointer_cast<const CompoundStmt>(localParFct->getBody());
				// FIXME: check in localParPoby all variables and change name to them(or change the name of the function parameters)
				
				
				for (auto iter = varNameMap.begin(); iter != varNameMap.end(); ++iter)
					std::cout << iter->first << " -> " << iter->second << std::endl;
				
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

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

	OclFunctionManager functionManager(converter);
	converter.setFunctionManager(&functionManager);

	// conduct conversion
	return converter.convert(source);
}

OclStmtConvert::OclStmtConvert(Converter& conversionContext) : simple_backend::StmtConverter(conversionContext) { }

OclFunctionManager::OclFunctionManager(Converter& conversionContext) : FunctionManager(conversionContext) { }





//// ------------------------------------ OclStmtConvert ---------------------------- ////

namespace {
std::ostream& OclprintFunctionParamter(std::ostream& out, CodePtr& context, const VariablePtr& param,
		VariableManager& varManager, TypeManager& typeManager, NameManager& nameManager) {

		// register ref-based variable within the variable manager
		if (param->getType()->getNodeType() == NT_RefType) {
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			varManager.addInfo(param, info);
		}

		// format parameter using type manager
		std::cout << "--------------- IT WORKS!!!!!You are inside OclprintFunctionParamter ---------------\n";
		return out << typeManager.formatParamter(context, param->getType(), nameManager.getVarName(param), true);
}
}

CodePtr OclFunctionManager::resolve(const LambdaPtr& lambda) {
	std::cout << "--------------- IT WORKS!!!!! You are inside OclFunction::resolve ---------------\n";
	// lookup definition
	auto pos = functions.find(lambda);
	if (pos != functions.end()) {
		return pos->second;
	}

	// provide some manager
	VariableManager& varManager = cc.getVariableManager();
	TypeManager& typeManager = cc.getTypeManager();
	NameManager& nameManager = cc.getNameManager();

	// get name
	string name = nameManager.getName(lambda);
	FunctionTypePtr funType = lambda->getType();


	// create function code for lambda
	CodePtr function = std::make_shared<CodeFragment>("Definition of " + name);
	CodeStream& cs = function->getCodeStream();

	// write the function header
	cs << typeManager.getTypeName(function, funType->getReturnType()) << " " << name << "(";

	if (lambda->isCapturing()) {
		cs << "void* _capture";
		if (!lambda->getParameterList().empty()) {
			cs << ", ";
		}
	}
	if (!lambda->getParameterList().empty()) {
		cs << join(", ", lambda->getParameterList(), [&, this](std::ostream& os, const VariablePtr& param) {
			OclprintFunctionParamter(os, function, param, (this->cc).getVariableManager(),(this->cc).getTypeManager(),(this->cc).getNameManager());
			// printFunctionParamter(out, context, param, cc.getVariableManager(), cc.getTypeManager(), cc.getNameManager())
		});
	}
	cs << ")";


	// add function body
	cs << " {" << CodeStream::indR << "\n";

	if (lambda->isCapturing()) {
		// extract capture list
		cs << "// --------- Captured Stuff - Begin -------------\n";

		// get name of struct from type manager
		string structName = typeManager.getFunctionTypeDetails(funType).functorName;

		int i = 0;
		for_each(lambda->getCaptureList(), [&](const VariablePtr& var) {
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;

			varManager.addInfo(var, info);

			// standard handling
			cs << typeManager.formatParamter(function, var->getType(), nameManager.getVarName(var), false);
			cs << " = ((" << structName << "*)_capture)->" << format("p%d", i++) << ";\n";

		});

		cs << "// --------- Captured Stuff -  End  -------------\n";
	}

	// generate the function body
	cc.getStmtConverter().convert(lambda->getBody(), function);

	cs << CodeStream::indL << "\n}\n";
	cs << "\n";

	// register and return result
	functions.insert(std::make_pair(lambda, function));
	return function;
}






//// ------------------------------------ OclStmtConvert ---------------------------- ////

typedef std::map<unsigned, unsigned> varNameMapType;
typedef std::map<unsigned, core::VariablePtr> qualifierMapType;

unsigned getVarName(varNameMapType& varNameMap, unsigned value){
	for (auto fit = varNameMap.find(value); fit != varNameMap.end(); fit = varNameMap.find(value)){
		value = fit->second;
	}
	return value;
}

void addQualifier(qualifierMapType& qualifierMap, unsigned value, 
					enum AddressSpaceAnnotation::addressSpace as) {
	auto&& fit = qualifierMap.find(value);
	if (fit != qualifierMap.end()) {
		const VariablePtr& var = fit->second;
		if(!var->hasAnnotation(AddressSpaceAnnotation::KEY)){
			AddressSpaceAnnotationPtr an(new AddressSpaceAnnotation(as));
			var->addAnnotation(an);
		}
	}
}

void moveQualifier(qualifierMapType& qualifierMap, unsigned oldName, unsigned newName){
	auto&& fit = qualifierMap.find(oldName);
	auto&& fit2 = qualifierMap.find(newName);
	if (fit != qualifierMap.end() && fit2 != qualifierMap.end()) {
		VariablePtr& oldVar = fit->second;
		VariablePtr& newVar = fit2->second;
		if(oldVar->hasAnnotation(AddressSpaceAnnotation::KEY)){
			newVar->addAnnotation(oldVar->getAnnotation(AddressSpaceAnnotation::KEY));
			oldVar->remAnnotation(AddressSpaceAnnotation::KEY);
		}
	}
	else {
		assert(false && "WTF: varName not in qualifierMap!!");
	}
}

void OclStmtConvert::visitLambdaExpr(const core::LambdaExprPtr& ptr) {
	ASTBuilder builder(ptr->getNodeManager());
	if(ptr->hasAnnotation(BaseAnnotation::KEY)) {
		std::cout << "Function with some Opencl Annotation...\n";
		BaseAnnotationPtr&& annotations = ptr->getAnnotation(BaseAnnotation::KEY);
		assert(annotations && "BaseAnnotation is empty");
		for(BaseAnnotation::AnnotationList::const_iterator iter = annotations->getAnnotationListBegin();
			iter < annotations->getAnnotationListEnd(); ++iter) {
			if(KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<KernelFctAnnotation>(*iter)) {
				std::cout << "Function with kernel annotation...\n";
				const Lambda::ParamList& oldParams = ptr->getParameterList();
				const CompoundStmtPtr& oldBody = dynamic_pointer_cast<const CompoundStmt>(ptr->getBody());
				const FunctionTypePtr& oldFuncType = dynamic_pointer_cast<const FunctionType>(ptr->getType());
				
				// Memory Qualifier Map
				qualifierMapType qualifierMap;
				// Variable Name Map
				varNameMapType backwardVarNameMap;
				varNameMapType forwardVarNameMap;
				
				// new paramList (case IR created from OpenCL frontend)
				for (uint i = 0; i < oldParams.size()-2; i++){
					const VariablePtr& tmpVar = oldParams.at(i);
					qualifierMap.insert(std::make_pair(tmpVar->getId(), tmpVar));
				}
				
				// new functionType
				const core::TypeList& oldArgs = oldFuncType->getArgumentTypes();
				const core::TypePtr& retType = oldFuncType->getReturnType();
				assert(retType->getName() == "unit" && "Return type of kernel functions must be void.");
				TypeList newArgs;
				for (uint i = 0; i < oldArgs.size()-2; i++){
					newArgs.push_back(oldArgs.at(i));
				}
				const FunctionTypePtr& newFuncType = builder.functionType(newArgs, retType);
				
				const vector<StatementPtr>& bodyCompoundStmt = oldBody->getStatements();
				
				const CallExprPtr& globalParallel = dynamic_pointer_cast<const CallExpr>(bodyCompoundStmt.back());
				
				const vector<ExpressionPtr>& globalExpr = globalParallel->getArguments();
				
				const JobExprPtr& globalJob = dynamic_pointer_cast<const JobExpr>(globalExpr.back());
				
				// Check for global variables
				const vector<DeclarationStmtPtr>& globalJobDecls = globalJob->getLocalDecls();
				for_each(globalJobDecls, [&](const DeclarationStmtPtr& curDecl) {
					unsigned newName = (curDecl->getVariable())->getId();
					unsigned oldName = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
					
					backwardVarNameMap.insert(std::make_pair(newName, oldName));
					forwardVarNameMap.insert(std::make_pair(oldName, newName));
					unsigned firstVal = getVarName(backwardVarNameMap, oldName);
					// Add global qualifier
					addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::GLOBAL);
				});
				
				const CaptureInitExprPtr& globalCapture =  dynamic_pointer_cast<const CaptureInitExpr>(globalJob->getDefaultStmt());
				
				const std::vector<ExpressionPtr>& globalNewValues = globalCapture->getValues();			
				
				const LambdaExprPtr& globalParFct = dynamic_pointer_cast<const LambdaExpr>(globalCapture->getLambda());
				
				const std::vector<VariablePtr>& globalOldValues = globalParFct->getCaptureList();
				
				// check for local variables
				auto&& iter2 = globalNewValues.begin();
				for (auto&& iter = globalOldValues.begin(); iter != globalOldValues.end(); ++iter, ++iter2){
					unsigned newName = (*iter)->getId();
					unsigned oldName = (dynamic_pointer_cast<const Variable>(*iter2))->getId();
					
					backwardVarNameMap.insert(std::make_pair(newName, oldName));
					forwardVarNameMap.insert(std::make_pair(oldName, newName));
					unsigned firstVal = getVarName(backwardVarNameMap, oldName);
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
					unsigned newName = (curDecl->getVariable())->getId(); 
					unsigned oldName = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
						
					backwardVarNameMap.insert(std::make_pair(newName, oldName));
					forwardVarNameMap.insert(std::make_pair(oldName, newName));
				});
				
				const CaptureInitExprPtr& localCapture =  dynamic_pointer_cast<const CaptureInitExpr>(localJob->getDefaultStmt());
				
				const std::vector<ExpressionPtr>& localNewValues = localCapture->getValues();
				
				const LambdaExprPtr& localParFct = dynamic_pointer_cast<const LambdaExpr>(localCapture->getLambda());
				
				const std::vector<VariablePtr>& localOldValues = localParFct->getCaptureList();
				
				iter2 = localNewValues.begin();
				for (auto&& iter = localOldValues.begin(); iter != localOldValues.end(); ++iter, ++iter2){
					unsigned newName = (*iter)->getId(); 
					unsigned oldName = (dynamic_pointer_cast<const Variable>(*iter2))->getId();
					
					backwardVarNameMap.insert(std::make_pair(newName, oldName));
					forwardVarNameMap.insert(std::make_pair(oldName, newName));
					qualifierMap.insert(std::make_pair(newName, *iter));
				}
				
				Lambda::ParamList newParams;
				for (uint i = 0; i < oldParams.size()-2; i++){
					unsigned oldName = (oldParams.at(i))->getId();
					unsigned newName = getVarName(forwardVarNameMap, oldName);
					moveQualifier(qualifierMap, oldName, newName);
					auto&& fit = qualifierMap.find(newName);
					if (fit != qualifierMap.end()) {
						newParams.push_back(fit->second);
					} else {
						assert(false && "WTF: varName not in qualifierMap!!");
					}
				}
				
				const CompoundStmtPtr& localParBody = dynamic_pointer_cast<const CompoundStmt>(localParFct->getBody());
				
				/*for (auto iter = backwardVarNameMap.begin(); iter != backwardVarNameMap.end(); ++iter)
					std::cout << iter->first << " -> " << iter->second << std::endl;
				
				std::cout << "_____NUOVA_____\n";
					
				for (auto iter = forwardVarNameMap.begin(); iter != forwardVarNameMap.end(); ++iter)
					std::cout << iter->first << " -> " << iter->second << std::endl;*/
				
				//LOG(INFO) << core::printer::PrettyPrinter(localParBody);
				
				LambdaExprPtr&& newFunc = builder.lambdaExpr(newFuncType, newParams, localParBody);
				// FIXME: aggiungere annotazione kernel
				
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

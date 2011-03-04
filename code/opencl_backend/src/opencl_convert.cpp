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

#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/ast_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/ocl/ocl_annotations.h"

#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/name_manager.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/job_manager.h"
#include "insieme/simple_backend/function_manager.h"

#include "insieme/simple_backend/formatting/operator_formatting.h"


namespace insieme {
namespace backend {
namespace ocl {

using namespace insieme::ocl;
using namespace insieme::core;
using namespace insieme::simple_backend;

namespace detail {
	simple_backend::formatting::FormatTable getOCLFormatTable(const core::lang::BasicGenerator& basic);
}

TargetCodePtr convert(const ProgramPtr& source) {

	// create and set up the converter
	Converter converter;

	// Prepare managers
	NodeManager& nodeManager = source->getNodeManager();
	converter.setNodeManager(&nodeManager);

	OclStmtConvert stmtConverter(converter, detail::getOCLFormatTable(nodeManager.basic));
	converter.setStmtConverter(&stmtConverter);

	NameManager nameManager;
	converter.setNameManager(&nameManager);

	TypeManager typeManager(nameManager);
	converter.setTypeManager(&typeManager);

	VariableManager variableManager;
	converter.setVariableManager(&variableManager);

	OclFunctionManager functionManager(converter);
	converter.setFunctionManager(&functionManager);

	JobManager jobManager(converter);
	converter.setJobManager(&jobManager);

	// conduct conversion
	return converter.convert(source);
}

OclStmtConvert::OclStmtConvert(Converter& conversionContext, const simple_backend::formatting::FormatTable& formats) : simple_backend::StmtConverter(conversionContext, formats) { }

OclFunctionManager::OclFunctionManager(Converter& conversionContext) : FunctionManager(conversionContext) { }





//// ------------------------------------ OclFunctionManager ---------------------------- ////

void OclFunctionManager::appendFunctionParameter(const simple_backend::CodeFragmentPtr& fragment, const core::VariablePtr& param) {
		// add OpenCL Memory Qualifier to parameter
		if(param->hasAnnotation(AddressSpaceAnnotation::KEY)){
			AddressSpaceAnnotationPtr ann = param->getAnnotation(AddressSpaceAnnotation::KEY);
			switch (ann->getAddressSpace()) {
				case AddressSpaceAnnotation::addressSpace::GLOBAL: 
					fragment << "__global ";
					break;
				case AddressSpaceAnnotation::addressSpace::LOCAL: 
					fragment << "__local ";
					break;
				case AddressSpaceAnnotation::addressSpace::CONSTANT: 
					fragment << "__constant ";
					break;
				case AddressSpaceAnnotation::addressSpace::PRIVATE: 
					fragment << "__private ";
					break;
				case AddressSpaceAnnotation::addressSpace::size:
					break;
			}
		}
		FunctionManager::appendFunctionParameter(fragment, param);
}

	
void OclFunctionManager::addFunctionPrefix(const simple_backend::CodeFragmentPtr& fragment, const core::LambdaPtr& lambda) {
	if(lambda->hasAnnotation(KernelFctAnnotation::KEY)){
		fragment << "__kernel ";
	}
}


//// ------------------------------------ OclStmtConvert ---------------------------- ////

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

void addBuiltinAnnotation(ASTBuilder& builder, qualifierMapType& qualifierMap, VariablePtr var, string s){
	TypePtr t = (builder.getNodeManager()).basic.getUInt4();
	core::TypeList tList;
	tList.push_back(t);
	LiteralPtr lit = builder.literal(builder.functionType(tList, t), s);
	BuiltinFunctionAnnotationPtr ann(new BuiltinFunctionAnnotation(lit));
	var->addAnnotation(ann);
	qualifierMap.insert(std::make_pair(var->getId(), var));	
}

void OclStmtConvert::visitLambdaExpr(const core::LambdaExprPtr& ptr) {
	ASTBuilder builder(ptr->getNodeManager());
	if(ptr->hasAnnotation(BaseAnnotation::KEY)) {
		LOG(INFO) << "Function with some Opencl Annotation...\n";
		BaseAnnotationPtr&& annotations = ptr->getAnnotation(BaseAnnotation::KEY);
		assert(annotations && "BaseAnnotation is empty");
		for(BaseAnnotation::AnnotationList::const_iterator iter = annotations->getAnnotationListBegin();
			iter < annotations->getAnnotationListEnd(); ++iter) {
			if(KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<KernelFctAnnotation>(*iter)) {
				LOG(INFO) << "Function with kernel annotation...\n";
				
				const LambdaPtr& oldLambda = ptr->getLambda();
				const Lambda::ParamList& oldParams = oldLambda->getParameterList();
				const CompoundStmtPtr& oldBody = dynamic_pointer_cast<const CompoundStmt>(oldLambda->getBody());
				const FunctionTypePtr& oldFuncType = oldLambda->getType();
				
				// new paramList (case IR created from OpenCL frontend)
				for (uint i = 0; i < oldParams.size()-2; i++){
					const VariablePtr& tmpVar = oldParams.at(i);
					qualifierMap.insert(std::make_pair(tmpVar->getId(), tmpVar));
				}
				
				// add builtin annotation for get_global_size & get_local_size to the variable
				addBuiltinAnnotation(builder, qualifierMap, oldParams.at(oldParams.size()-2), "get_global_size");
				addBuiltinAnnotation(builder, qualifierMap, oldParams.at(oldParams.size()-1), "get_local_size");
				
				// new functionType
				const core::TypeList& oldArgs = oldFuncType->getArgumentTypes();
				const core::TypePtr& retType = oldFuncType->getReturnType();
				assert(retType->getNodeManager().basic.isUnit(retType) && "Return type of kernel functions must be void.");
				TypeList newArgs;
				for (uint i = 0; i < oldArgs.size()-2; i++){
					newArgs.push_back(oldArgs.at(i));
				}
				const FunctionTypePtr& newFuncType = builder.functionType(newArgs, retType);
				
				const vector<StatementPtr>& bodyCompoundStmt = oldBody->getStatements();
				
				// add builtin for get_num_groups
				const DeclarationStmtPtr& dcl = dynamic_pointer_cast<const DeclarationStmt>(bodyCompoundStmt.front());
				addBuiltinAnnotation(builder, qualifierMap, dcl->getVariable(), "get_num_groups");
				
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
				}
				
				const CompoundStmtPtr& globalParBody = dynamic_pointer_cast<const CompoundStmt>(globalParFct->getBody());
				
				const vector<StatementPtr>& globalBodyStmts = globalParBody->getStatements();
				
				const CallExprPtr& localParallel =  dynamic_pointer_cast<const CallExpr>(globalBodyStmts.front());
				
				const vector<ExpressionPtr>& localExpr = localParallel->getArguments();
				
				const JobExprPtr& localJob = dynamic_pointer_cast<const JobExpr>(localExpr.back());
				
				const vector<DeclarationStmtPtr>& localJobDecls = localJob->getLocalDecls();
				
				// declarations that we want to add to the body of the function
				vector<DeclarationStmtPtr> newBodyDecls;
				
				for_each(localJobDecls, [&](const DeclarationStmtPtr& curDecl) {
					unsigned newName = (curDecl->getVariable())->getId(); 
					if (dynamic_pointer_cast<const Variable>(curDecl->getInitialization())){
						unsigned oldName = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
						
						backwardVarNameMap.insert(std::make_pair(newName, oldName));
						forwardVarNameMap.insert(std::make_pair(oldName, newName));
						unsigned firstVal = getVarName(backwardVarNameMap, oldName);
						// Add local qualifier
						addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::LOCAL);
					}
					else {
						// for example: v17 = initZero(ref<real<4>> // literal
						qualifierMap.insert(std::make_pair(newName, curDecl->getVariable()));
						addQualifier(qualifierMap, newName, AddressSpaceAnnotation::addressSpace::LOCAL);
						newBodyDecls.push_back(curDecl);
					}
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
				
				// modify the DeclarationStmts that we have to add to the body
				vector<StatementPtr> newRenameBodyDecls;
				for_each(newBodyDecls, [&](const DeclarationStmtPtr& curDecl) {
					unsigned oldName = (curDecl->getVariable())->getId();
					unsigned newName = getVarName(forwardVarNameMap, oldName);
					moveQualifier(qualifierMap, oldName, newName);
					auto&& fit = qualifierMap.find(newName);
					if (fit != qualifierMap.end()) {
						newRenameBodyDecls.push_back(builder.declarationStmt(fit->second,curDecl->getInitialization()));
					} else {
						assert(false && "WTF: varName not in qualifierMap!!");
					}
				});
				
				const CompoundStmtPtr& localParBody = dynamic_pointer_cast<const CompoundStmt>(localParFct->getBody());
				
				for_each(localParBody->getStatements(), [&](const StatementPtr& curStmt) {
					newRenameBodyDecls.push_back(curStmt);
				});
				
				CompoundStmtPtr newBody = builder.compoundStmt(newRenameBodyDecls);
				
				for (uint i = 0; i < oldArgs.size()-2; i++){
					newArgs.push_back(oldArgs.at(i));
				}
				
				LambdaExprPtr&& newFunc = builder.lambdaExpr(newFuncType, newParams, newBody);
				// add the annotation to the lambda and not to the lambdaExpr for then use
				// the sinmple_backend with addFunctionPrefix
				KernelFctAnnotationPtr an(new KernelFctAnnotation());
				const LambdaPtr& newLambda = newFunc->getLambda();
				newLambda->setAnnotations(oldLambda->getAnnotations());
				newLambda->addAnnotation(an);
				
				LOG(INFO) << "----- Insieme IR generated by the OpenCL Backend -----";
				LOG(INFO) << printer::PrettyPrinter(newLambda) << std::endl;
				
				FunctionManager& funManager = getConversionContext().getFunctionManager();
				const CodeFragmentPtr& code = getCurrentCodeFragment();
				code << funManager.getFunctionName(code, newFunc);
			}
		}
	} 
	else {
		simple_backend::StmtConverter::visitLambdaExpr(ptr);
	}
}

void OclStmtConvert::visitCallExpr(const CallExprPtr& ptr) {
	ASTBuilder builder(ptr->getNodeManager());
	if (ptr->getArguments().size()) {
		// check for builtin literal (get_global_size, get_num_groups, ...)	
		const VariablePtr& var = dynamic_pointer_cast<const Variable>(ptr->getArgument(0));
		if (var){
			unsigned firstVal = getVarName(backwardVarNameMap, var->getId());
			auto&& fit = qualifierMap.find(firstVal);
			if (fit != qualifierMap.end()) {
				if ((fit->second)->hasAnnotation(BuiltinFunctionAnnotation::KEY)){
					std::vector<ExpressionPtr> newArgs;
					newArgs.push_back(ptr->getArgument(1));
					CallExprPtr call = builder.callExpr((fit->second)->getAnnotation(BuiltinFunctionAnnotation::KEY)->getBuiltinLiteral(), newArgs);
					simple_backend::StmtConverter::visitCallExpr(call);
					return;
				}
			}
		}
		// check for barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE)
		const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(ptr->getArgument(0));
		if (call) {
			const LiteralPtr& lit = dynamic_pointer_cast<const Literal>(call->getFunctionExpr());
			if (lit) {
				if (lit->getValue() == "getThreadGroup") {
					const LiteralPtr& num = dynamic_pointer_cast<const Literal>(call->getArgument(0));
					const CodeFragmentPtr& code = getCurrentCodeFragment();
					if (num->getValue() == "0") {
						code << "barrier(CLK_LOCAL_MEM_FENCE)";
						return;
					}
					else if (num->getValue() == "1") {
						code << "barrier(CLK_GLOBAL_MEM_FENCE)";
						return;
					}
					else
    					assert(false && "Error: OpenCL Backend can only translate getThreadGroup(0 | 1)" );
				}
			}
		}
		// check for get_local_id & get_global_id
		const CaptureInitExprPtr& cap = dynamic_pointer_cast<const CaptureInitExpr>(ptr->getFunctionExpr());
		if (cap){
			const CompoundStmtPtr& comp = dynamic_pointer_cast<const CompoundStmt>
						(dynamic_pointer_cast<const LambdaExpr>(cap->getLambda())->getBody());
			if (comp){
				int size = comp->getStatements().size();
				if (size > 2) { // FIXME: improve the pattern :)
					const DeclarationStmtPtr& dec1 = dynamic_pointer_cast<const DeclarationStmt>((*comp)[0]);
					const DeclarationStmtPtr& dec2 = dynamic_pointer_cast<const DeclarationStmt>((*comp)[1]);
					const SwitchStmtPtr& swt = dynamic_pointer_cast<const SwitchStmt>((*comp)[2]);
					
					if (dec1 && dec2 && swt && 
						analysis::isCallOf(dec1->getInitialization(), builder.getBasicGenerator().getGetThreadId()) &&
						analysis::isCallOf(dec2->getInitialization(), builder.getBasicGenerator().getGetThreadId())
					) {
						TypePtr t = (builder.getNodeManager()).basic.getUInt4();
						core::TypeList tList;
						tList.push_back(t);
						LiteralPtr lit = builder.literal(builder.functionType(tList, t), "get_global_id");
						CallExprPtr call = builder.callExpr(lit, ptr->getArguments());
						simple_backend::StmtConverter::visitCallExpr(call);
						return;
					}
				}
				else if (size > 1) { // FIXME: improve the pattern :)
					const DeclarationStmtPtr& dec1 = dynamic_pointer_cast<const DeclarationStmt>((*comp)[0]);
					const SwitchStmtPtr& swt = dynamic_pointer_cast<const SwitchStmt>((*comp)[1]);
					
					if (dec1 && swt && 
						analysis::isCallOf(dec1->getInitialization(), builder.getBasicGenerator().getGetThreadId())
					) {
						TypePtr t = (builder.getNodeManager()).basic.getUInt4();
						core::TypeList tList;
						tList.push_back(t);
						LiteralPtr lit = builder.literal(builder.functionType(tList, t), "get_local_id");
						CallExprPtr call = builder.callExpr(lit, ptr->getArguments());
						simple_backend::StmtConverter::visitCallExpr(call);
						return;
					}
				}
			}
		}
	}
	simple_backend::StmtConverter::visitCallExpr(ptr);
}

void OclStmtConvert::visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
	const CodeFragmentPtr& code = getCurrentCodeFragment();
	VariablePtr var = ptr->getVariable();
	// format parameter using type manager and add OpenCL Memory Qualifier
	if(var->hasAnnotation(AddressSpaceAnnotation::KEY)){
		AddressSpaceAnnotationPtr ann = var->getAnnotation(AddressSpaceAnnotation::KEY);
		switch (ann->getAddressSpace()) {
			case AddressSpaceAnnotation::addressSpace::GLOBAL: 
				code << "__global ";
				break;
			case AddressSpaceAnnotation::addressSpace::LOCAL: 
				code << "__local ";
				break;
			case AddressSpaceAnnotation::addressSpace::CONSTANT: 
				code << "__constant ";
				break;
			case AddressSpaceAnnotation::addressSpace::PRIVATE: 
				code << "__private ";
				break;
			case AddressSpaceAnnotation::addressSpace::size:
				break;
		}
	}
	StmtConverter::visitDeclarationStmt(ptr);
}

namespace detail {
	simple_backend::formatting::FormatTable getOCLFormatTable(const core::lang::BasicGenerator& basic) {

	// get basic stuff ...
	simple_backend::formatting::FormatTable res = simple_backend::formatting::getBasicFormatTable(basic);

	// ... and add OCL operators ...
	NodeManager& manager = basic.getNodeManager();
	ASTBuilder builder(manager);

	#include "insieme/simple_backend/formatting/formats_begin.inc"

	{
		TypePtr t = (manager).basic.getUInt4();
		core::TypeList tList;
		tList.push_back(t);
		TypePtr t1 = (manager).basic.getInt4();
		core::TypeList tList1;
		LiteralPtr lit1 = builder.literal(builder.functionType(tList, t), "get_global_id");
		LiteralPtr lit2 = builder.literal(builder.functionType(tList, t), "get_local_id");
		LiteralPtr lit3 = builder.literal(builder.functionType(tList, t), "get_global_size");
		LiteralPtr lit4 = builder.literal(builder.functionType(tList, t), "get_local_size");
		LiteralPtr lit5 = builder.literal(builder.functionType(tList, t), "get_num_groups");
		// FIXME: check for the prototype of this function.. why int get...()
		LiteralPtr lit6 = builder.literal(builder.functionType(tList1, t1), "get_global_offset");
		// FIXME: check for the prototype of this function.. why int get...() 
		LiteralPtr lit7 = builder.literal(builder.functionType(tList1, t1), "get_work_dimension");
		ADD_FORMATTER(res, lit1, { OUT("get_global_id("); 		VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit2, { OUT("get_local_id("); 		VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit3, { OUT("get_global_size("); 		VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit4, { OUT("get_local_size("); 		VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit5, { OUT("get_num_groups("); 		VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit6, { OUT("get_global_offset(");	VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, lit7, { OUT("get_work_dimension(");	VISIT_ARG(0); OUT(")"); });
	}

	#include "insieme/simple_backend/formatting/formats_end.inc"
	return res;
}

}

} // namespace ocl
} // namespace backend
} // namespace insieme

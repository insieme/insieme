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

#include "insieme/frontend/conversion.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/global_variables.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"

#include "insieme/core/program.h"
#include "insieme/core/lang_basic.h"

#include "insieme/c_info/naming.h"
#include "insieme/c_info/location.h"

#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_annotations.h"

#include "insieme/core/transform/node_replacer.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/Frontend/TextDiagnosticPrinter.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace {

//TODO put it into a class
void printErrorMsg(std::ostringstream& errMsg, const frontend::ClangCompiler& clangComp, const clang::Decl* decl) {

	SourceManager& manager = clangComp.getSourceManager();
    clang::SourceLocation errLoc = decl->getLocStart();
    errMsg << " at location (" << frontend::utils::Line(errLoc, manager) << ":" <<
            frontend::utils::Column(errLoc, manager) << ").\n";


    /*Crashes
    DiagnosticInfo di(&diag);
    tdc->HandleDiagnostic(Diagnostic::Level::Warning, di);*/

    clang::Preprocessor& pp =  clangComp.getPreprocessor();
    pp.Diag(errLoc, pp.getDiagnostics().getCustomDiagID(Diagnostic::Warning, errMsg.str()));
}

// Covert clang source location into a c_info::SourceLocation object to be inserted in an CLocAnnotation
c_info::SourceLocation convertClangSrcLoc(SourceManager& sm, const SourceLocation& loc) {
	FileID&& fileId = sm.getFileID(loc);
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	return c_info::SourceLocation(fileEntry->getName(), sm.getSpellingLineNumber(loc), sm.getSpellingColumnNumber(loc));
};

} // End empty namespace

namespace insieme {
namespace frontend {
namespace conversion {

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr, Program& prog, const PragmaList& pragmaList):
	// cppcheck-suppress exceptNew
	stmtConv( ConversionFactory::makeStmtConverter(*this) ),
	// cppcheck-suppress exceptNew
	typeConv( ConversionFactory::makeTypeConverter(*this) ),
	// cppcheck-suppress exceptNew
	exprConv( ConversionFactory::makeExprConverter(*this) ),
	// cppcheck-suppress exceptNew
	mgr(mgr), builder(mgr), program(prog), pragmaMap(pragmaList), currTU(NULL) { }

core::ExpressionPtr ConversionFactory::tryDeref(const core::ExpressionPtr& expr) const {
	if(core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr( refTy->getElementType(), core::lang::OP_REF_DEREF_PTR, toVector<core::ExpressionPtr>(expr) );
	}
	return expr;
}

/* Function to convert Clang attributes of declarations to IR annotations (local version)
 * currently used for:
 * 	* OpenCL address spaces
 */
core::AnnotationPtr ConversionFactory::convertAttribute(const clang::VarDecl* varDecl) const {
    if(!varDecl->hasAttrs())
    	return core::AnnotationPtr();

	const AttrVec attrVec = varDecl->getAttrs();
	std::ostringstream ss;
	ocl::BaseAnnotation::AnnotationList declAnnotation;
	try {
	for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
		if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
			std::string sr = attr->getAnnotation().str();

			//check if the declaration has attribute __private
			if(sr == "__private") {
				DVLOG(2) << "           OpenCL address space __private";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::PRIVATE ));
				continue;
			}

			//check if the declaration has attribute __local
			if(sr == "__local") {
				DVLOG(2) << "           OpenCL address space __local";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::LOCAL ));
				continue;
			}

            // TODO global also for global variables

			//check if the declaration has attribute __global
			if(sr == "__global") {
				// keywords global and local are only allowed for parameters
				if(isa<const clang::ParmVarDecl>(varDecl)) {
					DVLOG(2) << "           OpenCL address space __global";
					declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::GLOBAL ));
					continue;
				}
				ss << "Address space __global not allowed for local variable";
				throw &ss;
			}

			//check if the declaration has attribute __constant
			if(sr == "__constant") {
				if(isa<const clang::ParmVarDecl>(varDecl)) {
					DVLOG(2) << "           OpenCL address space __constant";
					declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::CONSTANT ));					continue;
				}
				ss << "Address space __constant not allowed for local variable";
				throw &ss;
			}
		}

		// Throw an error if an unhandled attribute is found
		ss << "Unexpected attribute";
		throw &ss;
	}}
	catch(std::ostringstream *errMsg) {
        //show errors if unexpected patterns were found
        printErrorMsg(*errMsg, currTU->getCompiler(), varDecl);
	}
	return std::make_shared<ocl::BaseAnnotation>(declAnnotation);
}

core::ExpressionPtr ConversionFactory::lookUpVariable(const clang::VarDecl* varDecl) {
	ConversionContext::VarDeclMap::const_iterator fit = ctx.varDeclMap.find(varDecl);
	if(fit != ctx.varDeclMap.end())
		// variable found in the map, return it
		return fit->second;

	QualType&& varTy = varDecl->getType();
	core::TypePtr&& type = convertType( varTy.getTypePtr() );
	if(!varTy.isConstQualified() && !isa<const clang::ParmVarDecl>(varDecl)) {
		// add a ref in the case of variable which are not const or declared as function parameters
		type = builder.refType(type);
	}

	// check whether this is variable is defined as local or static
	// DLOG(INFO) << varDecl->getNameAsString() << " " << varDecl->hasGlobalStorage() << " " << varDecl->hasLocalStorage();
	if(varDecl->hasGlobalStorage()) {
		assert(ctx.currGlobalVar);
		// access the global data structure
		return builder.memberAccessExpr(tryDeref(ctx.currGlobalVar), core::Identifier(varDecl->getNameAsString()));
	}

	// variable is not in the map, create a new var and add it
	core::VariablePtr&& var = builder.variable(type);
	// add the var in the map
	ctx.varDeclMap.insert( std::make_pair(varDecl, var) );

	// Add the C name of this variable as annotation
	var->addAnnotation( std::make_shared<c_info::CNameAnnotation>(varDecl->getNameAsString()) );

	// Add OpenCL attributes
	core::AnnotationPtr&& attr = convertAttribute(varDecl);
	if(attr)
		var->addAnnotation(attr);

	return var;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT VARIABLE DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

core::ExpressionPtr ConversionFactory::defaultInitVal( const core::TypePtr& type ) const {
	if( *type == core::lang::TYPE_ALPHA_VAL ) {
		return core::lang::CONST_NULL_PTR_PTR;
	}
	// handle integers initialization
    if ( core::lang::isIntegerType(*type) ) {
        // initialize integer value
        return builder.literal("0", type);
    }
    if ( *type == core::lang::TYPE_CHAR_VAL ) {
		// initialize integer value
		return builder.literal("0", type);
	}
    // handle reals initialization
    if ( core::lang::isRealType(*type) ) {
        // in case of floating types we initialize with a zero value
        return builder.literal("0.0", type);
    }
    // handle refs initialization
    if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type) ) {
        // initialize pointer/reference types with the null value
    	return builder.callExpr( type, core::lang::OP_REF_VAR_PTR, toVector( defaultInitVal(refTy->getElementType()) ) );
    }
    // handle strings initialization
    if ( *type == core::lang::TYPE_STRING_VAL ) {
        return builder.literal("", type);
    }
    // handle booleans initialization
    if ( *type == core::lang::TYPE_BOOL_VAL ) {
        // boolean values are initialized to false
        return builder.literal("false", core::lang::TYPE_BOOL_PTR);
    }
    // Handle structs initialization
    if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(type) ) {
    	core::StructExpr::Members members;
    	const core::NamedCompositeType::Entries& entries = structTy->getEntries();
    	std::for_each(entries.begin(), entries.end(),
    		[ this, &members ](const core::NamedCompositeType::Entry& curr) {
    			members.push_back(core::StructExpr::Member(curr.first, this->defaultInitVal(curr.second)));
    		}
    	);
    	return builder.structExpr(structTy, members);
    }
    if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(type) ) {
		// todo
    	assert(unionTy); // silent compiler warning
	}

    //----------------  INTIALIZE VECTORS ---------------------------------
//    const Type* elemTy = NULL;
//    size_t arraySize = 0;
//    if ( ty->isExtVectorType() ) {
//    	const TypedefType* typedefType = dyn_cast<const TypedefType>(ty);
//        assert(typedefType && "ExtVectorType has unexpected class");
//        const ExtVectorType* vecTy = dyn_cast<const ExtVectorType>( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
//        assert(vecTy && "ExtVectorType has unexpected class");
//
//        elemTy = vecTy->getElementType()->getUnqualifiedDesugaredType();
//		arraySize = vecTy->getNumElements();
//    }

    // handle vectors initialization
    if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type) ) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		return builder.callExpr(vecTy, core::lang::OP_VECTOR_INIT_UNIFORM_PTR, toVector(initVal));
		// return builder.vectorExpr( std::vector<core::ExpressionPtr>(vecTy->getSize().getValue(), initVal) );
    }
    // handle arrays initialization
    if ( core::ArrayTypePtr&& vecTy = core::dynamic_pointer_cast<const core::ArrayType>(type) ) {
    	// FIXME
    	assert(vecTy); // silent compiler warning
    	// initialization for arrays is missing, returning NULL!
    	return core::lang::CONST_NULL_PTR_PTR;
    }
    assert(false && "Default initialization type not defined");
}

/**
 * InitListExpr describes an initializer list, which can be used to initialize objects of different types,
 * InitListExpr including struct/class/union types, arrays, and vectors. For example:
 *
 * struct foo x = { 1, { 2, 3 } };
 *
 * In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the type of the
 * LHS expression.
 */
core::ExpressionPtr ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
	bool isRef = false;
	core::TypePtr currType = type;
	if(core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type)) {
		isRef = true;
		currType = refType->getElementType();
	}

	core::ExpressionPtr retExpr;
	if(core::dynamic_pointer_cast<const core::VectorType>(currType) || core::dynamic_pointer_cast<const core::ArrayType>(currType)) {
		core::TypePtr elemTy = core::dynamic_pointer_cast<const core::SingleElementType>(currType)->getElementType();
		ExpressionList elements;
		// get all values of the init expression
		for(size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const clang::Expr* subExpr = initList->getInit(i);
			core::ExpressionPtr convExpr = convertInitExpr(subExpr, elemTy);
			// If the type is a refType we have to add a VAR.REF operation
			elements.push_back( convExpr );
		}
		retExpr = builder.vectorExpr(elements);
	}

	// in the case the initexpr is used to initialize a struct/class we need to create a structExpr
	// to initialize the structure
	if(core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType)) {
		core::StructExpr::Members members;
		for(size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const core::NamedCompositeType::Entry& curr = structTy->getEntries()[i];
			members.push_back( core::StructExpr::Member(curr.first, convertInitExpr(initList->getInit(i), curr.second)) );
		}
		retExpr = builder.structExpr(members);
	}

	assert(retExpr && "Couldn't convert initialization expression");

	if(isRef)
		retExpr = builder.callExpr( type, core::lang::OP_REF_VAR_PTR, toVector( retExpr ) );
	// create vector initializator
	return retExpr;
}

core::ExpressionPtr ConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type) const {
	if(!expr)
		return defaultInitVal(type);

	if(const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr ))
		return convertInitializerList( listExpr, type );

	core::ExpressionPtr&& retExpr = convertExpr( expr );
	if(core::dynamic_pointer_cast<const core::RefType>(type))
		retExpr = builder.callExpr( type, core::lang::OP_REF_VAR_PTR, toVector( retExpr ) );
	return retExpr;
}

core::DeclarationStmtPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {

	// logging
	DVLOG(1) << "\n****************************************************************************************\n"
			 << "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n"
			 << "-> at location: (" << utils::location(varDecl->getLocation(), currTU->getCompiler().getSourceManager()) << "): ";
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang VarDecl: \n"
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::DeclarationStmtPtr retStmt;

	if(const VarDecl* definition = varDecl->getDefinition()) {
		clang::QualType clangType = definition->getType();
		if(!clangType.isCanonical())
			clangType = clangType->getCanonicalTypeInternal();

		// we cannot analyze if the variable will be modified or not, so we make it of type ref<a'> if
		// it is not declared as const, successive dataflow analysis could be used to restrict the access
		// to this variable
		core::TypePtr&& type = convertType( clangType.getTypePtr() );
		if(!clangType.isConstQualified() && !isa<clang::ParmVarDecl>(definition))
			type = builder.refType( type );

		// initialization value
		core::ExpressionPtr&& initExpr = convertInitExpr(definition->getInit(), type);

		if(definition->hasGlobalStorage()) {
			// once we encounter static variables we do remove the declaration
			return core::DeclarationStmtPtr();
		}

		// lookup for the variable in the map
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(lookUpVariable(definition));
		assert(var);
		retStmt = builder.declarationStmt( var, initExpr );
	} else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");

	}
	// logging
	DVLOG(1) << "Converted into IR stmt: ";
	DVLOG(1) << "\t" << *retStmt;
	return retStmt;
}

void ConversionFactory::attachFuncAnnotations(core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl) {
	// ---------------------- Add annotations to this function ------------------------------
	//check Attributes of the function definition
	ocl::BaseAnnotation::AnnotationList kernelAnnotation;
	if(funcDecl->hasAttrs()) {
		const clang::AttrVec attrVec = funcDecl->getAttrs();

		for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
			if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
				//get annotate string
				llvm::StringRef sr = attr->getAnnotation();

				//check if it is an OpenCL kernel function
				if(sr == "__kernel") {
					DVLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<ocl::KernelFctAnnotation>() );
				}
			}
			else if(ReqdWorkGroupSizeAttr* attr = dyn_cast<ReqdWorkGroupSizeAttr>(*I)) {
				kernelAnnotation.push_back(std::make_shared<ocl::WorkGroupSizeAnnotation>(
						attr->getXDim(), attr->getYDim(), attr->getZDim())
				);
			}
		}
	}
	// --------------------------------- OPENCL ---------------------------------------------
	// if OpenCL related annotations have been found, create OclBaseAnnotation and
	// add it to the funciton's attribute
	if(!kernelAnnotation.empty())
		node.addAnnotation( std::make_shared<ocl::BaseAnnotation>(kernelAnnotation) );

	// --------------------------------- C NAME ----------------------------------------------
	// annotate with the C name of the function
	node.addAnnotation( std::make_shared<c_info::CNameAnnotation>( funcDecl->getName() ) );

	// ----------------------- SourceLocation Annotation -------------------------------------
	// for each entry function being converted we register the location where it was originally
	// defined in the C program
	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(funcDecl->getLocStart(), funcDecl->getLocEnd());
	PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);
	if(fit != pragmaMap.getDeclarationMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

//	node.addAnnotation( std::make_shared<c_info::CLocAnnotation>(
//		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.first),
//		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.second))
//	);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl) {
	// the function is not extern, a lambdaExpr has to be created
	assert(funcDecl->hasBody() && "Function has no body!");
	DVLOG(1) << "#----------------------------------------------------------------------------------#";
	DVLOG(1) << "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl
			 << "-> at location: (" << utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager()) << "): " << std::endl
			 << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
			 << "\tEmpty map: "    << ctx.recVarExprMap.size();

	if(!ctx.isRecSubFunc) {
		// add this type to the type graph (if not present)
		ctx.funcDepGraph.addNode(funcDecl);
		if( VLOG_IS_ON(2) ) {
			ctx.funcDepGraph.print( std::cout );
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = ctx.funcDepGraph.getStronglyConnectedComponents( funcDecl );

	if( !components.empty() ) {
		// we are dealing with a recursive type
		DVLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
				 << "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(),
			[ ] (std::set<const FunctionDecl*>::value_type c) {
				DVLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
			}
		);

		if(!ctx.isRecSubFunc) {
			if(ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end()) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
				var->addAnnotation( std::make_shared<c_info::CNameAnnotation>( funcDecl->getNameAsString() ) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert(std::make_pair(funcDecl, ctx.currVar));
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if(!ctx.isRecSubFunc) {
			std::for_each(components.begin(), components.end(),
				[ this ] (std::set<const FunctionDecl*>::value_type fd) {

					// we count how many variables in the map refers to overloaded versions of the same function
					// this can happen when a function get overloaded and the cycle of recursion can happen between
					// the overloaded version, we need unique variable for each version of the function

					if(this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end()) {
						core::VariablePtr&& var = this->builder.variable( this->convertType(GET_TYPE_PTR(fd)) );
						this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
						var->addAnnotation( std::make_shared<c_info::CNameAnnotation>( fd->getNameAsString() ) );
					}
				}
			);
		}
		if( VLOG_IS_ON(2) ) {
			DVLOG(2) << "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
				[] (ConversionContext::RecVarExprMap::value_type c) {
					DVLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
				}
			);
		}
	}

	core::ExpressionPtr retLambdaExpr;

	vector<core::VariablePtr> params;
	std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
		[ &params, this ] (ParmVarDecl* currParam) {
			params.push_back( core::dynamic_pointer_cast<const core::Variable>(this->lookUpVariable(currParam)) );
		}
	);

	// before redolving the body we have to set the currGlobalVar accordingly depending if
	// this function will use the global struct or not
	core::LambdaExpr::CaptureList captureList;
	core::VariablePtr parentGlobalVar = ctx.currGlobalVar;
	if(funcDecl->isMain()) {
		ctx.currGlobalVar = ctx.globalVar;
	} else if(ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		assert(parentGlobalVar && "Global data structure not forwarded until current function.");
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStructType) );
		captureList.push_back( builder.declarationStmt(var, parentGlobalVar) );
		ctx.currGlobalVar = var;
	}

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert(!ctx.isResolvingRecFuncBody && "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");
	if(!components.empty())
		ctx.isResolvingRecFuncBody = true;
	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	ctx.isResolvingRecFuncBody = false;

	// ADD THE GLOBALS
	if(funcDecl->isMain()) {
		core::CompoundStmtPtr&& compStmt = core::dynamic_pointer_cast<const core::CompoundStmt>(body);
		assert(compStmt);
		assert(ctx.globalVar && ctx.globalStructExpr);

		std::vector<core::StatementPtr> stmts;
		stmts.push_back( builder.declarationStmt(ctx.globalVar,
				builder.callExpr( builder.refType(ctx.globalStructType), core::lang::OP_REF_VAR_PTR, toVector<core::ExpressionPtr>( ctx.globalStructExpr ) )) );
		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), std::back_inserter(stmts));
		body = builder.compoundStmt(stmts);
	}

	// reset old global var
	ctx.currGlobalVar = parentGlobalVar;

	retLambdaExpr = builder.lambdaExpr( convertType( GET_TYPE_PTR(funcDecl) ), captureList, params, body);

	if( components.empty() ) {
		attachFuncAnnotations(retLambdaExpr, funcDecl);
		// Adding the lambda function to the list of converted functions
		ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );
		return retLambdaExpr;
	}

	// this is a recurive function call
	if(ctx.isRecSubFunc) {
		// if we are visiting a nested recursive type it means someone else will take care
		// of building the rectype node, we just return an intermediate type
		return retLambdaExpr;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	core::RecLambdaDefinition::RecFunDefs definitions;
	definitions.insert( std::make_pair(recVarRef, core::dynamic_pointer_cast<const core::LambdaExpr>(retLambdaExpr)) );

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

	std::for_each(components.begin(), components.end(),
		[ this, &definitions ] (std::set<const FunctionDecl*>::value_type fd) {

			//Visual Studios 2010 fix: full namespace
			insieme::frontend::conversion::ConversionFactory::ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
			assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
			this->ctx.currVar = tit->second;

			// we remove the variable from the list in order to fool the solver,
			// in this way it will create a descriptor for this type (and he will not return the TypeVar
			// associated with this recursive type). This behaviour is enabled only when the isRecSubType
			// flag is true
			this->ctx.recVarExprMap.erase(fd);
			definitions.insert( std::make_pair(this->ctx.currVar, core::dynamic_pointer_cast<const core::LambdaExpr>(this->convertFunctionDecl(fd)) ) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;
		}
	);
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;
	// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
	// ctx->recVarExprMap.clear();

	core::RecLambdaDefinitionPtr&& definition = builder.recLambdaDefinition(definitions);
	retLambdaExpr = builder.recLambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
		[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
			auto fit = this->ctx.recVarExprMap.find(fd);
			assert(fit != this->ctx.recVarExprMap.end());
			core::ExpressionPtr&& func = builder.recLambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(fd, func) );

			this->attachFuncAnnotations(func, fd);
		}
	);
	this->attachFuncAnnotations(retLambdaExpr, funcDecl);
	return retLambdaExpr;
}

// ------------------------------------ ClangTypeConverter ---------------------------

core::ProgramPtr ASTConverter::handleTranslationUnit(const clang::DeclContext* declCtx, const TranslationUnit& tu) {
	mFact.currTU = &tu;
	analysis::GlobalVarCollector globColl(mFact.program.getClangIndexer(), mFact.ctx.globalFuncMap);
	for(DeclContext::decl_iterator it = declCtx->decls_begin(), end = declCtx->decls_end(); it != end; ++it) {
		Decl* decl = *it;

		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {
			// finds a definition of this function if any
			const FunctionDecl* definition = NULL;
			// if this function is just a declaration, and it has no definition, we just skip it
			if(!funcDecl->hasBody(definition))
				continue;

			if(definition->isMain()) {
				globColl(decl);
				DLOG(INFO) << globColl;
				auto global = globColl.createGlobalStruct(mFact);
				mFact.ctx.globalStructType = global.first;
				mFact.ctx.globalStructExpr = global.second;
				mFact.ctx.globalVar = mFact.builder.variable(mFact.builder.refType(global.first));
				core::ExpressionPtr&& lambdaExpr = mFact.convertFunctionDecl(definition);
				mProgram = core::Program::addEntryPoint(*mFact.getNodeManager(), mProgram, lambdaExpr, true /* isMain */);
			}
		}
		// we only add the variables which are actually needed!
//		else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) { }
	}
	return mProgram;
}

core::LambdaExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
	mFact.currTU = &tu;
	core::StatementPtr&& bodyStmt = mFact.convertStmt( body );
	core::CallExprPtr&& callExpr = mFact.createCallExpr(toVector<core::StatementPtr>(bodyStmt), core::lang::TYPE_UNIT);

	c_info::CLocAnnotation::ArgumentList args;
	// look for variable names
	for_each(callExpr->getArguments().begin(), callExpr->getArguments().end(), [ &args ](const core::ExpressionPtr& expr){
		// because this callexpr was created out of a stmt block, we are sure
		// input arguments are Variables
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(expr);
		assert(var && "Argument of call expression is not a variable.");
		// we also have to look at the CNameAnnotation in order to find the name of the original variable

		std::shared_ptr<c_info::CNameAnnotation>&& nameAnn = var->getAnnotation(c_info::CNameAnnotation::KEY);
		assert(nameAnn && "Variable has not CName associated");
		args.push_back( nameAnn->getName() );
	});

	core::LambdaExprPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>( callExpr->getFunctionExpr() );
	// ------ Adding source location annotation (CLocAnnotation) -------
	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(body->getLocStart(), body->getLocEnd());
	PragmaStmtMap::StmtMap::const_iterator fit = mFact.getPragmaMap().getStatementMap().find(body);
	if(fit != mFact.getPragmaMap().getStatementMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}
//	lambdaExpr.addAnnotation( std::make_shared<c_info::CLocAnnotation>(
//		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.first),
//		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.second),
//		false, // this is not a function decl
//		args)
//	);

	return lambdaExpr;
}


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

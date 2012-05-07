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

#include "insieme/frontend/convert.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/error_report.h"


#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"

#include "insieme/utils/timer.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/type_utils.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/Frontend/TextDiagnosticPrinter.h>
#include "insieme/frontend/cpp/temporary_handler.h"

using namespace clang;
using namespace insieme;

namespace fe = insieme::frontend;

namespace {

// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(SourceManager& sm, const SourceLocation& loc) {
	FileID&& fileId = sm.getMainFileID();
	assert(!fileId.isInvalid() && "File is not valid!");
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	assert(fileEntry);
	return annotations::c::SourceLocation(fileEntry->getName(), sm.getSpellingLineNumber(loc), sm.getSpellingColumnNumber(loc));
};

} // End empty namespace

namespace insieme {
namespace frontend {
namespace conversion {


const clang::idx::TranslationUnit* ConversionFactory::getTranslationUnitForDefinition(FunctionDecl*& funcDecl) {
	/*
	 * if the function is not defined in this translation unit, maybe it is defined in another we already
	 * loaded use the clang indexer to lookup the definition for this function declarations
	 */
	clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, program.getClangProgram());
	ConversionFactory::TranslationUnitPair&& ret = program.getClangIndexer().getDefinitionFor(funcEntity);

	// function declaration not found. return the current translation unit
	if ( !ret.first ) {return NULL;}

	assert(ret.first && ret.second && "Translation unit for function not found");

	// update the funcDecl pointer to point to the correct function declaration 
	funcDecl = ret.first;
	return ret.second;
}

core::ProgramPtr ASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain) {
	/*
	 * Handling of the translation unit: we have to make sure to load the translation unit where the function is
	 * defined before starting the parser otherwise reading literals results in wrong values.
	 */
	FunctionDecl* def = const_cast<FunctionDecl*>(funcDecl);
	const clang::idx::TranslationUnit* clangTU = mFact.getTranslationUnitForDefinition(def);
	assert(clangTU && "Translation unit for function not found.");
	const TranslationUnit* oldTU = mFact.currTU;

	mFact.setTranslationUnit(Program::getTranslationUnit(clangTU));

	// Extract globals starting from this entry point
	mFact.ctx.globalFuncMap.clear();
	analysis::GlobalVarCollector globColl(mFact, clangTU, mFact.program.getClangIndexer(), mFact.ctx.globalFuncMap,
			mFact.ctx.polymorphicClassMap, mFact.ctx.offsetMap, mFact.ctx.virtualFunctionIdMap,
			mFact.ctx.finalOverriderMap);

	insieme::utils::Timer t("Globals.collect");
	globColl(def);

	VLOG(1)
		<< globColl;

	//~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Thread private requires to collect all the variables which are marked to be threadprivate
	omp::collectThreadPrivate(mFact.getPragmaMap(), mFact.ctx.thread_private);

	//~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//Omp flush clause forces the flushed variable to be volatile 
	//omp::collectVolatile(mFact.getPragmaMap(), mFact.ctx.volatiles);
	//~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	mFact.ctx.globalStruct = globColl.createGlobalStruct();
	if (mFact.ctx.globalStruct.first) {
		mFact.ctx.globalVar = mFact.builder.variable(mFact.builder.refType(mFact.ctx.globalStruct.first));
		if (!mFact.ctx.polymorphicClassMap.empty()) {
			mFact.updateVFuncOffsetTableExpr();
			mFact.updateVFuncTableExpr();
		}
	}
	mFact.ctx.globalIdentMap = globColl.getIdentifierMap();
	VLOG(2)
		<< mFact.ctx.globalStruct.first;
	VLOG(2)
			<< mFact.ctx.globalStruct.second;
	VLOG(2)
			<< mFact.ctx.globalVar;
	t.stop();
	LOG(INFO)
		<< t;

	const core::ExpressionPtr& expr = core::static_pointer_cast<const core::Expression>(
			mFact.convertFunctionDecl(def, true));

	core::ExpressionPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>(expr);

	// A marker node is allowed if it contains a lambda expression
	if (!lambdaExpr) {
		lambdaExpr = dynamic_pointer_cast<const core::MarkerExpr>(expr);

		if (lambdaExpr) {
			assert(
					static_pointer_cast<const core::MarkerExpr>(expr)->getSubExpression()->getNodeType() == core::NT_LambdaExpr && "Conversion of function returned a marker expression which does not contain a lambda expression");
		}
	}assert( lambdaExpr && "Conversion of function did not return a lambda expression");
	mProgram = core::Program::addEntryPoint(mFact.getNodeManager(), mProgram, lambdaExpr /*, isMain */);
	mFact.currTU = oldTU;
	return mProgram;
}

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::NodeManager& mgr, Program& prog) :
		// cppcheck-suppress exceptNew
		stmtConv(ConversionFactory::makeStmtConvert(*this)),
		// cppcheck-suppress exceptNew
		typeConv(ConversionFactory::makeTypeConvert(*this, prog)),
		// cppcheck-suppress exceptNew
		exprConv(ConversionFactory::makeExprConvert(*this, prog)),
		// cppcheck-suppress exceptNew
		mgr(mgr), builder(mgr), program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()), currTU(NULL) {
}

ConversionFactory::~ConversionFactory() {
	// dealloc StmtConverter
	ConversionFactory::cleanStmtConvert(stmtConv);
	// dealloc StmtConverter
	ConversionFactory::cleanTypeConvert(typeConv);
	// dealloc StmtConverter
	ConversionFactory::cleanExprConvert(exprConv);
}

core::ExpressionPtr ConversionFactory::tryDeref(const core::ExpressionPtr& expr) const {
	// core::ExpressionPtr retExpr = expr;
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), mgr.getLangBasic().getRefDeref(), expr);
	}
	return expr;
}

core::TypePtr ConversionFactory::tryDeref(const core::TypePtr& type) const {
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return refTy->getElementType();
	}
	return type;
}

/*  
 *  Register call expression handlers to be used during the clang to IR conversion
 */
//void ConversionFactory::registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler) {
//	auto it = callExprHanlders.insert( std::make_pair(funcDecl, handler) );
//	assert( !it.second && "Handler for function declaration already registered." );
//}
/* Function to convert Clang attributes of declarations to IR annotations (local version) currently used for:
 * 	-> OpenCL address spaces
 */
insieme::core::NodeAnnotationPtr ConversionFactory::convertAttribute(const clang::ValueDecl* varDecl) const {
	if (!varDecl->hasAttrs()) {
		return insieme::core::NodeAnnotationPtr();
	}

	std::ostringstream ss;
	annotations::ocl::BaseAnnotation::AnnotationList declAnnotation;
	try {
		for (AttrVec::const_iterator I = varDecl->attr_begin(), E = varDecl->attr_end(); I != E; ++I) {
			if (AnnotateAttr * attr = dyn_cast<AnnotateAttr>(*I)) {
				std::string&& sr = attr->getAnnotation().str();

				//check if the declaration has attribute __private
				if ( sr == "__private" ) {
					VLOG(2) << "           OpenCL address space __private";
					declAnnotation.push_back(
							std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::PRIVATE )
					);
					continue;
				}

				//check if the declaration has attribute __local
				if ( sr == "__local" ) {
					VLOG(2) << "           OpenCL address space __local";
					declAnnotation.push_back(
							std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::LOCAL )
					);
					continue;
				}

				// TODO global also for global variables

				//check if the declaration has attribute __global
				if ( sr == "__global" ) {
					// keywords global and local are only allowed for parameters

					if(isa<const clang::ParmVarDecl>(varDecl) || varDecl->getType().getTypePtr()->isPointerType()) {
						VLOG(2) << "           OpenCL address space __global";
						declAnnotation.push_back(
								std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::GLOBAL )
						);
						continue;
					}
					ss << "Address space __global not allowed for local scalar variable";
					throw &ss;
				}

				//check if the declaration has attribute __constant
				if ( sr == "__constant" ) {
					if ( isa<const clang::ParmVarDecl>(varDecl) ) {
						VLOG(2) << "           OpenCL address space __constant";
						declAnnotation.push_back(
								std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::CONSTANT )
						);
						continue;
					}
					ss << "Address space __constant not allowed for local variable";
					throw &ss;
				}
			}

			// Throw an error if an unhandled attribute is found
			ss << "Unexpected attribute";
			throw &ss;// FIXME define an exception class for this error
		}}
	catch ( std::ostringstream *errMsg ) {
		//show errors if unexpected patterns were found
		fe::utils::compilerMessage(fe::utils::DiagnosticLevel::Warning,
				varDecl->getLocStart(),
				errMsg->str(),
				currTU->getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//													Lookup Variable
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::lookUpVariable(const clang::ValueDecl* valDecl) {

	assert(currTU && "translation unit is null");
	/*
	 * Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	 */
	ConversionContext::VarDeclMap::const_iterator fit = ctx.varDeclMap.find(valDecl);
	if (fit != ctx.varDeclMap.end()) {
		// variable found in the map
		return fit->second;
	}

	/*
	 * The variable has not been converted into IR variable yet, therefore we create the IR variable and insert it
	 * to the map for successive lookups
	 *
	 * Conversion of the variable type
	 */
	QualType&& varTy = valDecl->getType();
	VLOG(2)
		<< varTy.getAsString(); // cm

	core::TypePtr&& irType = convertType( varTy.getTypePtr() );

	//auto&& vit = std::find(getVolatiles().begin(), getVolatiles().end(), valDecl);
	//// check wether the variable is marked to be volatile 
	if (varTy.isVolatileQualified()) {
		irType = builder.volatileType(irType);
	}

	bool isOclVector = !!dyn_cast<const ExtVectorType>(varTy->getUnqualifiedDesugaredType());
	if (!(varTy.isConstQualified()
			|| (isa<const clang::ParmVarDecl>(valDecl) && ((irType->getNodeType() != core::NT_VectorType
					&& irType->getNodeType() != core::NT_ArrayType) || isOclVector ) ))) {
		// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
		// only exception are OpenCL vectors
		irType = builder.refType(irType);
	}

	/*
	 * Check whether this is variable is defined as global or static. If static, it means the variable has been already
	 * defined in the global data structure so we don't have to create an IR variable but access (via the memberAccess
	 * operation) the relative member of the global data structure.
	 */
	const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);

	if (varDecl && varDecl->hasGlobalStorage()) {
		assert( ctx.globalVar && "Accessing global variable within a function not receiving the global struct");
		// access the global data structure
		const core::lang::BasicGenerator& gen = builder.getLangBasic();

		auto&& fit = ctx.globalIdentMap.find(varDecl);
		assert( fit != ctx.globalIdentMap.end() && "Variable not within global identifiers");

		// std::cout << fit->second << std::endl;
		const core::TypePtr& memberTy = ctx.globalStruct.first->getTypeOfMember(fit->second);
		assert(memberTy && "Member not found within global struct");

		assert(
				ctx.globalVar->getType()->getNodeType() == core::NT_RefType && "Global data structure passed as a non-ref");

		core::ExpressionPtr&& retExpr = builder.callExpr(
				builder.refType( memberTy ),
				gen.getCompositeRefElem(),
				toVector<core::ExpressionPtr>(
						ctx.globalVar, builder.getIdentifierLiteral(fit->second), builder.getTypeLiteral(memberTy)
				)
		);

		auto&& vit = std::find(ctx.thread_private.begin(), ctx.thread_private.end(), varDecl);
		if (vit != ctx.thread_private.end()) {
			omp::addThreadPrivateAnnotation(retExpr);
		}

		return utils::cast(retExpr, irType);
	}

	/*
	 * The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
	 * the IR variable and insert it into the map for future lookups
	 */
	core::VariablePtr&& var = builder.variable( irType );
	VLOG(2)
		<< "IR variable" << var.getType()->getNodeType() << "" << var<<":"<<varDecl;

	ctx.varDeclMap.insert(std::make_pair(valDecl, var));

	// Add the C name of this variable as annotation
	var->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (valDecl->getNameAsString()));

	// Add OpenCL attributes
	insieme::core::NodeAnnotationPtr&& attr = convertAttribute(valDecl);
	if (attr) {
		var->addAnnotation(attr);
	}assert(currTU && "translation unit is null");
	return var;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//											CONVERT VARIABLE DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::defaultInitVal(const core::TypePtr& type) const {
	//if ( mgr.getLangBasic().isAnyRef(type) ) {
	//return mgr.getLangBasic().getNull();
	//}
	// handle integers initialization
	if (mgr.getLangBasic().isInt(type)) {
		// initialize integer value
		return builder.literal("0", type);
	}
	if (mgr.getLangBasic().isChar(type)) {
		// initialize integer value
		return builder.literal("\'\\0\'", type);
	}
	// handle reals initialization
	if (mgr.getLangBasic().isReal(type)) {
		// in case of floating types we initialize with a zero value
		return builder.literal("0.0", type);
	}
	// handle refs initialization
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		// initialize pointer/reference types with undefined
		core::TypePtr elemType = refTy->getElementType();

		core::ExpressionPtr initValue;
		if (elemType->getNodeType() == core::NT_RefType) {
			// ref<ref<...>> => this is a pointer, init with 0 (null)
			initValue = builder.callExpr(elemType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elemType));
		} else {
			initValue = defaultInitVal(elemType);
		}

		return builder.refVar(initValue);
	}
	// handle strings initializationvalDec
	if (mgr.getLangBasic().isString(type)) {
		return builder.literal("", type);
	}
	// handle booleans initialization
	if (mgr.getLangBasic().isBool(type)) {
		// boolean values are initialized to false
		return builder.literal("false", mgr.getLangBasic().getBool());
	}

	// Handle structs initialization
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(type)) {
		return builder.callExpr(structTy, mgr.getLangBasic().getInitZero(), builder.getTypeLiteral(structTy));
	}

	// Handle unions initialization
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(type)) {
		assert(unionTy);
		// TODO: for now silent compiler warning
	}

	// handle vectors initialization
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type)) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		core::ExpressionPtr ret = builder.callExpr(vecTy,
				mgr.getLangBasic().getVectorInitUniform(),
				initVal,
				builder.getIntTypeParamLiteral(vecTy->getSize())
		);
		return ret;
	}

	// handle arrays initialization
	if ( core::ArrayTypePtr&& arrTy = core::dynamic_pointer_cast<const core::ArrayType>(type)) {
		if (arrTy->getElementType()->getNodeType() == core::NT_RefType) {
			const core::RefTypePtr& ref = core::static_pointer_cast<const core::RefType>(arrTy->getElementType());
			if (ref->getElementType()->getNodeType() != core::NT_VectorType) {
				return builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(arrTy));
			}
		}
		return builder.callExpr(arrTy, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(arrTy));
	}

	// handle any-ref initialization
	if (mgr.getLangBasic().isAnyRef(type)) {
		return mgr.getLangBasic().getNull();
	}

	// Initialization for volatile types
	if (core::analysis::isVolatileType(type)) {
		return builder.callExpr(mgr.getLangBasic().getVolatileMake(),
				defaultInitVal(core::analysis::getVolatileType(type)));
	}

	LOG(ERROR)
		<< "Default initializer for type: '" << *type << "' not supported!";
	assert(false && "Default initialization type not defined");
}

core::DeclarationStmtPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {
	assert(currTU && "translation unit is null");
	// logging
	VLOG(1)
		<< "\n****************************************************************************************\n"
				<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
				<< utils::location(varDecl->getLocation(), currTU->getCompiler().getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)
			<< "Dump of clang VarDecl: \n"
					<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::DeclarationStmtPtr retStmt;
	assert(currTU && "translation unit is null");
	if ( const VarDecl* definition = varDecl->getDefinition()) {
		clang::QualType clangType = definition->getType();
		if (!clangType.isCanonical()) {
			clangType = clangType->getCanonicalTypeInternal();
		}
		const Type* typePtr = clangType.getTypePtr();

		if (definition->hasGlobalStorage()) {
			// once we encounter static variables we do remove the declaration
			throw GlobalVariableDeclarationException();
		}

		// lookup for the variable in the map
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(lookUpVariable(definition));

		assert(currTU && "translation unit is null");
		assert(var);
		/* removed due to match OpenCL standard
		 // flag to determine if a variable should be initialized with zeros instead of uninitialized
		 bool zeroInit = false;

		 core::NodeType kind = var->getNodeType();
		 if (kind == core::NT_RefType) {
		 kind = core::static_pointer_cast<const core::RefType>(var->getType())->getElementType()->getNodeType();
		 }
		 // check for annotations which would lead to a zero init annotation
		 if(kind == core::NT_ArrayType || kind == core::NT_VectorType) {
		 if(var->hasAnnotation(annotations::ocl::BaseAnnotation::KEY)){
		 auto&& declarationAnnotation = var->getAnnotation(annotations::ocl::BaseAnnotation::KEY);
		 for(annotations::ocl::BaseAnnotation::AnnotationList::const_iterator I = declarationAnnotation->getAnnotationListBegin();
		 I < declarationAnnotation->getAnnotationListEnd(); ++I) {
		 if(annotations::ocl::AddressSpaceAnnotationPtr&& as = std::dynamic_pointer_cast<annotations::ocl::AddressSpaceAnnotation>(*I)){
		 if(annotations::ocl::AddressSpaceAnnotation::addressSpace::LOCAL == as->getAddressSpace() ||
		 annotations::ocl::AddressSpaceAnnotation::addressSpace::PRIVATE == as->getAddressSpace()) {
		 //TODO check why this fails:
		 //assert(!definition->getInit() && "OpenCL local variables cannot have an initialization expression");
		 zeroInit = true;
		 }
		 }
		 }
		 }
		 }
		 */
		/*
		 if(definition->getAnnotation().str() == "__local") {
		 zeroInit = true;
		 */

		// check if we have a reference or pointer -> need the pointee-type
		if (typePtr->isPointerType() || typePtr->isReferenceType()) {
			//VLOG(2) << var << " isPointerType " << typePtr->isPointerType();
			//VLOG(2) << var << " isReferenceType " << typePtr->isReferenceType();

			// get pointeetype ("deref" pointerType)
			typePtr = typePtr->getPointeeType().getTypePtr();
			//VLOG(2) << var << " PointeeType->isStructOrClassType " << typePtr->isStructureOrClassType();
		}

		//change thisstack2 only if we have a CXX object, pointer-to-CXX object, reference-to-CXX-object
		if (typePtr->isStructureOrClassType()) {

			ctx.thisStack2 = var;

		}

		// initialization value
		core::ExpressionPtr&& initExpr = convertInitExpr(definition->getInit(), var->getType(), false);
		assert(initExpr && "not correct initialization of the variable");

		retStmt = builder.declarationStmt(var, initExpr);

	}

	else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");

	}
// logging
	VLOG(2)
		<< "End of converting VarDecl";
	VLOG(1)
		<< "Converted into IR stmt: ";
	VLOG(1)
		<< "\t" << *retStmt;
	return retStmt;
}

core::ExpressionPtr ConversionFactory::attachFuncAnnotations(const core::ExpressionPtr& node,
		const clang::FunctionDecl* funcDecl) {
// ----------------------------------- Add annotations to this function -------------------------------------------
// check Attributes of the function definition
	annotations::ocl::BaseAnnotation::AnnotationList kernelAnnotation;

	if (funcDecl->hasAttrs()) {
		const clang::AttrVec attrVec = funcDecl->getAttrs();

		for (AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
			if (AnnotateAttr * attr = dyn_cast<AnnotateAttr>(*I)) {
				//get annotate string
				llvm::StringRef&& sr = attr->getAnnotation();

				//check if it is an OpenCL kernel function
				if ( sr == "__kernel" ) {
					VLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<annotations::ocl::KernelFctAnnotation>() );
				}
			}
			else if ( ReqdWorkGroupSizeAttr* attr = dyn_cast<ReqdWorkGroupSizeAttr>(*I) ) {
				kernelAnnotation.push_back(
						std::make_shared<annotations::ocl::WorkGroupSizeAnnotation>( attr->getXDim(), attr->getYDim(), attr->getZDim() )
				);
			}
		}
	}

// -------------------------------------------------- C NAME ------------------------------------------------------

// check for overloaded operator "function" (normal function has kind OO_None)
	clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
	if (operatorKind != OO_None) {
		string operatorAsString = boost::lexical_cast<string>(operatorKind);
		node->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > ("operator" + operatorAsString));
	} else {
		// annotate with the C name of the function
		node->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));
	}

// ---------------------------------------- SourceLocation Annotation ---------------------------------------------
	/*
	 * for each entry function being converted we register the location where it was originally defined in the C program
	 */
	std::pair<SourceLocation, SourceLocation>&& loc = std::make_pair(funcDecl->getLocStart(), funcDecl->getLocEnd());
	fe::pragma::PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);

	if (fit != pragmaMap.getDeclarationMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

	assert(currTU && "Translation unit not correctly set");
	node->addAnnotation(
			std::make_shared < annotations::c::CLocAnnotation
					> (convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.first), convertClangSrcLoc(
							currTU->getCompiler().getSourceManager(), loc.second)));

// ---------------------------------------------------- OPENCL ----------------------------------------------------
// if OpenCL related annotations have been found, create OclBaseAnnotation and add it to the funciton's attribute
	if (!kernelAnnotation.empty()) {
		// create new marker node
		core::MarkerExprPtr&& marker = builder.markerExpr(node);
		marker->addAnnotation( std::make_shared<annotations::ocl::BaseAnnotation>(kernelAnnotation) );
		return marker;
	}

	return node;
}

core::LambdaExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
	mFact.currTU = &tu;
//	core::StatementPtr&& bodyStmt = mFact.convertStmt( body );
//	core::ExpressionPtr&& callExpr = mFact.createCallExpr( toVector<core::StatementPtr>(bodyStmt), mgr.getLangBasic().getUnit() );

//	annotations::c::CLocAnnotation::ArgumentList args;
//	if(core::CaptureInitExprPtr&& captureExpr = core::dynamic_pointer_cast<const core::CaptureInitExpr>(callExpr)) {
//		// look for variable names
//		for_each(captureExpr->getArguments().begin(), captureExpr->getArguments().end(), [ &args ](const core::ExpressionPtr& expr){
//			// because this callexpr was created out of a stmt block, we are sure
//			// input arguments are Variables
//			core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(expr);
//			assert(var && "Argument of call expression is not a variable.");
//			// we also have to look at the CNameAnnotation in order to find the name of the original variable
//
//			std::shared_ptr<annotations::c::CNameAnnotation>&& nameAnn = var->getAnnotation(annotations::c::CNameAnnotation::KEY);
//			assert(nameAnn && "Variable has not CName associated");
//			args.push_back( nameAnn->getName() );
//		});
//	}

//	core::LambdaExprPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>( callExpr->getFunctionExpr() );
//	// ------ Adding source location annotation (CLocAnnotation) -------
//	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(body->getLocStart(), body->getLocEnd());
//	PragmaStmtMap::StmtMap::const_iterator fit = mFact.getPragmaMap().getStatementMap().find(body);
//	if(fit != mFact.getPragmaMap().getStatementMap().end()) {
//		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
//		loc.first = fit->second->getStartLocation();
//	}
//
//	lambdaExpr.addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
//		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.first),
//		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.second),
//		false, // this is not a function decl
//		args)
//	);
//
//	return lambdaExpr;
	return core::LambdaExprPtr();
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

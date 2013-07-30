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

#include <functional>

#include "insieme/frontend/clang.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/utils/source_locations.h"

#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/castTool.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/analysis/prunable_decl_visitor.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/annotations/c/location.h"
#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

using namespace clang;
using namespace insieme;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//   ANONYMOUS NAMESPACE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace {

/// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(SourceManager& sm, const SourceLocation& loc) {
	FileID&& fileId = sm.getMainFileID();
	assert(!fileId.isInvalid() && "File is not valid!");
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	assert(fileEntry);
	return annotations::c::SourceLocation(fileEntry->getName(), sm.getSpellingLineNumber(loc), sm.getSpellingColumnNumber(loc));
};

/// convert a initialization for a global
core::ExpressionPtr convertInitForGlobal (insieme::frontend::conversion::Converter& converter, const clang::VarDecl* var, core::TypePtr elementType){

	auto builder = converter.getIRBuilder();
	// and get the initial value
	core::ExpressionPtr initValue;

	if (var->hasInit() && !var->isStaticLocal()) {
		initValue = converter.convertInitExpr(var->getType().getTypePtr(), var->getInit(), elementType, true);
	}
	else if (clang::VarDecl* outDecl = const_cast<clang::VarDecl*>(var)->getOutOfLineDefinition ()){
		// initialization be out of class or something else, beware of dependent types
		if (!outDecl->getAnyInitializer()->getType().getTypePtr()->isDependentType() &&
			!outDecl->getAnyInitializer()->isInstantiationDependent())
			initValue = converter.convertInitExpr (var->getType().getTypePtr(), outDecl->getAnyInitializer(), elementType, false);
	}

	if (initValue){
		// strip of potential ref.var call ...
		if (core::analysis::isCallOf(initValue, builder.getNodeManager().getLangBasic().getRefVar())) {
			initValue = initValue.as<core::CallExprPtr>()[0];
		}

		// de-ref init values (for constructor calls)
		if (!core::types::isSubTypeOf(initValue->getType(), elementType)) {
			initValue = builder.deref(initValue);
		}

		assert(core::types::isSubTypeOf(initValue->getType(), elementType));
	}
	return initValue;
}


} // End empty namespace
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace insieme {
namespace frontend {

// ----------- conversion ------------

tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup) {
	// just delegate operation to converter
	Program program(manager, unit, setup);
	return conversion::Converter(manager, program).convert();
}


namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERTER
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//////////////////////////////////////////////////////////////////
///
Converter::Converter(core::NodeManager& mgr, const Program& prog) :
		staticVarCount(0), mgr(mgr), builder(mgr),
		program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()),
		irTranslationUnit(mgr), used(false)
		{

		if (prog.isCxx()){
			typeConvPtr = std::make_shared<CXXTypeConverter>(*this);
			exprConvPtr = std::make_shared<CXXExprConverter>(*this);
			stmtConvPtr = std::make_shared<CXXStmtConverter>(*this);
		} else{
			typeConvPtr = std::make_shared<CTypeConverter>(*this);
			exprConvPtr = std::make_shared<CExprConverter>(*this);
			stmtConvPtr = std::make_shared<CStmtConverter>(*this);
		}
}


tu::IRTranslationUnit Converter::convert() {
	assert(!used && "This one must only be used once!");
	used = true;

	assert(getCompiler().getASTContext().getTranslationUnitDecl());

	// Thread private requires to collect all the variables which are marked to be threadprivate
	omp::collectThreadPrivate(getPragmaMap(), thread_private);

	// collect all type definitions
	auto declContext = clang::TranslationUnitDecl::castToDeclContext(getCompiler().getASTContext().getTranslationUnitDecl());

	struct TypeVisitor : public analysis::PrunableDeclVisitor<TypeVisitor> {

		Converter& converter;
		TypeVisitor(Converter& converter) : converter(converter) {}

		void VisitRecordDecl(const clang::RecordDecl* typeDecl) {
			// we do not convert templates or partial spetialized classes/functions, the full
			// type will be found and converted once the instantaion is found
			converter.convertType(typeDecl->getTypeForDecl());
		}
		void VisitTypedefDecl(const clang::TypedefDecl* typeDecl) {
			// extract new symbol name
			auto symbol = converter.getIRBuilder().genericType(typeDecl->getQualifiedNameAsString());

			// get contained type
			auto res = converter.convertType(typeDecl->getUnderlyingType().getTypePtr());

			// frequently structs and their type definitions have the same name => in this case symbol == res and should be ignored
			if (res != symbol && res.isa<core::NamedCompositeTypePtr>()) {	// also: skip simple type-defs
				converter.getIRTranslationUnit().addType(symbol, res);
			}
		}
	} typeVisitor(*this);

	//typeVisitor.TraverseDecl(llvm::cast<clang::Decl>(declContext));
	typeVisitor.traverseDeclCtx (declContext);

	// collect all global declarations
	struct GlobalVisitor : public analysis::PrunableDeclVisitor<GlobalVisitor> {

		Converter& converter;
		GlobalVisitor(Converter& converter) : converter(converter) {}

		void VisitVarDecl(const clang::VarDecl* var) {
			// variables to be skipped
			if (!var->hasGlobalStorage()) return;
			if (var->hasExternalStorage()) return;
			if (var->isStaticLocal()) return;

			auto builder = converter.getIRBuilder();
			// obtain type
			auto type = converter.convertType(var->getType().getTypePtr());

			// all globals are mutable ..
			auto elementType = type;
			type = builder.refType(type);

			auto literal = builder.literal(type, utils::buildNameForVariable(var));
			if (insieme::utils::set::contains(converter.getThreadprivates(), var)) {
				omp::addThreadPrivateAnnotation(literal);
			}

			// NOTE: not all staticDataMember are seen here, so we take care of the "unseen"
			// ones in lookUpVariable

			auto initValue = convertInitForGlobal(converter, var, elementType);
			converter.getIRTranslationUnit().addGlobal(literal, initValue);
		}
	} varVisitor(*this);

	varVisitor.traverseDeclCtx(declContext);

	// collect all global declarations
	struct FunctionVisitor : public analysis::PrunableDeclVisitor<FunctionVisitor> {

		Converter& converter;
		FunctionVisitor(Converter& converter) : converter(converter) {}

		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {
			if (funcDecl->isTemplateDecl()) return;
			if (!funcDecl->doesThisDeclarationHaveABody()) return;
			converter.convertFunctionDecl(funcDecl);
			return;
		}
	} funVisitor(*this);

	funVisitor.traverseDeclCtx(declContext);

	// handle entry points (marked using insieme pragmas)
	for(pragma::PragmaPtr pragma : program.getPragmaList()) {
		// only interested in insieme-mark pragmas
		if (pragma->getType() != "insieme::mark") continue;
		const pragma::Pragma& insiemePragma = *pragma;
		if(!insiemePragma.isDecl()) continue;

		// this is a declaration, if it's a function add it to the entry points of the program
		const clang::FunctionDecl* funcDecl = dyn_cast<const clang::FunctionDecl>(insiemePragma.getDecl());
		assert(funcDecl && "Pragma insieme only valid for function declarations.");
		getIRTranslationUnit().addEntryPoints(convertFunctionDecl(funcDecl).as<core::LiteralPtr>());
	}

	// that's all
	return irTranslationUnit;
}


//////////////////////////////////////////////////////////////////
///
//void Converter::buildGlobalStruct(analysis::GlobalVarCollector& globColl){

	////~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//// Thread private requires to collect all the variables which are marked to be threadprivate
	//omp::collectThreadPrivate(getPragmaMap(), thread_private);

	////~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	////Omp flush clause forces the flushed variable to be volatile
	////omp::collectVolatile(getPragmaMap(), volatiles);
	////~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	//globalStruct = globColl.createGlobalStruct();
	//if (globalStruct.first) {
	//	globalVar = builder.variable(builder.refType(globalStruct.first));
	//}

	//globalIdentMap = globColl.getIdentifierMap();
	//globalFuncSet = globColl.getUsingGlobals();

	//VLOG(1) << "globals collected";
	//VLOG(2) << globalStruct.first;
	//VLOG(2) << globalStruct.second;
	//VLOG(2) << globalVar;
//}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::materializeReadOnlyParams(const core::StatementPtr& body, const vector<core::VariablePtr>& params){

	vector<core::StatementPtr> decls;
	core::StatementPtr newBody = body;

	for (auto currParam : params){
		auto fit = this->wrapRefMap.find(currParam);
		if ( fit != this->wrapRefMap.end() ) {

			// if the variable is never written in the function body, avoid materialization
			const core::VariablePtr& wrap = fit->second;
			if (core::analysis::isReadOnly(body, wrap)){
				// replace read uses
				newBody = core::transform::replaceAllGen (mgr, newBody, builder.deref(wrap), currParam, true);
				newBody = core::transform::replaceAllGen (mgr, newBody, wrap, builder.refVar(currParam), true);
				// this variables might apear in annotations inside:
				core::visitDepthFirstOnce (newBody, [&] (const core::StatementPtr& node){
					//if we have a OMP annotation
					if (node->hasAnnotation(omp::BaseAnnotation::KEY)){
						auto anno = node->getAnnotation(omp::BaseAnnotation::KEY);
						assert(anno);
						anno->replaceUsage (wrap, currParam);
					}
				});
				//cleanup the wrap cache to avoid future uses, this var does not exist anymore
				wrapRefMap.erase(currParam);
			}
			else{

				// FIXME:  structs pased as value will be wrapped ANYWAY...
				//   if i have a READ operation on a struct:   v= x->a;
				//   it wont be recognized as read only as the base is pased by reference
				//	this turns into an extra copy at the begining of every function

				// other case materialize a var, declare it before body
				decls.push_back( this->builder.declarationStmt(fit->second, this->builder.refVar( fit->first ) ));
			}
		}
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if (!decls.empty()) {
		// push the old body
		decls.push_back(newBody);
		newBody = builder.compoundStmt(decls);
	}
	return newBody;
}


//////////////////////////////////////////////////////////////////
///
void Converter::printDiagnosis(const clang::SourceLocation& loc){

	clang::Preprocessor& pp = getPreprocessor();
	// print warnings and errors:
	while (!warnings.empty()){
		if (getSourceManager().isLoadedSourceLocation (loc)){
			std::cerr << "loaded location:\n";
			std::cerr << "\t" << *warnings.begin() << std::endl;
		}
		else{
			pp.Diag(loc, pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Warning, *warnings.begin()) );
		}
		warnings.erase(warnings.begin());
	}
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::tryDeref(const core::ExpressionPtr& expr) const {
	// core::ExpressionPtr retExpr = expr;
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), mgr.getLangBasic().getRefDeref(), expr);
	}
	return expr;
}

//////////////////////////////////////////////////////////////////
///
core::TypePtr Converter::tryDeref(const core::TypePtr& type) const {
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return refTy->getElementType();
	}
	return type;
}

//////////////////////////////////////////////////////////////////
///
// Register call expression handlers to be used during the clang to IR conversion
//void Converter::registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler) {
//	auto it = callExprHanlders.insert( std::make_pair(funcDecl, handler) );
//	assert( !it.second && "Handler for function declaration already registered." );
//}
//  Function to convert Clang attributes of declarations to IR annotations (local version) currently used for:
// 	-> OpenCL address spaces
core::NodeAnnotationPtr Converter::convertAttribute(const clang::ValueDecl* varDecl) const {
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
				getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::lookUpVariable(const clang::ValueDecl* valDecl) {
	VLOG(1) << "LOOKUP Variable: " << valDecl->getNameAsString();
	if (VLOG_IS_ON(1)) valDecl->dump();

	// Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	auto varCacheHit = varDeclMap.find(valDecl);
	if (varCacheHit != varDeclMap.end()) {
		// variable found in the map
		return varCacheHit->second;
	}

	// The variable has not been converted into IR variable yet, therefore we create the IR variable and insert it
	// to the map for successive lookups

	// Conversion of the variable type
	QualType&& varTy = valDecl->getType();
	core::TypePtr&& irType = convertType( varTy.getTypePtr() );
	assert(irType && "type conversion for variable failed");

	VLOG(2)	<< "clang type: " << varTy.getAsString();
	VLOG(2)	<< "ir type:    " << irType;

	//// check whenever the variable is marked to be volatile
	if (varTy.isVolatileQualified()) {
		irType = builder.volatileType(irType);
	}

	bool isOclVector = !!dyn_cast<const ExtVectorType>(varTy->getUnqualifiedDesugaredType());
	if (!(varTy.isConstQualified() ||    						// is a constant
		 varTy.getTypePtr()->isReferenceType()  ||             // is a c++ reference
 	 	 (isa<const clang::ParmVarDecl>(valDecl) && 			// is the declaration of a parameter
		 ((irType->getNodeType() != core::NT_VectorType && irType->getNodeType() != core::NT_ArrayType) ||
		   isOclVector ) ))) {
		// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
		// only exception are OpenCL vectors
		irType = builder.refType(irType);
	}

	// if is a global variable, a literal will be generated, with the qualified name
	// (two qualified names can not coexist within the same TU)
	const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);
	if (varDecl && varDecl->hasGlobalStorage()) {
		VLOG(2)	<< varDecl->getQualifiedNameAsString() << " with global storage";
		// we could look for it in the cache, but is fast to create a new one, and we can not get
		// rid if the qualified name function
		std::string name = utils::buildNameForVariable(varDecl);
 		if(varDecl->isStaticLocal()) {
			VLOG(2)	<< "         isStaticLocal";
            if(staticVarDeclMap.find(varDecl) != staticVarDeclMap.end()) {
                name = staticVarDeclMap.find(varDecl)->second;
            } else {
                std::stringstream ss;
                //get source location and src file path
                //hash this string to create unique variable names
                clang::FullSourceLoc f(varDecl->getLocation(),getSourceManager());
                std::hash<std::string> str_hash;
                ss << name << str_hash(getSourceManager().getFileEntryForID(f.getFileID())->getName()) << staticVarCount++;
                staticVarDeclMap.insert(std::pair<const clang::VarDecl*,std::string>(varDecl,ss.str()));
                name = ss.str();
            }
        }

		// global/static variables are always leftsides (refType) -- solves problem with const
		if(!irType.isa<core::RefTypePtr>() ) {
			irType = builder.refType(irType);
		}

		if (varDecl->isStaticLocal()){
			if (!irType.isa<core::RefTypePtr>()) irType = builder.refType(irType);		// this happens whenever a static variable is constant
			irType = builder.refType (mgr.getLangExtension<core::lang::StaticVariableExtension>().wrapStaticType(irType.as<core::RefTypePtr>().getElementType()));
		}

		core::ExpressionPtr globVar =  builder.literal(name, irType);
		if (varDecl->isStaticLocal()){
			globVar = builder.accessStatic(globVar.as<core::LiteralPtr>());
		}

		// some member statics might be missing because of defined in a template which was ignored
		// since this is the fist time we get access to the complete type, we can define the
		// suitable initialization
		if (varDecl->isStaticDataMember()){
			VLOG(2)	<< "         is static data member";
			core::TypePtr&& elementType = convertType( varTy.getTypePtr() );
			auto initValue = convertInitForGlobal(*this, varDecl, elementType);
			// as we don't see them in the globalVisitor we have to take care of them here
			getIRTranslationUnit().addGlobal(globVar.as<core::LiteralPtr>(), initValue);
		}

		// OMP threadPrivate
 		if (insieme::utils::set::contains (thread_private, varDecl)){
			omp::addThreadPrivateAnnotation(globVar);
		}

		varDeclMap.insert( { valDecl, globVar } );
		return globVar;
	}

	// The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
	// the IR variable and insert it into the map for future lookups
	core::VariablePtr&& var = builder.variable( irType );
	VLOG(2) << "IR variable" << var.getType()->getNodeType() << "" << var<<":"<<varDecl->getNameAsString();
	VLOG(2) << "IR var type" << var.getType();

	varDeclMap.insert( { valDecl, var } );

	if ( !valDecl->getNameAsString().empty() ) {
		// Add the C name of this variable as annotation
		core::annotations::attachName(var,varDecl->getNameAsString());
	}

	// Add OpenCL attributes
	insieme::core::NodeAnnotationPtr&& attr = convertAttribute(valDecl);
	if (attr) {
		var->addAnnotation(attr);
	}

	return var;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::defaultInitVal(const core::TypePtr& valueType) const {

	// get type details
	core::TypePtr type = lookupTypeDetails(valueType);

	// Primitive types
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
		if(mgr.getLangBasic().isReal4(type))
			return builder.literal("0.0f", type);
		if(mgr.getLangBasic().isReal8(type))
			return builder.literal("0.0", type);
	}
	// handle booleans initialization
	if (mgr.getLangBasic().isBool(type)) {
		// boolean values are initialized to false
		return builder.literal("false", mgr.getLangBasic().getBool());
	}


	// FIXME: All types should have a default initialitation to undefined value
//	if (mgr.getLangBasic().isPrimitive(type)){
//		return builder.callExpr(mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(type));
//	}

	// handle strings initializationvalDec
	if (mgr.getLangBasic().isString(type)) {
		return builder.literal("", type);
	}

	// resolve symbol


	// Initialization for volatile types
	if (core::analysis::isVolatileType(type)) {
		return builder.callExpr(mgr.getLangBasic().getVolatileMake(),
				defaultInitVal(core::analysis::getVolatileType(type)));
	}

	core::TypePtr curType = type;
	if (type->getNodeType() == core::NT_RecType) {
		curType = type.as<core::RecTypePtr>()->unroll();
	}

	// Handle structs initialization
	if ( curType.isa<core::StructTypePtr>()) {
		return builder.getZero(type);
	}

	// Handle unions initialization
	if ( curType.isa<core::UnionTypePtr>()) {
		return builder.getZero(type);
	}

	// handle vectors initialization
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(curType)) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		return builder.callExpr(vecTy,
				mgr.getLangBasic().getVectorInitUniform(),
				initVal,
				builder.getIntTypeParamLiteral(vecTy->getSize())
		);
	}

	// handle any-ref initialization
	if (mgr.getLangBasic().isAnyRef(type)) {
		return mgr.getLangBasic().getRefNull();
	}

	assert(core::analysis::isRefType(curType) && "We cannot initialize any different type of non-ref");

	core::RefTypePtr refType = curType.as<core::RefTypePtr>();

	// handle arrays initialization
	if ( core::ArrayTypePtr&& arrTy = core::dynamic_pointer_cast<const core::ArrayType>(refType->getElementType())) {
		return builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(arrTy));
	}

	// handle refs initialization
	// initialize pointer/reference types with undefined
	core::TypePtr elemType = refType->getElementType();

	core::ExpressionPtr initValue;
	if (elemType->getNodeType() == core::NT_RefType) {
		// ref<ref<...>> => this is a pointer, init with 0 (null)
		initValue = builder.callExpr(elemType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elemType));
	} else {
		initValue = defaultInitVal(elemType);
	}
	return builder.refVar(initValue);


	// LOG(ERROR) << "Default initializer for type: '" << *type << "' not supported!";
	// assert(false && "Default initialization type not defined");
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::convertVarDecl(const clang::VarDecl* varDecl) {
	// logging
	VLOG(1)	<< "\n****************************************************************************************\n"
			<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
			<< utils::location(varDecl->getLocation(), getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)	<< "Dump of clang VarDecl: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::StatementPtr retStmt;
	if ( const VarDecl* definition = varDecl->getDefinition()) {
		// lookup for the variable in the map
		core::ExpressionPtr var = lookUpVariable(definition);
		printDiagnosis(definition->getLocStart());

		//TODO we should visit only staticlocal!!! change to isStaticLocal
		//if (definition->hasGlobalStorage()) {
		if (definition->isStaticLocal()) {
			// is the declaration of a variable with global storage, this means that is an static
			// static needs to be initialized during first execution of function.
			// but the var remains in the global storage (is an assigment instead of decl)
			//
			assert(var);
			assert(var.isa<core::CallExprPtr>());
			// the variable is being unwrapped by default in lookupVariable
			// we want the inner static object
			auto lit = var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>();

			if (definition->getInit())
				retStmt = builder.initStaticVariable(lit, convertInitExpr(definition->getType().getTypePtr(),
																		  definition->getInit(),
																		  var->getType().as<core::RefTypePtr>().getElementType(), false));
			else
				retStmt = builder.getNoOp();
		}
		else{
			// print diagnosis messages
			assert(var.isa<core::VariablePtr>());
			// initialization value
			core::ExpressionPtr initExpr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(), var->getType(), false);
			assert(initExpr && "not correct initialization of the variable");
			retStmt = builder.declarationStmt(var.as<core::VariablePtr>(), initExpr);
		}
	} else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");
	}

	VLOG(2)	<< "End of converting VarDecl";
	VLOG(1)	<< "Converted into IR stmt: ";
	VLOG(1)	<< "\t" << *retStmt;
	return retStmt;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::attachFuncAnnotations(const core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl) {
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

	pragma::attachPragma(node,funcDecl,*this).as<core::StatementPtr>();

// -------------------------------------------------- C NAME ------------------------------------------------------

// check for overloaded operator "function" (normal function has kind OO_None)
	clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
	if (operatorKind != OO_None) {
		string operatorAsString = boost::lexical_cast<string>(operatorKind);
		core::annotations::attachName(node,("operator" + operatorAsString));
	} else if( !funcDecl->getNameAsString().empty() ) {
		// annotate with the C name of the function
		core::annotations::attachName(node,(funcDecl->getNameAsString()));
	}

// ---------------------------------------- SourceLocation Annotation ---------------------------------------------
	/*
	 * for each entry function being converted we register the location where it was originally defined in the C program
	 */
	std::pair<SourceLocation, SourceLocation> loc { funcDecl->getLocStart(), funcDecl->getLocEnd() };
	fe::pragma::PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);

	if (fit != pragmaMap.getDeclarationMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

	node->addAnnotation(
			std::make_shared < annotations::c::CLocAnnotation
					> (convertClangSrcLoc(getSourceManager(), loc.first), convertClangSrcLoc(
							getSourceManager(), loc.second)));

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

//////////////////////////////////////////////////////////////////
///
/// InitListExpr describes an initializer list, which can be used to initialize objects of different
/// types, InitListExpr including struct/class/union types, arrays, and vectors. For example:
/// struct foo x = { 1, { 2, 3 } };
/// In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the
/// type of the LHS expression.
core::ExpressionPtr
Converter::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type)  {
	const Converter& convFact = *this;
	core::ExpressionPtr retIr;

//	ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_EXPR_CONVERSION(initList, retIr);

	// resolve potential type symbol within current translation unit
	core::TypePtr currType = lookupTypeDetails(type);

	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type)) {
		currType = lookupTypeDetails(refType->getElementType());
	}

	// Handles recursive types. Unroll once in order to reveal the actual type (hopefully it will be
	// a struct type)
	if (currType->getNodeType() == core::NT_RecType) {
		currType = currType.as<core::RecTypePtr>()->unroll();
	}

	if (currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType) {

		auto elemTy = currType.as<core::SingleElementTypePtr>()->getElementType();

		ExpressionList elements;
		// get all values of the init expression
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const clang::Expr* subExpr = initList->getInit(i);

			auto convExpr = convertInitExpr(NULL /*FIXME*/, subExpr, elemTy, false);

			assert(convExpr && "convExpr is empty");

			elements.push_back(utils::cast(convExpr, elemTy));
		}

		retIr = builder.vectorExpr(elements);
	}


	// in the case the initexpr is used to initialize a struct/class we need to create a structExpr
	// to initialize the structure
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType) ) {

		core::StructExpr::Members members;
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {

			const core::NamedTypePtr& curr = structTy->getEntries()[i];
			members.push_back(builder.namedValue(
						curr->getName(),
						convertInitExpr(NULL, initList->getInit(i), curr->getType(), false))
				);
		}
		retIr = builder.structExpr(structTy, members);
	}

	// in the case the initexpr is used to initialize a union
	//
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(currType)) {

		auto ie = convertInitExpr(NULL, initList->getInit(0), unionTy->getEntries()[0]->getType(), false);
		retIr = builder.unionExpr(unionTy, unionTy->getEntries()[0]->getName(), ie);

	//	core::StructExpr::Members members;
	//	for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
	//		const core::NamedTypePtr& curr = structTy->getEntries()[i];
	//		members.push_back(
	//				builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
	//	}
	//	retIr = builder.structExpr(members);
	}


	// debugging info if it failed
	if (!retIr) {
		std::cout << "Input:  "; initList->dump(); std::cout << "\n";
		std::cout << "Type:   " << *type << "\n";
		std::cout << "Detail: " << *currType << "\n";
		assert(false && "Couldn't convert initialization expression");
	}

	// create vector initializator
	return retIr;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr
Converter::convertInitExpr(const clang::Type* clangType, const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit)  {
	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	//FIXME: LOG isn't possible because of non existence of convFact
	//LOG_EXPR_CONVERSION(retIr);

	// if there is no initialization expression
	if (!expr) {

		// extract kind
		core::NodeType kind = type->getNodeType();

		if (kind == core::NT_RefType) {
			core::TypePtr elementType = type.as<core::RefTypePtr>()->getElementType();

			// special handling for nested references (pointers)
			if (elementType.isa<core::RefTypePtr>()) {
				return builder.refVar((zeroInit)?builder.getZero(elementType):builder.undefined(elementType));
			}

			// handle others using a recursive call
			return builder.refVar(convertInitExpr(clangType, expr, elementType, zeroInit));
		}


		// If the type of this declaration is translated as a array type then it may also include
		// C99 variable array declaration where the size of the array is encoded into the type. This
		// is not supported by the IR type system therefore we have to catch the situation and
		// allocate the correct amount of memory
		if (kind == core::NT_ArrayType && clangType && llvm::isa<clang::VariableArrayType>(clangType)) {
			// get the size
			auto size = convertExpr(llvm::dyn_cast<clang::VariableArrayType>(clangType)->getSizeExpr());
			auto arrType = type.as<core::ArrayTypePtr>();

			return retIr =
				builder.callExpr(type, mgr.getLangBasic().getArrayCreate1D(),
					builder.getTypeLiteral(arrType->getElementType()), builder.castExpr(mgr.getLangBasic().getUInt8(), size)
			);
		}

		// if no init expression is provided => use zero or undefined value
		return retIr = zeroInit ? builder.getZero(type) : builder.undefined(type);

//		// use default value ..
//		return retIr = defaultInitVal(type);
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		retIr = utils::cast( convertInitializerList(listExpr, type), type);
		return retIr;
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	// ============================================================================================
	// =============================== Handling of special cases  =================================
	// ============================================================================================

	if( core::analysis::isConstructorCall(retIr)){
		return retIr;
	}

	// if is a constructor call, we are done
	if (llvm::isa<clang::CXXConstructExpr>(expr) && retIr.isa<core::CallExprPtr>()){		// here you might even check whether it is a constructor call in the IR
		return retIr;
	}

	// If this is an initialization of an array using array.create (meaning it was originally a
	// malloc) then we expliticly invoke the ref.new to allocate the memory on the heap
	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		return retIr = builder.refNew(retIr);
	}

	// In the case the object we need to initialize is a ref<array...> then we are not allowed to
	// deref the actual initializer, therefore we assign the object as it is
	if ( utils::isRefArray(retIr->getType()) && utils::isRefArray(type ) ) {
		return retIr = utils::cast(retIr, type);
	}

	// If we have a string literal as initializer and we need to assign it to a ref<array<...>> we
	// can directly cast it using the ref.vector.to.ref.array and perform the assignment. We do not
	// need to create a copy of the object in the right hand side
	if ( utils::isRefVector(retIr->getType()) && retIr->getNodeType() == core::NT_Literal &&
		 utils::isRefArray(type ) ) {
		return retIr = utils::cast(retIr, type);
	}

	// this is a C++ reference ( int& ref = x)
	if (clangType && clangType->isReferenceType()){
		core::TypePtr targetType = this->convertType(clangType);
		core::TypePtr srcType = retIr->getType();

		// if is a CPP ref, convert to IR
		if (core::analysis::isCppRef(srcType)) {
			return retIr;

		}
		else if (core::analysis::isConstCppRef(srcType)) {

			// if we are here, we are initializing a const ref from a const ref or from a ref
			if (core::analysis::isCppRef(targetType)) {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp());
			}
			else {
				return retIr;
			}
		}
		else{
			//this reference is initialized with a variable
			if (core::analysis::isCppRef(targetType)) {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), retIr);
			}
			else {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), retIr);
			}
		}
	}

	// FIXME: if this is needed, maybe need to add a var to create a ref
//	// inner expression is null<int<X>> and outer is array, then rebuild something like
//	//  ref.null(type<array<...>>)
//	if (core::analysis::isCallOf( retIr, mgr.getLangBasic().getGetNull())){
//		return builder.deref(builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(type)));
//	}

	// ============================== End Special Handlings =======================================

	// Anytime we have to initialize a ref<'a> from another type of object we have to deref the
	// object in the right hand side and create a copy (ref.var).
	if (type->getNodeType() == core::NT_RefType ) {
		retIr = utils::cast(retIr, GET_REF_ELEM_TYPE(type));
		retIr = builder.refVar(retIr);
	} else {
		retIr = utils::cast(retIr, type);
	}

	assert(retIr);
	return retIr;
}

//
////////////////////////////////////////////////////////////////////
///// the globalVar parameter is added at the FIRST position of the function parameters
//core::FunctionTypePtr Converter::addGlobalsToFunctionType( const core::FunctionTypePtr& funcType) {
//       const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();
//       std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);
//       std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin() + 1);
//       // function is receiving a reference to the global struct as the first argument
//       argTypes[0] = builder.refType(globalStruct.first);
//       return builder.functionType(argTypes, funcType->getReturnType());
//}


//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::convertExpr(const clang::Expr* expr) const {
       assert(expr && "Calling convertExpr with a NULL pointer");
       return exprConvPtr->Visit(const_cast<Expr*>(expr));
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::convertStmt(const clang::Stmt* stmt) const {
       assert(stmt && "Calling convertStmt with a NULL pointer");
       return stmtutils::tryAggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));

}
/////////////////////////////////////////////////////////////////
//
core::FunctionTypePtr Converter::convertFunctionType(const clang::FunctionDecl* funcDecl){
	const clang::Type* type= GET_TYPE_PTR(funcDecl);
	core::FunctionTypePtr funcType = convertType(type).as<core::FunctionTypePtr>();

	// check whether it is actually a member function
	core::TypePtr ownerClassType;
	core::FunctionKind funcKind;
	if (const auto* decl = llvm::dyn_cast<clang::CXXConstructorDecl>(funcDecl)) {
		funcKind = core::FK_CONSTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXDestructorDecl>(funcDecl)) {
		funcKind = core::FK_DESTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
		funcKind = core::FK_MEMBER_FUNCTION;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else {
		// it is not a member function => just take the plain function
		assert(funcType->isPlain());
		return funcType;
	}

	core::TypePtr thisType = builder.refType(ownerClassType);

	// update return type
	core::TypePtr returnType;
	switch (funcKind){
		case core::FK_MEMBER_FUNCTION:
			returnType = funcType.getReturnType();
			break;
		case core::FK_CONSTRUCTOR:
		case core::FK_DESTRUCTOR:
			returnType = thisType;
			break;
		default:
			assert(false && "invalid state!");
			break;
	}

	// update function type
	core::TypeList params;
	params.push_back(thisType);
	for(auto cur : funcType->getParameterTypes()) {
		params.push_back(cur);
	}

	// build resulting function type
	return builder.functionType(params, returnType, funcKind);
}

//////////////////////////////////////////////////////////////////
//
core::TypePtr Converter::convertType(const clang::Type* type) {
	assert(type && "Calling convertType with a NULL pointer");
	return typeConvPtr->convert( type );
}

namespace {

	core::StatementPtr prepentInitializerList(const clang::CXXConstructorDecl* ctorDecl, const core::TypePtr& classType, const core::StatementPtr& body, Converter& converter) {
		auto& mgr = body.getNodeManager();
		core::IRBuilder builder(mgr);

		// nameless/anonymous structs/unions result in non-generic classtype
		// structs unions with name should be generic types
		assert( ( ctorDecl->getParent()->getName().empty() ||
				(!ctorDecl->getParent()->getName().empty() && classType.isa<core::GenericTypePtr>())) && "for convenion, this literal must keep the generic type");

		core::StatementList initList;
		for(auto it = ctorDecl->init_begin(); it != ctorDecl->init_end(); ++it) {

			core::StringValuePtr ident;
			core::ExpressionPtr expr;
			core::ExpressionPtr init;

			core::StatementPtr  initStmt;

			if((*it)->isBaseInitializer ()){

				expr = converter.convertExpr((*it)->getInit());
				init = builder.literal("this", builder.refType(classType));

				if(!insieme::core::analysis::isConstructorCall(expr)) {
					// base init is a non-userdefined-default-ctor call, drop it
					continue;
				}

				// if the expr is a constructor then we are initializing a member an object,
				// we have to substitute first argument on constructor by the
				core::CallExprAddress addr = core::CallExprAddress(expr.as<core::CallExprPtr>());
				expr = core::transform::replaceNode (mgr, addr->getArgument(0), init).as<core::CallExprPtr>();
				initList.push_back (expr);
			}
			else if ((*it)->isMemberInitializer ()){
				// create access to the member of the struct/class
				// we need to use the detailed version to build the reference member operation,
				// but we substitute it with the generic type as soon as we are done
				ident = builder.stringValue(((*it)->getMember()->getNameAsString()));
				core::LiteralPtr genThis      = builder.literal("this", builder.refType(classType));
				core::LiteralPtr completeThis = builder.literal("this", builder.refType (converter.lookupTypeDetails(classType)));
				init = builder.refMember( completeThis, ident);
				core::CallExprAddress addr(init.as<core::CallExprPtr>());
				init = core::transform::replaceNode(mgr, addr->getArgument(0), genThis).as<core::ExpressionPtr>();
				expr = converter.convertExpr((*it)->getInit());

				// parameter is some kind of cpp ref, but we want to use the value, unwrap it
				if (!IS_CPP_REF(init.getType().as<core::RefTypePtr>()->getElementType()) &&
					IS_CPP_REF(expr->getType())){
					expr = builder.deref(builder.toIRRef(expr));
				}
				// parameter is NOT cpp_ref but left hand side is -> wrap into cppref
				else if(IS_CPP_REF(init.getType().as<core::RefTypePtr>()->getElementType()) &&
					!IS_CPP_REF(expr->getType())) {

					if(core::analysis::isCppRef(init.getType().as<core::RefTypePtr>()->getElementType())) {
						expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), expr);
					} else if(core::analysis::isConstCppRef(init.getType().as<core::RefTypePtr>()->getElementType())){
						expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), expr);
					} else { assert(false); }
				}
				else{
					if (*converter.lookupTypeDetails(init->getType().as<core::RefTypePtr>()->getElementType()) != *converter.lookupTypeDetails(expr->getType()))
						expr = builder.tryDeref(expr);
				}
				initList.push_back(builder.assign( init, expr));
			}
			if ((*it)->isIndirectMemberInitializer ()){
				assert(false && "indirect init not implemented");
			}
			if ((*it)->isInClassMemberInitializer ()){
				assert(false && "in class member not implemented");
			}
			if ((*it)->isDelegatingInitializer ()){
				assert(false && "delegating init not implemented");
			}
			if ((*it)->isPackExpansion () ){
				assert(false && "pack expansion not implemented");
			}
		}

		// check whether there is something to do
		if (initList.empty()) return body;


		//ATTENTION: this will produce an extra compound around the  initializer list and old body
		// let fun ... {
		//   { intializer stuff };
		//   { original body };
		// }

		// build new
		return builder.compoundStmt(
				builder.compoundStmt(initList),
				body
		);
	}
}

//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
core::ExpressionPtr Converter::convertFunctionDecl(const clang::FunctionDecl* funcDecl) {

	VLOG(1) << "======================== FUNC: "<< funcDecl->getNameAsString() << " ==================================";

	// switch to the declaration containing the body (if there is one)
	funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!

	// obtain function type
	auto funcTy = convertFunctionType(funcDecl);

	// check whether function has already been converted
	auto pos = lambdaExprCache.find(funcDecl);
	if (pos != lambdaExprCache.end()) {
		return pos->second;		// done
	}

	// check whether function should be intersected
	if( getProgram().getInterceptor().isIntercepted(funcDecl) ) {
		auto irExpr = getProgram().getInterceptor().intercept(funcDecl, *this);
		lambdaExprCache[funcDecl] = irExpr;
		VLOG(2) << "\tintercepted: " << irExpr;
		return irExpr;
	}

	// handle pure virtual functions
	if( funcDecl->isPure() && llvm::isa<clang::CXXMethodDecl>(funcDecl)){
		VLOG(2) << "\tpure virtual function " << funcDecl;

		std::string callName = funcDecl->getNameAsString();
		core::ExpressionPtr retExpr = builder.literal(callName, funcTy);

		VLOG(2) << retExpr << " " << retExpr.getType();
		lambdaExprCache[funcDecl] = retExpr;
		return retExpr;
	}

	// handle external functions
	if(!funcDecl->hasBody()) {
		// TODO: move this to call expression handling
		if (funcDecl->getNameAsString() == "free") {
			//handle special function -- "free" -- here instead of in CallExr
			auto retExpr = builder.getLangBasic().getRefDelete();
			lambdaExprCache[funcDecl] = retExpr;
			return retExpr;
		}

		// handle extern functions
		auto retExpr = builder.literal(utils::buildNameForFunction(funcDecl), funcTy);

		// attach header file info
		utils::addHeaderForDecl(retExpr, funcDecl, program.getStdLibDirs());
		lambdaExprCache[funcDecl] = retExpr;
		return retExpr;
	}

	// --------------- convert potential recursive function -------------

	// -- assume function is recursive => add variable to lambda expr cache --
	core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
	assert(lambdaExprCache.find(funcDecl) == lambdaExprCache.end());
	lambdaExprCache[funcDecl] = symbol;

	// -- conduct the conversion of the lambda --
	core::LambdaExprPtr lambda;
	{
		assert(funcDecl->hasBody() && "At this point function should have a body!");

		// init parameter set
		vector<core::VariablePtr> params;
		std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [&](ParmVarDecl* currParam) {
			params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
		});

		// convert function body
		//   - set up context to contain current list of parameters and convert body
		Converter::ParameterList oldList = curParameter;
		curParameter = &params;

		core::StatementPtr body = convertStmt( funcDecl->getBody() );
		curParameter = oldList;

		// add initializer list
		if (funcTy->isConstructor()) {
			body = prepentInitializerList(llvm::cast<clang::CXXConstructorDecl>(funcDecl), funcTy->getObjectType(), body, *this);
		}

		// some cases value parameters have to be materialized in the
		// body of the function, to be able to written.
		body =  materializeReadOnlyParams(body,params);

		// handle potential this pointer
		if (funcTy->isMember()) {
			auto thisType = funcTy->getParameterTypes()[0];

			// add this as a parameter
			auto thisVar = builder.variable(thisType);
			params.insert(params.begin(), thisVar);

			// handle this references in body,
			body = core::transform::replaceAllGen (mgr, body, builder.literal("this", thisType), thisVar, true);
		}

		// build the resulting lambda
		lambda = builder.lambdaExpr(funcTy, params, body);
		VLOG(2) << lambda << " + function declaration: " << funcDecl;
	}


	// update cache
	assert_eq(lambdaExprCache[funcDecl], symbol) << "Don't touch this!";

	// finally, add some sugar
	attachFuncAnnotations(lambda, funcDecl);

	// if the conversion is complete
	lambdaExprCache[funcDecl] = symbol;

	// register function within resulting translation unit
	getIRTranslationUnit().addFunction(symbol, lambda);

	// annotate and return results
	return symbol;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							CXX STUFF
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



////~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
////							AST CONVERTER
////~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//core::CallExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
//	mFact.setTranslationUnit(&tu);
//
//	core::StatementPtr bodyStmt = mFact.convertStmt( body );
//	auto callExpr = core::transform::outline(mgr, bodyStmt);
//
//	annotations::c::CLocAnnotation::ArgumentList args;
//	auto lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
//
//	// ------ Adding source location annotation (CLocAnnotation) -------
//	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(body->getLocStart(), body->getLocEnd());
//	auto fit = mFact.getPragmaMap().getStatementMap().find(body);
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
//	return callExpr;
//}
//
//core::ProgramPtr ASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain /*=false*/) {
//
//	// Handling of the translation unit: we have to make sure to load the translation unit where the function is
//	// defined before starting the parser otherwise reading literals results in wrong values.
//	const TranslationUnit* rightTU = mFact.getTranslationUnitForDefinition(funcDecl);
//	assert(rightTU && "Translation unit for function not found.");
//	mFact.setTranslationUnit(rightTU);
//
//	// collect thread-private variables
//	omp::collectThreadPrivate(mFact.getPragmaMap(), mFact.getThreadprivates());
//
//	core::ExpressionPtr expr = mFact.convertFunctionDecl(funcDecl);
//
//	core::ExpressionPtr lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>(expr);
//
//	// A marker node is allowed if it contains a lambda expression
//	if (!lambdaExpr) {
//		lambdaExpr = dynamic_pointer_cast<const core::MarkerExpr>(expr);
//
//		if (lambdaExpr) {
//			assert(	static_pointer_cast<const core::MarkerExpr>(expr)->getSubExpression()->getNodeType() == core::NT_LambdaExpr &&
//					"Conversion of function returned a marker expression which does not contain a lambda expression");
//		}
//	}
//	assert( lambdaExpr && "Conversion of function did not return a lambda expression");
//
//	// globals in the main function, globals might need to be initialized
//	if (isMain)
//		lambdaExpr = addGlobalsInitialization(lambdaExpr.as<core::LambdaExprPtr>());
//
//	return core::IRBuilder(mgr).program(toVector(lambdaExpr));
//}
//
///////////////////////////////////////////////////////////
////
//core::LambdaExprPtr ASTConverter::addGlobalsInitialization(const core::LambdaExprPtr& mainFunc){
//
//	VLOG(1) << "";
//	VLOG(1) << "************************************************************************************";
//	VLOG(1) << "******************** Initialize Globals at program start ***************************";
//	VLOG(1) << "************************************************************************************";
//	VLOG(1) << "";
//
//	// 4 casses:
//	// extern, do nothing
//	// static in some function, we need to initialize the constructor flag
//	// with init, assign value
//	// without init, do not initialize
//
//	// we only want to init what we use, so we check it
//	core::NodeSet usedLiterals;
//	core::visitDepthFirstOnce (mainFunc, [&] (const core::LiteralPtr& literal){
//				usedLiterals.insert(literal);
//			});
//
//	core::IRBuilder builder(mainFunc->getNodeManager());
//	core::StatementList inits;
//
//	// ~~~~~~~~~~~~~~~~~~ INITIALIZE GLOBALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	for (auto git = globalCollector.globalsInitialization_begin(); git != globalCollector.globalsInitialization_end(); ++git){
//
//		if (mFact.getProgram().getInterceptor().isIntercepted((*git)->getQualifiedNameAsString())){
//			continue;
//		}
//
//		VLOG(2) << "initializing global: " << (*git)->getQualifiedNameAsString();
//
//		if(const clang::Expr* init = (*git)->getDefinition()->getInit()){
//			core::ExpressionPtr var = mFact.lookUpVariable((*git));
//			core::ExpressionPtr initValue;
//			//FIXME: why this is not done in the visitor???
//			if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( init )) {
//				initValue =  mFact.convertInitializerList(listExpr, var->getType());
//			}
//			else
//				initValue = mFact.convertExpr(init);
//
//			if(initValue->getType().isa<core::RefTypePtr>()){
//				initValue = utils::cast( initValue, var->getType().as<core::RefTypePtr>().getElementType());
//			}
//			core::StatementPtr assign = builder.assign (var, initValue);
//			inits.push_back(assign);
//		}
//	}
//
//	// ~~~~~~~~~~~~~~~~~~ PREPARE STATICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~A
//	for (auto sit = globalCollector.staticInitialization_begin(); sit != globalCollector.staticInitialization_end(); ++sit){
//		VLOG(2) << "initializing static: " << (*sit)->getQualifiedNameAsString();
//		core::ExpressionPtr var = mFact.lookUpVariable((*sit));
//		core::LiteralPtr litUse = var.isa<core::LiteralPtr>();
//		if (!litUse){
//			litUse = var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>();
//		}
//		assert (litUse && " no literal? who handled this global?");
//
//		// no need to touch it if never used
//		if (contains(usedLiterals, litUse)){
//			inits.push_back(builder.createStaticVariable(var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>()));
//		}
//	}
//
//	if (inits.empty())
//		return mainFunc;
//
//	return (core::transform::insert ( mainFunc->getNodeManager(), core::LambdaExprAddress(mainFunc)->getBody(), inits, 0)).as<core::LambdaExprPtr>();
//}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace


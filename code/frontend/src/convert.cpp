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

#include <functional>

#include "insieme/frontend/clang.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/analysis/prunable_decl_visitor.h"
#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/progress_bar.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/types/cast_tool.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/dump/text_dump.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"

#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/c/extern_c.h"
#include "insieme/annotations/c/include.h"

// for the console output, move somewhere
#include <boost/format.hpp>

using namespace clang;
using namespace insieme;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//   ANONYMOUS NAMESPACE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace {

bool isCppConstructor (const core::ExpressionPtr& expr){

	// constructor
	if (core::analysis::isConstructorCall(expr)){
		return true;
	}

	// array constructor
	core::NodeManager&  mgr = expr->getNodeManager();
	if (core::analysis::isCallOf(expr, mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor())  ) return true;
	if (core::analysis::isCallOf(expr, mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor2D())) return true;

	return false;
}


/// convert a initialization for a global
core::ExpressionPtr convertInitForGlobal (insieme::frontend::conversion::Converter& converter, const clang::VarDecl* var, core::TypePtr elementType){

	// and get the initial value
	core::ExpressionPtr initValue;

	if (var->hasInit() && !var->isStaticLocal()) {
		initValue = converter.convertExpr ( var->getInit() ) ;
	}
	else if (clang::VarDecl* outDecl = const_cast<clang::VarDecl*>(var)->getOutOfLineDefinition ()){
		// initialization be out of class or something else, beware of dependent types
		if ( outDecl->hasInit() &&
			!outDecl->getAnyInitializer()->getType().getTypePtr()->isDependentType() &&
			!outDecl->getAnyInitializer()->isInstantiationDependent())
			initValue = converter.convertExpr ( outDecl->getAnyInitializer() ) ;
	}

	// globals are just assigned, so do it carefully
	if (initValue){

		initValue = converter.getInitExpr (elementType.as<core::RefTypePtr>()->getElementType(), initValue);
	}

	return initValue;
}


inline unsigned countVars(const clang::DeclContext* declCtx){
	unsigned count(0);
	struct Counter : public insieme::frontend::analysis::PrunableDeclVisitor<Counter> {
		unsigned& count;
		Counter(unsigned& count) :count (count) {}
		void VisitVarDecl(const clang::VarDecl* var) {
			if (!var->hasGlobalStorage()) { return; }
			if (var->hasExternalStorage()) { return; }
			if (var->isStaticLocal()) { return; }
			count++;
		}
	} counter(count);
	counter.traverseDeclCtx(declCtx);
	return count;
}
inline unsigned countTypes(const clang::DeclContext* declCtx){
	unsigned count(0);
	struct Counter : public insieme::frontend::analysis::PrunableDeclVisitor<Counter> {
		unsigned& count;
		Counter(unsigned& count) :count (count) {}
		void VisitRecordDecl(const clang::RecordDecl* typeDecl) {
			if (!typeDecl->isCompleteDefinition() ) return;
			if (typeDecl->isDependentType() )      return;
			count++;
		}

		void VisitTypedefNameDecl(const clang::TypedefNameDecl* typedefDecl) {
			if (!typedefDecl->getTypeForDecl()) return;
			count++;
		}

	} counter(count);
	counter.traverseDeclCtx(declCtx);
	return count;
}
inline unsigned countFunctions(const clang::DeclContext* declCtx){
	unsigned count(0);
	struct Counter : public insieme::frontend::analysis::PrunableDeclVisitor<Counter> {
		unsigned& count;
		Counter(unsigned& count) :count (count) {}
		void VisitLinkageSpec(const clang::LinkageSpecDecl* link) {
			Counter vis(count);
			vis.traverseDeclCtx(llvm::cast<clang::DeclContext> (link));
		}
		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) { count++; }
	} counter(count);
	counter.traverseDeclCtx(declCtx);
	return count;
}


inline string  createPrefix(const std::string& name, unsigned pass, unsigned of){
	std::stringstream out;
	out << name << " " << pass << "/" << of;
	return out.str();
}

/**
 * some tool to print a progress bar, some day would be cool to have an infrastructure to do so
 */
inline void printProgress (const std::string& prefix, unsigned cur, unsigned max) {
	static unsigned lastPos = 0;
	unsigned a = ((float)cur/ (float)max) * 60.0f;
	if (a > lastPos) {
		lastPos = a;
		std::cout << "\r" << utils::ProgressBar(prefix, max, cur, 60, std::cout, false);
	}
	if (cur == max) {
		lastPos = 0;
	}
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
	TranslationUnit tu(manager, unit, setup);
	conversion::Converter c(manager, tu, setup);
	// add them and fire the conversion
	return c.convert();
}

namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERTER
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//////////////////////////////////////////////////////////////////
///
Converter::Converter(core::NodeManager& mgr, const TranslationUnit& tu, const ConversionSetup& setup) :
		staticVarCount(0),
		translationUnit(tu),
		convSetup(setup),
		pragmaMap(translationUnit.pragmas_begin(), translationUnit.pragmas_end()),
		irTranslationUnit(mgr), used(false),
		mgr(mgr), builder(mgr), feIR(mgr, getCompiler().isCXX()),
		lastTrackableLocation(),
		headerTagger(setup.getSystemHeadersDirectories(),setup.getInterceptedHeaderDirs(), setup.getIncludeDirectories(), getCompiler().getSourceManager())
{
	if (translationUnit.isCxx()){
		typeConvPtr = std::make_shared<CXXTypeConverter>(*this);
		exprConvPtr = std::make_shared<CXXExprConverter>(*this);
		stmtConvPtr = std::make_shared<CXXStmtConverter>(*this);
	} else{
		typeConvPtr = std::make_shared<CTypeConverter>(*this);
		exprConvPtr = std::make_shared<CExprConverter>(*this);
		stmtConvPtr = std::make_shared<CStmtConverter>(*this);
	}
	// tag the translation unit with as C++ if case
	irTranslationUnit.setCXX(translationUnit.isCxx());
	assert_true (irTranslationUnit.isEmpty()) << "the ir translation unit is not empty, should be before we start";

}

tu::IRTranslationUnit Converter::convert() {
	assert_false(used) << "This one must only be used once!";
	used = true;

	assert_true(getCompiler().getASTContext().getTranslationUnitDecl());

	// collect all type definitions
	auto declContext = clang::TranslationUnitDecl::castToDeclContext(getCompiler().getASTContext().getTranslationUnitDecl());

	unsigned count = countTypes(declContext);
	struct TypeVisitor : public analysis::PrunableDeclVisitor<TypeVisitor> {

		Converter& converter;
		unsigned count;
		unsigned processed;
		TypeVisitor(Converter& converter, unsigned count) : converter(converter), count(count), processed(0) {}

        Converter& getConverter() {
            return converter;
        }

		void VisitRecordDecl(const clang::RecordDecl* typeDecl) {
			if (!typeDecl->isCompleteDefinition() ) return;
			if (typeDecl->isDependentType() )      return;

			// we do not convert templates or partial spetialized classes/functions, the full
			// type will be found and converted once the instantaion is found
			converter.trackSourceLocation (typeDecl);
			converter.convertTypeDecl(typeDecl);
			converter.untrackSourceLocation ();

			if (converter.getConversionSetup().hasOption(ConversionSetup::ProgressBar)) {
				auto str = createPrefix( converter.getTranslationUnit().getJustFileName(),1,3);
				printProgress (str, ++processed, count);
			}
		}
		// typedefs and typealias
		void VisitTypedefNameDecl(const clang::TypedefNameDecl* typedefDecl) {
			if (!typedefDecl->getTypeForDecl()) return;

			// get contained type
			converter.trackSourceLocation (typedefDecl);
			converter.convertTypeDecl(typedefDecl);
			converter.untrackSourceLocation ();

			if (converter.getConversionSetup().hasOption(ConversionSetup::ProgressBar)) {
				auto str = createPrefix( converter.getTranslationUnit().getJustFileName(),1,3);
				printProgress (str, ++processed, count);
			}
		}
	} typeVisitor(*this, count);
	typeVisitor.traverseDeclCtx (declContext);

	// collect all global declarations
	count = countVars(declContext);
	struct GlobalVisitor : public analysis::PrunableDeclVisitor<GlobalVisitor> {

		Converter& converter;
		unsigned count;
		unsigned processed;
		GlobalVisitor(Converter& converter, unsigned count) : converter(converter), count (count), processed(0) {}

        Converter& getConverter() {
            return converter;
        }

		void VisitVarDecl(const clang::VarDecl* var) {
			// variables to be skipped
			if (!var->hasGlobalStorage()) { return; }
			if (var->hasExternalStorage()) { return; }
			if (var->isStaticLocal()) { return; }

			converter.trackSourceLocation (var);
			converter.lookUpVariable(var);
			converter.untrackSourceLocation();

			if (converter.getConversionSetup().hasOption(ConversionSetup::ProgressBar)) {
				auto str = createPrefix( converter.getTranslationUnit().getJustFileName(),2,3);
				printProgress (str, ++processed, count);
			}
		}
	} varVisitor(*this, count);
	varVisitor.traverseDeclCtx(declContext);

	// collect all global declarations
	count = countFunctions(declContext);
	unsigned processed = 0;
	struct FunctionVisitor : public analysis::PrunableDeclVisitor<FunctionVisitor> {

		Converter& converter;
		bool externC;
		unsigned count;
		unsigned& processed;
		FunctionVisitor(Converter& converter, bool Ccode, unsigned count, unsigned& processed)
		: converter(converter), externC(Ccode), count(count), processed(processed)
		{}

        Converter& getConverter() {
            return converter;
        }

		void VisitLinkageSpec(const clang::LinkageSpecDecl* link) {
			bool isC =  link->getLanguage () == clang::LinkageSpecDecl::lang_c;
			FunctionVisitor vis(converter, isC, count,processed);
			vis.traverseDeclCtx(llvm::cast<clang::DeclContext> (link));
		}

		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {
			processed++;
			if (funcDecl->isTemplateDecl() && !funcDecl->isFunctionTemplateSpecialization ()) return;

			converter.trackSourceLocation (funcDecl);
			core::ExpressionPtr irFunc = converter.convertFunctionDecl(funcDecl);
			converter.untrackSourceLocation ();
			if (externC) annotations::c::markAsExternC(irFunc.as<core::LiteralPtr>());
			if (converter.getConversionSetup().hasOption(ConversionSetup::ProgressBar)) {
				auto str = createPrefix( converter.getTranslationUnit().getJustFileName(),3,3);
				printProgress (str, processed, count);
			}
		}
	} funVisitor(*this, false, count,processed);
	funVisitor.traverseDeclCtx(declContext);

	//frontend done
	if (getConversionSetup().hasOption(ConversionSetup::ProgressBar)) std::cout << std::endl;

	//std::cout << " ==================================== " << std::endl;
	//std::cout << getIRTranslationUnit() << std::endl;
	//std::cout << " ==================================== " << std::endl;

	// that's all
	return irTranslationUnit;
}

//////////////////////////////////////////////////////////////////
///
const frontend::utils::HeaderTagger& Converter::getHeaderTagger() const{
	return headerTagger;
}


namespace {

	//if var is used to create a read-only variable
bool isUsedToCreateConstRef(const core::StatementPtr& body, const core::VariablePtr& var){

	auto& ext = body->getNodeManager().getLangExtension<core::lang::IRppExtensions>();
	bool flag = false;
	core::visitDepthFirstOnce (body, [&] (const core::CallExprPtr& call){
		if (core::analysis::isCallOf(call, ext.getRefIRToConstCpp())){
			if (call[0] == var ){
				flag = true;
			}
		}
	});
	return flag;
}

	//if var is used to create a right side ref
bool isUsedToCreateRValRef(const core::StatementPtr& body, const core::VariablePtr& var){

	auto& ext = body->getNodeManager().getLangExtension<core::lang::IRppExtensions>();
	bool flag = false;
	core::visitDepthFirstOnce (body, [&] (const core::CallExprPtr& call){
		if (core::analysis::isCallOf(call, ext.getRefIRToRValCpp())){
			if (call[0] == var ){
				flag = true;
			}
		}
	});
	return flag;
}

}

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
			auto transferAnnotations = [&] (const core::StatementPtr& node){
					//if we have a OMP annotation
					if (node->hasAnnotation(omp::BaseAnnotation::KEY)){
						auto anno = node->getAnnotation(omp::BaseAnnotation::KEY);
						assert_true(anno);
						anno->replaceUsage (wrap, currParam);
					}
				};

			// This is a materialize case, the materialize expression is implicit in C++
			// NOT to use a custom materialize node, turns into the need of distinguish
			// the cases to explicitly write refVar or not to do so, in the backend
			if (isUsedToCreateConstRef(body, wrap) || isUsedToCreateRValRef(body, wrap)) {
				auto access = builder.refVar(currParam);
				newBody = core::transform::replaceAllGen(mgr, newBody, wrap, access);
				core::visitDepthFirstOnce(newBody, transferAnnotations);
				wrapRefMap.erase(currParam);
			}
			// we only consider the usage of this variable in the local scope,
			// there is no need to materialize the variable with a ref.var when
			// we can do that more clean by using a scope declared variable
			else if (core::analysis::isReadOnlyWithinScope(body, wrap)) {
				// replace read uses
				newBody = core::transform::replaceAllGen(mgr, newBody, builder.deref(wrap), currParam);

				// this variables might apear in annotations inside:
				core::visitDepthFirstOnce(newBody, transferAnnotations);

				// cleanup the wrap cache to avoid future uses, this var does not exist anymore
				// at this point, no use of wrap should exist in the code;
				wrapRefMap.erase(currParam);
			}
			else {
				// other case materialize a var, declare it at the beginning of the body
				decls.push_back(this->builder.declarationStmt(fit->second, this->builder.refVar(fit->first)));
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
void Converter::printDiagnosis(const clang::SourceLocation& loc) {

	clang::Preprocessor& pp = getPreprocessor();
	// print warnings and errors:
	while (!warnings.empty()){
		if (!getConversionSetup().hasOption(ConversionSetup::NoWarnings)){
			if (getSourceManager().isLoadedSourceLocation (loc)){
				std::cerr << "\n\nloaded location:\n";
				std::cerr << "\t" << *warnings.begin() << std::endl;
			}
			else{
				std::cerr << "\n\n";
				pp.Diag(loc, pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Warning, *warnings.begin()) );
			}
		}
		warnings.erase(warnings.begin());
	}
}

//////////////////////////////////////////////////////////////////
///
void Converter::trackSourceLocation (const clang::Decl* decl){
	if (const clang::ClassTemplateSpecializationDecl* spet = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(decl)){
		lastTrackableLocation.push(spet->getPointOfInstantiation());
	}
	else{
		lastTrackableLocation.push(decl->getLocation ());
	}
}

//////////////////////////////////////////////////////////////////
///
void Converter::trackSourceLocation (const clang::Stmt* stmt){
	lastTrackableLocation.push(stmt->getLocStart());
}

//////////////////////////////////////////////////////////////////
///
void Converter::untrackSourceLocation (){
	assert_false(lastTrackableLocation.empty());
	lastTrackableLocation.pop();
}

//////////////////////////////////////////////////////////////////
///
std::string Converter::getLastTrackableLocation() const{
	if (!lastTrackableLocation.empty())
		return utils::location(lastTrackableLocation.top(), getSourceManager());
	else
		return "ERROR: unable to identify last input code location ";
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
core::ExpressionPtr Converter::lookUpVariableInWrapRefMap(const core::ExpressionPtr variable) {
    if(variable.isa<core::VariablePtr>() && wrapRefMap.find(variable.as<core::VariablePtr>()) != wrapRefMap.end()) {
        return wrapRefMap.find(variable.as<core::VariablePtr>())->second;
    }
    return variable;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::lookUpVariable(const clang::ValueDecl* valDecl) {
	VLOG(1) << "LOOKUP Variable: " << valDecl->getNameAsString();

	// Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	auto varCacheHit = varDeclMap.find(valDecl);
	if (varCacheHit != varDeclMap.end()) {
		// variable found in the map
		return varCacheHit->second;
	}


    core::NodePtr result = nullptr;
    for(auto extension : this->getConversionSetup().getExtensions()) {
        result = extension->Visit(valDecl, *this);
        if(core::ExpressionPtr re = result.isa<core::ExpressionPtr>()) {
            varDeclMap[valDecl] = re;
            break;
        }
    }

    if(!result) {
		if (VLOG_IS_ON(1)) valDecl->dump();

		// The variable has not been converted into IR variable yet, therefore we create the IR variable and
		// insert it to the map for successive lookups

		// Conversion of the variable type
		QualType&& varTy = valDecl->getType();
		core::TypePtr&& irType = convertType( varTy );
		assert_true(irType) << "type conversion for variable failed";

		VLOG(2)	<< "clang type: " << varTy.getAsString();
		VLOG(2)	<< "ir type:    " << irType;

		//// check whenever the variable is marked to be volatile
		if (varTy.isVolatileQualified()) {
			irType = builder.volatileType(irType);
		}

		bool isOclVector = !!varTy->getUnqualifiedDesugaredType()->isExtVectorType();
		bool isGCCVector = !!varTy->getUnqualifiedDesugaredType()->isVectorType();
		if (!(varTy.isConstQualified() ||    						// is a constant
			varTy.getTypePtr()->isReferenceType()  ||             // is a c++ reference
			(isa<const clang::ParmVarDecl>(valDecl) && 			// is the declaration of a parameter
			((irType->getNodeType() != core::NT_VectorType && irType->getNodeType() != core::NT_ArrayType) ||
			isOclVector || isGCCVector) ))) {
			// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
			// only exception are OpenCL vectors and gcc-vectors
			irType = builder.refType(irType);
		}
		else{
			// beware of const pointers, they may lack one ref (but do not change if a parameter)
			if (core::types::isRefArray(irType) && varTy.isConstQualified() && !llvm::isa<clang::ParmVarDecl>(valDecl)) {
				irType = builder.refType(irType);
			}
		}


		// if is a global variable, a literal will be generated, with the qualified name
		// (two qualified names can not coexist within the same TU)
		const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);

		// const static need an extra ref, we can not deal with pure value globals ;)
		if (varDecl->isStaticLocal() && !irType.isa<core::RefTypePtr>()){
			irType = builder.refType(irType);
		}

		if (varDecl && varDecl->hasGlobalStorage() && !varDecl->isStaticLocal()) {
			VLOG(2)	<< varDecl->getQualifiedNameAsString() << " with global storage";
			// we could look for it in the cache, but is fast to create a new one, and we can not get
			// rid if the qualified name function
			std::string name = utils::buildNameForVariable(varDecl);

			// global/static variables are always leftsides (refType) -- solves problem with const
			if(!irType.isa<core::RefTypePtr>() ) {
				irType = builder.refType(irType);
			}

			core::ExpressionPtr globVar =  builder.literal(name, irType);

			getHeaderTagger().addHeaderForDecl(globVar, valDecl);

            // attach pragma to global variable
			pragma::attachPragma(core::NodeList({globVar.as<core::NodePtr>()}), valDecl, *this);

			varDeclMap.insert( { valDecl, globVar } );

			// some member statics might be missing because of defined in a template which was ignored
			// since this is the fist time we get access to the complete type, we can define the
			// suitable initialization
			// if the variable is intercepted, we ignore the declaration, will be there once the header is attached
			if( !varDecl->isStaticLocal() && !varDecl->hasExternalStorage()) {
				//we don't add StaticLocal and External variables to the TU.globals
				//static local are initialized at first entry of their scope
				//extern vars are take care of by someone else

				VLOG(2)	<< varDecl->getQualifiedNameAsString() << "         is added to TU globals";
				auto initValue = convertInitForGlobal(*this, varDecl, irType);
				getIRTranslationUnit().addGlobal(globVar.as<core::LiteralPtr>(), initValue);
			}

		} else {
			// The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
			// the IR variable and insert it into the map for future lookups
			core::VariablePtr&& var = builder.variable( irType );
			VLOG(2) << "IR variable " << var.getType()->getNodeType() << " " << var<<":"<<varDecl->getNameAsString();
			VLOG(2) << "IR var type " << var.getType();

			if ( !valDecl->getNameAsString().empty() ) {
				// Add the C name of this variable as annotation
				core::annotations::attachName(var,varDecl->getNameAsString());
			}

			varDeclMap.insert( { valDecl, var } );
		}

		if(annotations::c::hasIncludeAttached(irType)) {
			VLOG(2) << " header " << annotations::c::getAttachedInclude(irType);
		}
	}

	for(auto extension : this->getConversionSetup().getExtensions()) {
		extension->PostVisit(valDecl, varDeclMap[valDecl], *this);
	}

	VLOG(2) << varDeclMap[valDecl];
	return varDeclMap[valDecl];
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
		if(mgr.getLangBasic().isReal16(type))
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

	// handle enum values
	auto& enumExt = mgr.getLangExtension<core::lang::EnumExtension>();
	if (enumExt.isEnumType(type)) {
		return builder.literal(type, "0");
	}

	assert_true(core::analysis::isRefType(curType)) << "We cannot initialize any different type of non-ref - found: " << curType << "\n";

	core::RefTypePtr refType = curType.as<core::RefTypePtr>();

	// handle arrays initialization
	if ( core::ArrayTypePtr&& arrTy = core::dynamic_pointer_cast<const core::ArrayType>(refType->getElementType())) {
		return builder.refReinterpret(mgr.getLangBasic().getRefNull(), arrTy);
	}

	// handle refs initialization
	// initialize pointer/reference types with undefined
	core::TypePtr elementType = refType->getElementType();

	core::ExpressionPtr initValue;
	if (elementType->getNodeType() == core::NT_RefType) {
		// ref<ref<...>> => this is a pointer, init with 0 (null)
		initValue = builder.callExpr(elementType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elementType));
	} else {
		initValue = defaultInitVal(elementType);
	}

	return builder.refVar(initValue);


	// LOG(ERROR) << "Default initializer for type: '" << *type << "' not supported!";
	// assert_fail() << "Default initialization type not defined";
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
		if (definition->isStaticLocal()) {
			// is the declaration of a variable with global storage, this means that is an static
			// static needs to be initialized during first execution of function.
			// but the var remains in the global storage (is an assigment instead of decl)
			//
			assert_true(var);
			assert(var.isa<core::VariablePtr>());
			assert(var->getType().isa<core::RefTypePtr>() && " all static variables need to be ref<'a>");

			// build global storage name
			std::string name;
			auto varType = var->getType().as<core::RefTypePtr>()->getElementType();
			auto irType = builder.refType (mgr.getLangExtension<core::lang::StaticVariableExtension>().wrapStaticType(varType));

			// cache the name (this is fishy, needs explanation)
			if(staticVarDeclMap.find(varDecl) != staticVarDeclMap.end()) {
				name = staticVarDeclMap.find(varDecl)->second;
			} else {
				name = utils::buildNameForGlobal(varDecl, getSourceManager());
				staticVarDeclMap.insert(std::pair<const clang::VarDecl*,std::string>(varDecl, name));
			}

			// generate initialization
			auto lit = builder.literal(name, irType);
			core::ExpressionPtr initIr;
			if (definition->getInit()) {
				initIr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(),
							var->getType().as<core::RefTypePtr>().getElementType(), false);

				auto call = initIr.isa<core::CallExprPtr>();
				if(call && call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->isConstructor()) {

					initIr = builder.deref(initIr);
				}

				// beware of non const initializers, for C codes is required that statics are initialized
				// with const expressions
				// NOTE:: C++ codes do not need const init
				bool isConst = definition->getInit()->isConstantInitializer(getCompiler().getASTContext(), false);

				// if default constructed, avoid artifacts, use the default initializator
				if( call && call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->isConstructor() &&
					call->getArguments().size() == 1 &&
					call->getArgument(0)->getType() == var->getType()){

					initIr = builder.getZero(var->getType().as<core::RefTypePtr>()->getElementType());
					isConst = false;
				}

				initIr =  builder.initStaticVariable(lit, initIr, isConst);
			}
			else{
				// build some default initializationA
				assert_true(builder.getZero(varType)) << "type needs to have a zero initializaiton";
				initIr = builder.getZero(varType);
				//this one is always const initialization
				initIr =  builder.initStaticVariable(lit, initIr, true);
			}

			retStmt = builder.declarationStmt(var.as<core::VariablePtr>(), initIr);
		}
		else{
			bool isConstant = false;
			// print diagnosis messages
			assert(var.isa<core::VariablePtr>());
			core::TypePtr initExprType;
			if(var->getType().isa<core::RefTypePtr>()) {
				initExprType = var->getType().as<core::RefTypePtr>()->getElementType();
				VLOG(2) << initExprType;
			} else if (core::analysis::isAnyCppRef(var->getType())) {
				initExprType = var->getType();
			} else {
				// is a constant variable (left side is not ref, right side does not need to create refvar)
				// const char name[] = "constant string";
				// vector<char,16> vX = "constatn string"
				isConstant = true;
				initExprType = var->getType();
			}
			assert_true(initExprType);


			// initialization value
			core::ExpressionPtr initExpr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(), initExprType, false);
			assert_true(initExpr) << "not correct initialization of the variable";

			// some Cpp cases do not create new var
			if (!core::analysis::isAnyCppRef(var->getType()) &&
				!isCppConstructor(initExpr) && !isConstant){
				initExpr = builder.refVar(initExpr);
			}

			// constants need to be derefed
			if(isConstant && isCppConstructor(initExpr)) {
				initExpr = builder.tryDeref(initExpr);
			}

			// finally create the var initialization
			retStmt = builder.declarationStmt(var.as<core::VariablePtr>(), initExpr);
		}
	} else {
		// this variable is extern
		assert_true(varDecl->isExternC()) << "Variable declaration is not extern";
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

	pragma::attachPragma(core::NodeList({node.as<core::NodePtr>()}), funcDecl, *this);

// -------------------------------------------------- C NAME ------------------------------------------------------

// check for overloaded operator "function" (normal function has kind OO_None)
	if (funcDecl->isOverloadedOperator()) {
		clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
		//in order to get a real name we have to use the getOperatorSpelling method
		//otherwise we get names like operator30 and this is for instance not
		//suitable when using STL containers (e.g., map needs < operator)...
		string operatorAsString = clang::getOperatorSpelling(operatorKind);
		core::annotations::attachName(node,("operator" + operatorAsString));
	} else if( !funcDecl->getNameAsString().empty() ) {
		// annotate with the C name of the function
		//core::annotations::attachName(node,(funcDecl->getNameAsString()));
		core::annotations::attachName(node,(utils::buildNameForFunction(funcDecl)));
	}
	if(core::annotations::hasAttachedName(node)) { VLOG(2) << "attachedName: " << core::annotations::getAttachedName(node);}

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

	utils::attachLocationFromClang(node, getSourceManager(), loc.first, loc.second);

	return node;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr
Converter::convertInitExpr(const clang::Type* clangType, const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit)  {
	core::ExpressionPtr retIr;

	// if there is no initialization expression
	if (!expr) {

		// extract kind
		core::NodeType kind = type->getNodeType();

		if (type.isa<core::RefTypePtr>()) {
			return (zeroInit)?builder.getZero(type):
				              builder.undefined(type);
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
	}

	// Convert the expression like any other expression
	auto initExpr = convertExpr(expr);

 	retIr = getInitExpr ( type, initExpr);

	// ctors are derefed as a side effect of the getInit routine, remove
	if (core::analysis::isCallOf(retIr, builder.getLangBasic().getRefDeref()) &&
		isCppConstructor(retIr.as<core::CallExprPtr>()[0])){
		return retIr.as<core::CallExprPtr>()[0];
	}

	if (lookupTypeDetails(type) ==  lookupTypeDetails(retIr->getType())) return retIr;
 	return core::types::smartCast(lookupTypeDetails(type), retIr);
}

core::ExpressionPtr Converter::createCallExprFromBody(const core::StatementPtr& stmt, const core::TypePtr& retType, bool lazy){
	stmtutils::StmtWrapper irStmts;
	irStmts.push_back(stmt);
	return createCallExprFromBody(irStmts, retType, lazy);
}
//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::createCallExprFromBody(const stmtutils::StmtWrapper& irStmts, const core::TypePtr& retType, bool lazy){

	stmtutils::StmtWrapper retStmts = irStmts;
    for(auto extension : getConversionSetup().getExtensions())
        retStmts = extension->PostVisit(static_cast<clang::Stmt*>(nullptr), retStmts, *this);

	return builder.createCallExprFromBody(stmtutils::tryAggregateStmts(builder, retStmts), retType, lazy);
}

//////////////////////////////////////////////////////////////////
/// takes care that the init expr and the target type fix (hint cppRefs wrapping/unwrapping etc)
core::ExpressionPtr Converter::getInitExpr (const core::TypePtr& targetType, const core::ExpressionPtr& init){

	core::ExpressionPtr retIr;
	//ONLY FOR DEBUGING HELP -- better debug output uses retIR
	FinalActions attachLog( [&] () {
        VLOG(1) << "************* Converter::getInitExpr ***************************";
		VLOG(1) << "targetType: " << targetType;
		VLOG(1) << "init: " << init << " (" << init->getType() << ")";
        if(retIr) {
            VLOG(1) << "retIr: " << *retIr << " type:( " << *retIr->getType() << " )";
            if(*targetType != *retIr->getType()) {
				VLOG(2) << "potential problem as retIr->getType differs from targetType";
				VLOG(2) << targetType << " != " << retIr->getType();
			}
        }
        VLOG(1) << "****************************************************************************************";
    } );

	// null expression is allowed on globals initializations
	if (!init) return (retIr = init);

	core::TypePtr elementType = lookupTypeDetails(targetType);

	//if it is a listtype we take care recursivle of the listelements
	if (core::encoder::isListType(init->getType())) {
		core::ExpressionPtr retIr;
		vector<core::ExpressionPtr> inits = core::encoder::toValue<vector<core::ExpressionPtr>,core::encoder::DirectExprListConverter>(init);

		// if recursive
		assert(!elementType.isa<core::RecTypePtr>() && "we dont work with recursive types in the frontend, only gen types");

		if ( core::lang::isSIMDVector(elementType) )  {
			auto internalVecTy = core::lang::getSIMDVectorType(elementType);
			auto membTy = internalVecTy.as<core::SingleElementTypePtr>()->getElementType();
			//TODO MOVE INTO SOME BUILDER HELPER
			auto initOp = mgr.getLangExtension<core::lang::SIMDVectorExtension>().getSIMDInitPartial();
			ExpressionList elements;
			// get all values of the init expression
			for (size_t i = 0; i < inits.size(); ++i) {
				elements.push_back(getInitExpr(membTy, inits[i] ));
			}

			return builder.callExpr(
					elementType,
					initOp,
					core::encoder::toIR<ExpressionList,core::encoder::DirectExprListConverter>(targetType->getNodeManager(), elements),
					builder.getIntTypeParamLiteral(internalVecTy->getSize()));

		}

		// if array or vector
		if (  elementType.isa<core::VectorTypePtr>() || elementType.isa<core::ArrayTypePtr>()) {

			auto membTy = elementType.as<core::SingleElementTypePtr>()->getElementType();

			ExpressionList elements;
			// get all values of the init expression
			for (auto elemInit : inits) {
				auto tmp = getInitExpr(membTy, elemInit );
				if (core::types::isRefVector(tmp->getType()) && core::types::isRefArray(membTy)){
					tmp = builder.callExpr(mgr.getLangBasic().getRefVectorToRefArray(), tmp);
				}

				elements.push_back(tmp);
			}
			retIr = builder.vectorExpr(elements);

			// if the sizes dont fit is a partial initialization
			if (elementType.isa<core::VectorTypePtr>() &&
				retIr->getType().as<core::VectorTypePtr>()->getSize() != elementType.isa<core::VectorTypePtr>()->getSize()){
				return builder.callExpr(
						builder.getLangBasic().getVectorInitPartial(),
						core::encoder::toIR<ExpressionList,core::encoder::DirectExprListConverter>(targetType->getNodeManager(), elements),
						builder.getIntTypeParamLiteral(elementType.isa<core::VectorTypePtr>()->getSize())
					);
			}

			assert_true(elementType.isa<core::VectorTypePtr>() ) << "not a vector";
			assert_true(retIr->getType().isa<core::VectorTypePtr>() ) << "not a vector";
			assert_eq( retIr->getType().as<core::VectorTypePtr>()->getSize() , elementType.isa<core::VectorTypePtr>()->getSize()) <<
						"vectors do not have same size";

			return retIr;
		}

		// if struct
		if (core::StructTypePtr&& structTy = elementType.isa<core::StructTypePtr>() ) {
			core::StructExpr::Members members;
			for (size_t i = 0; i < inits.size(); ++i) {
				const core::NamedTypePtr& curr = structTy->getEntries()[i];
				members.push_back(builder.namedValue(
							curr->getName(),
							getInitExpr (curr->getType(), inits[i])));
			}
			if (members.empty())
				retIr = builder.getZero(structTy);
			else {
				retIr = builder.structExpr(structTy, members);

				// fix type to generic type (not the resolved one)
				retIr = core::transform::replaceNode(mgr, core::NodeAddress(retIr).as<core::StructExprAddress>()->getType(), targetType).as<core::ExpressionPtr>();
			}
			return retIr;
		}

		// desperate times call for desperate measures
		// TODO: this might require some more work
		// this is a blind initialization of a generic targetType we know nothing about
		if (core::GenericTypePtr&& gen = elementType.isa<core::GenericTypePtr>()){

			// TODO: getting an empty list? how it got produced?
			if (inits.empty()){
				return builder.callExpr(mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(gen));
			}

			if (core::encoder::isListType(inits[0]->getType())){
				vector<core::ExpressionPtr> innerList = core::encoder::toValue<vector<core::ExpressionPtr>,core::encoder::DirectExprListConverter>(inits[0]);
				return builder.callExpr (gen, mgr.getLangBasic().getGenInit(), builder.getTypeLiteral(gen),  builder.tupleExpr(innerList));
			}
			else{
				return inits[0];
			}
		}

		if (inits[0].getType() == elementType){
			if (inits.size()==1) return inits[0];
			ExpressionList elements;
			for (size_t i = 0; i < inits.size(); ++i) {
				elements.push_back(inits[0]);
			}
			return (retIr = builder.vectorExpr(elements));
		}

		// any other case (unions may not find a list of expressions, there is an spetial encoding)
		std::cerr << "targetType to init: " << targetType << std::endl;
		std::cerr << "init expression: "    << init << " : " << init->getType() << std::endl;
		assert_fail() << "fallthrow while initializing generic typed global";
	}

	// the initialization is not a list anymore, this a base case
	//if types match, we are done
	if(core::types::isSubTypeOf(lookupTypeDetails(init->getType()), elementType)) {
		VLOG(2) << "is subtype of";
		return (retIr = init);
	}

	if ( core::UnionTypePtr unionTy = elementType.isa<core::UnionTypePtr>() ) {

		// here is the thing, the field comes hiden in a literal (the function called)
		// and the expression is the first paramenter
		if (init.as<core::CallExprPtr>()->getFunctionExpr().isa<core::LiteralPtr>()){
			core::StringValuePtr name =  init.as<core::CallExprPtr>()->getFunctionExpr().as<core::LiteralPtr>()->getValue();
			core::TypePtr entityType;
			for (unsigned i = 0; i < unionTy->getEntries().size(); ++i)
				if (*unionTy->getEntries()[i]->getName() == *name)
					entityType = unionTy->getEntries()[0]->getType();
			assert_true(entityType) << "the targetType of the entity could not be found";
			return  (retIr = builder.unionExpr(unionTy, name,getInitExpr(entityType, init.as<core::CallExprPtr>()[0])));
		}
		// it might be that is an empy initialization, retrieve the targetType to avoid nested variable creation
		return (retIr = init.as<core::CallExprPtr>()[0]);
	}

	////////////////////////////////////////////////////////////////////////////
	// if the type missmatch we might need to take some things in consideration:

	// init ref with memory location ( T b;  T& a = b; )
	if (core::analysis::isCppRef(elementType) && init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), init));
	}

	// init const ref with a mem location ( T b; const int& b; )
	if (core::analysis::isConstCppRef(elementType) && init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), init));
	}

	// init right side ref with memory location ( T&& a = T; )
	if (core::analysis::isRValCppRef(elementType) && init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToRValCpp(), init));
	}

	// init right side ref with memory location ( const T&& a = T; )
	if (core::analysis::isConstRValCppRef(elementType) && init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstRValCpp(), init));
	}

	// init const ref with a ref, add constancy ( T& b...; const T& a = b; )
	if (core::analysis::isConstCppRef(elementType) && core::analysis::isCppRef(init->getType())) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp(), init));
	}

	// init const ref with a right side ref, add constancy ( T&& b...; const T& a = b; )
	if (core::analysis::isConstCppRef(elementType) && core::analysis::isRValCppRef(init->getType())) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefRValCppToConstCpp(), init));
	}

	// init const ref with a const right side ref, add constancy ( const T&& b...; const T& a = b; )
	if (core::analysis::isConstCppRef(elementType) && core::analysis::isConstRValCppRef(init->getType())) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstRValCppToConstCpp(), init));
	}

	// init ref with a right side ref, add constancy ( T&& b...; T& a = b; )
	if (core::analysis::isCppRef(elementType) && core::analysis::isRValCppRef(init->getType())) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefRValCppToCpp(), init));
	}

	// init const ref with value, extend lifetime  ( const T& x = f() where f returns by value )
	if (core::analysis::isConstCppRef(elementType) && !init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(),
								builder.refVar (init)));
								//builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), init)));
	}

	// init const ref with value, extend lifetime  ( const T&& x = f() where f returns by value )
	if (core::analysis::isConstRValCppRef(elementType) && !init->getType().isa<core::RefTypePtr>()) {
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstRValCpp(),
								builder.refVar (init)));
								//builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), init)));
	}

	// from cpp ref to value or variable
	if (core::analysis::isAnyCppRef(init->getType())){
		if (elementType.isa<core::RefTypePtr>()) {
			return (retIr = builder.toIRRef(init));
		} else {
			return (retIr = builder.deref(builder.toIRRef(init)));  // might be a call by value to a function, and we need to derref
		}
	}

	// constructor
	if (isCppConstructor(init)) {
		retIr = builder.deref(init);
		assert_eq(lookupTypeDetails(retIr->getType()), lookupTypeDetails(elementType));
		return retIr;
	}

	if (init->getType().isa<core::RefTypePtr>() &&
		core::types::isSubTypeOf(lookupTypeDetails(init->getType().as<core::RefTypePtr>()->getElementType()), elementType)) {
		return (retIr = builder.deref(init));
	}

	if (builder.getLangBasic().isAny(elementType) ) {
		return (retIr = init);
	}

	if (builder.getLangBasic().isPrimitive (elementType) && builder.getLangBasic().isPrimitive(init->getType())) {
		return (retIr =core::types::castScalar(elementType, init));
	}

	if ( elementType.isa<core::VectorTypePtr>() ){
		core::ExpressionPtr initVal = init;
		if (core::types::isRefVector(init->getType())) {
			initVal =  builder.deref(initVal);
		}
		//it can be a partial initialization
		if(core::types::isSubTypeOf(initVal->getType(), elementType)) {
			return (retIr = initVal);
		}

		return (retIr = core::types::smartCast( initVal, elementType));
	}

	if (core::analysis::isVolatileType(elementType)) {
		if(!core::analysis::isVolatileType(init->getType())) {
			return (retIr = builder.makeVolatile(init));
		}
		return (retIr = init);
	}

    // the case of enum type initializations
    if(mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(init->getType())) {
        return (retIr = core::types::castScalar(elementType, init));
    }

	// the case of the Null pointer:
	if (core::analysis::isCallOf(init, builder.getLangBasic().getRefReinterpret())){
		return (retIr = builder.refReinterpret(init.as<core::CallExprPtr>()[0], elementType.as<core::RefTypePtr>()->getElementType()));
	}

	if (core::types::isRefArray(init->getType()) && core::types::isRefArray(targetType)){
		return (retIr = builder.refReinterpret(init, targetType.as<core::RefTypePtr>()->getElementType()));
	}

	std::cerr << "initialization fails: \n\t" << init << std::endl;
	std::cerr << "init type / details: \n\t" << init->getType() << " / " << lookupTypeDetails(init->getType()) << std::endl;
	std::cerr << "\t          target type: " << targetType << std::endl;
	std::cerr << "\t resolved target type: " << elementType << std::endl;

	assert_fail() << " fallthrow";
	return init;
}
//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::convertExpr(const clang::Expr* expr) const {
       assert_true(expr) << "Calling convertExpr with a NULL pointer";
       return exprConvPtr->Visit(const_cast<Expr*>(expr));
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::convertStmt(const clang::Stmt* stmt) const {
       assert_true(stmt) << "Calling convertStmt with a NULL pointer";
       return stmtutils::tryAggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));

}
/////////////////////////////////////////////////////////////////
//
core::FunctionTypePtr Converter::convertFunctionType(const clang::FunctionDecl* funcDecl){
	trackSourceLocation(funcDecl);
	core::FunctionTypePtr funcType = convertType(funcDecl->getType()).as<core::FunctionTypePtr>();

	// check whether it is actually a member function
	core::TypePtr ownerClassType;
	core::FunctionKind funcKind;
	if (const auto* decl = llvm::dyn_cast<clang::CXXConstructorDecl>(funcDecl)) {
		funcKind = core::FK_CONSTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl()->getCanonicalTypeInternal());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXDestructorDecl>(funcDecl)) {
		funcKind = core::FK_DESTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl()->getCanonicalTypeInternal());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
		if (decl->isStatic()){
			return funcType;
		}
		else{
			funcKind = core::FK_MEMBER_FUNCTION;
			ownerClassType = convertType(decl->getParent()->getTypeForDecl()->getCanonicalTypeInternal());
		}
	} else {
		// it is not a member function => just take the plain function
		assert_true(funcType->isPlain());
		return funcType;
	}
	untrackSourceLocation();

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
			assert_fail() << "invalid state!";
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
void Converter::convertTypeDecl(const clang::TypeDecl* decl){
	core::TypePtr res = nullptr;
    for(auto extension : this->getConversionSetup().getExtensions()) {
        core::NodePtr result = extension->Visit(decl, *this);
        if(result) {
            res = result.as<core::TypePtr>();
			typeCache[decl->getTypeForDecl()->getCanonicalTypeInternal ()] = res;
            break;
        }
    }

	if(!res) {
		// trigger the actual conversion
		res = convertType(decl->getTypeForDecl()->getCanonicalTypeInternal ());
	}

    // frequently structs and their type definitions have the same name
	// in this case symbol == res and should be ignored
	if(const clang::TypedefDecl* typedefDecl = llvm::dyn_cast<clang::TypedefDecl>(decl)) {
		auto symbol = builder.genericType(typedefDecl->getQualifiedNameAsString());
		if (res != symbol && res.isa<core::NamedCompositeTypePtr>()) {	// also: skip simple type-defs
			getIRTranslationUnit().addType(symbol, res);
		}
	}

	// handle pragmas
	core::NodeList list({res});
	list = pragma::attachPragma(list, decl, *this);
	assert_eq(1, list.size()) << "More than 1 node present";
	res = list.front().as<core::TypePtr>();

	for(auto extension : this->getConversionSetup().getExtensions()) {
        extension->PostVisit(decl, res, *this);
    }

}

//////////////////////////////////////////////////////////////////
//
core::TypePtr Converter::convertType(const clang::QualType& type) {
	return typeConvPtr->convert( type);
}

namespace {

	core::StatementPtr prepentInitializerList(const clang::CXXConstructorDecl* ctorDecl, const core::TypePtr& classType, const core::StatementPtr& body, Converter& converter) {
		auto& mgr = body.getNodeManager();
		core::FrontendIRBuilder builder(mgr);

		// nameless/anonymous structs/unions result in non-generic classtype
		// structs unions with name should be generic types
		assert( ( ctorDecl->getParent()->getName().empty() ||
				(!ctorDecl->getParent()->getName().empty() && classType.isa<core::GenericTypePtr>())) && "for convenion, this literal must keep the generic type");

		core::TypePtr thisType = builder.refType(classType);
		core::VariablePtr thisVar = converter.thisVariable(thisType);
		core::StatementList initList;

		for(auto it = ctorDecl->init_begin(); it != ctorDecl->init_end(); ++it) {

			core::StringValuePtr ident;	// the identifier of the member to initialize
			core::ExpressionPtr toInit;	// the access to the member to initialize

			core::ExpressionPtr expr = converter.convertExpr((*it)->getInit());		// convert the init expr
			bool isCtor =  insieme::core::analysis::isConstructorCall(expr);		// check if the initExpr is a ctor

			core::ExpressionPtr result;	// the resulting expr to be used to init the member accessed by "toInit" and initialized with "expr"
			// for a ctor call will be : ctor(toInit, params...)
			// for everything else:		 toInit := expr;

			if((*it)->isBaseInitializer ()){

				toInit = thisVar;

				if(!isCtor) {
					// base init is a non-userdefined-default-ctor call, drop it
					continue;
				}

				// if the expr is a constructor then we are initializing a member an object,
				// we have to substitute first argument on constructor by the
				core::CallExprAddress addr = core::CallExprAddress(expr.as<core::CallExprPtr>());
				result = core::transform::replaceNode (mgr, addr->getArgument(0), toInit).as<core::CallExprPtr>();
			} else if ((*it)->isMemberInitializer ()){

				// construct the member access based on the type and the init expression
				core::TypePtr membTy = converter.convertType((*it)->getMember()->getType());
				core::VariablePtr genThis = thisVar;

				bool isCtor =  insieme::core::analysis::isConstructorCall(expr);

				ident = builder.stringValue(((*it)->getMember()->getNameAsString()));
				toInit =  builder.callExpr (builder.refType(membTy),
										  builder.getLangBasic().getCompositeRefElem(), genThis,
										  builder.getIdentifierLiteral(ident), builder.getTypeLiteral(membTy));

				if(isCtor) {
					//TODO: the member to init is a reference? what do we do?
					assert( !core::analysis::isAnyCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType()) && "memberinit with a cppref" );

					VLOG(2) << expr;
					// if the expr is a constructor then we are initializing a member an object,
					// we have to substitute first argument on constructor by the
					core::CallExprAddress addr = core::CallExprAddress(expr.as<core::CallExprPtr>());
					result = core::transform::replaceNode (mgr, addr->getArgument(0), toInit).as<core::CallExprPtr>();
				} else {
					// parameter is some kind of cpp ref, but we want to use the value, unwrap it
					if (!core::analysis::isAnyCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType()) &&
						core::analysis::isAnyCppRef(expr->getType())){
						expr = builder.deref(builder.toIRRef(expr));
					}
					// parameter is NOT cpp_ref but left hand side is -> wrap into cppref
					else if(core::analysis::isAnyCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType()) &&
						!core::analysis::isAnyCppRef(expr->getType())) {

						if(core::analysis::isCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType())) {
							expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), expr);
						} else if(core::analysis::isConstCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType())){
							expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), expr);
						} else { assert_fail(); }
					}
					else{
						// magic tryDeref if not a pointer.... because!
						// dont deref if we have a refref as init and an anyref as expr
						// otherwise void* elements in init list will fail...
						if (!core::types::isRefArray(expr->getType()) && !(core::types::isRefRef(toInit.getType()) &&
																	builder.getLangBasic().isAnyRef(expr->getType())))
							expr = builder.tryDeref(expr);
					}

					// finally build the assigment
					result = builder.assign( toInit, expr);
				}
			} else if ((*it)->isIndirectMemberInitializer ()){
				// this supports indirect init of anonymous member structs/union
				const clang::IndirectFieldDecl* ind = 	(*it)->getIndirectMember () ;
				toInit = thisVar;

				// build a chain of nested access
				clang::IndirectFieldDecl::chain_iterator ind_it = ind->chain_begin ();
				clang::IndirectFieldDecl::chain_iterator end = ind->chain_end ();
				for (; ind_it!= end; ++ind_it){
					assert(llvm::isa<clang::FieldDecl>(*ind_it));
					const clang::FieldDecl* field = llvm::cast<clang::FieldDecl>(*ind_it);
					core::TypePtr fieldTy = converter.convertType(llvm::cast<FieldDecl>(*ind_it)->getType());
					if ((*ind_it)->getNameAsString().empty()){
						ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(field->getFieldIndex()));
					}
					else{
						ident = builder.stringValue(field->getNameAsString());
					}
					toInit = builder.callExpr (builder.refType(fieldTy), builder.getLangBasic().getCompositeRefElem(),
											 toInit, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(fieldTy));
				}

				if(isCtor) {
					//TODO: the member to init is a reference? what do we do?
					assert( !core::analysis::isAnyCppRef(toInit.getType().as<core::RefTypePtr>()->getElementType()) && "memberinit with a cppref" );

					VLOG(2) << expr;
					// if the expr is a constructor then we are initializing a member an object,
					// we have to substitute first argument on constructor by the
					core::CallExprAddress addr = core::CallExprAddress(expr.as<core::CallExprPtr>());
					result = core::transform::replaceNode (mgr, addr->getArgument(0), toInit).as<core::CallExprPtr>();
				} else {
					// finally build the assigment
					result = builder.assign( toInit, expr);
				}
			} else if ((*it)->isInClassMemberInitializer ()){
				assert_not_implemented();
			} else if ((*it)->isDelegatingInitializer ()){
				assert_not_implemented();
			} else  if ((*it)->isPackExpansion () ){
				assert_not_implemented();
			}

			VLOG(2) << result;
			initList.push_back(result);
		}

		// check whether there is something to do
		if (initList.empty()) return body;


		if(body.isa<core::CompoundStmtPtr>()){
			for(auto x : body.as<core::CompoundStmtPtr>()) {
				initList.push_back(x);
			}
		}
		else{
			initList.push_back(body);
		}

		return builder.compoundStmt( initList);

	}
} // anonymous namespace


//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
void Converter::convertFunctionDeclImpl(const clang::FunctionDecl* funcDecl) {

	VLOG(1) << "======================== FUNC: "<< funcDecl->getNameAsString() << " ==================================";

	// obtain function type
	auto funcTy = convertFunctionType(funcDecl);

	// handle pure virtual functions
	if( funcDecl->isPure() && llvm::isa<clang::CXXMethodDecl>(funcDecl)){
		VLOG(2) << "\tpure virtual function " << funcDecl;

		std::string callName = funcDecl->getNameAsString();
		core::ExpressionPtr symbol = builder.literal(callName, funcTy);

		VLOG(2) << symbol<< " " << symbol.getType();
		lambdaExprCache[funcDecl] = symbol;
		return ;
	}

	// handle external functions
	if(!funcDecl->hasBody()) {
		// TODO: move this to the interceptor
		if (funcDecl->getNameAsString() == "free") {
			//handle special function -- "free" -- here instead of in CallExr
			auto retExpr = builder.getLangBasic().getRefDelete();

			// handle issue with typing of free when not including stdlib.h
			core::FunctionTypePtr freeTy = typeCache[funcDecl->getType()].as<core::FunctionTypePtr>();
			typeCache[funcDecl->getType()]=  builder.functionType(freeTy->getParameterTypeList(), builder.getLangBasic().getUnit());

			lambdaExprCache[funcDecl] = retExpr;
			return ;
		}

		//-----------------------------------------------------------------------------------------------------
		//     						Handle of 'special' built-in functions
		//-----------------------------------------------------------------------------------------------------
		if (funcDecl->getNameAsString() == "__builtin_alloca") {
			auto symbol = builder.literal("alloca", funcTy);
			lambdaExprCache[funcDecl] = symbol;
			return;
		}

		// handle extern functions
		auto symbol = builder.literal(utils::buildNameForFunction(funcDecl), funcTy);

		// attach header file info
		getHeaderTagger().addHeaderForDecl(symbol, funcDecl);
		lambdaExprCache[funcDecl] = symbol;
		return ;
	}

	// ---------------  check cases in wich this declaration should not be converted -------------------
	//  checkout this stuff: http://stackoverflow.com/questions/6496545/trivial-vs-standard-layout-vs-pod
	if (const clang::CXXConstructorDecl* ctorDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(funcDecl)){
		core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
		lambdaExprCache[funcDecl] = symbol;

		// non public constructors, or non user provided ones should not be converted
		if (!ctorDecl->isUserProvided () ) {
			if( ctorDecl->isDefaultConstructor() && !ctorDecl->getParent()->isPOD()) {
				VLOG(2) << "HERE1";
			} else {
				return;
			}
		}
		else if(ctorDecl->getParent()->isTrivial())
			return;

	}
	if (const clang::CXXDestructorDecl* dtorDecl = llvm::dyn_cast<clang::CXXDestructorDecl>(funcDecl)){
		core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
		lambdaExprCache[funcDecl] = symbol;

		if (!dtorDecl->isUserProvided () )
			return;
		else if(dtorDecl->getParent()->isTrivial())
			return;
	}


	// --------------- convert potential recursive function -------------

	// -- assume function is recursive => add variable to lambda expr cache --
	core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
	lambdaExprCache[funcDecl] = symbol;

	// -- conduct the conversion of the lambda --
	core::LambdaExprPtr lambda;
	{
		assert_true(funcDecl->hasBody()) << "At this point function should have a body!";

		// init parameter set
		vector<core::VariablePtr> params;
		std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [&](ParmVarDecl* currParam) {
			params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
		});

		//   - set up context to contain current list of parameters and convert body
		Converter::ParameterList oldList = curParameter;
		curParameter = &params;

		// convert function body
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
			body = core::transform::replaceAllGen (mgr, body, thisVariable(thisType), thisVar);

			// in constructors, replace all empty returns by a return of this:
			if (funcTy->isConstructor() || funcTy->isDestructor()) {
				auto emptyReturn = builder.returnStmt(builder.getLangBasic().getUnitConstant());
				auto newReturn   = builder.returnStmt(thisVar);
				body = core::transform::replaceAllGen (mgr, body, emptyReturn, newReturn);
			}
		}

		// build the resulting lambda
		lambda = builder.lambdaExpr(funcTy, params, body);
		VLOG(2) << " DONE: function declaration: " << funcDecl->getQualifiedNameAsString();
	}

	if (funcTy->isMember()) {
		// if we are dealing with a memberFunction, we create a meta-info and store it in the IRTU
		// all meta-infos are merged together when leaving the realm of the TU (when turning into
		// IRProgram) and attacthed to the according type -- this is handeled in the IRTU

		//the class thype of the current member
		core::TypePtr classType = funcTy->getParameterTypes()[0].as<core::RefTypePtr>()->getElementType();
		core::ClassMetaInfo classInfo;

		if (funcTy->isConstructor()) {
			//prevent multiple entries of ctors
			if(!classInfo.containsConstructor(symbol))
				classInfo.addConstructor(symbol);
		}
		else if (funcTy->isDestructor()){
			classInfo.setDestructor(symbol);
			classInfo.setDestructorVirtual(llvm::cast<clang::CXXMethodDecl>(funcDecl)->isVirtual());
		}
		else {
			std::string functionname;
            if(funcDecl->isOverloadedOperator()) {
				//set name of the overloadop -- use the plain simple one from the funcdecl for the BE only
				//the symbol is the rich one (with templates, const yadayadayada)
            	functionname = funcDecl->getNameAsString();
			} else {
				//for everything else use the rich name
				functionname = symbol->getStringValue();
			}
			VLOG(2) << "funcName used: " << functionname;
			VLOG(2) << "qualName: " << funcDecl->getQualifiedNameAsString();
			VLOG(2) << "symbol "<< symbol;

            if (!classInfo.hasMemberFunction(functionname, funcTy, llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst())){
				classInfo.addMemberFunction(functionname, symbol,
											llvm::cast<clang::CXXMethodDecl>(funcDecl)->isVirtual(),
											llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst());
			}
		}
		getIRTranslationUnit().addMetaInfo(classType, classInfo);
	}

	// update cache
	assert_eq(lambdaExprCache[funcDecl], symbol) << "Don't touch this!";

	// finally, add some sugar
	attachFuncAnnotations(lambda, funcDecl);

	// when conversion is complete
	lambdaExprCache[funcDecl] = symbol;

	// register function within resulting translation unit
	getIRTranslationUnit().addFunction(symbol, lambda);
}

core::ExpressionPtr Converter::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool symbolic) {

	// switch to the declaration containing the body (if there is one)
	funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!

	// check whether function has already been converted
	auto pos = lambdaExprCache.find(funcDecl);
	if (pos != lambdaExprCache.end()) {
		return pos->second;		// done
	}

	core::NodePtr result = nullptr;
    for(auto extension : this->getConversionSetup().getExtensions()) {
        result = extension->Visit(funcDecl, *this, symbolic);
        if(core::ExpressionPtr res = result.isa<core::ExpressionPtr>()) {
            //if extension does not return a symbol, create the symbol
            //and check if the extension returned a lambda expr.
            //add this lambda expr to the ir tu and fill the lambda cacheF
            if(core::LiteralPtr symb = res.isa<core::LiteralPtr>()) {
                addToLambdaCache(funcDecl, symb);
            } else {
                auto funcTy = convertFunctionType(funcDecl);
                core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
                assert(res.isa<core::LambdaExprPtr>() && "if the extension does not return a symbol it must return a lambda expresion");
                addToLambdaCache(funcDecl, symbol);
                getIRTranslationUnit().addFunction(symbol, res.as<core::LambdaExprPtr>());
            }
            break;
        };
    }

	if(!result) {
		if(symbolic) {
			// produce only a symbol to be called
			// the actual conversion happens when we encounter the funcDecl in the DeclContext
			auto funcTy = convertFunctionType(funcDecl);
			result = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
		} else {
			convertFunctionDeclImpl(funcDecl);
			result = lambdaExprCache[funcDecl];
		}
    }
    assert_true(result);

    core::ExpressionPtr expr = result.as<core::ExpressionPtr>();

    for(auto extension : this->getConversionSetup().getExtensions()) {
        auto ret = extension->PostVisit(funcDecl, expr, *this, symbolic);
        if(ret) {
            expr=ret.as<core::ExpressionPtr>();
        }
    }

    // the function has already been converted
    return expr;
}

core::ExpressionPtr Converter::getCallableExpression(const clang::FunctionDecl* funcDecl){
	auto retIr = convertFunctionDecl(funcDecl,true);
	VLOG(2) << "callableExpression: " << retIr;
	return retIr;
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace


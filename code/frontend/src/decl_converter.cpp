/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/decl_converter.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"

#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/c/extern_c.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace frontend {
namespace conversion {

	DeclConverter::DeclConverter(Converter& converter) : converter(converter), builder(converter.getIRBuilder()) {}

	// Converters -----------------------------------------------------------------------------------------------------

	DeclConverter::ConvertedVarDecl DeclConverter::convertVarDecl(const clang::VarDecl* varDecl) const {
		auto irType = converter.convertVarType(varDecl->getType());
		auto var = builder.variable(irType);
		if(varDecl->getInit()) {
			return {var, converter.convertCxxArgExpr(varDecl->getInit())};
		} else {
			return {var, {}};
		}
	}

	namespace {
		bool isIrMethod(const clang::FunctionDecl* funDecl) {
			if(!funDecl) return false;
			auto meth = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl);
			return meth && !meth->isStatic();
		}

		core::TypePtr getThisType(Converter& converter, const clang::CXXMethodDecl* methDecl) {
			auto parentType = converter.convertType(converter.getCompiler().getASTContext().getRecordType(methDecl->getParent()));
			auto refKind = core::lang::ReferenceType::Kind::Plain;
			switch(methDecl->getRefQualifier()) {
			case clang::RefQualifierKind::RQ_LValue: refKind = core::lang::ReferenceType::Kind::CppReference; break;
			case clang::RefQualifierKind::RQ_RValue: refKind = core::lang::ReferenceType::Kind::CppRValueReference; break;
			case clang::RefQualifierKind::RQ_None: break; // stop warnings
			}
			return core::lang::buildRefType(parentType, methDecl->isConst(), methDecl->isVolatile(), refKind);
		}

		core::FunctionTypePtr getFunMethodTypeInternal(Converter& converter, const clang::FunctionDecl* funDecl) {
			// first get function type
			auto funType = converter.convertType(funDecl->getType()).as<core::FunctionTypePtr>();
			// is this a method? if not, we are done
			const clang::CXXMethodDecl* methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl);
			if(!isIrMethod(methDecl)) return funType;
			// now build "this" type
			auto thisType = getThisType(converter, methDecl);
			// add "this" parameter to param list
			auto paramList = funType->getParameterTypeList();
			paramList.insert(paramList.begin(), thisType);
			// handle return type for constructors and destructors
			auto retType = funType->getReturnType();
			const clang::CXXConstructorDecl* constrDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(methDecl);
			const clang::CXXDestructorDecl* destrDecl = llvm::dyn_cast<clang::CXXDestructorDecl>(methDecl);
			if(constrDecl || destrDecl) { retType = thisType; }
			// determine kind
			auto kind = core::FunctionKind::FK_MEMBER_FUNCTION;
			if(constrDecl) {
				kind = core::FunctionKind::FK_CONSTRUCTOR;
			} else if(destrDecl) {
				kind = core::FunctionKind::FK_DESTRUCTOR;
			}
			// build type
			auto newFunType = converter.getIRBuilder().functionType(paramList, retType, kind);
			VLOG(2) << "Converted method type from: " << dumpClang(methDecl) << "\n to: " << dumpColor(newFunType)
				    << "(retType: " << *newFunType->getReturnType() << ")\n";
			return newFunType;
		}

		core::StatementList getConstructorInitExpressions(Converter& converter, const clang::FunctionDecl* funcDecl) {
			const clang::CXXConstructorDecl* constrDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(funcDecl);
			if(!constrDecl) return {};

			core::IRBuilder builder(converter.getNodeManager());
			core::StatementList retStmts;
			for(const auto& init: constrDecl->inits()) {
				auto irThis = converter.getVarMan()->getThis();
				auto irMember = irThis;

				// check if field or delegating initialization
				auto fieldDecl = init->getMember();
				if(fieldDecl) {
					// access IR field
					auto access = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
					auto retType = converter.convertVarType(fieldDecl->getType().getUnqualifiedType());
					auto fieldType = core::lang::ReferenceType(retType).getElementType();
					// if Cpp ref or Cpp Rref then use that reference type as field type
					bool unwrap = false;
					if(!core::lang::isPlainReference(retType)) {
						fieldType = retType;
						retType = builder.refType(fieldType);
						unwrap  = true;
					}
					irMember = builder.callExpr(retType, access, irThis, builder.getIdentifierLiteral(fieldDecl->getNameAsString()),
						                        builder.getTypeLiteral(fieldType));
					if(unwrap) {
						irMember = builder.deref(irMember);
					}
				}
				// handle CXXDefaultInitExpr
				auto clangInitExpr = init->getInit();
				if(auto defaultInitExpr = llvm::dyn_cast<clang::CXXDefaultInitExpr>(clangInitExpr)) {
					clangInitExpr = defaultInitExpr->getExpr();
				}
				// handle ConstructExprs
				if(auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(clangInitExpr)) {
					auto converted = utils::convertConstructExpr(converter, constructExpr, irMember).as<core::CallExprPtr>();
					// cast "this" as required for base constructor calls
					auto targetObjType = core::analysis::getObjectType(converted->getFunctionExpr()->getType());
					auto adjustedArgs = converted->getArgumentList();
					adjustedArgs[0] = core::lang::buildRefParentCast(adjustedArgs[0], targetObjType);
					converted = builder.callExpr(converted->getType(), converted->getFunctionExpr(), adjustedArgs);
					retStmts.push_back(converted);
					continue;
				}

				// build init expression
				retStmts.push_back(builder.initExpr(irMember, converter.convertCxxArgExpr(clangInitExpr)));
			}
			return retStmts;
		}

		core::LambdaExprPtr convertFunMethodInternal(Converter& converter, const core::FunctionTypePtr& funType,
			                                         const clang::FunctionDecl* funcDecl, const string& name) {
			const core::IRBuilder& builder = converter.getIRBuilder();
			// switch to the declaration containing the body (if there is one)
			funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!
			const clang::CXXMethodDecl* methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl);
			if(funcDecl->hasBody()) {
				converter.getVarMan()->pushScope(false);
				core::VariableList params;
				core::StatementList bodyStmts;
				// set return type (for return statement conversion)
				converter.getVarMan()->setRetType(core::transform::materialize(funType->getReturnType()));
				// handle implicit "this" for methods
				if(isIrMethod(funcDecl)) {
					auto thisType = getThisType(converter, methDecl);
					auto thisVar = builder.variable(builder.refType(thisType));
					params.push_back(thisVar);
					converter.getVarMan()->setThis(thisVar);
				}
				// handle other parameters
				for(auto param : funcDecl->parameters()) {
					auto irParam = converter.getDeclConverter()->convertVarDecl(param);
					params.push_back(irParam.first);
					converter.getVarMan()->insert(param, irParam.first);
				}
				// convert body and build full expression (must be within scope!)
				auto bodyRange = builder.wrapBody(converter.convertStmt(funcDecl->getBody())).getStatements();
				// handle constructor init expressions (must be within scope!)
				bodyStmts = getConstructorInitExpressions(converter, funcDecl);
				std::copy(bodyRange.cbegin(), bodyRange.cend(), std::back_inserter(bodyStmts));
				converter.getVarMan()->popScope();
				auto body = builder.compoundStmt(bodyStmts);
				auto lambda = builder.lambda(funType, params, body);
				auto funExp = builder.lambdaExpr(lambda, name);
				return funExp;
			} else {
				return core::LambdaExprPtr();
			}

			assert_not_implemented();
			return core::LambdaExprPtr();
		}
	}

	core::ExpressionPtr DeclConverter::convertFunctionDecl(const clang::FunctionDecl* funcDecl, string name, bool genLiteral) const {
		if(name.empty()) name = insieme::utils::mangle(funcDecl->getNameAsString());
		auto funType = getFunMethodTypeInternal(converter, funcDecl);
		if(genLiteral) {
			return converter.getIRBuilder().literal(name, funType);
		}
		return convertFunMethodInternal(converter, funType, funcDecl, name);
	}

	DeclConverter::ConvertedMethodDecl DeclConverter::convertMethodDecl(const clang::CXXMethodDecl* methDecl, const core::ParentsPtr& parents,
		                                                                const core::FieldsPtr& fields, bool declOnly) const {
		ConvertedMethodDecl ret;

		// handle default constructor / destructor / operators in accordance with core
		if(methDecl->isDefaulted()) {
			auto thisType = getThisType(converter, methDecl);
			if(llvm::dyn_cast<clang::CXXDestructorDecl>(methDecl)) {
				ret.lambda = converter.getIRBuilder().getDefaultDestructor(thisType);
				ret.lit = converter.getIRBuilder().getLiteralForDestructor(ret.lambda->getType());
			} else if(auto constDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(methDecl)) {
				if(constDecl->isDefaultConstructor()) {
					ret.lambda = converter.getIRBuilder().getDefaultConstructor(thisType, parents, fields);
				}
				else if(constDecl->isCopyConstructor()) {
					ret.lambda = converter.getIRBuilder().getDefaultCopyConstructor(thisType, parents, fields);
				}
				else if(constDecl->isMoveConstructor()) {
					ret.lambda = converter.getIRBuilder().getDefaultMoveConstructor(thisType, parents, fields);
				} else {
					assert_not_implemented() << "Can't translate defaulted constructor: " << dumpClang(methDecl);
				}
				ret.lit = converter.getIRBuilder().getLiteralForConstructor(ret.lambda->getType());
			} else {
				if(methDecl->isCopyAssignmentOperator()) {
					ret.memFun = converter.getIRBuilder().getDefaultCopyAssignOperator(thisType, parents, fields);
				} else if(methDecl->isMoveAssignmentOperator()) {
					ret.memFun = converter.getIRBuilder().getDefaultMoveAssignOperator(thisType, parents, fields);
				} else {
					assert_not_implemented() << "Can't translate defaulted method: " << dumpClang(methDecl);
				}
				ret.lambda = ret.memFun->getImplementation().as<core::LambdaExprPtr>();
				ret.lit = converter.getIRBuilder().literal(ret.lambda->getReference()->getNameAsString(), ret.lambda->getType());
			}
			if(!converter.getFunMan()->contains(methDecl)) converter.getFunMan()->insert(methDecl, ret.lit);
		}
		// non-default cases
		else {
			string name =  utils::buildNameForFunction(methDecl);
			auto funType = getFunMethodTypeInternal(converter, methDecl);
			if(funType->getKind() == core::FK_CONSTRUCTOR) { ret.lit = builder.getLiteralForConstructor(funType); }
			else if(funType->getKind() == core::FK_DESTRUCTOR) { ret.lit = builder.getLiteralForDestructor(funType); }
			else { ret.lit = builder.getLiteralForMemberFunction(funType, name); }
			if(!converter.getFunMan()->contains(methDecl)) converter.getFunMan()->insert(methDecl, ret.lit);
			if(!declOnly) {
				ret.lambda = convertFunMethodInternal(converter, funType, methDecl, ret.lit->getStringValue());
				ret.memFun = builder.memberFunction(methDecl->isVirtual(), name, ret.lit);
			}
		}
		return ret;
	}

	// Visitors -------------------------------------------------------------------------------------------------------

	void DeclConverter::VisitDeclContext(const clang::DeclContext* context) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitDeclContext: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n ";
		if(VLOG_IS_ON(2)) context->dumpDeclContext();
		for(auto decl : context->decls()) {
			VLOG(2) << "~~~~~~~~~~~~~~~~ VisitDeclContext, decl : " << dumpClang(decl);
			Visit(decl);
		}
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							Structs, Unions and Classes
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitRecordDecl(const clang::RecordDecl* typeDecl) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitRecordDecl: " << dumpClang(typeDecl);
		// we do not convert templates or partial specialized classes/functions, the full
		// type will be found and converted once the instantiation is found
		if(!typeDecl->isCompleteDefinition()) { return; }
		if(typeDecl->isDependentType()) { return; }

		converter.trackSourceLocation(typeDecl);
		converter.convertType(converter.getCompiler().getASTContext().getTypeDeclType(typeDecl));
		converter.untrackSourceLocation();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							Typedefs and type aliases
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitTypedefNameDecl(const clang::TypedefNameDecl* typedefDecl) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitTypedefNameDecl: " << dumpClang(typedefDecl);
		if(!typedefDecl->getTypeForDecl()) { return; }

		// get contained type
		converter.trackSourceLocation(typedefDecl);
		//convertTypeDecl(typedefDecl);
		converter.untrackSourceLocation();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							Variable declarations
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	namespace {
		struct DeclaredTag {};
	}

	void DeclConverter::VisitVarDecl(const clang::VarDecl* var) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitVarDecl: " << dumpClang(var);

		// if handled by a plugin, don't do anything else
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			if(!extension->VarDeclVisit(var, converter)) return;
		}

		// non-global variables are to be skipped
		if(!var->hasGlobalStorage()) { return; }

		converter.trackSourceLocation(var);
		auto convertedVar = convertVarDecl(var);
		auto name = utils::getNameForGlobal(var, converter.getSourceManager());
		auto globalLit = builder.literal(convertedVar.first->getType(), name);

		// for global arrays, purge size information (this is required for sized and unsized arrays in different translation units to be correctly unified)
		auto refT = core::lang::ReferenceType(globalLit->getType());
		auto elemType = refT.getElementType();
		if(core::lang::isArray(elemType)) {
			auto newArrType = core::lang::ArrayType::create(core::lang::ArrayType(elemType).getElementType());
			auto newRefType = core::lang::ReferenceType::create(newArrType, refT.isConst(), refT.isVolatile(), refT.getKind());
			globalLit = builder.literal(newRefType, name);
		}

		// mark as extern and tag with source header if required
		if(var->hasExternalStorage() && !globalLit->hasAttachedValue<DeclaredTag>()) annotations::c::markExtern(globalLit);
		converter.applyHeaderTagging(globalLit, var);

		// handle pragmas attached to decls
		globalLit = pragma::handlePragmas({globalLit}, var, converter).front().as<core::LiteralPtr>();

		// store variable in variable manager
		converter.getVarMan()->insert(var, globalLit);

		// handle initialization if not extern
		if(!var->hasExternalStorage()) {
			auto init = (var->getInit()) ? converter.convertInitExpr(var->getInit()) : builder.getZero(elemType);
			init = utils::fixTempMemoryInInitExpression(globalLit, init);
			core::annotations::attachName(globalLit, name);
			// remove extern tag, add declared tag
			annotations::c::markExtern(globalLit, false);
			globalLit->attachValue<DeclaredTag>();
			converter.getIRTranslationUnit().addGlobal(globalLit, init);
		}

		converter.untrackSourceLocation();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					 Linkage spec, e.g. extern "C"
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitLinkageSpec(const clang::LinkageSpecDecl* link) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitLinkageSpec: " << dumpClang(link);
		bool prevExternC = inExternC;
		inExternC = link->getLanguage() == clang::LinkageSpecDecl::lang_c;
		VisitDeclContext(llvm::cast<clang::DeclContext>(link));
		inExternC = prevExternC;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					 Function declarations
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {

		// if handled by a plugin, don't do anything else
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			if(!extension->FuncDeclVisit(funcDecl, converter)) return;
		}
		// we only want to visit actual specialized templates
		if(funcDecl->isTemplateDecl() && !funcDecl->isFunctionTemplateSpecialization()) { return; }

		converter.trackSourceLocation(funcDecl);
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitFunctionDecl: " << dumpClang(funcDecl);
		bool isDefinition = funcDecl->isThisDeclarationADefinition();
		// switch to the declaration containing the body (if there is one)
		funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!

		// convert prototype
		auto funType = getFunMethodTypeInternal(converter, funcDecl);
		auto name = utils::buildNameForFunction(funcDecl);
		core::LiteralPtr irLit = builder.literal(name, funType);
		// add required annotations
		if(inExternC) { annotations::c::markAsExternC(irLit); }
		converter.applyHeaderTagging(irLit, funcDecl->getCanonicalDecl());
		// insert first before converting the body - skip if we already handled this decl
		if(!converter.getFunMan()->contains(funcDecl->getCanonicalDecl())) converter.getFunMan()->insert(funcDecl->getCanonicalDecl(), irLit);

		// if definition, convert body
		if(isDefinition) {
			VLOG(2) << "~~~~~~~~~~~~~~~~ VisitFunctionDecl - isDefinition: " << dumpClang(funcDecl);
			core::LambdaExprPtr irFunc = convertFunctionDecl(funcDecl, name).as<core::LambdaExprPtr>();
			if(irFunc) {
				irFunc = pragma::handlePragmas({irFunc}, funcDecl, converter).front().as<core::LambdaExprPtr>();
				converter.getIRTranslationUnit().addFunction(irLit, irFunc);
			}
		}

		converter.untrackSourceLocation();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					 Function template declarations
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitFunctionTemplateDecl(const clang::FunctionTemplateDecl* funcTempDecl) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitFunctionTemplateDecl: " << dumpClang(funcTempDecl);
		for(auto spec : funcTempDecl->specializations()) {
			VLOG(2) << "~~~~~~~ -> visit Specialization: " << dumpClang(spec);
			Visit(spec);
		}
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					 Namespace declarations
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	void DeclConverter::VisitNamespaceDecl(const clang::NamespaceDecl* namespaceDecl) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitNamespaceDecl: " << dumpClang(namespaceDecl);
		for(auto decl : namespaceDecl->decls()) {
			Visit(decl);
		}
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/frontend/decl_converter.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/core/analysis/default_delete_member_semantics.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/backend_interception_info.h"
#include "insieme/core/annotations/default_delete.h"
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
		// variable insertion is required prior to translation of the init expression, as it might use it
		if(!varDecl->hasGlobalStorage()) converter.getVarMan()->insert(varDecl, var);

		core::ExpressionPtr varInit = nullptr;
		if(varDecl->getInit()) {
			varInit = converter.convertCxxArgExpr(varDecl->getInit());
		}

		for(auto extension : converter.getConversionSetup().getExtensions()) {
			auto extResult = extension->PostVisit(varDecl, var, varInit, converter);
			//if the extension did any changes, we have to replace the variable in the variable manager
			if(extResult.first != var || extResult.second != varInit) {
				var = extResult.first;
				varInit = extResult.second;
				if(!varDecl->hasGlobalStorage()) {
					converter.getVarMan()->undefine(varDecl);
					converter.getVarMan()->insert(varDecl, var);
				}
			}
		}

		return {var, varInit};
	}

	namespace {
		bool isIrMethod(const clang::FunctionDecl* funDecl) {
			if(!funDecl) return false;
			auto meth = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl);
			return meth && !meth->isStatic();
		}

		core::FunctionTypePtr getFunMethodTypeInternal(Converter& converter, const clang::FunctionDecl* funDecl) {
			// first get function type
			auto funType = converter.convertType(funDecl->getType()).as<core::FunctionTypePtr>();
			// is this a method? if not, we are done
			const clang::CXXMethodDecl* methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl);
			if(!isIrMethod(methDecl)) return funType;
			// now build "this" type
			auto thisType = utils::getThisType(converter, methDecl);
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
				core::TypePtr initTargetType;
				if(fieldDecl) {
					// access IR field
					auto access = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
					auto retType = converter.convertVarType(fieldDecl->getType().getUnqualifiedType());
					auto fieldType = core::lang::ReferenceType(retType).getElementType();
					// if Cpp ref or Cpp Rref then we need to cast the init to that type later on
					if(!core::lang::isPlainReference(retType)) {
						fieldType = retType;
						initTargetType = fieldType;
						retType = builder.refType(fieldType);
					}
					irMember = builder.callExpr(retType, access, irThis, builder.getIdentifierLiteral(fieldDecl->getNameAsString()),
						                        builder.getTypeLiteral(fieldType));
				}
				// handle CXXDefaultInitExpr
				auto clangInitExpr = init->getInit();
				if(auto defaultInitExpr = llvm::dyn_cast<clang::CXXDefaultInitExpr>(clangInitExpr)) {
					clangInitExpr = defaultInitExpr->getExpr();
				}
				if(auto cleanupExpr = llvm::dyn_cast<clang::ExprWithCleanups>(clangInitExpr)) {
					clangInitExpr = cleanupExpr->getSubExpr();
				}

				// extensions get a chance to handle this constructor init expression. if they do, we won't perform the default handling
				bool handledByExtension = false;
				for(auto extension : converter.getConversionSetup().getExtensions()) {
					if(auto extensionResult = extension->Visit(init, clangInitExpr, irMember, converter)) {
						retStmts.push_back(extensionResult);
						handledByExtension = true;
						break;
					}
				}
				if(handledByExtension) {
					continue;
				}

				// handle ConstructExprs
				if(auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(clangInitExpr)) {
					auto converted = utils::convertConstructExpr(converter, constructExpr, irMember);
					if(auto convertedCall = converted.isa<core::CallExprPtr>()) {
						// cast "this" as required for base constructor calls
						auto targetObjType = core::analysis::getObjectType(convertedCall->getFunctionExpr()->getType());
						auto adjustedArgs = convertedCall->getArgumentList();
						adjustedArgs[0] = core::lang::buildRefParentCast(adjustedArgs[0], targetObjType);
						converted = builder.callExpr(convertedCall->getType(), convertedCall->getFunctionExpr(), adjustedArgs);
					}
					retStmts.push_back(converted);
					continue;
				}

				// build init expression
				auto irInit = converter.convertCxxArgExpr(clangInitExpr);
				irInit = core::transform::castInitializationIfNotMaterializing(initTargetType ? initTargetType : irMember->getType(), irInit);
				retStmts.push_back(builder.initExpr(irMember, irInit));
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
					auto thisType = utils::getThisType(converter, methDecl);
					auto thisVar = builder.variable(builder.refType(thisType));
					params.push_back(thisVar);
					converter.getVarMan()->setThis(thisVar);
				}
				// handle other parameters
				for(auto param : funcDecl->parameters()) {
					auto irParam = converter.getDeclConverter()->convertVarDecl(param);
					params.push_back(irParam.first);
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

				// attach backend interception information
				core::annotations::attachBackendInterceptionInfo(funExp, { funcDecl->getNameAsString() });

				return funExp;
			} else {
				return core::LambdaExprPtr();
			}

			assert_not_implemented();
			return core::LambdaExprPtr();
		}
	}

	core::ExpressionPtr DeclConverter::convertFunctionDecl(const clang::FunctionDecl* funcDecl, string name, bool genLiteral) const {
		if(name.empty()) name = utils::buildNameForFunction(funcDecl, converter);
		auto funType = getFunMethodTypeInternal(converter, funcDecl);
		if(genLiteral) {
			return converter.getIRBuilder().literal(name, funType);
		}
		return convertFunMethodInternal(converter, funType, funcDecl, name);
	}

	core::analysis::MemberProperties DeclConverter::convertMethodDecl(const clang::CXXMethodDecl* methDecl, const core::ParentsPtr& parents,
		                                                                const core::FieldsPtr& fields, bool declOnly, bool addDefaultedAndDeletedAnnotations) const {
		core::analysis::MemberProperties ret;
		auto& builder = converter.getIRBuilder();

		// handle defaulted and deleted constructor / destructor / operators in accordance with core
		// deleted constructors which are not one of the default members are handled in the else block
		if(methDecl->isDefaulted()
				|| (methDecl->isDeleted() && utils::isDefaultClassMember(methDecl))) {
			bool defaulted = methDecl->isDefaulted();
			auto thisType = utils::getThisType(converter, methDecl);
			if(llvm::dyn_cast<clang::CXXDestructorDecl>(methDecl)) {
				auto type = core::analysis::buildDefaultDestructorType(thisType);
				ret.literal = builder.getLiteralForDestructor(type);
			} else if(auto constDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(methDecl)) {
				core::FunctionTypePtr type;
				if(constDecl->isDefaultConstructor()) {
					type = core::analysis::buildDefaultDefaultConstructorType(thisType);
				}
				else if(constDecl->isCopyConstructor()) {
					type = core::analysis::buildDefaultCopyConstructorType(thisType);
				}
				else if(constDecl->isMoveConstructor()) {
					type = core::analysis::buildDefaultMoveConstructorType(thisType);
				} else {
					assert_fail() << "Can't translate defaulted constructor: " << dumpClang(methDecl);
				}
				ret.literal = builder.getLiteralForConstructor(type);
			} else {
				core::FunctionTypePtr type;
				if(methDecl->isCopyAssignmentOperator()) {
					type = core::analysis::buildDefaultCopyAssignOperatorType(thisType);
				} else if(methDecl->isMoveAssignmentOperator()) {
					type = core::analysis::buildDefaultMoveAssignOperatorType(thisType);
				} else {
					assert_fail() << "Can't translate defaulted method: " << dumpClang(methDecl);
				}
				ret.literal = builder.getLiteralForMemberFunction(type, insieme::utils::getMangledOperatorAssignName());
				ret.memberFunction = builder.memberFunction(false, insieme::utils::getMangledOperatorAssignName(), ret.literal);
			}
			// mark the literal as defaulted or deleted
			if(!declOnly && addDefaultedAndDeletedAnnotations) {
				if(defaulted) core::annotations::markDefaultedPreTU(ret.literal);
				else core::annotations::markDeletedPreTU(ret.literal);
			}

			if(!converter.getFunMan()->contains(methDecl)) converter.getFunMan()->insert(methDecl, ret.literal);
		}
		// non-defaulted/deleted cases
		else {
			string name =  utils::buildNameForFunction(methDecl, converter);
			auto funType = getFunMethodTypeInternal(converter, methDecl);
			if(methDecl->isStatic()) { ret.literal = builder.literal(name, funType); } // static members are not methods in IR, need to treat here
			else if(funType->getKind() == core::FK_CONSTRUCTOR) { ret.literal = builder.getLiteralForConstructor(funType); }
			else if(funType->getKind() == core::FK_DESTRUCTOR) { ret.literal = builder.getLiteralForDestructor(funType); }
			else { ret.literal = builder.getLiteralForMemberFunction(funType, name); }
			if(!converter.getFunMan()->contains(methDecl)) converter.getFunMan()->insert(methDecl, ret.literal);
			if(!declOnly) {
				ret.lambda = convertFunMethodInternal(converter, funType, methDecl, ret.literal->getStringValue());
				ret.memberFunction = builder.memberFunction(methDecl->isVirtual(), name, ret.literal);
			}
		}
		return ret;
	}

	// Visitors -------------------------------------------------------------------------------------------------------

	void DeclConverter::VisitDeclContext(const clang::DeclContext* context) {
		VLOG(2) << "~~~~~~~~~~~~~~~~ VisitDeclContext: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n ";
		//if(VLOG_IS_ON(2)) context->dumpDeclContext();
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
		core::TypePtr arrayType;
		if(core::lang::isArray(elemType)) {
			// we keep the original  size for later fixing though
			arrayType = elemType;
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
			// if we originally dropped the size information of an global array init
			if(arrayType) {
				// we change the type if the init expr back. (from type ref<array,inf>,...>)
				auto memLocAddr = core::InitExprAddress(init.as<core::InitExprPtr>())->getType().as<core::GenericTypeAddress>()->getTypeParameter(0);
				init = core::transform::replaceNode(init->getNodeManager(), memLocAddr, arrayType).as<core::ExpressionPtr>();
			}
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
		auto name = utils::buildNameForFunction(funcDecl, converter);
		core::LiteralPtr irLit = builder.literal(name, funType);
		// if this is a static member function,  we change the name to use and create the literal differently
		if(const auto& methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
			if(methodDecl->isStatic()) {
				// get the name of the record type for prefixing
				const auto& recordType = converter.convertType(clang::QualType(methodDecl->getParent()->getTypeForDecl(), 0)).isa<core::GenericTypePtr>();
				assert_true(recordType);

				irLit = builder.getLiteralForStaticMemberFunction(funType, recordType->getFamilyName(), name);
				name = irLit->getStringValue();
			}
		}
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

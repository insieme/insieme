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

#include "insieme/frontend/extensions/interceptor_extension.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/core/analysis/default_delete_member_semantics.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/variadic_template_extension.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/utils/name_mangling.h"

#include <boost/program_options.hpp>

namespace insieme {
namespace frontend {
namespace extensions {

	InterceptorExtension::InterceptorExtension() {
	}

	FrontendExtension::FlagHandler InterceptorExtension::registerFlag(boost::program_options::options_description& options) {
		// create lambda
		auto lambda = [&](const ConversionJob& job) {
			// check if the default activated plugins have been deactivated manually
			if(job.hasOption(frontend::ConversionJob::NoDefaultExtensions)) { return false; }
			return true;
		};
		return lambda;
	}

	boost::optional<std::string> InterceptorExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// TODO implement a method for global sort check of extensions which is extensible in outside projects
		// interceptor needs to be the first extension in the extension list
		//if(setup.getExtensions().begin()->get() != this) {
		//	return boost::optional<std::string>("InterceptorExtension should be the first Extension");
		//}
		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	namespace {

		template<typename TemplateParm>
		string getTemplateTypeParmName(const TemplateParm* parm) {
			return format("T_%d_%d", parm->getDepth(), parm->getIndex());
		}
		template <typename TemplateParm>
		core::TypePtr getTypeVarForTemplateTypeParmType(const core::IRBuilder& builder, const TemplateParm* parm) {
			return builder.typeVariable(getTemplateTypeParmName(parm));
		}
		template<typename TemplateParm>
		core::VariadicTypeVariablePtr getTypeVarForVariadicTemplateTypeParmType(const core::IRBuilder& builder, const TemplateParm* parm) {
			return builder.variadicTypeVariable("V_" + getTemplateTypeParmName(parm));
		}
		template<typename TemplateParm>
		core::VariadicGenericTypeVariablePtr getTypeVarForVariadicTemplateTemplateTypeParmType(const core::IRBuilder& builder, const TemplateParm* parm) {
			return builder.variadicGenericTypeVariable("V_T_" + getTemplateTypeParmName(parm));
		}

		template <typename TemplateParm>
		core::TypePtr getGenericRefType(const core::IRBuilder &builder, const TemplateParm* parm) {
			auto name = getTemplateTypeParmName(parm);
			return builder.genericType("ref", toVector<core::TypePtr>(builder.typeVariable(name), builder.typeVariable(name + "_a"),
				                                                      builder.typeVariable(name + "_b"), builder.typeVariable(name + "_c")));
		}

		void convertTemplateParameters(const clang::TemplateParameterList* tempParamList, const core::IRBuilder& builder,
			                           core::TypeList& templateGenericParams) {
			for(auto tempParam : *tempParamList) {
				core::TypePtr typeVar;
				// handles parameter packs in each branch below
				// (we can't do it here since we need the specific types, and they have no common interface)
				auto handleParamPack = [&templateGenericParams, &builder](const auto& parm) {
					if(parm->isParameterPack()) {
						core::TypePtr typeVar = getTypeVarForVariadicTemplateTypeParmType(builder, parm);
						if(llvm::dyn_cast<clang::TemplateTemplateParmDecl>(parm)) {
							typeVar = getTypeVarForVariadicTemplateTemplateTypeParmType(builder, parm);
						}
						templateGenericParams.push_back(typeVar);
						// we only need arguments up to the first top-level variadic, the rest can be deduced, so we break
						return true;
					}
					return false;
				};
				if(auto templateParamTypeDecl = llvm::dyn_cast<clang::TemplateTypeParmDecl>(tempParam)) {
					if(handleParamPack(templateParamTypeDecl)) break;
					typeVar = getGenericRefType(builder, templateParamTypeDecl);
				} else if(auto templateNonTypeParamDecl = llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(tempParam)) {
					if(handleParamPack(templateNonTypeParamDecl)) break;
					typeVar = getTypeVarForTemplateTypeParmType(builder, templateNonTypeParamDecl);
				} else if(auto templateTemplateParamDecl = llvm::dyn_cast<clang::TemplateTemplateParmDecl>(tempParam)) {
					if(handleParamPack(templateTemplateParamDecl)) break;
					core::TypeList paramTypeList;
					convertTemplateParameters(templateTemplateParamDecl->getTemplateParameters(), builder, paramTypeList);
					typeVar = builder.genericTypeVariable("T_" + getTemplateTypeParmName(templateTemplateParamDecl), paramTypeList);
				} else {
					tempParam->dump();
					assert_not_implemented() << "Unexpected kind of template parameter\n";
				}
				templateGenericParams.push_back(typeVar);
			}
		}

		core::TypePtr adjustTemplateTypeReferenceQualifierKind(const core::TypePtr& type) {
			if(!core::lang::isPlainReference(type)) return type;
			auto refT = core::lang::ReferenceType(type);
			refT.setKind(core::lang::ReferenceType::Kind::Qualified);
			return refT.toType();
		}

		core::TypePtr convertTemplateArgument(conversion::Converter& converter, const clang::TemplateArgument& arg) {
			switch(arg.getKind()) {
			case clang::TemplateArgument::Expression: return adjustTemplateTypeReferenceQualifierKind(converter.convertVarType(arg.getAsExpr()->getType()));
			case clang::TemplateArgument::Type: return adjustTemplateTypeReferenceQualifierKind(converter.convertVarType(arg.getAsType()));
			case clang::TemplateArgument::Integral: return converter.getIRBuilder().numericType(arg.getAsIntegral().getSExtValue());
			case clang::TemplateArgument::Template: {
				auto tempDecl = arg.getAsTemplate().getAsTemplateDecl();
				core::TypeList paramTypes;
				convertTemplateParameters(tempDecl->getTemplateParameters(), converter.getIRBuilder(), paramTypes);
				auto genType = converter.getIRBuilder().genericType(insieme::utils::mangle(tempDecl->getQualifiedNameAsString()), paramTypes);
				converter.applyHeaderTagging(genType, tempDecl->getTemplatedDecl());
				return genType;
			}
			case clang::TemplateArgument::Pack: assert_not_implemented() << "Template parameter packs are handled in convertTemplateArguments\n";
			case clang::TemplateArgument::Null:
			case clang::TemplateArgument::Declaration:
			case clang::TemplateArgument::NullPtr:
			case clang::TemplateArgument::TemplateExpansion:
			break;
			}
			assert_not_implemented() << "Unsupported template argument kind\n";
			return {};
		}

		void convertTemplateArguments(const clang::TemplateArgumentList& tempArgList, conversion::Converter& converter, core::TypeList& templateArgsTypes) {
			for(auto arg: tempArgList.asArray()) {
				if(arg.getKind() == clang::TemplateArgument::Pack) {
					for(auto innerArg : arg.getPackAsArray()) {
						templateArgsTypes.push_back(convertTemplateArgument(converter, innerArg));
					}
					// we only need arguments up to the first top-level variadic, the rest can be deduced
					break;
				} else {
					templateArgsTypes.push_back(convertTemplateArgument(converter, arg));
				}
			}
			// If we don't have any template arguments here, we encode this information in order to be able to recognice this in the backend again and print the empty list <>
			if(templateArgsTypes.empty()) {
				templateArgsTypes.push_back(converter.getNodeManager().getLangExtension<core::lang::VariadicTemplateExtension>().getEmptyVariadicTemplateArguments());
			}
		}

		// used to replace concrete types with type variables in
		// fully generic template function types (which are the targets of instantiation)
		core::FunctionTypePtr fullyGenerifyFunType(const core::FunctionTypePtr& genericFunType) {
			core::IRBuilder builder(genericFunType->getNodeManager());
			size_t paramNum = 0;
			return core::transform::transformBottomUpGen(genericFunType, [&](const core::GenericTypePtr& genTy) {
				if(!core::lang::isBuiltIn(genTy) && insieme::utils::isMangled(genTy->getName()->getValue())) {
					core::TypeList newTypeParams;
					for(auto& typeParm : genTy->getTypeParameterList()) {
						if(typeParm.isa<core::TypeVariablePtr>()
							|| typeParm.isa<core::VariadicTypeVariablePtr>()
							|| typeParm.isa<core::GenericTypeVariablePtr>()
							|| typeParm.isa<core::VariadicGenericTypeVariablePtr>()
							|| core::lang::isReference(typeParm)) {
							newTypeParams.push_back(typeParm);
						}
						else {
							newTypeParams.push_back(builder.typeVariable(::format("TX_%d", paramNum++)));
						}
					}
					auto replacement = builder.genericType(genTy->getName(), genTy->getParents(), builder.types(newTypeParams));
					core::transform::utils::migrateAnnotations(genTy, replacement);
					return replacement;
				}
				return genTy;
			});
		}

		std::pair<core::ExpressionPtr, core::TypePtr> generateCallee(conversion::Converter& converter, const clang::Decl* decl, const clang::Expr* clangExpr,
			                                                         const clang::ASTTemplateArgumentListInfo* explicitTemplateArgs) {
			const core::IRBuilder& builder(converter.getIRBuilder());

			auto funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
			if(!funDecl) {
				assert_not_implemented();
				return {};
			}

			// convert to literal depending on whether we are dealing with a function or a method
			auto litConverter = [&converter, &builder](const clang::FunctionDecl* funDecl) {
				core::LiteralPtr lit;

				if(auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl)) {
					lit = converter.getDeclConverter()->convertMethodDecl(methDecl, builder.parents(), builder.fields(), true, false).literal;
				} else {
					lit = builder.literal(utils::buildNameForFunction(funDecl, converter), converter.convertType(funDecl->getType()));
				}

				// special case handling for std::function operator() calls
				if(boost::starts_with(lit.as<core::LiteralPtr>()->getStringValue(), "IMP_std_colon__colon_function::IMP__operator_call_")) {
					auto litFunType = lit->getType().as<core::FunctionTypePtr>();
					if(core::analysis::isGeneric(litFunType->getReturnType())) {
						auto retType = builder.typeVariable("__std_fun_ret_type");
						lit = builder.literal(lit.getValue(), builder.functionType(litFunType->getParameterTypes(), retType, litFunType->getKind()));
					}
				}

				return lit;
			};

			core::ExpressionPtr lit = litConverter(funDecl);
			auto retType = lit->getType().as<core::FunctionTypePtr>()->getReturnType();

			// handle non-templated functions
			if(!funDecl->isTemplateInstantiation()) {
				auto funType = lit->getType().as<core::FunctionTypePtr>();
				// if return type is type variable, and we have a target type, instantiate
				if(clangExpr && core::analysis::isGeneric(retType)) {
					retType = converter.convertExprType(clangExpr);
					auto concreteFunType = builder.functionType(funType->getParameterTypes(), retType, funType->getKind());
					lit = builder.instantiate(concreteFunType,lit);
				}
				// return the concrete return type but potentially generic literal
				converter.applyHeaderTagging(lit, decl);
				return {lit, retType};
			}

			// first: handle generic template in order to generate generic function
			core::TypeList templateGenericParams, templateConcreteParams;
			auto templateDecl = funDecl->getPrimaryTemplate();
			if(templateDecl) {
				// build map for generic template parameters
				convertTemplateParameters(templateDecl->getTemplateParameters(), builder, templateGenericParams);

				// build list of concrete params for instantiation of this call
				convertTemplateArguments(*(funDecl->getTemplateSpecializationInfo()->TemplateArguments), converter, templateConcreteParams);
			}
			// translate uninstantiated pattern instead of instantiated version
			auto pattern = funDecl->getTemplateInstantiationPattern();

			// only provide explicit instantiation list if one was provided by the user
			auto explicitInstantiationTypes = builder.types(templateConcreteParams);
			auto explicitGenericTypeParams = builder.types(templateGenericParams);
			if(templateDecl && !explicitTemplateArgs) {
				explicitInstantiationTypes = builder.types({});
				explicitGenericTypeParams = builder.types({});
			}

			auto genericFunLit = litConverter(pattern);
			auto genericFunType = genericFunLit->getType().as<core::FunctionTypePtr>();

			genericFunType = fullyGenerifyFunType(genericFunType);

			genericFunType = builder.functionType(genericFunType->getParameterTypes(), genericFunType->getReturnType(), genericFunType->getKind(),
			                                      explicitGenericTypeParams);
			auto innerLit = builder.literal(genericFunLit->getValue(), genericFunType);
			converter.applyHeaderTagging(innerLit, decl);

			// cast to concrete type at call site
			auto concreteFunctionType = lit->getType().as<core::FunctionTypePtr>();

			// if we don't need to do any type instantiation
			if(!concreteFunctionType->isMember() && templateConcreteParams.empty() && core::analysis::isReturnTypePotentiallyDeducible(genericFunType)) {
				return {innerLit, concreteFunctionType.getReturnType()};
			}

			concreteFunctionType = builder.functionType(concreteFunctionType->getParameterTypes(), concreteFunctionType->getReturnType(),
				                                        concreteFunctionType->getKind(), explicitInstantiationTypes);

			return {builder.instantiate(concreteFunctionType, innerLit),
				    concreteFunctionType->getReturnType()};
		}

		core::CallExprPtr interceptMethodCall(conversion::Converter& converter, const clang::Decl* decl,
			                                  std::function<core::ExpressionPtr(const core::TypePtr&)> thisArgFactory, clang::CallExpr::arg_const_range args,
			                                  const clang::Expr* clangExpr, const clang::ASTTemplateArgumentListInfo* explicitTemplateArgs) {
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(decl);
				if(methDecl) {
					auto calleePair = generateCallee(converter, decl, clangExpr, explicitTemplateArgs);
					auto convMethodLit = calleePair.first;
					auto funType = convMethodLit->getType().as<core::FunctionTypePtr>();
					auto retType = calleePair.second;
					auto thisArg = thisArgFactory(retType);
					// Implicit materialization of this argument is not performed in clang AST
					thisArg = frontend::utils::prepareThisExpr(converter, thisArg);
					VLOG(2) << "Interceptor: intercepted clang method/constructor call\n" << dumpClang(decl) << "\n";
					auto retCall = utils::buildCxxMethodCall(converter, retType, convMethodLit, thisArg, args);
					return retCall;
				}
			}
			return nullptr;
		}

		const clang::TemplateTypeParmType* getPotentiallyReferencedTemplateTypeParmType(const clang::Type* type) {
			auto ret = llvm::dyn_cast<clang::TemplateTypeParmType>(type);
			if(ret) return ret;
			auto ref = llvm::dyn_cast<clang::ReferenceType>(type);
			if(ref) return getPotentiallyReferencedTemplateTypeParmType(ref->getPointeeType().getTypePtr());
			return {};
		}
	}

	core::ExpressionPtr InterceptorExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		const core::IRBuilder& builder = converter.getIRBuilder();
		VLOG(3) << "Intercepting Expression\n";

		// directly build init exprs of intercepted structural types (main FE cannot look up their member types)
		if(auto initList = llvm::dyn_cast<clang::InitListExpr>(expr)) {
			if(auto clangTt = llvm::dyn_cast<clang::TagType>(initList->getType()->getCanonicalTypeUnqualified())) {
				if(!clangTt->isEnumeralType() && converter.getHeaderTagger()->isIntercepted(clangTt->getDecl())) {
					core::ExpressionList initExps;
					for(unsigned i = 0; i < initList->getNumInits(); ++i) { // yes, that is really the best way to do this in clang 3.6
						initExps.push_back(converter.convertInitExpr(initList->getInit(i)));
					}
					core::ExpressionPtr retIr = builder.initExprTemp(converter.convertType(initList->getType()), initExps);
					retIr = utils::fixTempMemoryInInitExpressionInits(retIr);
					return retIr;
				}
			}
		}

		// decl refs to intercepted functions
		if(auto dr = llvm::dyn_cast<clang::DeclRefExpr>(expr)) {
			auto decl = dr->getDecl();
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				// translate functions
				if(llvm::dyn_cast<clang::FunctionDecl>(decl)) {
					core::ExpressionPtr lit = generateCallee(converter, decl, nullptr, dr->getOptionalExplicitTemplateArgs()).first;
					VLOG(2) << "Interceptor: intercepted clang fun\n" << dumpClang(decl) << " -> converted to literal: " << *lit << " of type "
						    << *lit->getType() << "\n";
					return lit;
				}
				// as well as global variables
				auto varDecl = llvm::dyn_cast<clang::VarDecl>(decl);
				if(varDecl) {
					// only intercept variables with global storage
					if(!varDecl->hasGlobalStorage()) return nullptr;
					auto lit = builder.literal(insieme::utils::mangle(decl->getQualifiedNameAsString()), converter.convertVarType(expr->getType()));
					converter.applyHeaderTagging(lit, decl);
					VLOG(2) << "Interceptor: intercepted clang lit\n" << dumpClang(decl) << " -> converted to literal: " << *lit << " of type "
						    << *lit->getType() << "\n";
					return lit;
				}
				// and enum constants
				if(llvm::dyn_cast<clang::EnumConstantDecl>(decl)) {
					const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(decl->getDeclContext())->getTypeForDecl());
					core::ExpressionPtr exp =
						builder.literal(insieme::utils::mangle(decl->getQualifiedNameAsString()), converter.convertType(clang::QualType(enumType, 0)));
					converter.applyHeaderTagging(exp, decl);
					exp = builder.numericCast(exp, converter.convertType(expr->getType()));
					VLOG(2) << "Interceptor: intercepted clang enum\n" << dumpClang(decl) << " -> converted to expression: " << *exp << " of type "
						    << *exp->getType() << "\n";
					return exp;
				}
			}
		}
		// member calls and their variants
		const clang::ASTTemplateArgumentListInfo* explicitTemplateArgs = nullptr;
		if(auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) {
			// for array initializations, we need to build an init expr rather than a constructor call
			if(expr->getType()->isArrayType()) {
				assert_eq(construct->getNumArgs(), 0) << "Interceptor: can't construct array of objects with constructor args";
				core::ExpressionPtr retIr = builder.initExprTemp(converter.convertType(expr->getType()));
				retIr = utils::fixTempMemoryInInitExpressionInits(retIr);
				return retIr;
			}
			auto thisFactory = [&](const core::TypePtr& retType){ return core::lang::buildRefTemp(retType); };
			return interceptMethodCall(converter, construct->getConstructor(), thisFactory, construct->arguments(), nullptr, explicitTemplateArgs);
		}
		if(auto newExp = llvm::dyn_cast<clang::CXXNewExpr>(expr)) {
			// array case does not need to be intercepted -- only needs type, which is intercepted further down in the AST
			if(newExp->isArray()) return nullptr;
			if(auto construct = newExp->getConstructExpr()) {
				auto thisFactory = [&](const core::TypePtr& retType){ return builder.undefinedNew(retType); };
				auto ret = interceptMethodCall(converter, construct->getConstructor(), thisFactory, construct->arguments(), nullptr, explicitTemplateArgs);
				if(ret) return core::lang::buildPtrFromRef(ret);
			}
		}
		if(auto delExp = llvm::dyn_cast<clang::CXXDeleteExpr>(expr)) {
			auto qt = delExp->getDestroyedType();
			if(auto delType = llvm::dyn_cast<clang::TagType>(qt.getCanonicalType())) {
				if(converter.getHeaderTagger()->isIntercepted(delType->getDecl())) {
					auto exprToDelete = converter.convertExpr(delExp->getArgument());
					if(delExp->isArrayForm()) return builder.refDelete(core::lang::buildPtrToArray(exprToDelete));
					core::TypePtr thisType = builder.refType(converter.convertType(qt));
					auto destructor = builder.getLiteralForDestructor(builder.functionType(toVector(thisType), thisType, core::FK_DESTRUCTOR));
					return builder.refDelete(builder.callExpr(destructor, core::lang::buildPtrToRef(exprToDelete)));
				}
			}
		}
		if(auto memberCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(expr)) {
			auto callee = llvm::dyn_cast<clang::MemberExpr>(memberCall->getCallee());
			if(callee) explicitTemplateArgs = callee->getOptionalExplicitTemplateArgs();
			auto thisFactory = [&](const core::TypePtr& retType) { return converter.convertExpr(memberCall->getImplicitObjectArgument()); };
			return interceptMethodCall(converter, memberCall->getCalleeDecl(), thisFactory, memberCall->arguments(), memberCall, explicitTemplateArgs);
		}
		if(auto operatorCall = llvm::dyn_cast<clang::CXXOperatorCallExpr>(expr)) {
			auto decl = operatorCall->getCalleeDecl();
			auto callee = llvm::dyn_cast<clang::MemberExpr>(operatorCall->getCallee());
			if(callee) explicitTemplateArgs = callee->getOptionalExplicitTemplateArgs();
			if(decl) {
				if(llvm::dyn_cast<clang::CXXMethodDecl>(decl)) {
					auto argList = operatorCall->arguments();
					auto thisFactory = [&](const core::TypePtr& retType) { return converter.convertExpr(*argList.begin()); };
					decltype(argList) remainder(argList.begin() + 1, argList.end());
					return interceptMethodCall(converter, decl, thisFactory, remainder, operatorCall, explicitTemplateArgs);
				}
			}
		}

		return nullptr;
	}

	core::TypePtr InterceptorExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		clang::QualType type = typeIn;
		const core::IRBuilder& builder = converter.getIRBuilder();

		// lookup template parameters of intercepted tagtypes
		if(auto injected = llvm::dyn_cast<clang::InjectedClassNameType>(type.getUnqualifiedType())) {
			auto spec = injected->getInjectedSpecializationType()->getUnqualifiedDesugaredType();
			if(auto tempSpecType = llvm::dyn_cast<clang::TemplateSpecializationType>(spec)) {
				auto key = tempSpecType->getCanonicalTypeUnqualified().getTypePtr();
				if(!::containsKey(templateSpecializationMapping, key)) {
					key->dump();
					assert_fail() << "Template injected specialization type encountered, but no mapping available. Key ^^";
				}
				return templateSpecializationMapping[key];
			}
		}

		// lookup TemplateSpecializationTypes
		if(auto tempSpecType = llvm::dyn_cast<clang::TemplateSpecializationType>(type->getCanonicalTypeUnqualified())) {
			if(::containsKey(templateSpecializationMapping, tempSpecType)) return templateSpecializationMapping[tempSpecType];
		}

		// handle template parameters of intercepted template functions
		if(auto ttpt = llvm::dyn_cast<clang::TemplateTypeParmType>(type.getUnqualifiedType())) {
			return getTypeVarForTemplateTypeParmType(builder, ttpt);
		}

		// handle dependent name types ("typename t")
		if(auto depName = llvm::dyn_cast<clang::DependentNameType>(type.getUnqualifiedType())) {
			return builder.typeVariable(insieme::utils::mangle(utils::getNameForDependentNameType(depName)));
		}

		// handle pack expansion type ("Args...")
		if(auto packExp = llvm::dyn_cast<clang::PackExpansionType>(type.getUnqualifiedType())) {
			auto templateTypeParmType = getPotentiallyReferencedTemplateTypeParmType(packExp->getPattern().getTypePtr());
			assert_true(templateTypeParmType) << "Unexpected template parameter pack type";
			return getTypeVarForVariadicTemplateTypeParmType(builder, templateTypeParmType);
		}

		// handle class, struct and union interception
		if(auto tt = llvm::dyn_cast<clang::TagType>(type->getCanonicalTypeUnqualified())) {

			// do not intercept enums, they are simple
			if(tt->isEnumeralType()) return nullptr;

			auto decl = tt->getDecl();
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				auto genType = converter.getIRBuilder().genericType(utils::getNameForTagDecl(converter, decl).first);

				// for templates: convert template arguments to generic type parameters
				if(auto templateDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(decl)) {
					auto genericDecl = templateDecl->getSpecializedTemplate();

					// store class template template type parameters in map
					core::TypeList genericTypeParams;
					convertTemplateParameters(genericDecl->getTemplateParameters(), builder, genericTypeParams);
					auto genericGenType = converter.getIRBuilder().genericType(insieme::utils::mangle(decl->getQualifiedNameAsString()), genericTypeParams);

					// store injected class name in specialization map (take care to store correct key for partial specializations)
					const clang::TemplateSpecializationType* tempSpecType = nullptr;
					auto specOrPartial = templateDecl->getSpecializedTemplateOrPartial();
					if(auto partialDecl = specOrPartial.dyn_cast<clang::ClassTemplatePartialSpecializationDecl*>()) {
						tempSpecType =
							llvm::dyn_cast<clang::TemplateSpecializationType>(partialDecl->getInjectedSpecializationType()->getUnqualifiedDesugaredType());
					} else {
						tempSpecType =
							llvm::dyn_cast<clang::TemplateSpecializationType>(genericDecl->getInjectedClassNameSpecialization()->getUnqualifiedDesugaredType());
					}
					assert_false(tempSpecType == nullptr) << "Unexpected kind of template specialization\n";
					templateSpecializationMapping[tempSpecType->getCanonicalTypeUnqualified().getTypePtr()] = genericGenType;

					// build concrete genType
					core::TypeList concreteTypeArguments;
					convertTemplateArguments(templateDecl->getTemplateArgs(), converter, concreteTypeArguments);
					genType = converter.getIRBuilder().genericType(insieme::utils::mangle(decl->getQualifiedNameAsString()), concreteTypeArguments);
				}

				converter.applyHeaderTagging(genType, decl);
				VLOG(2) << "Interceptor: intercepted clang type\n" << dumpClang(decl) << " -> converted to generic type: " << *genType << "\n";
				return genType;
			}
		}
		return nullptr;
	}

	bool InterceptorExtension::FuncDeclVisit(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& converter) {
		return !converter.getHeaderTagger()->isIntercepted(decl);
	}

	bool InterceptorExtension::VarDeclVisit(const clang::VarDecl* decl, insieme::frontend::conversion::Converter& converter) {
		return !converter.getHeaderTagger()->isIntercepted(decl);
	}

} // extensions
} // frontend
} // insieme

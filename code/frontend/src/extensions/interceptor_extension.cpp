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

#include "insieme/frontend/extensions/interceptor_extension.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/pointer.h"
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

	FrontendExtension::flagHandler InterceptorExtension::registerFlag(boost::program_options::options_description& options) {
		// create lambda
		auto lambda = [&](const ConversionJob& job) {
			// check if the default activated plugins have been deactivated manually
			if(job.hasOption(frontend::ConversionJob::NoDefaultExtensions)) { return false; }
			return true;
		};
		return lambda;
	}

	boost::optional<std::string> InterceptorExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// interceptor needs to be the first extension in the extension list
		if(setup.getExtensions().begin()->get() != this) {
			return boost::optional<std::string>("InterceptorExtension should be the first Extension");
		}
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
				if(auto templateParamTypeDecl = llvm::dyn_cast<clang::TemplateTypeParmDecl>(tempParam)) {
					if(templateParamTypeDecl->isParameterPack()) {
						templateGenericParams.push_back(getTypeVarForVariadicTemplateTypeParmType(builder, templateParamTypeDecl));
						// we only need arguments up to the first top-level variadic, the rest can be deduced
						break;
					} else {
						typeVar = getGenericRefType(builder, templateParamTypeDecl);
					}
				} else if(auto templateNonTypeParamDecl = llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(tempParam)) {
					typeVar = getTypeVarForTemplateTypeParmType(builder, templateNonTypeParamDecl);
				} else if(auto templateTemplateParamDecl = llvm::dyn_cast<clang::TemplateTemplateParmDecl>(tempParam)) {
					if(templateTemplateParamDecl->isParameterPack()) {
						templateGenericParams.push_back(getTypeVarForVariadicTemplateTemplateTypeParmType(builder, templateTemplateParamDecl));
						// we only need arguments up to the first top-level variadic, the rest can be deduced
						break;
					} else {
						core::TypeList paramTypeList;
						convertTemplateParameters(templateTemplateParamDecl->getTemplateParameters(), builder, paramTypeList);
						typeVar = builder.genericTypeVariable("T_" + getTemplateTypeParmName(templateTemplateParamDecl), paramTypeList);
					}
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
		}

		std::pair<core::ExpressionPtr, core::TypePtr> generateCallee(conversion::Converter& converter, const clang::Decl* decl,
			                                                         const clang::Expr* clangExpr = nullptr) {
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
					lit = converter.getDeclConverter()->convertMethodDecl(methDecl, builder.parents(), builder.fields(), true).lit;
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
				// if defaulted, fix "this" type to generic type
				auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(funDecl);
				if(methDecl && methDecl->isDefaulted()) {
					// check if method of class template specialization
					auto recordDecl = methDecl->getParent();
					if(auto specializedDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl)) {
						auto paramTypes = funType->getParameterTypeList();
						auto genericDecl = specializedDecl->getSpecializedTemplate();
						core::TypeList genericTypeParams;
						convertTemplateParameters(genericDecl->getTemplateParameters(), builder, genericTypeParams);
						auto genericGenType =
							converter.getIRBuilder().genericType(insieme::utils::mangle(specializedDecl->getQualifiedNameAsString()), genericTypeParams);
						auto prevThisType = core::analysis::getReferencedType(paramTypes[0]);
						auto thisType = genericGenType;
						lit = core::transform::replaceAllGen(converter.getNodeManager(), lit, prevThisType, thisType);
					}
				}
				// if return type is type variable, and we have a target type, instantiate
				if(clangExpr && core::analysis::isGeneric(retType)) {
					retType = converter.convertExprType(clangExpr);
					auto concreteFunType = builder.functionType(funType->getParameterTypes(), retType, funType->getKind());
					lit = builder.callExpr(builder.getLangBasic().getTypeInstantiation(), builder.getTypeLiteral(concreteFunType), lit);
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

			auto genericFunLit = litConverter(pattern);
			auto genericFunType = genericFunLit->getType().as<core::FunctionTypePtr>();
			genericFunType = builder.functionType(genericFunType->getParameterTypes(), genericFunType->getReturnType(), genericFunType->getKind(),
													builder.types(templateGenericParams));
			auto innerLit = builder.literal(genericFunLit->getValue(), genericFunType);
			converter.applyHeaderTagging(innerLit, decl);

			// cast to concrete type at call site
			auto concreteFunctionType = lit->getType().as<core::FunctionTypePtr>();

			// if we don't need to do any type instantiation
			if(templateConcreteParams.empty() && core::analysis::isReturnTypePotentiallyDeducible(genericFunType)) {
				return {innerLit, concreteFunctionType.getReturnType()};
			}

			concreteFunctionType = builder.functionType(concreteFunctionType->getParameterTypes(), concreteFunctionType->getReturnType(),
				                                        concreteFunctionType->getKind(), builder.types(templateConcreteParams));

			return {builder.callExpr(builder.getLangBasic().getTypeInstantiation(), builder.getTypeLiteral(concreteFunctionType), innerLit),
				    concreteFunctionType->getReturnType()};
		}

		core::CallExprPtr interceptMethodCall(conversion::Converter& converter, const clang::Decl* decl,
			                                  std::function<core::ExpressionPtr(const core::TypePtr&)> thisArgFactory,
			                                  clang::CallExpr::arg_const_range args, const clang::Expr* clangExpr = nullptr) {
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(decl);
				if(methDecl) {
					auto calleePair = generateCallee(converter, decl, clangExpr);
					auto convMethodLit = calleePair.first;
					auto funType = convMethodLit->getType().as<core::FunctionTypePtr>();
					auto retType = calleePair.second;
					auto thisArg = thisArgFactory(retType);
					if(!core::lang::isReference(thisArg)) thisArg = frontend::utils::convertMaterializingExpr(converter, thisArg);
					VLOG(2) << "Interceptor: intercepted clang method/constructor call\n" << dumpClang(decl) << "\n";
					auto retCall = utils::buildCxxMethodCall(converter, retType, convMethodLit, thisArg, args);
					return retCall;
				}
			}
			return nullptr;
		}

		core::CallExprPtr fixMaterializedReturnType(conversion::Converter& converter, const clang::CallExpr* clangCall, const core::CallExprPtr& call) {
			if(!call) return call;
			auto irRetType = converter.convertExprType(clangCall);
			auto newCall = converter.getIRBuilder().callExpr(irRetType, call->getFunctionExpr(), call->getArgumentDeclarations());
			core::transform::utils::migrateAnnotations(call, newCall);
			return newCall;
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
		// decl refs to intercepted functions
		if(auto dr = llvm::dyn_cast<clang::DeclRefExpr>(expr)) {
			auto decl = dr->getDecl();
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				// translate functions
				if(llvm::dyn_cast<clang::FunctionDecl>(decl)) {
					core::ExpressionPtr lit = generateCallee(converter, decl, nullptr).first;
					VLOG(2) << "Interceptor: intercepted clang fun\n" << dumpClang(decl) << " -> converted to literal: " << *lit << " of type "
						    << *lit->getType() << "\n";
					return lit;
				}
				// as well as global variables
				if(llvm::dyn_cast<clang::VarDecl>(decl)) {
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
		if(auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) {
			auto thisFactory = [&](const core::TypePtr& retType){ return core::lang::buildRefTemp(retType); };
			return interceptMethodCall(converter, construct->getConstructor(), thisFactory, construct->arguments());
		}
		if(auto newExp = llvm::dyn_cast<clang::CXXNewExpr>(expr)) {
			if(auto construct = newExp->getConstructExpr()) {
				auto thisFactory = [&](const core::TypePtr& retType){ return builder.undefinedNew(retType); };
				auto ret = interceptMethodCall(converter, construct->getConstructor(), thisFactory, construct->arguments());
				if(ret) return core::lang::buildPtrFromRef(ret);
			}
		}
		if(auto memberCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(expr)) {
			auto thisFactory = [&](const core::TypePtr& retType){ return converter.convertExpr(memberCall->getImplicitObjectArgument()); };
			return fixMaterializedReturnType(converter, memberCall,
				                             interceptMethodCall(converter, memberCall->getCalleeDecl(), thisFactory, memberCall->arguments(), memberCall));
		}
		if(auto operatorCall = llvm::dyn_cast<clang::CXXOperatorCallExpr>(expr)) {
			auto decl = operatorCall->getCalleeDecl();
			if(decl) {
				if(llvm::dyn_cast<clang::CXXMethodDecl>(decl)) {
					auto argList = operatorCall->arguments();
					auto thisFactory = [&](const core::TypePtr& retType){ return converter.convertExpr(*argList.begin()); };
					decltype(argList) remainder(argList.begin()+1, argList.end());
					return fixMaterializedReturnType(converter, operatorCall, interceptMethodCall(converter, decl, thisFactory, remainder, operatorCall));
				}
			}
		}

		return nullptr;
	}

	core::TypePtr InterceptorExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		clang::QualType type = typeIn;
		const core::IRBuilder& builder = converter.getIRBuilder();

		// handle template parameters of intercepted tagtypes
		if(auto injected = llvm::dyn_cast<clang::InjectedClassNameType>(type.getUnqualifiedType())) {
			auto spec = injected->getInjectedSpecializationType()->getUnqualifiedDesugaredType();
			if(auto tempSpecType = llvm::dyn_cast<clang::TemplateSpecializationType>(spec)) {
				auto key = tempSpecType->getCanonicalTypeUnqualified().getTypePtr();
				assert_true(::containsKey(templateSpecializationMapping, key)) << "Template injected specialization type encountered, but no mapping available";
				return templateSpecializationMapping[key];
			}
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

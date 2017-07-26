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

#include "insieme/backend/addons/std_init_lists.h"

#include <boost/algorithm/string.hpp>

#include "insieme/annotations/std_init_list.h"
#include "insieme/annotations/c/include.h"

#include "insieme/backend/operator_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/preprocessor.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/common/env_vars.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		/**
		 * A language module defining a custom ref deref operation.
		 */
		class StdInitListBackendExtension : public core::lang::Extension {

			friend class core::NodeManager;

			StdInitListBackendExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

		  public:
			// this extension is based upon the symbols defined by the reference module
			IMPORT_MODULE(core::lang::ReferenceExtension);

			LANG_EXT_LITERAL(InitListCreate, "init_list_create", "(type<'a>, 'b...) -> ref<IMP_std_colon__colon_initializer_list<'a>>")

			LANG_EXT_LITERAL(InitListAssign, "init_list_assign", "('a, 'b) -> 'b")

		};

		/**
		 * A preprocessor extension which searches for the IR equivalent of std::initializer list objects and replaces them with the IR which
		 * would have been generated had they been intercepted by the frontend.
		 */
		class ReplaceStdInitListPreprocessor : public PreProcessor {
			virtual core::NodePtr process(const Converter& Converter, const core::NodePtr& code) override {
				auto& mgr = code->getNodeManager();
				core::IRBuilder builder(mgr);
				const auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
				const auto& initListExt = mgr.getLangExtension<StdInitListBackendExtension>();

				utils::set::PointerSet<core::TypePtr> replacedTypes;

				// we replace all TygTypes with the correct GenericType replacement
				auto res = core::transform::transformBottomUp(code, [&](const core::TagTypePtr& tagType) -> core::TypePtr {
					if(annotations::isStdInitList(tagType)) {
						// here we create a replacement generic type which would have been created if the type had been intercepted
						auto paramType = annotations::getStdInitListElementType(tagType);
						auto genType = builder.genericType("IMP_std_colon__colon_initializer_list", builder.types({ annotations::getStdInitListElementType(tagType) }));
						core::transform::utils::migrateAnnotations(tagType, genType);
						annotations::c::attachInclude(genType, "initializer_list");
						core::annotations::attachName(genType, "std::initializer_list");

						replacedTypes.insert(genType);
						return genType;
					}
					return tagType;
				}, core::transform::globalReplacement);

				auto getName = [](const core::NodePtr& node) -> std::string {
					std::string name;
					if(const auto& lambdaExpr = node.isa<core::LambdaExprPtr>()) name = lambdaExpr->getReference()->getNameAsString();
					if(const auto& lambdaRef = node.isa<core::LambdaReferencePtr>()) name = lambdaRef->getNameAsString();
					if(const auto& lit = node.isa<core::LiteralPtr>()) name = lit->getStringValue();
					assert_true(name != "") << "Unable to get name for node " << *node;
					assert_true(boost::contains(name, "::"));
					auto methodName = name.substr(name.find("::") + 2);

					if(boost::starts_with(methodName, "IMP_size")) return "IMP_size";
					if(boost::starts_with(methodName, "IMP_begin")) return "IMP_begin";
					if(boost::starts_with(methodName, "IMP_end")) return "IMP_end";
					if(boost::starts_with(methodName, utils::getMangledOperatorAssignName())) return utils::getMangledOperatorAssignName();

					return name;
				};

				// we undo the changes made during the frontend cleanup to reflect copy semantics in the IR
				res = core::transform::transformBottomUpGen(res, [&](const core::CallExprPtr& call) -> core::NodePtr {
					// we undo the changes performed by the frontend cleanup to encode copy operations on initializer_lists
					if(refExt.isCallOfRefCast(call) || refExt.isCallOfRefKindCast(call)) {
						auto callArg = call->getArgument(0).isa<core::CallExprPtr>();
						if(callArg && core::analysis::isConstructorCall(callArg)) {
							auto ctorArg = callArg.as<core::CallExprPtr>()->getArgument(0);
							if(refExt.isCallOfRefTemp(ctorArg)) {
								auto callRefType = callArg->getType();
								if(core::analysis::isRefType(callRefType)) {
									auto callType = core::analysis::getReferencedType(callRefType).isa<core::GenericTypePtr>();
									if(replacedTypes.find(callType) != replacedTypes.end()) {
										// if we end up here, we have a call to ref_temp inside a ctor call inside a ref_cast.
										// in this case we return a call to ref_decl inside the ctor call
										auto ctorCallAddress = core::CallExprAddress(callArg);
										return builder.deref(
												core::transform::replaceNode(mgr, ctorCallAddress->getArgument(0), core::lang::buildRefDecl(callRefType)).as<core::ExpressionPtr>());
									}
								}
							}
						}
					}
					return call;
				}, core::transform::globalReplacement);

				// now we need to check all the calls and generate replacement literals for them
				res = core::transform::transformBottomUpGen(res, [&](const core::CallExprPtr& call) -> core::NodePtr {
					auto callee = call->getFunctionExpr();
					auto calleeType = callee->getType().as<core::FunctionTypePtr>();
					if(calleeType->isMember()) {
						auto objectType = core::analysis::getObjectType(calleeType);
						// if we are calling a member of a replaced type
						if(replacedTypes.find(objectType) != replacedTypes.end()) {
							core::ExpressionPtr replacementCallee = callee;
							if(calleeType->isConstructor()) {
								// we create a replacement literal for the default ctor
								if(call->getNumArguments() == 1) {
									replacementCallee = builder.literal("IMP_std_colon__colon_initializer_list::ctor", calleeType);

									// the other ctors get replaced with a call to a custom IR symbol, which will be replaced later on
								} else {
									auto oldArgs = call->getArgumentList();
									auto args = core::ExpressionList(oldArgs.begin() + 1, oldArgs.end());
									args.insert(args.begin(), builder.getTypeLiteral(objectType.as<core::GenericTypePtr>()->getTypeParameter(0)));
									return builder.callExpr(call->getType(), initListExt.getInitListCreate(), args);
								}
							}

							// member functions size, begin and end get their name changed, assignment operators get replaced by a custom IR symbol
							if(calleeType->isMemberFunction()) {
								auto name = getName(callee);
								if(name == utils::getMangledOperatorAssignName()) {
									replacementCallee = initListExt.getInitListAssign();
								} else {
									replacementCallee = builder.literal(name, calleeType);
								}
							}

							// dtor calls are always invalid
							if(calleeType->isDestructor()) assert_fail() << "Call to dtor of std::initializer_list encountered";

							annotations::c::attachInclude(replacementCallee, "initializer_list");
							return core::transform::replaceNode(mgr, core::CallExprAddress(call).getFunctionExpr(), replacementCallee);
						}
					}
					return call;
				}, core::transform::globalReplacement);

				return res;
			}

			virtual std::ostream& printTo(std::ostream& out) const override { return out << "ReplaceStdInitListPreprocessor"; }
		};

		OperatorConverterTable getStdInitListOperatorTable(core::NodeManager& mgr) {
			OperatorConverterTable res;
			const auto& initListExt = mgr.getLangExtension<StdInitListBackendExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[initListExt.getInitListCreate()] = OP_CONVERTER {
				std::vector<c_ast::NodePtr> args;
				for(unsigned i = 1; i < call->getNumArguments(); ++i) {
					args.push_back(CONVERT_ARG(i));
				}
				return c_ast::init(C_NODE_MANAGER.get(), args);
			};

			res[initListExt.getInitListAssign()] = OP_CONVERTER {
				return c_ast::assign(c_ast::deref(CONVERT_ARG(0)), CONVERT_ARG(1));
			};

			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}

	} // end anonymous namespace

	void StdInitListAddon::installOn(Converter& converter) const {
		// we register the preprocessor and operator converter by default, unless the user wanted us to disable it
		if(!getenv(INSIEME_BE_INIT_LIST_TESTING)) {
			converter.setPreProcessor(makePreProcessorSequence(converter.getPreProcessor(), makePreProcessor<ReplaceStdInitListPreprocessor>()));
			converter.getFunctionManager().getOperatorConverterTable().insertAll(getStdInitListOperatorTable(converter.getNodeManager()));
		}
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

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

			LANG_EXT_LITERAL(InitListAssign, "init_list_assign", "(ref<'a,f,'b,'c>, 'a) -> ref<'a,f,'b,'c>")
		};

		/**
		 * A preprocessor extension which searches for the IR equivalent of std::initializer list objects and replaces them with the IR which
		 * would have been generated had they been intercepted by the frontend.
		 */
		class ReplaceStdInitListPreprocessor : public PreProcessor {
			virtual core::NodePtr process(const Converter& Converter, const core::NodePtr& code) override {
				auto& mgr = code->getNodeManager();
				core::IRBuilder builder(mgr);
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
								replacementCallee = builder.literal("IMP_std_colon__colon_initializer_list::ctor", calleeType);

								// member functions size, begin and end get their name changed, assignment operators get replaced by a custom IR symbol
							} else if(calleeType->isMemberFunction()) {
								auto name = getName(callee);
								if(name == utils::getMangledOperatorAssignName()) {
									replacementCallee = initListExt.getInitListAssign();
									return builder.callExpr(initListExt.getInitListAssign(), call->getArgument(0), builder.deref(call->getArgument(1)));
								} else {
									replacementCallee = builder.literal(name, calleeType);
								}

								// dtor calls are always invalid
							} else if(calleeType->isDestructor()) assert_fail() << "Call to dtor of std::initializer_list encountered";

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

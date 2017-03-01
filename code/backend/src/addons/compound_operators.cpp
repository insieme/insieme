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
 *
 */
#include "insieme/backend/addons/compound_operators.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/preprocessor.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/compound_operators.h"

#include "insieme/core/transform/node_replacer.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		/**
		 * A language module defining a custom ref deref operation.
		 */
		class CompOpsBackendExtension : public core::lang::Extension {

			/**
			 * Allow the node manager to create instances of this class.
			 */
			friend class core::NodeManager;

			/**
			 * Creates a new instance based on the given node manager.
			 */
			CompOpsBackendExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

		public:

			/**
			 * Wrap ref deref operations of compound ops in a custom ref deref, in order
			 * to circumvent inappropriate dereferencing
			 */
			LANG_EXT_LITERAL(CompoundRefDeref, "compound_ref_deref", "(ref<'a,'c,'v,'k>) -> 'a")

		};

		/**
		 * A preprocessor extension which searches for expressions containing ref_derefs of compound assignments.
		 * If so, it re-wraps this compOp in a 'compound_ref_deref', using the backend extension defined above.
		 */
		class RemoveDerefForCompOps : public PreProcessor {
			virtual core::NodePtr process(const Converter& Converter, const core::NodePtr& code) override {
				auto& nm = code->getNodeManager();
				core::IRBuilder builder(nm);

				const auto& refExt = nm.getLangExtension<core::lang::ReferenceExtension>();
				const auto& cOBExt = nm.getLangExtension<CompOpsBackendExtension>();

				return core::transform::transformBottomUp(code, [&](const core::CallExprPtr& call) {
					if (call->getFunctionExpr() == refExt.getRefDeref()) {
						const auto& argument = core::analysis::getArgument(call, 0);
						if(core::lang::isCompoundAssignmentOperation(argument)) {
							// If 'argument' is a compOp, modify the IR:
							// ref_deref(argument)  --->  compound_ref_deref(argument)
							return builder.callExpr(call->getType(),
							                        cOBExt.getCompoundRefDeref(), argument);
						}
					}
					return call;
				}, core::transform::globalReplacement);
			}

			virtual std::ostream& printTo(std::ostream& out) const override { return out << "RemoveDerefForCompOps"; }
		};

		/**
		 * Operator converter to transform the IR into an appropriate AST for C code
		 */
		OperatorConverterTable getCompoundOpsOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::CompoundOpsExtension>();
			const auto& cOBExt = manager.getLangExtension<CompOpsBackendExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			// Part A: Ignore the 'compound_ref_deref' which we added above, since this is not needed in C(++), only in IR
			res[cOBExt.getCompoundRefDeref()] = OP_CONVERTER { return CONVERT_ARG(0); };

			// Part B: A macro for all the compOps to generate an appropriate AST
			#define COMP_OP_CONVERSION_RULE(EXT_NAME, C_AST_NAME)                                               \
				res[ext.get##EXT_NAME()] = OP_CONVERTER {                                                       \
					return c_ast::binaryOp(c_ast::BinaryOperation::C_AST_NAME,                                  \
					                       c_ast::derefIfNotImplicit(CONVERT_ARG(0), ARG(0)), CONVERT_ARG(1));  \
				};

			COMP_OP_CONVERSION_RULE(CompAssignAdd,        AdditionAssign)
			COMP_OP_CONVERSION_RULE(CompAssignSubtract,   SubtractionAssign)
			COMP_OP_CONVERSION_RULE(CompAssignMultiply,   MultiplicationAssign)
			COMP_OP_CONVERSION_RULE(CompAssignDivide,     DivisionAssign)
			COMP_OP_CONVERSION_RULE(CompAssignModulo,     ModuloAssign)
			COMP_OP_CONVERSION_RULE(CompAssignBitwiseAnd, BitwiseAndAssign)
			COMP_OP_CONVERSION_RULE(CompAssignBitwiseOr,  BitwiseOrAssign)
			COMP_OP_CONVERSION_RULE(CompAssignBitwiseXor, BitwiseXOrAssign)
			COMP_OP_CONVERSION_RULE(CompAssignLeftShift,  BitwiseLeftShiftAssign)
			COMP_OP_CONVERSION_RULE(CompAssignRightShift, BitwiseRightShiftAssign)

			#undef COMP_OP_CONVERSION_RULE

			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}

	} // end anonymous namespace

	void CompoundOps::installOn(Converter& converter) const
	{
		converter.setPreProcessor(makePreProcessorSequence(converter.getPreProcessor(), makePreProcessor<RemoveDerefForCompOps>()));

		converter.getFunctionManager().getOperatorConverterTable().insertAll(getCompoundOpsOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

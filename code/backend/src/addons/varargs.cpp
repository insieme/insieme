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
#include "insieme/backend/addons/varargs.h"
#include "insieme/core/annotations/naming.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/lang/varargs_extension.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* handleVarArgsType(ConversionContext& context, const core::TypePtr& type) {
			auto& ext = type->getNodeManager().getLangExtension<core::lang::VarArgsExtension>();

			// check whether this is the type of interest
			if (!ext.isVarList(type)) return nullptr;

			// build up TypeInfo for complex type
			c_ast::CNodeManager& manager = *context.getConverter().getCNodeManager();

			// create the resulting type representation
			return type_info_utils::createInfo(manager.create<c_ast::VarArgsType>());
		}

		const TypeInfo* handleVaListType(ConversionContext& context, const core::TypePtr& type) {
			auto& ext = type->getNodeManager().getLangExtension<core::lang::VarArgsExtension>();

			// check whether this is the type of interest
			if (!ext.isVarList(type)) return nullptr;

			// build up TypeInfo for complex type
			c_ast::CNodeManager& manager = *context.getConverter().getCNodeManager();

			// create the resulting type representation
			auto res = type_info_utils::createInfo(manager, "va_list");
			c_ast::CodeFragmentPtr decl = c_ast::IncludeFragment::createNew(context.getConverter().getFragmentManager(), "stdarg.h");
			res->declaration = decl;
			res->definition = decl;

			return res;
		}

		OperatorConverterTable getVarArgsOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<insieme::core::lang::VarArgsExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getVaarg()] = OP_CONVERTER {

				// const FunctionInfo& info = getInfo(static_pointer_cast<const core::Literal>(fun));
				c_ast::CallPtr res = c_ast::call(C_NODE_MANAGER->create("va_arg"));

				res->arguments.push_back(CONVERT_ARG(0));
				res->arguments.push_back(CONVERT_TYPE(core::analysis::getRepresentedType(ARG(1))));

				return res;
			};

			#include "insieme/backend/operator_converter_end.inc"
			return res;
		}
	}

	void VarArgs::installOn(Converter& converter) const {
		// register type handler
		converter.getTypeManager().addTypeHandler(handleVarArgsType);
		converter.getTypeManager().addTypeHandler(handleVaListType);
		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getVarArgsOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

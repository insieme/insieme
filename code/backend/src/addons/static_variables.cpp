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

#include "insieme/backend/addons/static_variables.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/static_vars.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* StaticVariableTypeHandler(const Converter& converter, const core::TypePtr& type) {
			static const TypeInfo* NOT_HANDLED = NULL;

			core::NodeManager& manager = type.getNodeManager();
			const core::lang::StaticVariableExtension& ext = manager.getLangExtension<core::lang::StaticVariableExtension>();

			// check whether it is a cpp reference
			if (!ext.isStaticType(type)) {
				return NOT_HANDLED;	// not handled by this handler
			}

			// unwrap static type wrapper
			TypeManager& typeManager = converter.getTypeManager();
			return &typeManager.getTypeInfo(ext.unwrapStaticType(type));

		}


//		c_ast::NodePtr convertStatement(ConversionContext& context, const core::NodePtr& node) {
//			static const c_ast::NodePtr NOT_HANDLED;
//
//			if (node.isa<core::LiteralPtr>())
//			return NOT_HANDLED;
//
//		}



		OperatorConverterTable getStaticVariableOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;

//			const auto& ext = manager.getLangExtension<core::lang::StaticVariableExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

//			res[ext.getRefCppToIR()] 	  = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });
//			res[ext.getRefConstCppToIR()] = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });
//
//			res[ext.getRefIRToCpp()] 	  = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
//			res[ext.getRefIRToConstCpp()] = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
//
//			res[ext.getRefCppToConstCpp()]= OP_CONVERTER({ return CONVERT_ARG(0); });
//
//			// FIXME: find the right place for this
//			res[ext.getMaterialize()]	  = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0));});


			#include "insieme/backend/operator_converter_end.inc"
			return res;
		}

	}

	void StaticVariables::installOn(Converter& converter) const {

		// registers type handler
		converter.getTypeManager().addTypeHandler(StaticVariableTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getStaticVariableOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

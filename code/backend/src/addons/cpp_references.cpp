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

#include "insieme/backend/addons/cpp_references.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/ir++_extension.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* CppRefTypeHandler(const Converter& converter, const core::TypePtr& type) {
			static const TypeInfo* NOT_HANDLED = NULL;

			// check whether it is a cpp reference
			if (!(core::analysis::isCppRef(type) || core::analysis::isConstCppRef(type))) {
				return NOT_HANDLED;	// not handled by this handler
			}

			// build up TypeInfo for C++ reference
			TypeManager& typeManager = converter.getTypeManager();

			// determine const flag
			bool isConst = core::analysis::isConstCppRef(type);

			// get information regarding base type
			const TypeInfo& baseInfo = typeManager.getTypeInfo(core::analysis::getCppRefElementType(type));

			// copy base information
			TypeInfo* refInfo = new TypeInfo(baseInfo);

			// alter r / l / external C type
			refInfo->lValueType = c_ast::ref(refInfo->lValueType, isConst);

			refInfo->rValueType = c_ast::ref(refInfo->rValueType, isConst);

			refInfo->externalType = c_ast::ref(refInfo->externalType, isConst);

			return refInfo;
		}


		OperatorConverterTable getCppRefOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;

			const auto& ext = manager.getLangExtension<core::lang::IRppExtensions>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getRefCppToIR()] 	  = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });
			res[ext.getRefConstCppToIR()] = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });

			res[ext.getRefIRToCpp()] 	  = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
			res[ext.getRefIRToConstCpp()] = OP_CONVERTER({
					core::ExpressionPtr arg = ARG(0);
					// if inner node is a materialize we should not deref
					if (core::analysis::isCallOf(arg, LANG_EXT_CPP.getMaterialize())){
						return CONVERT_ARG(0);
					}
					//if inner node is a constructor, we dont deref neither
					if (core::analysis::isConstructorCall(arg)){
						return CONVERT_ARG(0);
					}

					// is if anything else, we must deref
					return c_ast::deref(CONVERT_ARG(0)); 
			});

			res[ext.getRefCppToConstCpp()]= OP_CONVERTER({ return CONVERT_ARG(0); });

			// FIXME: find the right place for this
			res[ext.getMaterialize()]	  = OP_CONVERTER({ return CONVERT_ARG(0);});


			#include "insieme/backend/operator_converter_end.inc"
			return res;
		}

	}

	void CppReferences::installOn(Converter& converter) const {

		// registers type handler
		converter.getTypeManager().addTypeHandler(CppRefTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getCppRefOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

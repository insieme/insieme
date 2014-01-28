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
#include "insieme/backend/name_manager.h"

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

			TypeManager& typeManager = converter.getTypeManager();
			auto manager = converter.getCNodeManager();

			// build up TypeInfo for C++ reference

			// determine const flag
			bool isConst = core::analysis::isConstCppRef(type);

			// get information regarding base type
			core::TypePtr innType = core::analysis::getCppRefElementType(type);
			const TypeInfo& baseInfo = typeManager.getTypeInfo(innType);

			// if the inner type is somehow complex, we extract it in a typedef
			if((innType.isa<core::RefTypePtr>()) && (innType.as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_ArrayType)) {
				c_ast::IdentifierPtr typeName = manager->create(converter.getNameManager().getName(type));

				// construct a typedef for the inner type, this saves us from pains
				TypeInfo* res = new TypeInfo(baseInfo);
				c_ast::TypeDefinitionPtr def = manager->create<c_ast::TypeDefinition>(baseInfo.rValueType, typeName);
				
				// construct decl and def
				res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def);
				res->definition = res->declaration;

				// dependencies
				res->declaration->addDependency (baseInfo.declaration);

				// R / L value names
				res->rValueType = c_ast::ref(manager->create<c_ast::NamedType>(typeName), isConst);
				res->lValueType = 	res->rValueType;

				// external type handling
				res->externalType = res->rValueType;
				res->externalize = &type_info_utils::NoOp;
				res->internalize = &type_info_utils::NoOp;
				return res;

			// otherwise we proceed as ususal
			}
			else{
				// copy base information
				TypeInfo* refInfo = new TypeInfo(baseInfo);
				
				// alter r / l / external C type
				refInfo->lValueType = c_ast::ref(refInfo->lValueType, isConst);
				refInfo->rValueType = c_ast::ref(refInfo->rValueType, isConst);
				refInfo->externalType = c_ast::ref(refInfo->externalType, isConst);

				return refInfo;
			}
		}


		OperatorConverterTable getCppRefOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::IRppExtensions>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getRefCppToIR()] 	  = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });
			res[ext.getRefConstCppToIR()] = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0)); });
			res[ext.getRefIRToCpp()] 	  = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
			res[ext.getRefIRToConstCpp()] = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
			res[ext.getRefCppToConstCpp()]= OP_CONVERTER({ return CONVERT_ARG(0); });

			// FIXME: find the right place for this
			res[ext.getMaterialize()]	  = OP_CONVERTER({ return c_ast::ref(CONVERT_ARG(0));});


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

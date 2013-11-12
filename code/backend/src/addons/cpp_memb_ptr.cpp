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

#include "insieme/backend/addons/cpp_memb_ptr.h"


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

		const TypeInfo* CppMembPtrHandler(const Converter& converter, const core::TypePtr& type) {
			static const TypeInfo* NOT_HANDLED = NULL;

			// check whether it is a cpp reference
			if (!(core::analysis::isMemberPointer(type))) {
				return NOT_HANDLED;	// not handled by this handler
			}

			// build up TypeInfo for C++ member pointer
			TypeManager& typeManager = converter.getTypeManager();
			auto manager = converter.getCNodeManager();
			
			core::StructTypePtr structType = type.isa<core::StructTypePtr>();

			// get information regarding base type
			//const TypeInfo& baseInfo = typeManager.getTypeInfo();
			std::cout << "name of 0 " << structType[0]->getName().getValue() << std::endl;
			std::cout << "type of 0 " << structType[0]->getType() << std::endl;

			std::cout << "name of 2 " << structType[2]->getName().getValue() << std::endl;
			std::cout << "type of 2 " << structType[2]->getType() << std::endl;

		 	TypeInfo baseInfo  = typeManager.getTypeInfo(core::analysis::getRepresentedType(structType[0]->getType()));
		 	TypeInfo fieldType = typeManager.getTypeInfo(core::analysis::getRepresentedType(structType[2]->getType()));

			c_ast::IdentifierPtr typeName = manager->create(converter.getNameManager().getName(type));
			c_ast::MemberFieldPointerPtr memberPtrType = manager->create<c_ast::MemberFieldPointer>(baseInfo.rValueType, fieldType.rValueType );
			c_ast::TypeDefinitionPtr def = manager->create<c_ast::TypeDefinition>(memberPtrType, typeName);

			TypeInfo* res = new TypeInfo();
			
			// construct decl and def
			res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def);
			res->definition = res->declaration;

			// dependencies
			res->declaration->addDependency (baseInfo.declaration);
			res->declaration->addDependency (fieldType.declaration);

			// R / L value names
			res->rValueType = manager->create<c_ast::NamedType>(typeName);
			res->lValueType = res->rValueType;

			// external type handling
			res->externalType = res->rValueType;
			res->externalize = &type_info_utils::NoOp;
			res->internalize = &type_info_utils::NoOp;

			// done
			return res;
		}


		OperatorConverterTable getLocalOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::IRppExtensions>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getMemberPointerCtor()] 	  = OP_CONVERTER({ 
					c_ast::NodePtr objTy = CONVERT_TYPE(core::analysis::getRepresentedType(ARG(0)->getType()));
					c_ast::NodePtr lit  =  C_NODE_MANAGER->create(ARG(1).as<core::LiteralPtr>()->getStringValue());
					return c_ast::ref ( c_ast::scope( objTy, lit ));
				});

			res[ext.getMemberPointerAccess()] 	  = OP_CONVERTER({ 
					return pointerToMember(CONVERT_ARG(0), CONVERT_ARG(1)); 
				});

			#include "insieme/backend/operator_converter_end.inc"
			return res;
		}

	}

	void CppMembAddon::installOn(Converter& converter) const {

		// registers type handler
		converter.getTypeManager().addTypeHandler(CppMembPtrHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getLocalOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

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

#include "insieme/backend/addons/longlong_type.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/ir++_extension.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace backend {
namespace addons {

	namespace {
	    
		const TypeInfo* LongLongTypeHandler(const Converter& converter, const core::TypePtr& type) {
            // Checking if type is a long long
			const core::StructTypePtr& structType = type.isa<core::StructTypePtr>();
			if (!structType) 
                return NULL;
			if(!(structType->getEntries().size() == 1))
                return NULL;
			const core::NamedTypePtr t0 = structType->getEntries()[0];
			if(t0->getName()->getValue().compare("longlong_val"))
                return NULL;

            // Converting the long long
			c_ast::CNodeManager& manager = *converter.getCNodeManager();

            if(type->getNodeManager().getLangBasic().isInt8(t0->getType()))
	            return type_info_utils::createInfo(manager, "long long");
            else
	            return type_info_utils::createInfo(manager, "unsigned long long");
	    }

        OperatorConverterTable getLongLongTypeOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::IRppExtensions>();

			#include "insieme/backend/operator_converter_begin.inc"

            // TO
		    res[ext.getLongToLongLong()] = OP_CONVERTER({ return CONVERT_ARG(0); });
		    res[ext.getULongToULongLong()] = OP_CONVERTER({ return CONVERT_ARG(0); });

		    // From
		    res[ext.getLongLongToLong()] = OP_CONVERTER({  return CONVERT_ARG(0); });
		    res[ext.getULongLongToULong()] = OP_CONVERTER({ return CONVERT_ARG(0); });

		    // between
		    res[ext.getLongLongToULongLong()] = OP_CONVERTER({ return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); });
		    res[ext.getULongLongToLongLong()] = OP_CONVERTER({ return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); });

			#include "insieme/backend/operator_converter_end.inc"

            return res;
        }

    }

	void LongLongType::installOn(Converter& converter) const {

		// registers type handler
		converter.getTypeManager().addTypeHandler(LongLongTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getLongLongTypeOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

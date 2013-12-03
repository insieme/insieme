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

		OperatorConverterTable getVarArgsOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<insieme::core::lang::VarArgsExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getVaarg()] 	  = OP_CONVERTER({ 

			    //const FunctionInfo& info = getInfo(static_pointer_cast<const core::Literal>(fun));
                c_ast::CallPtr res = c_ast::call(C_NODE_MANAGER->create("va_arg"));

                res->arguments.push_back(CONVERT_ARG(0));
                res->arguments.push_back(CONVERT_TYPE(core::analysis::getRepresentedType(ARG(1))));

                return res; 
            });

		    #include "insieme/backend/operator_converter_end.inc"
			return res;
		}

	}

	void VarArgs::installOn(Converter& converter) const {

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getVarArgsOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

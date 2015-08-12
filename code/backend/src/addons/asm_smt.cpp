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

#include "insieme/backend/addons/asm_stmt.h"

#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"


#include "insieme/core/lang/asm_extension.h"

namespace insieme {
namespace backend {
namespace addons {

namespace {

OperatorConverterTable getOperatorTable(core::NodeManager& manager) {
	OperatorConverterTable res;
	const auto& ext = manager.getLangExtension<core::lang::AsmStmtExtension>();
	
#include "insieme/backend/operator_converter_begin.inc"
	
	res[ext.getAsmStmt()] 	  = OP_CONVERTER({
	
		std::string value("asm");
		
		///  syntax:     asm [volatile] ( ASMSTRING [ : [ OUTPUTS ] : [INPUTS] [ : CLOBBERS ] ] )
		//
		//	ASMSTRING: just a string with the asm to execute
		//	OUTPUTS:    STRING(EXPR) [ , STRING(EXPR) ]*
		//	INPUTS :    STRING(EXPR) [ , STRING(EXPR) ]*
		//	CLOBBERS:   STRING [ , STRING ]*
		
		insieme::core::lang::AsmStmtWrapper wrapper = insieme::core::lang::fromIr(ARG(0));
		
		if(wrapper._volatile) {
			value.append(" volatile");
		}
		
		value.append("(\"");
		value.append(wrapper.asmString);
		value.append("\"");
		
		// if simple (only asm string) asm("text");
		if(wrapper.inputs.empty() && wrapper.outputs.empty() && wrapper.clobbers.empty()) {
			value.append(")");
			return C_NODE_MANAGER->create<c_ast::OpaqueExpr>(value);
		}
		
		
		value.append(":");
		std::stringstream ss;
		if(wrapper.outputs.empty()) {
			ss << " /* no output */ ";
		}
		else{
			ss << join(",", wrapper.outputs, [&](std::ostream& out, const std::pair<std::string, core::ExpressionPtr>& item) {
				out << "\"" << item.first << "\"(" << c_ast::toC(CONVERT_EXPR(item.second)) << ")";
			});
		}
		
		
		ss << ":";
		if(wrapper.inputs.empty()) {
			ss << " /* no inputs */ ";
		}
		else{
			ss << join(",", wrapper.inputs, [&](std::ostream& out, const std::pair<std::string, core::ExpressionPtr>& item) {
				out << "\"" << item.first << "\"(" << c_ast::toC(CONVERT_EXPR(item.second)) << ")";
			});
		}
		
		if(!wrapper.clobbers.empty()) {
			ss << ":";
			ss << join(",", wrapper.clobbers, [&](std::ostream& out, const std::string& item) {
				out << "\"" << item << "\"";
			});
		}
		
		value.append(ss.str());
		value.append(")");
		return C_NODE_MANAGER->create<c_ast::OpaqueExpr>(value);
	});
	
#include "insieme/backend/operator_converter_end.inc"

	return res;
}

}

void AsmStmt::installOn(Converter& converter) const {

	// register additional operators
	converter.getFunctionManager().getOperatorConverterTable().insertAll(getOperatorTable(converter.getNodeManager()));
	
}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

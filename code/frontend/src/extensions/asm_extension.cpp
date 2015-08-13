/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/clang.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/extensions/asm_extension.h"

#include "insieme/core/lang/asm_extension.h"

using namespace insieme::frontend;

namespace insieme {
namespace frontend {
namespace extensions {

	//
	//	To IR:
	//	the idea is to create a literal function call which reproduces the generic variadic asm function call
	//
	//		the asm stmt has the following form:
	//		asm (
	//			TEMPLATE
	//			: [ OUTPUTS ]
	//			: [ INPUTS ]
	//			: [ CLOBBERS ]
	//			)
	//
	//		where:
	//
	//		TEMPLATE:  is a string containing the assembler templated instructions list
	//		OUTPUTS, INPUTS: are a comma separated list of pairs string - variable:    "some text"(VAR) ,  ...
	//		CLOBBLER: are a list of registers which will be modified by the asm execution (comma separated list of strings)
	//


	stmtutils::StmtWrapper ASMExtension::Visit(const clang::Stmt* stmt, frontend::conversion::Converter& convFact) {
		stmtutils::StmtWrapper ret;

		if(const clang::AsmStmt* asmStmt = llvm::dyn_cast<clang::AsmStmt>(stmt)) {
			core::IRBuilder builder = convFact.getIRBuilder();

			std::string assemblerString = asmStmt->generateAsmString(convFact.getCompiler().getASTContext());

			auto expand = [&](char lookup, const char* replacement) {
				int last = 0;
				unsigned it;
				string rep = replacement;
				while((it = assemblerString.find(lookup, last)) < assemblerString.length()) {
					last = it + rep.length();
					assemblerString.replace(it, 1, rep);
				}
			};

			expand('\\', "\\\\");
			expand('\n', "\\n");
			expand('\t', "\\t");
			expand('\b', "\\b");
			expand('\a', "\\a");
			expand('\v', "\\v");
			expand('\r', "\\r");
			expand('\f', "\\f");
			expand('\?', "\\\?");
			expand('\'', "\\\'");
			expand('\"', "\\\"");
			expand('\0', "\\0");


			insieme::core::lang::AsmStmtWrapper wrap(assemblerString, asmStmt->isVolatile());

			for(unsigned i = 0; i < asmStmt->getNumOutputs(); ++i) {
				wrap.addOutput(asmStmt->getOutputConstraint(i).str(), builder.deref(convFact.convertExpr(asmStmt->begin_outputs()[i])));
			}

			for(unsigned i = 0; i < asmStmt->getNumInputs(); ++i) {
				wrap.addInput(asmStmt->getInputConstraint(i).str(), convFact.convertExpr(asmStmt->begin_inputs()[i]));
			}

			for(unsigned i = 0; i < asmStmt->getNumClobbers(); ++i) {
				wrap.addClobber(asmStmt->getClobber(i).str());
			}

			ret.push_back(insieme::core::lang::toIR(convFact.getNodeManager(), wrap));
		}

		return ret;
	}

} // namespace extension
} // namespace frontend
} // namespace insieme

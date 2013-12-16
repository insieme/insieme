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

#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/utils/logging.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
template<typename Enum>
std::set<Enum> getFlags(const ExpressionPtr& flagExpr) {

	const lang::BasicGenerator& gen = flagExpr->getNodeManagerPtr()->getLangBasic();
	std::set<Enum> flags;
	// remove cast to uint<8>
	if (flagExpr.isa<CallExprPtr>() && gen.isScalarCast(flagExpr.as<CallExprPtr>()->getFunctionExpr())){
		recursiveFlagCheck(flagExpr.as<CallExprPtr>()->getArgument(0), flags);
	}else
	if (const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(flagExpr)) {
		recursiveFlagCheck(cast->getSubExpression(), flags);
	} else
		LOG(ERROR) << "No flags found in " << flagExpr << "\nUsing default settings";

	return flags;
}
}

const NodePtr BufferMapper::resolveElement(const NodePtr& ptr) {
	return ptr;
}

BufferReplacer::BufferReplacer(ProgramPtr& prog) : prog(prog) {
	NodePtr root = prog->getChild(0);
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	TreePatternPtr clCreateBuffer = irp::callExpr(pattern::any, irp::literal("clCreateBuffer"),
			pattern::any << var("flags", pattern::any) << var("size", pattern::any) << var("host_ptr", pattern::any) << pattern::any);
	TreePatternPtr bufferDecl = irp::declarationStmt(var("buffer", pattern::any), clCreateBuffer);
	TreePatternPtr bufferAssign = irp::callExpr(irp::atom(mgr.getLangBasic().getUnit()), irp::atom(mgr.getLangBasic().getRefAssign()),
			var("buffer", pattern::any) << clCreateBuffer);
	TreePatternPtr buffer = bufferDecl | bufferAssign;
	visitDepthFirst(prog, [&](const NodePtr& node) {
		MatchOpt createBuffer = buffer->matchPointer(node);

		if(createBuffer)
			std::cout << "\nyipieaiey: " << createBuffer->getVarBinding("host_ptr").getValue() << std::endl << std::endl;
/*		if(const LiteralPtr literal = dynamic_pointer_cast<const Literal>(fun)) {
			if(literal->getStringValue().compare("clCreateBuffer") == 0) {

			}
		}*/
		return;
	});

}

} //namespace ocl
} //namespace frontend
} //namespace insieme


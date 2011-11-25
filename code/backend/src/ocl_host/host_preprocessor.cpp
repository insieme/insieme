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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"


#include "insieme/core/ir_check.h"
#include "insieme/utils/logging.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/ocl_host/host_extensions.h"
#include "insieme/backend/ocl_host/host_preprocessor.h"

#include "insieme/transform/pattern/irconvert.h"
#include "insieme/transform/pattern/irpattern.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"

using namespace insieme::transform::pattern;
using namespace insieme::core;

namespace insieme {
namespace backend {
namespace ocl_host {

using insieme::transform::pattern::any;
using insieme::transform::pattern::anyList;

	/*class BufferReplacer : public core::transform::CachedNodeMapping {

		core::NodeManager& manager;
		const Extensions& extensions;

	public:

		BufferReplacer(core::NodeManager& manager) :
			manager(manager),  extensions(manager.getLangExtension<Extensions>()) {}

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {
		//LOG(INFO) << "Before Host preprocessing: " << core::printer::PrettyPrinter(ptr);
			//core::IRBuilder builder(manager);
			//auto& basic = manager.getLangBasic();
			//auto& hostExt = manager.getLangExtension<ocl_host::Extensions>();


			// perform conversion in post-order
			core::NodePtr res = ptr->substitute(manager, *this);

			// only interested in lambda expressions
			if (res->getNodeType() != core::NT_LambdaExpr) {
				return res;
			}

			//LOG(INFO) << "CODE: " << core::printer::PrettyPrinter(res);

			return res;
		}
	};*/

	core::NodePtr HostPreprocessor::process(core::NodeManager& manager, const core::NodePtr& code) {
		// the converter does the magic
		//BufferReplacer replacer(manager);
		//return replacer.map(code);

		core::IRBuilder builder(manager);

		TreePatternPtr wrapGlobal = irp::callExpr(any, irp::literal("_ocl_wrap_global", any),
									irp::callExpr(any, irp::literal("tuple.member.access", any),
									irp::callExpr(irp::literal("ref.deref", any), var("var") << *any)
									<< var("num") << *any) << *any);
		visitDepthFirst(code, [&](const CallExprPtr& call) {
			auto&& match = wrapGlobal->match(toTree(call));
			if (match) {
				VariablePtr var = static_pointer_cast<const Variable>( match->getVarBinding("var").getTree()->getAttachedValue<NodePtr>() );
				std::cout << var << std::endl;
				ExpressionPtr num = static_pointer_cast<const Expression>( match->getVarBinding("num").getTree()->getAttachedValue<NodePtr>() );
				std::cout << num << std::endl;
			}
		});

		std::cout << core::printer::PrettyPrinter(code);
		return code;
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme

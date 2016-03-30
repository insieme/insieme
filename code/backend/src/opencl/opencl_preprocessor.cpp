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

#include "insieme/annotations/meta_info/meta_infos.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/backend/opencl/opencl_backend.h"
#include "insieme/backend/opencl/opencl_preprocessor.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/backend/opencl/opencl_analysis.h"
#include "insieme/backend/opencl/opencl_transform.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {
namespace opencl {

	using namespace insieme::annotations::opencl;
	using namespace insieme::annotations::opencl_ns;

	OffloadSupportPre::OffloadSupportPre() :
		PreProcessor()
	{ }

	core::NodePtr OffloadSupportPre::process(const Converter& converter, const core::NodePtr& code) {
		// node manager used by this extension
		core::NodeManager& manager = converter.getNodeManager();
		core::IRBuilder builder(manager);
		// this map will be filled by the visitor
		core::NodeMap replacements;
		// traverse through the tree and find nodes which are valid for offloading
		for_each(analysis::getOffloadAbleStmts(code), [&](const NodePtr& node) {
			auto requirements = analysis::getVariableRequirements(manager, code, node.as<core::StatementPtr>());
			// we outline the compound such that we can implement our pick between default & opencl kernel
			auto callExpr = transform::outline(manager, node.as<core::StatementPtr>(), requirements);
			auto fallback = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
			// put together the variants used by the pick -- first one is the fallback which will run on cpu
			core::ExpressionList variants;
			variants.push_back(fallback);

			auto generated = transform::toOcl(converter, manager, code, callExpr, requirements);
			std::copy(generated.begin(), generated.end(), std::back_inserter(variants));

			// build a pick for the generated variants
			auto pickExpr = builder.pickVariant(variants);
			// ...and call them -- runtime will decide which one
			replacements.insert(std::make_pair(node, builder.callExpr(manager.getLangBasic().getUnit(), pickExpr, callExpr->getArguments())));
		});
		// fast-path
		if(replacements.empty()) return code;
		// slow-path
		return core::transform::replaceAll(manager, code, replacements, core::transform::globalReplacement);
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme

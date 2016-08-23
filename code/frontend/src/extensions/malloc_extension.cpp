/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/extensions/malloc_extension.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace frontend {
namespace extensions {

	using namespace core;


	boost::optional<std::string> MallocExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		if(!setup.hasExtension<InterceptorExtension>()) return std::string("MallocExtension requires the InterceptorExtension to be loaded");
		return {};
	}

	core::tu::IRTranslationUnit MallocExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		auto& nodeMan = tu.getNodeManager();
		auto& feExt = nodeMan.getLangExtension<utils::FrontendInspireModule>();
		auto irTu = core::tu::toIR(nodeMan, tu);
		irTu = core::transform::transformBottomUpGen(irTu, [&feExt](const core::LiteralPtr& mallocLit) -> core::ExpressionPtr {
			if(insieme::utils::demangle(mallocLit->getStringValue()) == "malloc") {
				return feExt.getMallocWrapper();
			}
			return mallocLit;
		}, core::transform::globalReplacement);
		return core::tu::fromIR(irTu);
	}

} // extensions
} // frontend
} // insieme

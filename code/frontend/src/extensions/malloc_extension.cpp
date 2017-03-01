/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/frontend/extensions/malloc_extension.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/memory.h"
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
		auto& memExt = nodeMan.getLangExtension<core::lang::MemoryExtension>();
		auto irTu = core::tu::toIR(nodeMan, tu);
		irTu = core::transform::transformBottomUpGen(irTu, [&memExt](const core::LiteralPtr& lit) -> core::ExpressionPtr {
			if(insieme::utils::demangle(lit->getStringValue()) == "malloc") {
				return memExt.getMallocWrapper();
			}
			if(insieme::utils::demangle(lit->getStringValue()) == "free") {
				return memExt.getFreeWrapper();
			}
			return lit;
		}, core::transform::globalReplacement);
		return core::tu::fromIR(irTu);
	}

} // extensions
} // frontend
} // insieme

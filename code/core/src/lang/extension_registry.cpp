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
 */

#include "insieme/core/lang/extension_registry.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/asm_extension.h"
#include "insieme/core/lang/complex.h"
#include "insieme/core/lang/compound_operators.h"
#include "insieme/core/lang/datapath.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/lang/io.h"
#include "insieme/core/lang/memory.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/lang/time.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/utils/assert.h"


namespace insieme {
namespace core {
namespace lang {

	namespace {
		// returns a new std::pair which can be inserted into the map of extension factories
		template <typename T>
		std::pair<const std::string, std::function<const Extension&(NodeManager&)>> getExtensionFactory(const std::string& extensionName) {
			return std::make_pair(extensionName, [](NodeManager& manager) -> const Extension& { return manager.getLangExtension<T>(); });
		}
	}

	ExtensionRegistry::ExtensionRegistry() {
		// fill the map with named extensions in here
		extensionFactories.insert(getExtensionFactory<ArrayExtension>("ext.array"));
		extensionFactories.insert(getExtensionFactory<AsmStmtExtension>("ext.asm"));
		extensionFactories.insert(getExtensionFactory<ComplexExtension>("ext.complex"));
		extensionFactories.insert(getExtensionFactory<CompoundOpsExtension>("ext.compound_ops"));
		extensionFactories.insert(getExtensionFactory<DatapathExtension>("ext.datapath"));
		extensionFactories.insert(getExtensionFactory<EnumExtension>("ext.enum"));
		extensionFactories.insert(getExtensionFactory<InputOutputExtension>("ext.io"));
		extensionFactories.insert(getExtensionFactory<InstrumentationExtension>("ext.instrumentation"));
		extensionFactories.insert(getExtensionFactory<MemoryExtension>("ext.memory"));
		extensionFactories.insert(getExtensionFactory<ParallelExtension>("ext.parallel"));
		extensionFactories.insert(getExtensionFactory<PointerExtension>("ext.pointer"));
		extensionFactories.insert(getExtensionFactory<ReferenceExtension>("ext.reference"));
		extensionFactories.insert(getExtensionFactory<StaticVariableExtension>("ext.static"));
		extensionFactories.insert(getExtensionFactory<TimeExtension>("ext.time"));
		extensionFactories.insert(getExtensionFactory<VarArgsExtension>("ext.varargs"));
	}

	const boost::optional<std::function<const Extension&(NodeManager&)>> ExtensionRegistry::lookupExtensionFactory(const std::string& extensionName) const {
		auto pos = extensionFactories.find(extensionName);
		if (pos != extensionFactories.end()) {
			// return the factory
			return pos->second;
		}
		// return failed result
		return boost::optional<std::function<const Extension&(NodeManager&)>>();
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme

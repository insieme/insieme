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

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

#include <gtest/gtest.h>

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

	struct BuilderModule {
		NodeManager& mgr;
		BuilderModule(NodeManager& mgr) : mgr(mgr) {}
	};

	template<typename ... Modules>
	struct Builder : public virtual Modules... {
		Builder(NodeManager& mgr) : Modules(mgr)...  {}

		int f() { return 0; };
	};

	struct DefaultBuilder : public Builder<> {
		DefaultBuilder(NodeManager& mgr) : Builder<>(mgr) {}
	};

	struct ModuleA : public BuilderModule {
		ModuleA(NodeManager& mgr) : BuilderModule(mgr) {}
		int a() { return 0; };
	};

	struct ModuleB : public BuilderModule {
		ModuleB(NodeManager& mgr) : BuilderModule(mgr) {}
		int b() { return 0; };
	};


	TEST(BuilderTest, ModularStructure) {

		NodeManager mgr;

		DefaultBuilder builder(mgr);
		builder.f();

		Builder<ModuleA> builderA(mgr);
		builderA.a();

		Builder<ModuleA, ModuleB> builderAB(mgr);
		builderAB.a();
		builderAB.b();


	}


}
}

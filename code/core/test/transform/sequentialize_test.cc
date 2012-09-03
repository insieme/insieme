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

#include <gtest/gtest.h>

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace core {
namespace transform {

	TEST(Manipulation, SequentializeAtomic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		StatementPtr code = analysis::normalize(builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	ref<int> a = var(2);"
				"	atomic.fetch.and.add(a, 10);"
				"}"
		).as<StatementPtr>());

		ASSERT_TRUE(code);

		EXPECT_EQ(
				"{ref<int<4>> v0 = ref.var(2); rec v0.{v0=fun(ref<'a> v1, 'a v2) {return rec v0.{v0=fun(ref<'a> v1, (('a)=>bool) v2, (('a)=>'a) v3) {'a v4 = ref.deref(v1); if(v2(ref.deref(v1))) {ref.assign(v1, v3(ref.deref(v1)));} else {}; return v4;}}(v1, bind(v3){rec v0.{v0=fun('a v1) {return v1;}}(true)}, bind(v4){gen.add(v4, v2)});}}(v0, 10);}",
				toString(*code)
			);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());


		auto res = analysis::normalize(transform::trySequentialize(mgr, code));
//		std::cout << core::printer::PrettyPrinter(res) << "\n";
		EXPECT_EQ(
				"{ref<int<4>> v0 = ref.var(2); rec v0.{v0=fun(ref<'a> v1, 'a v2) {return rec v0.{v0=fun(ref<'a> v1, 'a v2) {'a v3 = ref.deref(v1); ref.assign(v1, gen.add(ref.deref(v1), v2)); return v3;}}(v1, v2);}}(v0, 10);}",
				toString(*res)
		);
		EXPECT_TRUE(check(res, checks::getFullCheck()).empty()) << check(res, checks::getFullCheck());
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme

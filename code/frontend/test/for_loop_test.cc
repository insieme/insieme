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

#include "insieme/frontend/frontend.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/test/test_utils.h"

#include "test_utils.inc"

namespace insieme {
namespace frontend {

	TEST(StmtConversion, NoMaterialization) {

		Source src(
				R"(

					int main() {

						float sum = 0;
						for(int i=0; i<10; i++) {
							sum += 1.0;
						}

					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(ConversionJob(src).execute(mgr));

//		dump(res);

		// check that there is no materialization
		auto code = toString(core::printer::PrettyPrinter(res));
		EXPECT_PRED2(notContainsSubString, code, "decl ref<int<4>>");

	}

	TEST(StmtConversion, ForToWhileLoop1) {

		Source src(
				R"(

					int main() {

						for(int i=0; i<10; i++) {
							i++;
						}

					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(ConversionJob(src).execute(mgr));

		//dump(res);

		// check that there is no materialization
		auto code = toString(core::printer::PrettyPrinter(res));
		EXPECT_PRED2(containsSubString, code, "while");
	}
	
	TEST(StmtConversion, ForToWhileLoop2) {

		Source src(
				R"(

					void f(int* x) { *x++; };

					int main() {

						for(int i=0; i<10; i++) {
							f(&i);
						}

					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(ConversionJob(src).execute(mgr));

		//dump(res);

		// check that there is no materialization
		auto code = toString(core::printer::PrettyPrinter(res));
		EXPECT_PRED2(containsSubString, code, "while");
	}
	
	

	TEST(StmtConversion, Materialization) {

		Source src(
				R"(

					void f(int* x) { };

					int main() {

						for(int i=0; i<10; i++) {
							f(&i);
						}

					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(ConversionJob(src).execute(mgr));

//		dump(res);

		// check that there is no materialization
		auto code = toString(core::printer::PrettyPrinter(res));
		EXPECT_PRED2(containsSubString, code, "decl ref<int<4>>");

	}


	TEST(StmtConversion, ForLoopIteratorMaterializationThreadPrivate) {

		Source src(
				R"(

					float x;

					#pragma omp threadprivate (x)

					int main() {

						float sum = 0;

						for(int i=0; i<10; i++) {
							sum += x;
						}

					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		ConversionJob job(src);
		job.setOption(ConversionJob::OpenMP);
		auto res = builder.normalize(job.execute(mgr));

		//dump(res);

		// check that there is no materialization
		auto code = toString(core::printer::PrettyPrinter(res));
		EXPECT_PRED2(notContainsSubString, code, "decl ref<int<4>>");
	}

} // end namespace frontend
} // end namespace insieme

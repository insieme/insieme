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

#include "template_interception.h"

int main() {
	#pragma test expect_ir(R"(lit("IMP_templateFun_int_returns_int" : (int<4>) -> int<4>)(1))")
	templateFun(1);
	#pragma test expect_ir(R"(lit("IMP_templateFun_double_returns_double" : (real<8>) -> real<8>)(lit("2.0E+0":real<8>)))")
	templateFun(2.0);
	#pragma test expect_ir(R"(lit("IMP_templateFun_unsigned_long_long_returns_unsigned_long_long" : (uint<16>) -> uint<16>)(lit("3":uint<16>)))")
	templateFun(3ull);

	#pragma test expect_ir(R"(var ref<IMP_TemplateClass_int,f,f,plain> v0 = lit("IMP_TemplateClass_int::ctor" : IMP_TemplateClass_int::())(v0);)")
	TemplateClass<int> intInstance;
	#pragma test expect_ir(R"(var ref<IMP_TemplateClass_double,f,f,plain> v0 = lit("IMP_TemplateClass_double::ctor" : IMP_TemplateClass_double::())(v0);)")
	TemplateClass<double> doubleInstance;
	#pragma test expect_ir(R"(var ref<IMP_TemplateClass_bool,f,f,plain> v0 = lit("IMP_TemplateClass_bool::ctor" : IMP_TemplateClass_bool::())(v0);)")
	TemplateClass<bool> boolInstance;
}

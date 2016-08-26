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

int main() {

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<int<4>,f,f> v1;}")
	{
		int x, y;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1;}")
	{
		int x, *y;
	}

	#pragma test expect_ir("if(int_ne(1, 0)) { var ref<int<4>,f,f> v0; }")
	if(1) {
		int a;
	}

	#pragma test expect_ir("if(int_ne(1, 0)) { var ref<int<4>,f,f> v0; } else { var ref<real<4>,f,f> v0; }")
	if(1) {
		int a;
	} else {
		float b;
	}

	#pragma test expect_ir("if(int_ne(1, 0)) { var ref<int<4>,f,f> v0; } else { if(int_ne(0, 0)) { var ref<real<4>,f,f> v0; } }")
	if(1) {
		int a;
	} else if(0) {
		float b;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; if(int_ne(c_style_assignment(v0, 1), 0)) { } }")
	{
		int i;
		if(i = 1) { }
	}

	#pragma test expect_ir("while(int_ne(0, 0)) { var ref<real<8>,f,f> v0; }")
	while(0) {
		double a;
	}

	#pragma test expect_ir("while(int_ne(0, 0)) { break; }")
	while(0) {
		break;
	}

	#pragma test expect_ir("while(int_ne(0, 0)) { continue; }")
	while(0) {
		continue;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; while(int_ne(c_style_assignment(v0, 1), 0)) { break; } }")
	{
		int i;
		while(i = 1) { break; }
	}

	#pragma test expect_ir("{ var ref<bool,f,f> v0 = false;  while(!*v0 || 0!=0) { v0 = true; { var ref<int<4>,f,f> v1; } } }")
	do {
		int x;
	} while(0);

	#pragma test expect_ir("{" R"(var ref<int<4>,f,f> v0; {
		var ref<bool,f,f> v1 = false;
		while(!*v1 || int_ne(c_style_assignment(v0, 1), 0)) {
			v1 = true;
			{ break; }
		} } })")
	{
		int i;
		do {
			break;
		} while(i = 1);
	}

	#pragma test expect_ir(R"({ var ref<int<4>,f,f> v0;
		switch(*v0) {
			case 0: { return 5 in ref<int<4>>; }
			case 4: { 5; 6; break; }
			case 5: { 6; break; }
			case 8: { return 6 in ref<int<4>>; }
			default: { break; } }
		})")
	{
		int a;
		switch(a) {
			case 0: return 5;
			case 4: 5;
			case 5: 6; break;
			case 8: return 6;
			default: break;
		}
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f,plain> v0 = 0;
			switch(*v0) {
				case 0: {
					break;
				}
			}
			switch(*v0) { }
		})")
	{
		int a = 0;
		switch(a) case 0: break;
		switch(a) a=7;
	}

	#pragma test expect_ir(R"({ var ref<int<4>,f,f> v0;
		switch(c_style_assignment(v0, 0)) {
			case 0: { return 5 in ref<int<4>>; }
			default: { break; } }
		})")
	{
		int a;
		switch(a=0) {
			case 0: return 5;
			default: break;
		}
	}

	#pragma test expect_ir(R"({ {
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		switch(0) {
			default: {
				break;
			}
		};
	}; })")
	{
		switch(0) {
		int a;
		default: break;
		}
	}

	int magic;

	#pragma test expect_ir("{var ref<int<4>,f,f> v0; { v0 = 0; while(*v0 < 10) { v0; gen_post_inc(v0); } } }")
	{
		int i;
		for(i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { v0; comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1) {
			k;
		}
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { v0; comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1) k;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { { comp_assign_add(v0, 1); continue; } comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1) { continue; }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0 < 5) { { if (*v0 == 3) { comp_assign_add(v0, 1); continue; }; if (*v0 == 4) { comp_assign_add(v0, 1); continue; }; } comp_assign_add(v0, 1); } } }")
	{
		for(int k = 2; k < 5; k+=1) {
			if (k==3) continue;
			if (k==4) continue;
		}
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0 = 0; { while(*v0 < 10) { v0; gen_post_inc(v0); } } }")
	{
		int i = 0;
		for(; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0 = 0; { while(true) { break; gen_post_inc(v0); } } }")
	{
		int i = 0;
		for(;; i++) {
			break;
		}
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0 = 0; { while(true) { { gen_post_inc(v0); continue; } gen_post_inc(v0); } } }")
	{
		int i = 0;
		for(;; i++) {
			continue;
		}
	}

	#pragma test expect_ir("{ { while(true) { break; } } }")
	{
		for(;;) {
			break;
		}
	}

	#pragma test expect_ir("{ { var ref<int<4>,f,f> v0 = 0; while(*v0 < 0) {}; } }")
	{
		for(int i = 0; i < 0;) {
		}
	}

	return 0;
}

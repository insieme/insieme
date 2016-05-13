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

enum e { A, B, C };

typedef enum f { D, E, F } f;

typedef enum g { G, H=4, I } g;

void eConsumer(enum e e) { }

enum e eProducer() { return A; }

int main() {

	// undefined initialization
	#pragma test expect_ir("var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = ref_decl(type_lit(ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain>));")
	enum e e1;

	// initialized with an enum constant
	#pragma test expect_ir("using \"ext.enum\"; var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);")
	enum e e2 = A;

	// initialized with an integer
	#pragma test expect_ir("using \"ext.enum\"; var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = enum_from_int(type_lit((type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>)), 7);")
	enum e e3 = 7;

	int magic;
	// initialized with another enum variable
	#pragma test expect_ir(R"(using "ext.enum"; {
	    var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = ref_decl(type_lit(ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain>));
	    var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v1 = *v0;
	    })")
	{
		enum e e4a;
		enum e e4b = e4a;
	}

	// passing an enum to a function
	#pragma test expect_ir(R"(using "ext.enum";
	    def IMP_eConsumer = function (v0 : ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain>) -> unit { };
	    {
	        var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = ref_decl(type_lit(ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain>));
	        IMP_eConsumer(*v0);
	    })")
	{
		enum e e5;
		eConsumer(e5);
	}

	// getting an enum from a function
	#pragma test expect_ir(R"(using "ext.enum";
	    def IMP_eProducer = function () -> (type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>) {
			return (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
		};
	    {
	        var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = IMP_eProducer();
	    })")
	{
		enum e e6 = eProducer();
	}

	// typedef-ed enum
	#pragma test expect_ir("var ref<(type<enum_def<IMP_f,int<4>,enum_entry<IMP_f_colon__colon_D,0>,enum_entry<IMP_f_colon__colon_E,1>,enum_entry<IMP_f_colon__colon_F,2>>>, int<4>),f,f,plain> v0 = ref_decl(type_lit(ref<(type<enum_def<IMP_f,int<4>,enum_entry<IMP_f_colon__colon_D,0>,enum_entry<IMP_f_colon__colon_E,1>,enum_entry<IMP_f_colon__colon_F,2>>>, int<4>),f,f,plain>));")
	f f1;

	// enum with custom values
	#pragma test expect_ir("var ref<(type<enum_def<IMP_g,int<4>,enum_entry<IMP_g_colon__colon_G,0>,enum_entry<IMP_g_colon__colon_H,4>,enum_entry<IMP_g_colon__colon_I,5>>>, int<4>),f,f,plain> v0 = ref_decl(type_lit(ref<(type<enum_def<IMP_g,int<4>,enum_entry<IMP_g_colon__colon_G,0>,enum_entry<IMP_g_colon__colon_H,4>,enum_entry<IMP_g_colon__colon_I,5>>>, int<4>),f,f,plain>));")
	g g1;

	// comparison operators
	#pragma test expect_ir(R"(using "ext.enum"; {
		var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
		enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0))==5;
		enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0))<enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 1));
	    num_cast(enum_to_int(*v0), type_lit(uint<4>))<num_cast(enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 2)), type_lit(uint<4>));
	})")
	{
		enum e e1 = A;
		A == 5;
		A < B;
		e1 < C;
	}

	// other operators
	#pragma test expect_ir(R"(using "ext.enum"; {
		var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
		enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0))+5;
		enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0))&enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 1));
		num_cast(enum_to_int(*v0), type_lit(uint<4>))|num_cast(enum_to_int((type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 2)), type_lit(uint<4>));
	})")
	{
		enum e e1 = A;
		A + 5;
		A & B;
		e1 | C;
	}

	// enum ptr
	#pragma test expect_ir(R"(using "ext.enum"; {
		var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
		var ref<ptr<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>)>,f,f,plain> v1 = ptr_from_ref(v0);
	})")
	{
		enum e e1 = A;
		enum e *e2 = &e1;
	}
}

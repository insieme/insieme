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
	#pragma test expect_ir("var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)));")
	enum e e1;

	// initialized with an enum constant
	#pragma test expect_ir("using \"ext.enum\"; var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var_init(int_to_enum(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)), num_cast(num_cast(0u, type_lit(int<4>)), type_lit(uint<4>))));")
	enum e e2 = A;

	// initialized with an integer
	#pragma test expect_ir("using \"ext.enum\"; var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var_init(int_to_enum(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)), num_cast(7, type_lit(uint<4>))));")
	enum e e3 = 7;

	// initialized with another enum variable
	#pragma test expect_ir(R"(using "ext.enum"; {
	    var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)));
	    var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v1 = ref_var_init(*v0);
	    })")
	{
		enum e e4a;
		enum e e4b = e4a;
	}

	// passing an enum to a function
	#pragma test expect_ir(R"(using "ext.enum";
	    def IMP_eConsumer = function (v0 : ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain>) -> unit { };
	    {
	        var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)));
	        IMP_eConsumer(*v0);
	    })")
	{
		enum e e5;
		eConsumer(e5);
	}

	// getting an enum from a function
	#pragma test expect_ir(R"(using "ext.enum";
	    def IMP_eProducer = function () -> (enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>) { return int_to_enum(type_lit((enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>)), num_cast(num_cast(0u, type_lit(int<4>)), type_lit(uint<4>))); };
	    {
	        var ref<(enum_def<IMP_e,enum_entry<A,0>,enum_entry<B,1>,enum_entry<C,2>>, uint<4>),f,f,plain> v0 = ref_var_init(IMP_eProducer());
	    })")
	{
		enum e e6 = eProducer();
	}


	// typedef-ed enum
	#pragma test expect_ir("var ref<(enum_def<IMP_f,enum_entry<D,0>,enum_entry<E,1>,enum_entry<F,2>>, uint<4>),f,f,plain> v0 = ref_var(type_lit((enum_def<IMP_f,enum_entry<D,0>,enum_entry<E,1>,enum_entry<F,2>>, uint<4>)));")
	f f1;


	// enum with custom values
	#pragma test expect_ir("var ref<(enum_def<IMP_g,enum_entry<G,0>,enum_entry<H,4>,enum_entry<I,5>>, uint<4>),f,f,plain> v0 = ref_var(type_lit((enum_def<IMP_g,enum_entry<G,0>,enum_entry<H,4>,enum_entry<I,5>>, uint<4>)));")
	g g1;
}

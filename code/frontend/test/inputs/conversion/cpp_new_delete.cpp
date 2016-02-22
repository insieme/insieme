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

struct SimplestConstructor {
	SimplestConstructor() = default;
	~SimplestConstructor() = default;
};

#define SimplestConstructor_IR R"( def struct IMP_SimplestConstructor { }; )"

int main() {
	;

	// Base types ----------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_ref(ref_new(type_lit(int<4>)));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int;
		delete i;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_ref(ref_new_init(42));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int{42};
		delete i;
	}

	// Base type arrays ----------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_array(ref_new(type_lit(array<int<4>,50>)));
		ref_delete(ptr_to_array(*i));
	})")
	{
		int* arri = new int[50];
		delete [] arri;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_array(ref_new_init(*<ref<array<int<4>,50>,f,f,plain>>(ref_temp(type_lit(array<int<4>,50>))) {1,2,3}));
		ref_delete(ptr_to_array(*i));
	})")
	{
		int* arri = new int[50]{1, 2, 3};
		delete [] arri;
	}

	// Class types ---------------------------------------------------------------------------------------------------------------------------------------------
	
	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor))));
		ref_delete(ptr_to_ref(*v0));
	})")
	{
		SimplestConstructor *simple = new SimplestConstructor;
		delete simple;
	}

	//#pra gma test expect_ir(SimplestConstructor_IR, R"({
	//	var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = object_array_new(type_lit(IMP_SimplestConstructor), 3, IMP_SimplestConstructor::);
	//	delete(ptr_to_array(*v0));
	//})")
	{
		SimplestConstructor* arrsimple = new SimplestConstructor[3];
		delete [] arrsimple;
	}


	return 0;
}

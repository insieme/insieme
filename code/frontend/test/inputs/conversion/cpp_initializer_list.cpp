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
#include <initializer_list>

struct SimplestConstructor {
	SimplestConstructor() = default;
	~SimplestConstructor() = default;
};

struct S {
    SimplestConstructor v[3];
    S(std::initializer_list<SimplestConstructor> l) { // list-initialization in ctor
         //std::cout << "constructed with a " << l.size() << "-element list\n";
         append(l);
    }
    void append(std::initializer_list<SimplestConstructor> l) { // list-initialization as argument
        for(int i=0; i<l.size(); ++i) {
			v[i] = *(l.begin()+i);
        }
    }
    int c_arr() const {
        return {3};  // list-initialization in return statement
    }
};

#define SimplestConstructor_IR R"( def struct IMP_SimplestConstructor { }; )"

int main() {
	//this IR test pragma is not correct yet. just committed for testing purposes.
	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ref_var_init(ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor)))));
		ref_delete(ptr_to_ref(*v0));
	})")
	{
		S obj_s({SimplestConstructor(), SimplestConstructor()});
	}

	return 0;
}

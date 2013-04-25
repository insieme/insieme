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


// builtin types
void basic_type_test() {

	#pragma test "ref<int<4>> v1 = ref.var(1)"
	int i = 1;

	#pragma test "ref<uint<4>> v1 = ref.var(1u)"
	unsigned int ui = 1;

	#pragma test "ref<real<4>> v1 = ref.var(1.0f)"
	float f = 1;

	#pragma test "{ref<real<4>> v1 = ref.var(0.0f); ref<int<4>> v2 = ref.var(int.sub(cast<int<4>>(0), 1)); ref.assign(v1, int.to.real(ref.deref(v2), 4)); rec v0.{v0=fun(ref<'a> v1) {ref.assign(v1, gen.add(ref.deref(v1), 1)); return ref.deref(v1);}}(v1); ref.assign(v2, real.to.int(ref.deref(v1), 4));}"
	{
		float x;
		int y = -1;

		x = y;
		++x;
		y = (int) x;
	}
}

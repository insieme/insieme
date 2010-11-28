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

// start code fragment :: type_declaration___insieme_unnamed_userdefined_type_type_0 //
struct __insieme_unnamed_userdefined_type_type_0 { 
};

// start code fragment :: fundef_codefragment_g //
int g(int a){
	return (a - 1);;
}

// start code fragment :: fundef_codefragment_f //
int f(int a, int b){
	return (g((a + b)) + 1000);;
}

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	struct __insieme_unnamed_userdefined_type_type_0 var_8 = ((struct __insieme_unnamed_userdefined_type_type_0){});
	return f(argc, argc);;
}

// start code fragment :: unnamed //


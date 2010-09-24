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

//OpenCL definitions
/*#define __private __attribute__((address_space(0))) //default value
#define private __attribute__((address_space(0)))
#define __local __attribute__((address_space(1)))
#define local __attribute__((address_space(1)))
#define __global __attribute__((address_space(2)))
#define global __attribute__((address_space(2)))
#define __constant __attribute__((address_space(3)))
#define constant __attribute__((address_space(3)))*/

//define address spaces
#define __private __attribute__((annotate("__private"))) //default value
#define private __attribute__((annotate("__private")))
#define __local __attribute__((annotate("__local")))
#define local __attribute__((annotate("__local")))
#define __global __attribute__((annotate("__global")))
#define global __attribute__((annotate("__global")))
#define __constant __attribute__((annotate("__constant")))
#define constant __attribute__((annotate("__constant")))

#define __kernel __attribute__((annotate("__kernel")))
#define kernel __attribute__((annotate("__kernel")))

//define build-in vecto types
#define ivec(T,v) typedef __attribute__((ext_vector_type(v))) T T##v; typedef __attribute__((ext_vector_type(v))) unsigned T u##T##v;
#define fvec(T,v) typedef __attribute__((ext_vector_type(v))) T T##v;

ivec(char,2)
ivec(char,3)
ivec(char,4)
ivec(char,8)
ivec(char,16)

ivec(short,2)
ivec(short,3)
ivec(short,4)
ivec(short,8)
ivec(short,16)

ivec(int,2)
ivec(int,3)
ivec(int,4)
ivec(int,8)
ivec(int,16)

#if _WIN64 || __amd64__
ivec(long,2)
ivec(long,3)
ivec(long,4)
ivec(long,8)
ivec(long,16)
#else
typedef __attribute__((ext_vector_type(2))) long long long2; typedef __attribute__((ext_vector_type(2))) unsigned long long ulong2;
typedef __attribute__((ext_vector_type(3))) long long long3; typedef __attribute__((ext_vector_type(3))) unsigned long long ulong3;
typedef __attribute__((ext_vector_type(4))) long long long4; typedef __attribute__((ext_vector_type(4))) unsigned long long ulong4;
typedef __attribute__((ext_vector_type(8))) long long long8; typedef __attribute__((ext_vector_type(8))) unsigned long long ulong8;
typedef __attribute__((ext_vector_type(16))) long long long16; typedef __attribute__((ext_vector_type(16))) unsigned long long ulong16;
#endif

fvec(float,2)
fvec(float,3)
fvec(float,4)
fvec(float,8)
fvec(float,16)


//define build-in functions
unsigned int get_global_id(unsigned int dmindx);
unsigned int get_global_size(unsigned int dmindx);
unsigned int get_global_offset(unsigned int dmindx);
unsigned int get_gourp_id(unsigned int dmindx);
unsigned int get_num_groups(unsigned int dmindx);
unsigned int get_local_id(unsigned int dmindx);
unsigned int get_local_size(unsigned int dmindx);



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
#include <stdlib.h>
#include "CL/cl.h"

struct Kernel {
	cl_kernel fct;
	cl_command_queue queue;
};

cl_kernel kernel1;
struct Kernel kernel2;
cl_kernel* kernel3;
struct Kernel* kernel4;

cl_mem dev_ptr1;
int main() {
	cl_kernel kernel5;
	struct Kernel kernel6;
	cl_kernel* kernel7;
	struct Kernel* kernel8;

	clSetKernelArg(kernel1, 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel2.fct, 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel3[0], 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel4[0].fct, 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel5, 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel6.fct, 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel7[0], 0, sizeof(cl_mem), (void*)&dev_ptr1);
	clSetKernelArg(kernel8[0].fct, 0, sizeof(cl_mem), (void*)&dev_ptr1);
}

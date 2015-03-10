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

// TODO: find/fix scope issue for nested pragmas!

#define LOOP_CODE for(int i = 0; i < 10; ++i) { b = i; }

#pragma insieme mark
int muha() {
	return 0;
}

#pragma insieme mark
int main() {
#pragma insieme kernelFile "path/to/imaginary/kernel/file"
{
}
	int a;
#pragma insieme datarange (a = 0 : 9)
{
	a = 0;
}
	int b = 0;
#pragma insieme transform "0"
{
	LOOP_CODE
}
#pragma insieme iterations 42
{
	LOOP_CODE
}
#pragma insieme strip (1,1)
{
	LOOP_CODE
}
#pragma insieme interchange (1,1)
{
	LOOP_CODE
}
#pragma insieme tile (1,1)
{
	LOOP_CODE
}
#pragma insieme unroll (1)
{
	LOOP_CODE
}
#pragma insieme fuse (1,1)
{
	LOOP_CODE
}
#pragma insieme split (1,1)
{
	LOOP_CODE
}
#pragma insieme stamp (1,1)
{
	LOOP_CODE
}
#pragma insieme reschedule (1)
{
	LOOP_CODE
}
#pragma insieme parallelize (1)
{
	LOOP_CODE
}
#pragma insieme rstrip (1)
{
	LOOP_CODE
}
#pragma insieme fun_unroll (1)
{
	LOOP_CODE
}
#pragma insieme info id:1(b)
{
	LOOP_CODE
}
	return 0;
}

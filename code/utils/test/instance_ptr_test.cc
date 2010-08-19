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

#include <string>
#include <iostream>

#include <gtest/gtest.h>
#include "instance_ptr.h"

using std::string;
using std::cout;
using std::endl;



TEST(InstancePtr, NullTest) {

	InstancePtr<int> null = InstancePtr<int>(NULL);

	InstancePtr<int> ptrN(0);
	EXPECT_TRUE ( ptrN == null );
	EXPECT_TRUE ( ptrN == ptrN );
	EXPECT_FALSE ( ptrN );

	int a = 10;
	InstancePtr<int> ptrA(&a);
	EXPECT_FALSE ( ptrA == null );
	EXPECT_FALSE ( null == ptrA );
	EXPECT_FALSE ( ptrA == ptrN );
	EXPECT_FALSE ( ptrN == ptrA );
	EXPECT_TRUE ( !!ptrA );

	InstancePtr<int> ptrA2(&a);
	EXPECT_TRUE ( ptrA == ptrA2 );
	EXPECT_TRUE ( ptrA2 == ptrA );

	int b = 12;
	InstancePtr<int> ptrB(&b);
	EXPECT_FALSE ( ptrB == null );
	EXPECT_FALSE ( null == ptrB );
	EXPECT_FALSE ( ptrB == ptrN );
	EXPECT_FALSE ( ptrN == ptrB );
	EXPECT_TRUE ( !!ptrB );

	EXPECT_FALSE ( ptrA == ptrB );
	EXPECT_FALSE ( ptrB == ptrA );

}


TEST(InstancePtr, Size) {
	// just ensures
	EXPECT_LE ( sizeof (InstancePtr<int>), 2*sizeof(int*) );
}

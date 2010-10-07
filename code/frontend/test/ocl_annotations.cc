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

#include <gtest/gtest.h>

#include "ocl/ocl_annotations.h"

namespace insieme {
namespace frontend {

bool testKernelFct(OclKernelFctAnnotation kernel) {
    return kernel.isKernelFct();
}

void testWorkGroupSize(OclWorkGroupSizeAnnotation wgs, int* param){
    param[0] = wgs.getXdim();
    param[1] = wgs.getYdim();
    param[2] = wgs.getZdim();
}

OclAddressSpaceAnnotation::addressSpace testAddressSpace(OclAddressSpaceAnnotation space){
    return space.getAddressSpace();
}
/* TODO fixme
TEST(ocl_properties, DefaultInitialization) {

    core::Annotatable function;

    const OclKernelFctAnnotationPtr kernel;

    const OclAddressSpaceAnnotationPtr space;

//    const OclWorkGroupSizeAnnotationPtr wgs;

//    printf("trallalal %d %d %d\n", wgs->getXdim(), wgs->getYdim(), wgs->getZdim());

    function.addAnnotation(kernel);
//    function.addAnnotation(wgs);

    function.getAnnotation(OclAnnotation::KEY);
//    EXPECT_TRUE(testKernelFct(function.getAnnotation(OclAnnotation::KEY)));

    EXPECT_EQ(OclAddressSpaceAnnotation::addressSpace::PRIVATE, space.getAddressSpace());

    EXPECT_EQ(1, wgs.getXdim());
    EXPECT_EQ(2, wgs.getYdim());
    EXPECT_EQ(3, wgs.getZdim());

}*/

TEST(ocl_properties, oldVersion) {

    insieme::core::Annotatable obj;

    OclKernelFctAnnotation function;

    EXPECT_TRUE(function.isKernelFct());

    OclAddressSpaceAnnotation space;

    EXPECT_EQ(OclAddressSpaceAnnotation::addressSpace::PRIVATE, space.getAddressSpace());

    OclWorkGroupSizeAnnotation wgs(1,2,3);

    EXPECT_EQ(static_cast<unsigned short>(1), wgs.getXdim());
    EXPECT_EQ(static_cast<unsigned short>(2), wgs.getYdim());
    EXPECT_EQ(static_cast<unsigned short>(3), wgs.getZdim());

}

}
}

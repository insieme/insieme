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
namespace ocl {

TEST(ocl_properties, FunctionAnnotations) {

    core::Annotatable function;

    ocl::OclBaseAnnotation::OclAnnotationList functionAnnotation;

    functionAnnotation.push_back(std::make_shared<ocl::OclKernelFctAnnotation>());
    functionAnnotation.push_back(std::make_shared<ocl::OclWorkGroupSizeAnnotation>(1,2,3));

    function.addAnnotation(std::make_shared<ocl::OclBaseAnnotation>(functionAnnotation));

    core::AnnotationMap aMap = function.getAnnotations();

    EXPECT_EQ(static_cast<unsigned int>(1), aMap.size());

    std::shared_ptr<insieme::core::Annotation> oa = (*aMap.find(&OclBaseAnnotation::KEY)).second;

    //does not work in Hudson
//    EXPECT_TRUE(std::dynamic_pointer_cast<OclBaseAnnotation>(oa));


    if(ocl::OclBaseAnnotationPtr oclKernelAnnotation = std::dynamic_pointer_cast<ocl::OclBaseAnnotation>(oa)) {
        EXPECT_EQ(static_cast<unsigned int>(2), oclKernelAnnotation->getNumAnnotations());
        for(size_t i = 0; i < oclKernelAnnotation->getNumAnnotations(); ++i) {
            ocl::OclAnnotationPtr ocl = oclKernelAnnotation->getAnnotationByIndex(i);
            if(ocl::OclKernelFctAnnotationPtr kf = std::dynamic_pointer_cast<ocl::OclKernelFctAnnotation>(ocl))
                EXPECT_TRUE(kf->isKernelFct());
            if(ocl::OclWorkGroupSizeAnnotationPtr wgs = std::dynamic_pointer_cast<ocl::OclWorkGroupSizeAnnotation>(ocl)) {
                EXPECT_EQ(static_cast<unsigned int>(1), wgs->getXdim());
                EXPECT_EQ(static_cast<unsigned int>(2), wgs->getYdim());
                EXPECT_EQ(static_cast<unsigned int>(3), wgs->getZdim());
            }
        }
    }
    else
        EXPECT_TRUE(false);



/*

    const OclAddressSpaceAnnotationPtr space(new OclAddressSpaceAnnotation());
    if(OclKernelFctAnnotationPtr s = std::dynamic_pointer_cast<OclKernelFctAnnotation>(oa))

    EXPECT_EQ(1, wgs.getXdim());
    EXPECT_EQ(2, wgs.getYdim());
    EXPECT_EQ(3, wgs.getZdim());

    EXPECT_EQ(OclAddressSpaceAnnotation::addressSpace::PRIVATE, space.getAddressSpace());
    */
}

TEST(ocl_properties, DeclarationAnnotations) {
    core::Annotatable declaration;

    ocl::OclBaseAnnotation::OclAnnotationList functionAnnotations;

    functionAnnotations.push_back(std::make_shared<ocl::OclAddressSpaceAnnotation>());

    declaration.addAnnotation(std::make_shared<ocl::OclBaseAnnotation>(functionAnnotations));

    auto declarationAnnotation = declaration.getAnnotation(ocl::OclBaseAnnotation::KEY);

    EXPECT_EQ(static_cast<unsigned int>(1), declarationAnnotation->getNumAnnotations());

    for(ocl::OclBaseAnnotation::OclAnnotationList::const_iterator I = declarationAnnotation->getListBegin();
            I < declarationAnnotation->getListEnd(); ++I) {
        if(ocl::OclAddressSpaceAnnotationPtr as = std::dynamic_pointer_cast<ocl::OclAddressSpaceAnnotation>(*I)){
            EXPECT_EQ(ocl::OclAddressSpaceAnnotation::addressSpace::PRIVATE, as->getAddressSpace());
        }
    }

}

} //namespace ocl
} //namespace frontend
} //namespace insieme

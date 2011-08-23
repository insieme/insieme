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

#include "insieme/annotations/ocl/ocl_annotations.h"

namespace insieme {
namespace annotations {
namespace ocl {

TEST(ocl_properties, FunctionAnnotations) {

	insieme::utils::Annotatable<> function;

    ocl::BaseAnnotation::AnnotationList functionAnnotation;

    functionAnnotation.push_back(std::make_shared<ocl::KernelFctAnnotation>());
    functionAnnotation.push_back(std::make_shared<ocl::WorkGroupSizeAnnotation>(1,2,3));

    function.addAnnotation(std::make_shared<ocl::BaseAnnotation>(functionAnnotation));

    insieme::utils::Annotatable<>::annotation_map_type aMap = function.getAnnotations();

    EXPECT_EQ(static_cast<unsigned int>(1), aMap.size());

    std::shared_ptr<insieme::utils::Annotation> oa = (*aMap.find(&BaseAnnotation::KEY)).second;

    //does not work in Hudson
//    EXPECT_TRUE(std::dynamic_pointer_cast<BaseAnnotation>(oa));


    if(ocl::BaseAnnotationPtr oclKernelAnnotation = std::dynamic_pointer_cast<ocl::BaseAnnotation>(oa)) {
        for(ocl::BaseAnnotation::AnnotationList::const_iterator I = oclKernelAnnotation->getAnnotationListBegin();
                I < oclKernelAnnotation->getAnnotationListEnd(); ++I) {
            ocl::AnnotationPtr ocl = std::dynamic_pointer_cast<ocl::Annotation>(*I);
			assert(ocl && "Wrong Annotation");
            if(ocl::KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<ocl::KernelFctAnnotation>(ocl))
                EXPECT_TRUE(kf->isKernelFct());
            if(ocl::WorkGroupSizeAnnotationPtr wgs = std::dynamic_pointer_cast<ocl::WorkGroupSizeAnnotation>(ocl)) {
                EXPECT_EQ(static_cast<unsigned int>(1), wgs->getXdim());
                EXPECT_EQ(static_cast<unsigned int>(2), wgs->getYdim());
                EXPECT_EQ(static_cast<unsigned int>(3), wgs->getZdim());
            }
        }
    }
    else
        EXPECT_TRUE(false);

}

TEST(ocl_properties, DeclarationAnnotations) {
	insieme::utils::Annotatable<> declaration;

    ocl::BaseAnnotation::AnnotationList functionAnnotations;

    functionAnnotations.push_back(std::make_shared<ocl::AddressSpaceAnnotation>());

    declaration.addAnnotation(std::make_shared<ocl::BaseAnnotation>(functionAnnotations));

    auto declarationAnnotation = declaration.getAnnotation(ocl::BaseAnnotation::KEY);

    for(ocl::BaseAnnotation::AnnotationList::const_iterator I = declarationAnnotation->getAnnotationListBegin();
            I < declarationAnnotation->getAnnotationListEnd(); ++I) {
        if(ocl::AddressSpaceAnnotationPtr as = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)){
            EXPECT_EQ(ocl::AddressSpaceAnnotation::addressSpace::PRIVATE, as->getAddressSpace());
        }
    }

}

} // end namespace ocl
} // end namespace annotations
} // end namespace insieme

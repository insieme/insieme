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

#pragma once

#include "annotation.h"

#define DEFINE_TYPE(Type) \
    class Type; \
    typedef std::shared_ptr<Type> Type##Ptr;

namespace insieme {
namespace frontend {

DEFINE_TYPE(OclAnnotation);
DEFINE_TYPE(OclKernelFctAnnotation);
DEFINE_TYPE(OclWorkGroupSizeAnnotation);
DEFINE_TYPE(OclAddressSpaceAnnotation);

/** Base class for OpenCL related annotations
 ** */
class OclAnnotation : public core::Annotation {
public:
    static const core::StringKey<OclAnnotationPtr> KEY;

    OclAnnotation() : core::Annotation() {  }
    const core::AnnotationKey* getKey() const { return &KEY; }
};


/** Annotation class intended to mark functions as OpenCL kernel functions.
 ** Should be used to annotate OpenCL kernel functions
 ** Default value is isKernelFct() = true
 ** */
class OclKernelFctAnnotation : public OclAnnotation {

private:
    bool kf;
public:

    OclKernelFctAnnotation() : OclAnnotation(), kf(true){ }

    const std::string getAnnotationName() const { return "OclKernelFctAnnotation"; }

    void setKernelFct(bool isKernelFct);

    bool isKernelFct() const;
};

/** Annotation class intended to mark store the required work group size if given.
 ** Should be used to annotate OpenCL kernel functions with attribute
 ** reqd_work_group_size set
 ** */
class OclWorkGroupSizeAnnotation : public OclAnnotation {

private:
    const unsigned int xDim, yDim, zDim;
public:
    OclWorkGroupSizeAnnotation(unsigned int x, unsigned int y, unsigned int z) :
        OclAnnotation(), xDim(x), yDim(y), zDim(z) { }

	const std::string getAnnotationName() const { return "OclWorkGroupSizeAnnotation"; }

    unsigned int getXdim() const;
    unsigned int getYdim() const;
    unsigned int getZdim() const;
//    unsigned int* getDims();
};


/** Annotation class intended to keep OpenCL address spaces information.
 ** Should be used to annotate OpenCL variable declarations inside kernel functions
 ** Default value is getAddressSpace() = addressSpace::PRIVATE
 ** */
class OclAddressSpaceAnnotation : public OclAnnotation {
public:
    enum addressSpace{
        PRIVATE,
        LOCAL,
        GLOBAL,
        CONSTANT,
        size
    };

private:
    addressSpace as;
public:
    static const core::StringKey<OclAddressSpaceAnnotation> KEY;

    const std::string getAnnotationName() const { return "OclAddressSpaceAnnotation"; }

    OclAddressSpaceAnnotation() : OclAnnotation(), as(addressSpace::PRIVATE) { }

    OclAddressSpaceAnnotation(addressSpace space) : core::Annotation(), as(space) { }

    OclAddressSpaceAnnotation(addressSpace space) : OclAnnotation(), as(space) { }

    bool setAddressSpace(addressSpace newAs);

    addressSpace getAddressSpace() const;
};


} // namespace c_info
} // namespace insieme

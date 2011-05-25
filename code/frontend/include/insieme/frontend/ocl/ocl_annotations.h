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

#include "insieme/core/annotation.h"
#include "insieme/core/expressions.h"

#define DEFINE_TYPE(Type) \
    class Type; \
    typedef std::shared_ptr<Type> Type##Ptr;

namespace insieme {
namespace ocl {

DEFINE_TYPE(BaseAnnotation);
DEFINE_TYPE(Annotation);
DEFINE_TYPE(KernelFctAnnotation);
DEFINE_TYPE(WorkGroupSizeAnnotation);
DEFINE_TYPE(AddressSpaceAnnotation);

class BaseAnnotation : public core::CompoundAnnotation<ocl::Annotation> {
public:
	static const string NAME;
    static const core::StringKey<BaseAnnotation> KEY;

    BaseAnnotation(core::CompoundAnnotation<ocl::Annotation>::AnnotationList& annotationList) :
    	core::CompoundAnnotation<ocl::Annotation>(annotationList) { }

    const core::AnnotationKey* getKey() const { return &KEY; }
    const std::string& getAnnotationName() const { return NAME; }
};


/**
 * Base class for OpenCL related annotations
 */
class Annotation {
    //needed to make OclAnnotation polymorphic
    virtual const std::string& getAnnotationName() const = 0;
};


/** Annotation class intended to mark functions as OpenCL kernel functions.
 ** Should be used to annotate OpenCL kernel functions
 ** Default value is isKernelFct() = true
 ** */
class KernelFctAnnotation : public Annotation , public core::Annotation {

private:
    bool kf;
public:
    static const string NAME;
	static const core::StringKey<KernelFctAnnotation> KEY;

    KernelFctAnnotation() : ocl::Annotation(), core::Annotation(), kf(true){ }

    const std::string& getAnnotationName() const { return NAME; }

    void setKernelFct(bool isKernelFct);

    bool isKernelFct() const;

	const core::AnnotationKey* getKey() const { return &KEY; }
};

/** Annotation class intended to mark store the required work group size if given.
 ** Should be used to annotate OpenCL kernel functions with attribute
 ** reqd_work_group_size set
 ** */
class WorkGroupSizeAnnotation : public Annotation {

private:
    const unsigned int xDim, yDim, zDim;
public:
    static const string NAME;
    WorkGroupSizeAnnotation(unsigned int x, unsigned int y, unsigned int z) :
        Annotation(), xDim(x), yDim(y), zDim(z) { }

    const std::string& getAnnotationName() const { return NAME; }

    unsigned int getXdim() const;
    unsigned int getYdim() const;
    unsigned int getZdim() const;
//    unsigned int* getDims();
};


/** Annotation class intended to keep OpenCL address spaces information.
 ** Should be used to annotate OpenCL variable declarations inside kernel functions
 ** Default value is getAddressSpace() = addressSpace::PRIVATE
 ** */
class AddressSpaceAnnotation : public Annotation , public core::Annotation {
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
    static const string NAME;
    static const core::StringKey<AddressSpaceAnnotation> KEY;

    const std::string& getAnnotationName() const { return NAME; }

    AddressSpaceAnnotation() : ocl::Annotation(), core::Annotation(), as(addressSpace::PRIVATE) { }

    AddressSpaceAnnotation(addressSpace space) : ocl::Annotation(), core::Annotation(), as(space) { }

    bool setAddressSpace(addressSpace newAs);

    addressSpace getAddressSpace() const;

	const core::AnnotationKey* getKey() const { return &KEY; }

};

typedef std::shared_ptr<AddressSpaceAnnotation> AddressSpaceAnnotationPtr;


/** Annotation class intended to keep OpenCL built-in functions information.
 ** Should be used to annotate OpenCL variable that have to be translated to
 ** built-in functions by the OpenCL Back-end
 ** */
class BuiltinFunctionAnnotation : public Annotation , public core::Annotation {
private:
    core::LiteralPtr lit;
public:
    static const string NAME;
    static const core::StringKey<BuiltinFunctionAnnotation> KEY;

    const std::string& getAnnotationName() const { return NAME; }

    BuiltinFunctionAnnotation(core::LiteralPtr l) : ocl::Annotation(), core::Annotation(), lit(l) { }

    core::LiteralPtr getBuiltinLiteral() const;

	const core::AnnotationKey* getKey() const { return &KEY; }

};

typedef std::shared_ptr<BuiltinFunctionAnnotation> BuiltinFunctionAnnotationPtr;

} // namespace ocl
} // namespace insieme

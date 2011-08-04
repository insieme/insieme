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

#include "insieme/annotations/ocl/ocl_annotations.h"


namespace insieme {
namespace annotations {
namespace ocl {

const string BaseAnnotation::NAME = "OclAnnotation";
const utils::StringKey<BaseAnnotation> BaseAnnotation::KEY("OpenCL");

const string KernelFctAnnotation::NAME = "OclKernelFctAnnotation";
const utils::StringKey<KernelFctAnnotation> KernelFctAnnotation::KEY("KernelFctAnnotationKey");

const string BuiltinFunctionAnnotation::NAME = "OclBuiltinFunctionAnnotation";
const utils::StringKey<BuiltinFunctionAnnotation> BuiltinFunctionAnnotation::KEY("BuiltinFunctionAnnotationKey");

const string AddressSpaceAnnotation::NAME = "OclAddressSpaceAnnotation";
const utils::StringKey<AddressSpaceAnnotation> AddressSpaceAnnotation::KEY("AddressSpaceAnnotationKey");

const string KernelFileAnnotation::NAME = "OclKernelFileAnnotation";
const utils::StringKey<KernelFileAnnotation> KernelFileAnnotation::KEY("KernelFileAnnotationKey");

const string WorkGroupSizeAnnotation::NAME = "OclWorkGroupSizeAnnotation";

void KernelFctAnnotation::setKernelFct(bool isKernelFct) { kf = isKernelFct; }

bool KernelFctAnnotation::isKernelFct() const { return kf; }

unsigned int WorkGroupSizeAnnotation::getXdim() const { return xDim; }
unsigned int WorkGroupSizeAnnotation::getYdim() const { return yDim; }
unsigned int WorkGroupSizeAnnotation::getZdim() const { return zDim; }
/*
unsigned int* OclWorkGroupSizeAnnotation::getDims() {
    unsigned int dims[3] = {xDim, yDim, zDim};
    return dims;
}*/

AddressSpaceAnnotation::addressSpace AddressSpaceAnnotation::getAddressSpace() const { return as; }

bool AddressSpaceAnnotation::setAddressSpace(addressSpace newAs){
    if(as > addressSpace::size)
        return false;
    else
        as = newAs;
    return true;
}

core::LiteralPtr BuiltinFunctionAnnotation::getBuiltinLiteral() const { return lit; }

} // namespace ocl
} // namespace annotations
} // namespace insieme

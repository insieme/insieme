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
#include "expressions.h"
#include <memory.h>

namespace insieme {
namespace c_info {
namespace omp {

class OmpAnnotation : public core::Annotation {
public:
    static const core::StringKey<OmpAnnotation> KEY;

    OmpAnnotation() : core::Annotation() { }
    const core::AnnotationKey* getKey() const { return &KEY; }
};

typedef std::shared_ptr<OmpAnnotation> OmpAnnotationPtr;

class OmpCompoundAnnotation: public OmpAnnotation {
public:
	typedef std::vector<OmpAnnotationPtr> AnnotationVect;

	OmpCompoundAnnotation(): OmpAnnotation() { }

protected:
	AnnotationVect children;
};

class OmpParallel: public OmpCompoundAnnotation { };

class OmpFor: public OmpCompoundAnnotation { };

class OmpBarrier: public OmpAnnotation {
public:
	OmpBarrier() : OmpAnnotation() { }
};

class OmpPrivate: public OmpAnnotation {
public:
	typedef std::vector<core::VarExprPtr> VarList;

	OmpPrivate(const VarList& varList) : OmpAnnotation(), varList(varList) { }
	const VarList& getVarList() const { return varList; }
private:
	VarList varList;
};

class OmpFirstPrivate: public OmpAnnotation { };

class OmpLastPrivate: public OmpAnnotation { };

class OmpIf: public OmpAnnotation { };

class OmpReduction: public OmpAnnotation { };

class OmpMaster: public OmpAnnotation {
public:
	OmpMaster() : OmpAnnotation() { }
};

} // End omp namespace
} // End c_info namespace
} // End insieme namespace

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

/*
 * data_annotations.h
 *
 *  Created on: Dec 6, 2011
 *      Author: klaus
 */

#pragma once


#include "insieme/utils/annotation.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace annotations {

using namespace insieme::core;


class LoopAnnotation : public NodeAnnotation {
	size_t iterations;

public:
	static const string NAME;
    static const utils::StringKey<LoopAnnotation> KEY;

    const utils::AnnotationKey* getKey() const { return &KEY; }
    const std::string& getAnnotationName() const { return NAME; }

//    LoopAnnotation() {} iterations has to be initialized
    LoopAnnotation(size_t iterations): iterations(iterations) {}

	size_t getIterations() const;

    virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
		// always copy the annotation
		assert(&*ptr == this && "Annotation pointer should reference this annotation!");
		after->addAnnotation(ptr);
		return true;
	}
};

typedef std::shared_ptr<LoopAnnotation> LoopAnnotationPtr;

} // end namespace insieme
} // end namespace annotations

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::LoopAnnotation& lAnnot);

} // end namespace std

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

#include "core/annotation.h"
#include "utils/map_utils.h"

namespace insieme {
namespace core {


void Annotatable::addAnnotation(const AnnotationPtr& annotation) const {

	// check pre-condition
	assert ( annotation && "Cannot add NULL annotation!" );

	// insert new element
	auto key = annotation->getKey();
	auto value = std::make_pair(key, annotation);
	auto res = map->insert(value);

	if (!res.second) {
		// equivalent element already present => remove old and add new element
		map->erase(res.first);
		res = map->insert(value);
	}

	// check post-condition
	assert ( res.second && "Insert not successful!");
	assert ( hasAnnotation(key) && "Insert not successful!");
	assert ( &*((*map->find(key)).second)==&*annotation && "Insert not successful!");
};


bool hasSameAnnotations(const Annotatable& annotatableA, const Annotatable& annotatableB) {

	// extract maps
	const AnnotationMap& mapA = annotatableA.getAnnotations();
	const AnnotationMap& mapB = annotatableB.getAnnotations();

	// compare maps
	return insieme::utils::map::equal(mapA, mapB, equal_target<AnnotationPtr>());
}


} // end namespace core
} // end namespace insieme

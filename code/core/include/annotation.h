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

#include <memory>
#include <unordered_map>
#include <boost/functional/hash.hpp>

#include "hash_utils.h"
#include "functional_utils.h"

class AnnotationKey : public insieme::utils::HashableImmutableData<AnnotationKey> {
	AnnotationKey(std::size_t hashCode) : HashableImmutableData(hashCode) {};
};


class Annotation {
public:
	virtual const AnnotationKey* getKey() const = 0;
};

typedef std::shared_ptr<Annotation> ManagedAnnotation;
typedef std::unordered_map<const AnnotationKey*, ManagedAnnotation, hash_target<const AnnotationKey*>> AnnotationMap;
typedef std::shared_ptr<AnnotationMap> SharedAnnotationMap;

class Annotatable {
	SharedAnnotationMap map;
public:
	void addAnnotation(const ManagedAnnotation& annotation) {
		map->insert(std::make_pair(annotation->getKey(), annotation));
	};

	ManagedAnnotation getAnnotation(const AnnotationKey* key) const {
		auto pos = map->find(key);
		if (pos == map->end()) {
			return ManagedAnnotation();
		}
		return (*pos).second;
	}

	ManagedAnnotation getAnnotation(const AnnotationKey& key) const {
		return getAnnotation(&key);
	}

	void remAnnotation(const AnnotationKey* key) {
		map->erase(map->find(key));
	}

	void remAnnotation(const AnnotationKey& key) {
		remAnnotation(&key);
	}

	bool contains(const AnnotationKey* key) const {
		return map->find(key) != map->end();
	}

	bool contains(const AnnotationKey& key) const {
		return contains(&key);
	}
};

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

#include <string>
#include <set>

#include "insieme/core/ir_node.h"

namespace insieme {
namespace annotations {
namespace mpi {

struct CallID : public core::NodeAnnotation {

	static const string 					NAME;
	static const utils::StringKey<CallID> 	KEY;
	
	typedef std::set<size_t> DependenceSet;
	typedef DependenceSet::const_iterator const_iterator;

	CallID(size_t id, const DependenceSet& deps = DependenceSet()) : m_id(id), m_deps(deps) { }

	inline bool migrate(const core::NodeAnnotationPtr& ptr, 
						const core::NodePtr& before, 
						const core::NodePtr& after) const { return false; }

	inline const std::string& getAnnotationName() const {return NAME;}
	inline const utils::AnnotationKeyPtr getKey() const { return &KEY; }

	inline std::ostream& printTo(std::ostream& out) const {
		return out << "mpi::call id(" << m_id << ") deps(" << join(", ", m_deps) << ")";
	}

	inline const_iterator deps_begin() const { return m_deps.begin(); }
	inline const_iterator deps_end() const { return m_deps.end(); }

	inline const DependenceSet& deps() const { return m_deps; }

	inline size_t id() const { return m_id; }

private:
	size_t 			m_id;
	DependenceSet 	m_deps;
};

} // end mpi namespace
} // end annotations namespace
} // end insieme namespace


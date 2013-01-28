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

#include "insieme/analysis/access/access_class.h"

namespace insieme {
namespace analysis {
namespace access {

	std::ostream& AccessClass::printTo(std::ostream& out) const {
		return out << "AccessClass(" << uid << ")"
			// print list of accesses in this class
			<< " [" << join(",", accesses, [&](std::ostream& jout, const AccessPtr& cur) {
					jout << cur;
					}) << "]"

		// Print the ID of the parent class if any
		<< " PARENT(" << (!parentClass.expired() ?
					utils::numeric_cast<std::string>(parentClass.lock()->getUID()) :
					"NONE"
					)
			<< ")"

			// Print the direct subclasses for this class
			<< " SUB_CLASSES {" << join(",", subClasses,
					[&](std::ostream& jout, const Dependence& cur) {
					jout << std::get<0>(cur).lock()->getUID() << ":" << cast<Access>(std::get<1>(cur));
					})
		<< "}";
	}


	/** 
	 * Given an access class (which contains accesses to the same memory area, this function returns the
	 * actual addresses from the root used during the analysis 
	 */
	std::set<core::ExpressionAddress> extractRealAddresses(const AccessClass& cl, const TmpVarMap& map) {

		std::set<core::ExpressionAddress> ret;
		std::for_each(cl.begin(), cl.end(), [&](const AccessPtr& cur) { 
				if (cur->isFinal())
					ret.insert(cur->getAddress().getAbsoluteAddress(map).as<core::ExpressionAddress>()); 
			});
		return ret;
	}


	void addSubClasses(const AccessClassPtr& thisClass, AccessClassSet& collect) {

		for (const auto& cur : thisClass->getSubClasses()) {

			auto thisSubClass = std::get<0>(cur).lock();

			if(collect.insert(thisSubClass).second) {
				addSubClasses(thisSubClass, collect);	
			}
		}
	}

	AccessClassSet getConflicting(const AccessClassSet& classes) {
	
		AccessClassSet ret;
		for (const auto& classPtr : classes) {
			addSubClasses(classPtr, ret);

			AccessClassPtr cur = classPtr;

			AccessClassPtr parent;
			while ( parent = cur->getParentClass() )  {
				ret.insert(parent);
				cur = parent;
			}
		}
		return ret;

	}

} // end access namespace 
} // end analysis namespace 
} // end insieme namespace 

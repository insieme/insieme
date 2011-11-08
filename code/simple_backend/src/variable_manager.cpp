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

#include "insieme/simple_backend/variable_manager.h"

#include "insieme/core/ir_expressions.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace simple_backend {

	using namespace core;

	// -------------------------------- Variable Manager -----------------------------------------

	const VariableManager::VariableInfo& VariableManager::getInfo(const VariablePtr& variable) const {
		auto pos = variableMap.find(variable);
		if(pos == variableMap.end()) {
			LOG(INFO) << "Illegal access to variable v" << variable->getId();
		}
		assert(pos != variableMap.end() && "Tried to look up undefined Variable!");
		return (*pos).second;
	}

	void VariableManager::addInfo(const VariablePtr& variable, const VariableManager::VariableInfo& info) {
		auto res = variableMap.insert(std::make_pair(variable, info));
		if (res.second) {
			return;
		}
		variableMap.erase(res.first);
		res = variableMap.insert(std::make_pair(variable,info));
		assert(res.second && "Replacement failed!");
	}

	void VariableManager::removeInfo(const VariablePtr& variable) {
		variableMap.erase(variable);
	}

	bool VariableManager::hasInfoFor(const VariablePtr& variable) const {
		return variableMap.find(variable) != variableMap.end();
	}


} // end namespace simple_backend
} // end namespace insieme

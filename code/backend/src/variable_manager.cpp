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

#include "insieme/backend/variable_manager.h"

#include "insieme/core/ir_expressions.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/type_manager.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	const VariableInfo& VariableManager::getInfo(const core::VariablePtr& var) const {
		// find requested variable within info
		auto pos = infos.find(var);
		if (pos != infos.end()) {
			return pos->second;
		}

		LOG(FATAL) << "Requesting info for unknown variable " << *var << " of type " << *var->getType() << "!!!";

		assert(pos != infos.end() && "Requested variable infos for unknown variable!");
		return pos->second;
	}

	const VariableInfo& VariableManager::addInfo(const Converter& converter, const core::VariablePtr& var, VariableInfo::MemoryLocation location) {
		// forward call more detailed implementation
		return addInfo(converter, var, location, converter.getTypeManager().getTypeInfo(var->getType()));
	}

	const VariableInfo& VariableManager::addInfo(const Converter& converter, const core::VariablePtr& var, VariableInfo::MemoryLocation location, const TypeInfo& typeInfo) {

		// create new variable info instance (implicit)
		VariableInfo& info = infos[var];

		// obtain type info
		info.typeInfo = &typeInfo;

		// obtain name and type of the variable
		c_ast::TypePtr type = (location == VariableInfo::DIRECT)?info.typeInfo->lValueType:info.typeInfo->rValueType;
		c_ast::IdentifierPtr name = converter.getCNodeManager()->create(converter.getNameManager().getName(var));

		info.var = c_ast::var(type, name);
		info.location = location;

		return info;
	}


	void VariableManager::remInfo(const core::VariablePtr& var) {
		// just delete from internal map
		infos.erase(var);
	}

} // end namespace backend
} // end namespace insieme

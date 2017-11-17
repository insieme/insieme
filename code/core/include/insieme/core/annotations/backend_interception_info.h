/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <string>
#include <vector>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node_annotation.h"

/**
 * A header file for annotating backend instantiation informations on nodes
 */

namespace insieme {
namespace core {
namespace annotations {

	struct BackendInterceptionInfo : public value_annotation::copy_on_migration {

		std::string qualifiedName;

		std::vector<std::string> instantiationArguments;

		BackendInterceptionInfo(const std::string& qualifiedName) : qualifiedName(qualifiedName) {}

		BackendInterceptionInfo(const std::string& qualifiedName, const std::vector<std::string>& instantiationArguments) :
			qualifiedName(qualifiedName), instantiationArguments(instantiationArguments) {}

		bool operator==(const BackendInterceptionInfo& other) const {
			return qualifiedName == other.qualifiedName && instantiationArguments == other.instantiationArguments;
		}
	};

	bool hasBackendInterceptionInfo(const insieme::core::NodePtr& node);

	void attachBackendInterceptionInfo(const insieme::core::NodePtr& node, const BackendInterceptionInfo& interceptionInfo);

	BackendInterceptionInfo getBackendInterceptionInfo(const insieme::core::NodePtr& node);

} // end namespace annotations
} // end namespace core
} // end namespace insieme

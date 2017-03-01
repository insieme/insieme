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
 *
 */
#pragma once

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace backend {

	/**
	 * This class offers a list of IR extensions required within the backend. Such
	 * extensions include additional literals representing i.g. C operators or procedures of the
	 * runtime interface.
	 */
	class IRExtensions : public core::lang::Extension {
	  public:
		/**
		 * The name of the global literal introduced by the preprocessor.
		 */
		static const string GLOBAL_ID;

	  private:
		friend class core::NodeManager;

		/**
		 * Creates a new instance of this IRExtension set. The given manager is used to construct
		 * the included literals.
		 *
		 * @param manager the manager to be used to construct the required types and literals
		 */
		IRExtensions(core::NodeManager& manager);

	  public:

		// import reference definitions
		IMPORT_MODULE(core::lang::ReferenceExtension);

	};


} // end namespace backend
} // end namespace insieme

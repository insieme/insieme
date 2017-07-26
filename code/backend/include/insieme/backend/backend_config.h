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

namespace insieme {
namespace backend {

	/**
	 * The class for backend configuration
	 */
	class BackendConfig {
	  public:
		BackendConfig() : mainFunctionName("main"), instrumentMainFunction(false), addIRCodeAsComment(false){};

		std::string mainFunctionName;
		std::vector<std::string> additionalHeaderFiles;
		bool instrumentMainFunction;

		bool addIRCodeAsComment;

		/**
		 * Indicates where the generated OpenCL kernel shall be dumped to.
		 * If non-empty iff OpenCL backend is in use and the user supplied --dump-kernel flag
		 */
		std::string dumpOclKernel;
	};

	typedef std::shared_ptr<BackendConfig> BackendConfigPtr;

} // end namespace backend
} // end namespace insieme

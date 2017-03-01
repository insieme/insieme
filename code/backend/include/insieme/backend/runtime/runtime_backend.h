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

#include "insieme/backend/backend.h"

namespace insieme {
namespace backend {
namespace runtime {

	// A forward declaration of the sequential backend implementation
	class RuntimeBackend;
	typedef std::shared_ptr<RuntimeBackend> RuntimeBackendPtr;

	/**
	 * The facade for the backend capable of generating code to be used by the runtime backend.
	 *
	 * This backend converts the given IR representation into C99 / C++11 target code interacting with
	 * the Insieme Runtime environment.
	 */
	class RuntimeBackend : public Backend {

	  public:
		/**
		 * A constructor of this kind of backend accepting an operator table extender.
		 */
		RuntimeBackend(const BackendConfigPtr& config = std::make_shared<BackendConfig>())
		    : Backend(std::vector<AddOnPtr>(), config) {}


		/**
		 * A factory method obtaining a smart pointer referencing a
		 * fresh instance of the runtime backend using the default configuration.
		 *
		 * @return a smart pointer to a fresh instance of the runtime backend
		 */
		static RuntimeBackendPtr getDefault();


	  protected:
		/**
		 * Creates a converter instance capable of converting IR code into C / C++ code utilizing
		 * the runtime for realizing parallel constructs.
		 *
		 * @param manager the manager to be utilized for the conversion
		 * @return a converter instance conducting the code conversion
		 */
		virtual Converter buildConverter(core::NodeManager& manager) const;
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme

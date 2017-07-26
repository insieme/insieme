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

#include "insieme/backend/runtime/runtime_backend.h"

namespace insieme {
namespace backend {
namespace opencl {

	class OpenCLBackend;
	typedef runtime::RuntimeBackendPtr OpenCLBackendPtr;

	/**
	 * The purpose of this backend is to produce OpenCL powered code target code which can be run by IRT
	 */
	class OpenCLBackend : public runtime::RuntimeBackend {
	public:
		OpenCLBackend(const BackendConfigPtr& config);
		static OpenCLBackendPtr getDefault(const BackendConfigPtr& config = std::make_shared<BackendConfig>());

	protected:
		virtual Converter buildConverter(core::NodeManager& manager) const;
	};

	class KernelBackend;
	typedef std::shared_ptr<KernelBackend> KernelBackendPtr;

	/**
	 * The purpose of this 'sub'-backend is to transform a given OclIR-node into OpenCL-Kernel target code
	 */
	namespace transform { class StepContext; }
	class KernelBackend : public Backend {
		const transform::StepContext& sc;
	public:
		KernelBackend(const transform::StepContext& sc, const BackendConfigPtr& config);
		static KernelBackendPtr getDefault(const transform::StepContext& sc, const BackendConfigPtr& config = std::make_shared<BackendConfig>());

	  protected:
		virtual Converter buildConverter(core::NodeManager& manager) const;
	};

} // end namespace opencl
} // end namespace backend
} // end namespace insieme

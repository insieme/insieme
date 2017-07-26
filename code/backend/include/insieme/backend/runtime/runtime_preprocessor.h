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

#include "insieme/core/forward_decls.h"
#include "insieme/backend/preprocessor.h"

namespace insieme {
namespace backend {
namespace runtime {

	enum class PickImplementationHint { CALL, SWITCH };

	/**
	 * A pre-processor wrapping the entry point of the given code into a newly generated
	 * lambda instantiating and running a standalone version of the insieme runtime.
	 */
	class StandaloneWrapper : public PreProcessor {
	  public:
		virtual core::NodePtr process(const backend::Converter& converter, const core::NodePtr& code) override;

		virtual std::ostream& printTo(std::ostream& out) const override { return out << "StandaloneWrapper"; }
	};

	/**
	 * A pre-processor converting all job expressions, calls to parallel and pfors into runtime
	 * equivalents. After this pass, the resulting program will no longer contain any of those
	 * primitives.
	 *
	 * Yes, the name is a working title ...
	 */
	class WorkItemizer : public PreProcessor {
	  public:
		WorkItemizer() {}
		virtual core::NodePtr process(const backend::Converter& converter, const core::NodePtr& code) override;

		virtual std::ostream& printTo(std::ostream& out) const override { return out << "WorkItemizer"; }
	};

	/**
	 * A pre-processor counting the number of regions marked within the application and adding a call to an
	 * instrumentation init-function forwarding this information to the init_context method.
	 */
	struct InstrumentationSupport : public PreProcessor {
		virtual core::NodePtr process(const backend::Converter& converter, const core::NodePtr& code) override;

		virtual std::ostream& printTo(std::ostream& out) const override { return out << "InstrumentationSupport"; }
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme

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

#include <chrono>
#include <cstdlib>
#include <map>

#include <boost/optional.hpp>

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	typedef void* StablePtr;
	typedef void* HaskellNodeAddress;

	// -- Analysis Result

	template <typename T>
	struct AnalysisResult {
		StablePtr new_context_hs;
		bool timeout;
		T result;
	};

	//  -- Context

	class Context {

		StablePtr context_hs;

		core::NodePtr root;

		std::map<core::NodeAddress, HaskellNodeAddress> addresses;

		std::chrono::microseconds timelimit;

	  public:

		Context();
		Context(const core::NodePtr& node);
		~Context();

		// move semantics
		Context(const Context& other) = delete;
		Context(Context&& other) = delete;

		Context& operator=(const Context& other) = delete;
		Context& operator=(Context&& other) = delete;

		std::chrono::microseconds getTimelimit() const;
		void setTimelimit(std::chrono::microseconds t);

		// -- general context interface requirements --

		void dumpStatistics() const;

		void dumpSolution(const std::string& filenamePrefix = "solution", bool generateGraph = false) const;


		// -- haskell engine specific requirements --

		StablePtr getHaskellContext() const;
		void setHaskellContext(StablePtr);

		void setRoot(const core::NodePtr&);
		core::NodePtr getRoot() const;

		HaskellNodeAddress resolveNodeAddress(const core::NodeAddress& addr);
		core::NodeAddress resolveNodeAddress(const HaskellNodeAddress& addr);

		template <typename T>
		boost::optional<T> unwrapResult(AnalysisResult<T*>* result) {
			assert_true(result);

			boost::optional<T> ret = {};

			if(!result->timeout) {
				setHaskellContext(result->new_context_hs);

				ret = std::move(*result->result);
			}

			// even on timeout result->result holds an allocated object
			delete result->result;

			free(result);
			return ret;
		}

		template <typename T>
		boost::optional<T> unwrapResult(AnalysisResult<T>* result) {
			assert_true(result);

			boost::optional<T> ret = {};

			if(!result->timeout) {
				setHaskellContext(result->new_context_hs);

				ret = result->result;
			}

			free(result);
			return ret;
		}

	  private:

		void clear();

	};

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

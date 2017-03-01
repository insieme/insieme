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

#include <string>
#include <boost/timer.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace utils {

	/**
	 * Simple timer used to measured time.
	 */
	class Timer : public boost::timer {
		double lastStep;
		double mElapsed;
		std::string mName;
		bool isStopped;

		friend std::ostream& operator<<(std::ostream& out, const Timer& timer);

	  public:
		Timer(const std::string& name = "Time") : boost::timer(), lastStep(0.0), mName(name), isStopped(false) {}
		/**
		 * Stops the timer returning the elapsed amount of seconds
		 */
		double stop();

		/**
		 * Obtains time since start or last lap time.
		 */
		double step();

		/**
		 * Return the elapsed amount of seconds
		 */
		double getTime() const;
	};

	std::ostream& operator<<(std::ostream& out, const Timer& timer);

	template <class Ret, log::Level L = DEBUG>
	Ret measureTimeFor(const std::string& timerName, const std::function<Ret()>& task) {
		Timer timer(timerName);
		Ret ret = task(); // execute the job
		timer.stop();
		LOG(L) << timer;
		return ret;
	}

	// Specialization for void returning functions
	template <log::Level L = DEBUG>
	void measureTimeFor(const std::string& timerName, const std::function<void()>& task) {
		Timer timer(timerName);
		task(); // execute the job
		timer.stop();
		LOG(L) << timer;
	}

} // end utils namespace
} // end insieme namespace

// a macro capturing the time of the given command
#define TIME(CMD)                                                                                                                                              \
	([&]() -> double {                                                                                                                                         \
		insieme::utils::Timer timer;                                                                                                                           \
		CMD;                                                                                                                                                   \
		timer.stop();                                                                                                                                          \
		return timer.getTime();                                                                                                                                \
	})()

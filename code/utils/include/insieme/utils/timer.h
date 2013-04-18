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

#pragma once

#include <string>
#include <boost/timer.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace utils {

	/**
	 * Simple timer used to measured time.
	 */
	class Timer: public boost::timer {
		double mElapsed;
		std::string mName;
		bool isStopped;

		friend std::ostream& operator<<(std::ostream& out, const Timer& timer);
	public:
		Timer(const std::string& name = "Time"): 
			boost::timer(), mName(name), isStopped(false) { }
		/**
		 * Stops the timer returning the elapsed amount of seconds
		 */
		double stop();

		/**
		 * Return the elapsed amount of seconds
		 */
		double getTime() const;
	};

	std::ostream& operator<<(std::ostream& out, const Timer& timer);

	template <class Ret, log::Level L=DEBUG>
	Ret measureTimeFor(const std::string& timerName, const std::function<Ret ()>& task) {
		Timer timer(timerName);
		Ret ret = task(); // execute the job
		timer.stop();
		LOG(L) << timer;
		return ret;
	}

	// Specialization for void returning functions 
	template <log::Level L=DEBUG>
	void measureTimeFor(const std::string& timerName, const std::function<void ()>& task) {
		Timer timer(timerName);
		task(); // execute the job
		timer.stop();
		LOG(L) << timer;
	}

} // end utils namespace
} // end insieme namespace

// a macro capturing the time of the given command
#define TIME(CMD) ([&]()->double { insieme::utils::Timer timer; CMD; timer.stop(); return timer.getTime(); })()


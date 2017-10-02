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

#include "insieme/utils/timer.h"

#include <sstream>
#include <cassert>

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	namespace {
		double curTime() {
			auto t = std::chrono::high_resolution_clock::now().time_since_epoch();
			auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(t).count();
			return ms / 1000.0;
		}
	}

	Timer::Timer(const std::string& name /*= "Time"*/) : lastStep(0.0), mName(name), isStopped(false) {
		startTime = curTime();
	}

	double Timer::elapsed() {
		return curTime() - startTime;
	}

	double Timer::stop() {
		mElapsed = elapsed();
		isStopped = true;
		return mElapsed;
	}

	double Timer::step() {
		double cur = elapsed();
		double res = cur - lastStep;
		lastStep = cur;
		return res;
	}

	double Timer::getTime() const {
		assert_true(isStopped) << "Cannot read time of a running timer.";
		return mElapsed;
	}

	std::ostream& operator<<(std::ostream& out, const Timer& timer) {
		std::string&& time = format("%.3f", timer.getTime());

		std::string&& frame = std::string(timer.mName.size() + time.size() + 14, '*');
		out << std::endl << frame << std::endl;
		out << "* " << timer.mName << ":    " << time << " secs *" << std::endl;
		return out << frame << std::endl;
	}

} // end utils namespace
} // end insieme namespace


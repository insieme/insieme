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

namespace insieme {
namespace utils {

	class Colorize {

		bool colorize;

	  public:

		Colorize(bool colorize = true) : colorize(colorize) {}

		std::string reset()        const { return !colorize ? "" : "\033[0m";    }
		std::string black()        const { return !colorize ? "" : "\033[0;30m"; }
		std::string gray()         const { return !colorize ? "" : "\033[1;30m"; }
		std::string red()          const { return !colorize ? "" : "\033[0;31m"; }
		std::string light_red()    const { return !colorize ? "" : "\033[1;31m"; }
		std::string green()        const { return !colorize ? "" : "\033[0;32m"; }
		std::string light_green()  const { return !colorize ? "" : "\033[1;32m"; }
		std::string brown()        const { return !colorize ? "" : "\033[0;33m"; }
		std::string yellow()       const { return !colorize ? "" : "\033[1;33m"; }
		std::string blue()         const { return !colorize ? "" : "\033[0;34m"; }
		std::string light_blue()   const { return !colorize ? "" : "\033[1;34m"; }
		std::string purple()       const { return !colorize ? "" : "\033[0;35m"; }
		std::string light_purple() const { return !colorize ? "" : "\033[1;35m"; }
		std::string cyan()         const { return !colorize ? "" : "\033[0;36m"; }
		std::string light_cyan()   const { return !colorize ? "" : "\033[1;36m"; }
		std::string light_gray()   const { return !colorize ? "" : "\033[0;37m"; }
		std::string white()        const { return !colorize ? "" : "\033[1;37m"; }

	};

} // end namespace utils
} // end namespace insieme

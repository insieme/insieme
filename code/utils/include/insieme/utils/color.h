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

namespace insieme {
namespace utils {

	class Colorize {

	  private:

		enum Color {
			RESET,
			BLACK,
			GRAY,
			RED,
			LIGHT_RED,
			GREEN,
			LIGHT_GREEN,
			BROWN,
			YELLOW,
			BLUE,
			LIGHT_BLUE,
			PURPLE,
			LIGHT_PURPLE,
			CYAN,
			LIGHT_CYAN,
			LIGHT_GRAY,
			WHITE,
		};

		bool colorize;

	  public:

		Colorize(bool colorize = true) : colorize(colorize) {}

		std::string getColor(Color c) const {
			if(colorize) {
				switch(c) {
				case RESET:        return "\033[0m";
				case BLACK:        return "\033[0;30m";
				case GRAY:         return "\033[1;30m";
				case RED:          return "\033[0;31m";
				case LIGHT_RED:    return "\033[1;31m";
				case GREEN:        return "\033[0;32m";
				case LIGHT_GREEN:  return "\033[1;32m";
				case BROWN:        return "\033[0;33m";
				case YELLOW:       return "\033[1;33m";
				case BLUE:         return "\033[0;34m";
				case LIGHT_BLUE:   return "\033[1;34m";
				case PURPLE:       return "\033[0;35m";
				case LIGHT_PURPLE: return "\033[1;35m";
				case CYAN:         return "\033[0;36m";
				case LIGHT_CYAN:   return "\033[1;36m";
				case LIGHT_GRAY:   return "\033[0;37m";
				case WHITE:        return "\033[1;37m";
				}
			}
			return "";
		}

		std::string reset()        const { return getColor(RESET);        }
		std::string black()        const { return getColor(BLACK);        }
		std::string gray()         const { return getColor(GRAY);         }
		std::string red()          const { return getColor(RED);          }
		std::string light_red()    const { return getColor(LIGHT_RED);    }
		std::string green()        const { return getColor(GREEN);        }
		std::string light_green()  const { return getColor(LIGHT_GREEN);  }
		std::string brown()        const { return getColor(BROWN);        }
		std::string yellow()       const { return getColor(YELLOW);       }
		std::string blue()         const { return getColor(BLUE);         }
		std::string light_blue()   const { return getColor(LIGHT_BLUE);   }
		std::string purple()       const { return getColor(PURPLE);       }
		std::string light_purple() const { return getColor(LIGHT_PURPLE); }
		std::string cyan()         const { return getColor(CYAN);         }
		std::string light_cyan()   const { return getColor(LIGHT_CYAN);   }
		std::string light_gray()   const { return getColor(LIGHT_GRAY);   }
		std::string white()        const { return getColor(WHITE);        }
	};

} // end namespace utils
} // end namespace insieme

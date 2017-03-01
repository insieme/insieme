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

#include <ostream>
#include <iostream>

namespace insieme {
namespace utils {

	struct Printable;
	struct VirtualPrintable;

}
}

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::utils::VirtualPrintable& printable);

}

namespace insieme {
namespace utils {

	/**
	 * A class forming an interface for printable classes. Implementing this interface allows
	 * classes to be printed to output streams using a member function.
	 */
	struct Printable {
		/**
		 * A method to be implemented by sub-classes allowing instances to be printed to the
		 * output stream.
		 *
		 * @param out the stream this instance should be printed to
		 * @return the stream passed as an argument
		 */
		// std::ostream& printTo(std::ostream& out) const { .. }
	};

	/**
	 * A base class for all printable objects within a type hierarchy depending on a virtual printTo function.
	 */
	struct VirtualPrintable : public Printable {
		/**
		 * Allow the output operator to access protected members.
		 */
		friend std::ostream& std::operator<<(std::ostream&, const VirtualPrintable&);

	  protected:
		/**
		 * A method to be implemented by sub-classes allowing instances to be printed to the
		 * output stream.
		 *
		 * @param out the stream this instance should be printed to
		 * @return the stream passed as an argument
		 */
		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

} // end of namespace utils
} // end of namespace insieme

namespace std {

	template <typename T>
	typename std::enable_if<std::is_base_of<insieme::utils::Printable, T>::value && !std::is_base_of<insieme::utils::VirtualPrintable, T>::value,
	                        std::ostream&>::type
	operator<<(std::ostream& out, const T& printable) {
		return printable.printTo(out);
	}
}

namespace insieme {
namespace utils {


	/**
	 * A class forming an adapter from class supporting the output operator <<
	 * and classes implementing the printable interface.
	 */
	template <typename T>
	class PrintWrapper : public VirtualPrintable {
		const T& content;

	  public:
		PrintWrapper(const T& content) : content(content){};
		std::ostream& printTo(std::ostream& out) const {
			return out << content;
		}
	};

	template <typename T>
	PrintWrapper<T> toVirtualPrintable(const T& element) {
		return PrintWrapper<T>(element);
	}

} // end of namespace utils
} // end of namespace insieme

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::utils::VirtualPrintable& printable) {
		return printable.printTo(out);
	}

} // end of namespace std

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

#include <ostream>
#include <iostream>

namespace insieme {
namespace utils {
	class Printable;
	class VirtualPrintable;
}
}

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::utils::VirtualPrintable& printable);

	template<typename T>
	typename std::enable_if<
			std::is_base_of<insieme::utils::Printable, T>::value &&
			!std::is_base_of<insieme::utils::VirtualPrintable, T>::value
		, std::ostream&>::type
	operator<<(std::ostream& out, const T& printable) {
		return printable.printTo(out);
	}
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
		// std::ostream& printToInternal(std::ostream& out) const { .. }

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
		virtual std::ostream& printTo(std::ostream& out) const =0;
	};


	/**
	 * A class forming an adapter from class supporting the output operator <<
	 * and classes implementing the printable interface.
	 */
	template<typename T>
	class PrintWrapper : public VirtualPrintable {
		const T& content;
	public:
		PrintWrapper(const T& content) : content(content) {};
		std::ostream& printTo(std::ostream& out) const { return out << content; }
	};

	template<typename T>
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


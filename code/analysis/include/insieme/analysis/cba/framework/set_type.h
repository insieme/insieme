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
#include <functional>

#include <boost/noncopyable.hpp>

#include "insieme/analysis/cba/framework/forward_decl.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::string;

	// ----------- set types ------------------

	/**
	 * An abstract base class for set types to be handled by the analysis.
	 * Instances are expected to be immutable global constants.
	 */
	class SetType : public boost::noncopyable {

		/**
		 * The name of this set for printing and debugging issues.
		 */
		const string name;

	protected:

		SetType(const string& name) : name(name) {}

	public:

		const string& getName() const {
			return name;
		}

		bool operator==(const SetType& other) const {
			// the identity of a set type is fixed by its address
			return this == &other;
		}

		bool operator!=(const SetType& other) const {
			return !(*this == other);
		}

	};

	// all set types are global constants => plain pointers can be used safely
	typedef const SetType* SetTypePtr;

	/**
	 * A special type of set type fixing the element type of the represented type.
	 */
	template<
		typename E,
		template<typename C> class R
	>
	struct TypedSetType : public SetType {

		// expose member types
		typedef E element_type;
		template<typename T> struct resolver_type { typedef R<T> type; };

		/**
		 * A simple constructor just forwarding the name of the resulting set.
		 */
		TypedSetType(const string& name) : SetType(name) {}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

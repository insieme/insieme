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

#include <cstdint>

#include <boost/operators.hpp>

#include "insieme/utils/printable.h"
#include "insieme/core/arithmetic/arithmetic.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A base class for indices to be utilized for analyzing operations
	 * on data structures within data values.
	 */
	template<typename DerivedIndex>
	struct Index :
			public boost::equality_comparable<DerivedIndex>,
			public boost::partially_ordered<DerivedIndex>,
			public utils::Printable
	{};

	/**
	 * A base class for all indices actually modeling array indices.
	 */
	template<typename DerivedIndex>
	struct ArrayIndex : public Index<DerivedIndex> {};


	// -------------------------------------------------------------------------------------------------

	/**
	 * An index distinguishing different named fields.
	 */
	class NominalIndex : public Index<NominalIndex> {

		/**
		 * The name to be represented - as a reference to save the copy-overhead.
		 */
		string name;

		std::size_t hashCode;

	public:

		NominalIndex(const string& name = "") : name(name), hashCode(std::hash<string>()(name)) {};

		NominalIndex(const NominalIndex& other) : name(other.name), hashCode(other.hashCode) {}

		bool operator==(const NominalIndex& other) const {
			return this == &other || name == other.name;
		}

		bool operator<(const NominalIndex& other) const {
			return name < other.name;
		}

//		NominalIndex& operator=(const NominalIndex& other) {
//			return *new (this) NominalIndex(other.name);
//		}

		std::size_t hash() const {
			return hashCode;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

	};

	/**
	 * Adds hashing support for the nominal index class.
	 */
	inline std::size_t hash_value(const NominalIndex& index) {
		return index.hash();
	}

	// -------------------------------------------------------------------------------------------------

	/**
	 * The simplest form of an index - there is only a single instance, all array operations are
	 * applied to the same instance.
	 */
	class UnitIndex : public ArrayIndex<UnitIndex> {

	public:

		UnitIndex() {}
		UnitIndex(const core::arithmetic::Formula& f) {}

		bool operator==(const UnitIndex& other) const {
			return true;	// all instances are equivalent
		}

		bool operator<(const UnitIndex& other) const {
			return false;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "*";
		}

	};

	/**
	 * Adds hashing support for the unit index class.
	 */
	inline std::size_t hash_value(const UnitIndex& index) {
		return 0;	// all the same
	}

	/**
	 * An index distinguishing concrete indices (if the value of a subscript is known) from cases
	 * where the index is unknown or too complex to be evaluated statically.
	 */
	class SingleIndex : public ArrayIndex<SingleIndex> {

		/**
		 * A flag determining whether this instances is representing a concrete index
		 * or an unknown index.
		 */
		bool concrete;

		/**
		 * The represented index - if it is known.
		 */
		std::uint64_t index;

	public:

		SingleIndex() : concrete(false), index(0) {}

		SingleIndex(std::uint64_t index) : concrete(true), index(index) {}

		SingleIndex(const core::arithmetic::Formula& f)
			: concrete(f.isInteger()), index((concrete)?f.getIntegerValue():0) {}

		bool operator==(const SingleIndex& other) const {
			if (this == &other) return true;
			return concrete == other.concrete && index == other.index;
		}

		bool operator<(const SingleIndex& other) const {
			if (!concrete) return false;
			if (!other.concrete) return true;
			return index < other.index;
		}

		bool isConcrete() const {
			return concrete;
		}

		std::uint64_t getIndex() const {
			assert(isConcrete());
			return index;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!concrete) return out << "*";
			return out << index;
		}

	};

	/**
	 * Adds hashing support for the single index class.
	 */
	inline std::size_t hash_value(const SingleIndex& index) {
		return (index.isConcrete()) ? std::hash<std::uint64_t>()(index.getIndex()) : 743;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

#include "insieme/analysis/cba/framework/data_index.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/hash_utils.h"

#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace {

		/**
		 * An abstract base class forming elements along data paths - all elements are immutable
		 * and shared if possible. The memory management is handled internally using reference
		 * counting.
		 *
		 * This class is a base class for all kind of elements - which are specialized depending
		 * on the index to be utilized for navigating data objects.
		 */
		class DataPathElement :
				public utils::VirtualPrintable,
				public utils::HashableImmutableData<DataPathElement>,
				public boost::equality_comparable<DataPathElement> {

		private:

			// ref-counting variable
			mutable std::size_t refCount;

		protected:

			typedef const DataPathElement* ptr_type;
			const ptr_type head;

		public:

			DataPathElement(ptr_type head, std::size_t hash)
				: HashableImmutableData<DataPathElement>(hash), refCount(0), head(head) {
				if(head) head->incRefCount();
			}

			virtual ~DataPathElement() {
				assert_eq(refCount, 0) << "Destroyed with invalid ref counter!";
				if (head) head->decRefCount();
			}

			void incRefCount() const {
				refCount++;
			}

			void decRefCount() const {
				--refCount;
				if (refCount == 0) delete this;
			}

			ptr_type getParent(unsigned levels = 1) const {
				if (levels == 0) return this;
				if (levels == 1) return head;
				assert_true(head) << "No parent level " << levels << " present!";
				return head->getParent(levels-1);
			}

			bool operator==(const DataPathElement& other) const {

				// check identity
				if (this == &other) return true;

				// check hashes
				if (this->hash() != other.hash()) return false;

				// check index part
				if (!equalIndex(other)) return false;

				// check head part
				return equalTarget(head, other.head);
			}

			virtual bool equalIndex(const DataPathElement& other) const =0;
		};


		/**
		 * A template for concrete data path element types. Each data path step
		 * is represented by an instance of this class which is encapsulating the index
		 * step to be taken along the path.
		 *
		 * @tparem Index the type of index to be followed within a data object
		 */
		template<typename Index>
		class ConcreteDataPathElement : public DataPathElement {

			Index index;

		public:

			ConcreteDataPathElement(ptr_type head, const Index& index)
				: DataPathElement(head, utils::combineHashes((head)?hash_value(*head):0, hash_value(index))), index(index) {}

			std::ostream& printTo(std::ostream& out) const {
				if (head) {
					out << *head << ".";
				} else {
					out << "#.";
				}
				return out << index;
			}

			virtual bool equalIndex(const DataPathElement& other) const {
				// check type and identical index
				return typeid(*this) == typeid(other) && index == static_cast<const ConcreteDataPathElement&>(other).index;
			}

		};

	} // end anonymous namespace


	/**
	 * A class representing data paths within data objects. Every allocated object possess an
	 * internal data structure (e.g. it is a simple scalar, a struct of scalars, a struct with
	 * nested structs, an array of structs, ...). A data path is describing the abstract path
	 * from the root of this representation to a sub-structure or even an individual element.
	 */
	class DataPath :
			public utils::Printable, public utils::Hashable,
			public boost::equality_comparable<DataPath> {

		typedef const DataPathElement* ptr_type;
		ptr_type path;

	public:

		DataPath(ptr_type ptr = nullptr) : path(ptr) {
			if (path) path->incRefCount();
		}

		DataPath(DataPath&& other) : path(other.path) {
			other.path = nullptr;
		}

		DataPath(const DataPath& other) : path(other.path) {
			if (path) path->incRefCount();
		}

		~DataPath() {
			if (path) path->decRefCount();
		}

		DataPath& operator=(const DataPath& other) {
			if (path == other.path) return *this;
			if (path) path->decRefCount();
			path = other.path;
			if (path) path->incRefCount();
			return *this;
		}

		bool operator==(const DataPath& other) const {
			// check identity
			if (this == &other) return true;

			// check for equal path element
			if (path == other.path) return true;

			// check whether one of the paths is null (both null handled above)
			if ((!path && other.path) || (path && !other.path)) return false;

			// check hash
			if (this->hash() != other.hash()) return false;

			// compare the represented paths
			return equalTarget(path, other.path);
		}

		// path concatenation
		template<typename Index>
		DataPath& operator<<=(const Index& index) {
			// the memory allocated here is managed by its own reference counting
			auto element = new ConcreteDataPathElement<Index>(path, index);
			assert_ne(path, element);
			element->incRefCount();
			if(path) path->decRefCount();
			path = element;
			return *this;
		}

		// path concatenation
		template<typename Element>
		DataPath operator<<(const Element& element) const {
			return DataPath(*this) <<= element;
		}

		// eliminating tailing elements from this path
		DataPath pop(unsigned levels = 1) const {
			if (levels == 0) return *this;
			assert_true(path) << "No such parent!";
			return DataPath(path->getParent(levels));
		}

		std::ostream& printTo(std::ostream& out) const {
			if (!path) return out << "#";
			return out << *path;
		}

		std::size_t hash() const {
			return (path) ? path->hash() : 0;
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

#include <typeindex>

#include "insieme/analysis/cba/framework/entities/data_index.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/hash_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace detail {

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

			// the type of index used by the sub-class
			std::type_index index_type;

			// ref-counting variable
			mutable std::size_t refCount;

		protected:

			typedef const DataPathElement* ptr_type;
			const ptr_type head;

			// the length of this path
			std::size_t length;

		public:

			DataPathElement(std::type_index index_type, ptr_type head, std::size_t hash)
				: HashableImmutableData<DataPathElement>(hash), index_type(index_type), refCount(0), head(head), length((head)?head->length+1:1) {
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

				// check index type
				if (index_type != other.index_type) return false;

				// check index part
				if (!equalIndex(other)) return false;

				// check head part
				return equalTarget(head, other.head);
			}

			bool operator<(const DataPathElement& other) const {
				// quick test - equality
				if (this==&other) {
					return false;
				}

				// make sure this one is the shorter path
				if (length > other.length) {
					return !(other < *this);
				}

				// reduce length to equal size
				if (length < other.length) {
					return (*this == *other.head) || (*this < *(other.head));
				}

				// now the length is equally long
				assert(length == other.length);

				// implement lexicographical order
				return *head < *other.head || (*head == *other.head && lessIndex(other));
			}

			ptr_type operator<<(const ptr_type& other) const {
				// if there is nothing to append we are done
				if (!other) return this;

				// append recursively parent-list
				ptr_type res = this->operator<<(other->head);

				// build new node with updated header list and current index
				return other->createCopyWith(res);
			}

			ptr_type appendHead(const DataPathElement& element) const {
				return element.createCopyWith(this);
			}

			template<typename Op>
			void visit(const Op& op) const {
				if (head) head->visit(op);
				op(*this);
			}

			template<typename Op>
			void visitReverse(const Op& op) const {
				op(*this);
				if (head) head->visit(op);
			}

			template<typename Manager>
			typename Manager::value_type createEmpty(Manager& mgr) const {
				return mgr.createEmpty(index_type);
			}

			virtual bool equalIndex(const DataPathElement& other) const =0;
			virtual bool lessIndex(const DataPathElement& ohter) const =0;
			virtual ptr_type createCopyWith(const ptr_type& head) const =0;
			virtual bool isOverlapping(const DataPathElement& other) const =0;

			virtual std::ostream& printTo(std::ostream& out) const =0;
			virtual std::ostream& printToReverse(std::ostream& out) const =0;
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
				: DataPathElement(typeid(Index), head, utils::combineHashes((head)?hash_value(*head):0, hash_value(index))), index(index) {}

			std::ostream& printTo(std::ostream& out) const {
				if (head) {
					out << *head << ".";
				} else {
					out << "#.";
				}
				return out << index;
			}

			std::ostream& printToReverse(std::ostream& out) const {
				out << index;
				if (head) {
					out << ".";
					head->printToReverse(out);
				} else {
					out << ".#";
				}
				return out;
			}

			const Index& getIndex() const {
				return index;
			}

			virtual bool equalIndex(const DataPathElement& other) const {
				// check type and identical index
				return typeid(*this) == typeid(other) && index == static_cast<const ConcreteDataPathElement&>(other).index;
			}

			virtual bool lessIndex(const DataPathElement& other) const {
				// check type and compare index
				return typeid(*this) == typeid(other) && index < static_cast<const ConcreteDataPathElement&>(other).index;
			}

			virtual ptr_type createCopyWith(const ptr_type& head) const {
				return new ConcreteDataPathElement<Index>(head, index);
			}

			virtual bool isOverlapping(const DataPathElement& other) const {
				// it is a different type (conservative) or overlapping indices
				return typeid(*this) != typeid(other) || overlap(index, static_cast<const ConcreteDataPathElement&>(other).index);
			}
		};

		inline bool less_than(const DataPathElement* a, const DataPathElement* b) {
			// if equal => not less
			if (a == b) return false;

			// if a is null, it is less
			if (!a && b) return true;

			// if b is null, it is not bigger
			if (!b) return false;

			// now both should be not null
			return *a < *b;
		}

		// a type-definition for a managed data path element
		typedef const detail::DataPathElement* DataPathElementPtr;

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

		typedef const detail::DataPathElement* ptr_type;
		ptr_type path;

		// a flag indicating whether this data path is up or down (upwards the data structure or downwards)
		bool down;

	public:

		DataPath(ptr_type ptr = nullptr) : path(ptr), down(true) {
			if (path) path->incRefCount();
		}

		DataPath(DataPath&& other) : path(other.path), down(other.down) {
			other.path = nullptr;
		}

		DataPath(const DataPath& other) : path(other.path), down(other.down) {
			if (path) path->incRefCount();
		}

		~DataPath() {
			if (path) path->decRefCount();
		}

		DataPath& operator=(const DataPath& other) {
			// update down flag (cheap)
			down = other.down;
			// update the data path
			if (path == other.path) return *this;
			if (path) path->decRefCount();
			path = other.path;
			if (path) path->incRefCount();
			return *this;
		}

		bool operator==(const DataPath& other) const {
			// check identity
			if (this == &other) return true;

			// if both are root paths it is equivalent
			if (!path && !other.path) return true;

			// check for equal direction
			if (down != other.down) return false;

			// check for equal path element
			if (path == other.path) return true;

			// check whether one of the paths is null (both null handled above)
			if ((!path && other.path) || (path && !other.path)) return false;

			// check hash
			if (this->hash() != other.hash()) return false;

			// compare the represented paths
			return equalTarget(path, other.path);
		}

		bool operator<(const DataPath& other) const {
			// compare direction
			if (down && !other.down) return true;
			if (!down && other.down) return false;
			assert_eq(down, other.down);

			// compare paths (depending on direction
			return  ( down && detail::less_than(path, other.path)) ||
					(!down && detail::less_than(other.path, path));
		}

		// ----------------------------------------------------------------
		//								Narrow
		// ----------------------------------------------------------------

	private:

		// path concatenation
		DataPath& moveDown(const detail::DataPathElement& element) {

			// check whether data path points into wrong direction
			if (!down && !isRoot()) {
				// remove top-most element from path (needs to be equivalent to element)
				popHead(element);
				return *this;
			}

			// fix direction
			down = true;

			// extend path by given element
			auto newPath = path->appendHead(element);
			newPath->incRefCount();
			if(path) path->decRefCount();
			path = newPath;
			return *this;
		}

	public:

		template<typename Index>
		DataPath& operator<<=(const Index& index) {
			// turn index into a path element and add it
			moveDown(detail::ConcreteDataPathElement<Index>(path, index));
			return *this;
		}


		DataPath& operator<<=(const DataPath& extension) {

			// deal with empty extension
			if (extension.isRoot()) return *this;

			// deal with empty local state
			if (isRoot()) {
				path = extension.path;
				path->incRefCount();
				down = extension.down;
				return *this;
			}

			// in all other case append paths step by step
			if (extension.isNarrow()) {
				extension.visit([&](const detail::DataPathElement& cur){ moveDown(cur); });
			} else {
				assert(extension.isExpand());
				extension.visit([&](const detail::DataPathElement& cur) { moveUp(cur); });
			}

			// done
			return *this;
		}

		// path concatenation
		template<typename Element>
		DataPath operator<<(const Element& element) const {
			return DataPath(*this) <<= element;
		}

		DataPath operator<<(const DataPath& path) const {
			return DataPath(*this) <<= path;
		}

		// ----------------------------------------------------------------
		//								Expand
		// ----------------------------------------------------------------

	private:

		// path concatenation
		DataPath& moveUp(const detail::DataPathElement& element) {

			// check whether data path points into wrong direction
			if (down && !isRoot()) {
				// remove top-most element from path (needs to be equivalent to element)
				popHead(element);
				return *this;
			}

			// fix direction
			down = false;


			// extend path by given element
			auto newPath = path->appendHead(element);
			newPath->incRefCount();
			if(path) path->decRefCount();
			path = newPath;
			return *this;
		}

	public:

		template<typename Index>
		DataPath& operator>>=(const Index& index) {
			// turn index into a path element and add it
			moveUp(detail::ConcreteDataPathElement<Index>(path, index));
			return *this;
		}


		DataPath& operator>>=(const DataPath& extension) {

			// deal with empty extension
			if (extension.isRoot()) return *this;

			// deal with empty local state
			if (isRoot()) {
				path = extension.path;
				path->incRefCount();
				down = !extension.down;
				return *this;
			}

			// in all other case append paths step by step
			if (extension.isNarrow()) {
				extension.visit([&](const detail::DataPathElement& cur){ moveUp(cur); });
			} else {
				assert(extension.isExpand());
				extension.visit([&](const detail::DataPathElement& cur) { moveDown(cur); });
			}

			// done
			return *this;
		}

		// path concatenation
		template<typename Element>
		DataPath operator>>(const Element& element) const {
			return DataPath(*this) >>= element;
		}

		DataPath operator>>(const DataPath& path) const {
			return DataPath(*this) >>= path;
		}

		// eliminating tailing elements from this path
		DataPath pop(unsigned levels = 1) const {
			if (levels == 0) return *this;
			assert_true(path) << "No such parent!";
			return DataPath(path->getParent(levels));
		}

		bool isRoot() const {
			return !path;
		}

		bool isExpand() const {
			return !down || isRoot();
		}

		bool isNarrow() const {
			return down || isRoot();
		}

		template<typename Op>
		void visit(const Op& op) const {
			if (!path) return;
			path->visit(op);
		}

		template<typename Op>
		void visitReverse(const Op& op) const {
			if (!path) return;
			path->visitReverse(op);
		}

		template<typename Manager>
		typename Manager::value_type createEmpty(Manager& mgr) const {
			return path->createEmpty(mgr);
		}

		vector<detail::DataPathElementPtr> getSteps() const {
			vector<detail::DataPathElementPtr> res;
			visit([&](const detail::DataPathElement& cur) { res.push_back(&cur); });
			return res;
		}

		bool isOverlapping(const DataPath& other) const {

			// if they are equivalent it is for sure overlapping
			if (*this == other) return true;

			// if they work in different directions => they are overlapping
			if (isExpand() && other.isNarrow()) return true;
			if (isNarrow() && other.isExpand()) return true;


			auto stepsA = getSteps();
			auto stepsB = other.getSteps();

			// check common path for divergences
			size_t steps = std::min(stepsA.size(), stepsB.size());
			for(size_t i=0; i<steps; i++) {
				if (!stepsA[i]->isOverlapping(*(stepsB[i]))) return false;
			}

			// it is overlapping
			return true;
		}

		std::ostream& printTo(std::ostream& out) const {
			if (!path) return out << "#";
			return (down) ? path->printTo(out) : path->printToReverse(out);
		}

		std::size_t hash() const {
			auto res = (path) ? path->hash() : 0;
			return (down) ? res : ~res;
		}

	private:

		void popHead(const detail::DataPathElement& element) {
			assert_true(path) << "Unable to delete head of empty list!";

			// make sure the head index is the given value
			assert_true(path->isOverlapping(element))
				<< "Path:    " << *path << "\n"
				<< "Element: " << element << "\n";

			// remove the head
			popHead();
		}

		void popHead() {

			// get involved nodes
			auto oldPath = path;
			auto newPath = path->getParent();

			// update path
			path = newPath;

			// update reference counting
			if (path) path->incRefCount();
			oldPath->decRefCount();
		}

	};

	typedef typename vector<detail::DataPathElementPtr>::const_iterator data_path_iterator;

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

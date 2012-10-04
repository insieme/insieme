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

#include "insieme/core/ir_address.h"

#include "insieme/utils/hash_utils.h"

namespace insieme {
namespace core {

namespace detail {


	/**
	 * One element / step within a IR Address path.
	 */
	struct PathElement {

		/**
		 * The pointer to the node referenced by this path element.
		 */
		const NodePtr ptr;

		/**
		 * The index of this node within its parents child list.
		 */
		const std::size_t index;

		/**
		 * The path element addressing the parent node. If set to null, this element is
		 * considered to reference the root element.
		 */
		const PathElement* const parent;

		/**
		 * The depth of this path element. The depth is equivalent to the number of nodes along
		 * the path from the root node to this path element.
		 */
		const std::size_t depth;

		/**
		 * The hash code for the path ending at this element.
		 */
		const std::size_t hash;

	private:

		/**
		 * A reference counter for memory management. This counter contains the
		 * number of times this element is referenced by other objects. Within the constructor
		 * the ref counter is set to 1. When decreasing it to 0, the instance will automatically
		 * be freed.
		 */
		mutable std::size_t refCount;

	public:

		/**
		 * Creates a new path element using the given values. The internal ref-counter will
		 * be set to 0.
		 *
		 * @param ptr a pointer to the node addressed by this element path
		 * @param index the index of the addressed node within its parent node.
		 * @param parent a pointer to the path element referencing the parent node - null if this element is referencing the root node.
		 */
		PathElement(const NodePtr& ptr, std::size_t index, const PathElement* const parent)
			: ptr(ptr), index(index), parent(parent), depth((parent)?parent->depth+1:1), hash(computeHash(ptr, index, parent)), refCount(0) {

			if (parent) parent->incRefCount();
		};

	private:

		/**
		 * A private destructor for path elements. Having this one private effectively prevents instances on
		 * the stack and people to invoke delete on heap allocated instances.
		 */
		~PathElement() {
			// reduce reference counter in parent node
			if (parent) parent->decRefCount();
		}

	public:

		/**
		 * Obtains the path element referencing the root node of this path.
		 * @return a pointer to the requested path element
		 */
		const PathElement* getRoot() const {
			return (parent)?(parent->getRoot()):this;
		}

		/**
		 * Obtains the parent on the given level of this path.
		 *
		 * @param level if set to 0, it will be this node. If set to 1, it will be
		 * 				the immediate parent, 2 the parents parent and so forth. Must not
		 * 				be larger or equal the depth of this node.
		 * @return a pointer to the requested path element
		 */
		const PathElement* getParent(unsigned level = 1) const {
			assert(level < depth);
			return (level==0)?this:parent->getParent(level-1);
		}

		/**
		 * Verifies whether this path element is valid. A node is valid if it is a root node
		 * or when the referenced node is the correct child of the parent node.
		 */
		bool isValid() const {
			// valid if: there is no parent or the referenced node is the correct child of the parent
			return !parent || parent->ptr->getChildList()[index] == ptr;
		}

		/**
		 * Increment the reference counter for this path element.
		 */
		void incRefCount() const {
			++refCount;
		}

		/**
		 * Decrement the reference counter for this path element.
		 */
		std::size_t decRefCount() const {
			assert(refCount > 0);
			int res = --refCount;
			if (refCount == 0) {
				// commit suicide
				delete this;
			}
			return res;
		}

		/**
		 * Obtains the current reference count value.
		 */
		std::size_t getRefCount() const {
			return refCount;
		}

		/**
		 * Computes a hash code for the given combination of values.
		 *
		 * @param ptr the node pointer to be pointing to
		 * @param index the index of the node within its parent's child list
		 * @param the parent of the given path element.
		 * @return a proper hash value for a node based on the given values
		 */
		static std::size_t computeHash(const NodePtr& ptr, std::size_t index, const PathElement* const parent) {
			std::size_t seed = boost::hash_value(index);
			boost::hash_combine(seed, *ptr);
			boost::hash_combine(seed, ((parent)?parent->hash:0));
			return seed;
		}

		/**
		 * An equality operator to compare the path ending at this node with the path ending at the
		 * given other node.
		 *
		 * @param other the path to be compared to
		 */
		bool operator==(const PathElement& other) const {
			if (this == &other) {
				return true;
			}

			// quick-check hash
			if (hash != other.hash) {
				return false;
			}

			// compare root nodes
			if (!parent && !other.parent) {
				// compare associated pointer
				return *ptr == *other.ptr;
			}

			// compare the rest (cheap stuff first)
			return	(index == other.index) && (*parent == *other.parent);
		}

		/**
		 * Realizes a lexicographical order on the address paths.
		 */
		bool operator<(const PathElement& other) const {

			// quick test - shortcut
			if (this==&other) {
				return false;
			}

			// make sure this one is the shorter path
			if (depth > other.depth) {
				return !(other < *this);
			}

			// reduce length to equal size
			if (depth < other.depth) {
				return (*this == *other.parent) || (*this < *(other.parent));
			}

			// now the length is equally long
			assert(depth == other.depth);

			// implement lexicographical order
			return *parent < *other.parent || (*parent == *other.parent && index < other.index);

		}

//
// TODO: re-evaluate the pool support for those small objects => it was slower the last time
//
//		void* operator new(std::size_t size);
//
//		void operator delete(void* raw, std::size_t size);

	};
//
//	/**
//	 * A dummy type to make the element type pool unique.
//	 */
//	struct PathElementPoolTag {};
//
//	/**
//	 * Define the pool to be used for path element memory allocation.
//	 */
//	typedef boost::singleton_pool<PathElementPoolTag, sizeof(PathElement)> PathElementPool;
//
//
//	void* PathElement::operator new(std::size_t size) {
//
//		// check size for proper input ...
//		if (size != sizeof(PathElement)) {
//			// ... otherwise let the default implementation handle it
//			return ::operator new(size);
//		}
//
//		// allocate element within pool
//		void* res = PathElementPool::malloc();
//		if (res) {
//			return res;
//		}
//
//		// not successful => let default implementation deal with it
//		return ::operator new(size);
//	}
//
//	void PathElement::operator delete(void* raw, std::size_t size) {
//
//		// check size and membership
//		//if (size != sizeof(PathElement) || !PathElementPool::is_from(raw)) {
//		if (size != sizeof(PathElement)) {
//			// default is doing the job
//			::operator delete(raw);
//			return;
//		}
//
//		// remove element from pool
//		PathElementPool::free(raw);
//
//	}

}

Path::Path(const detail::PathElement* element) : element(element) {
	if (element) element->incRefCount();
}

Path::Path(const NodePtr& node) : element(new detail::PathElement(node, 0, NULL)) {
	element->incRefCount();
}

Path::Path(const Path& other) : element(other.element) {
	if (element) element->incRefCount();
}

Path::Path(Path&& other) : element(other.element) {
	other.element = NULL;
};

Path::~Path() {
	// decrease the element's ref-counter. If the ref-counter
	// is zero, the element will be deleted automatically.
	if (element) element->decRefCount();
}

std::size_t Path::hashSingleStepPath(const NodePtr& node) {
	return detail::PathElement::computeHash(node, 0, NULL);
}

const NodePtr& Path::getAddressedNode() const {
	return element->ptr;
}

const NodePtr& Path::getRootNode() const {
	return element->getRoot()->ptr;
}

Path Path::switchRoot(const NodePtr& newRoot) const {
	if (getLength() <= 1) {
		return Path(newRoot);
	}
	return getPathToParent().switchRoot(newRoot).extendForChild(getIndex());
}

const NodePtr& Path::getParentNode(unsigned level) const {
	return element->getParent(level)->ptr;
}

Path Path::getPathToRoot() const {
	return Path(element->getRoot());
}

Path Path::getPathToParent(unsigned level) const {
	return Path(element->getParent(level));
}

Path Path::extendForChild(unsigned index) const {
	assert(element && "Invalid Path cannot be extended.");

	const NodePtr& cur = element->ptr;
	const NodeList& list = cur->getChildList();
	assert(index < list.size() && "Child Index out of bound!");

	return Path(new detail::PathElement(list[index], index, element));
}

std::size_t Path::getIndex() const {
	return element->index;
}

std::size_t Path::getLength() const {
	return element->depth;
}

bool Path::isValid() const {
	// actual test - but should never be violated
	assert((!element || element->isValid()) && "All paths should be valid!");

	// return relevant part of test
	return element;
}

/**
 * Computes a hash value for this path.
 */
std::size_t Path::hash() const {
	return (element)?element->hash:0;
}

/**
 * The copy assignment operator.
 */
Path& Path::operator=(const Path& source) {
	// check for self-assignment
	if (this == &source || element == source.element) {
		return *this;
	}

	// update element pointer
	if (element) element->decRefCount();
	element = source.element;
	if (element) element->incRefCount();

	// return modified version of this instance
	return *this;
}

/**
 * The move assignment operator.
 */
Path& Path::operator=(Path&& source) {
	if (this == &source) {
		return *this;
	}

	// reduce current elements ref-counter
	if (element) element->decRefCount();

	// use element of passed path
	element = source.element;

	// make source object invalid
	source.element = NULL;

	// return modified version of this instance
	return *this;
}

/**
 * The equals operator for paths. To paths are considered equal
 * if they describe the same sequence of nodes, starting from the root, going
 * to the addressed leaf.
 *
 * @param other the path to be compared with.
 */
bool Path::operator==(const Path& other) const {
	// compare for identity + compare the path element
	return (this==&other) || (element == other.element) || (element && other.element && *element == *other.element);
}

bool Path::operator<(const Path& other) const {
	// compare the paths - an empty path is the smallest path,
	return (!element && other.element) || (element && other.element && *element < *other.element);
}

std::ostream& Path::printTo(std::ostream& out) const {
	if (element) {
		return out << *element;
	}
	return out << " - empty path - ";
}

Path Path::concat(const Path& a, const Path& b) {
	if (b.getLength() == 1) return a;
	return concat(a, b.getPathToParent()).extendForChild(b.getIndex());
}

} // end namespace core
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::core::detail::PathElement& element) {
		if (element.parent) {
			return out << *element.parent << "-" << element.index;
		}
		return out << element.index;
	}

	std::ostream& operator<<(std::ostream& out, const insieme::core::Path& path) {
		return path.printTo(out);
	}

}

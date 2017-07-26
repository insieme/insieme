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

#include <boost/operators.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_node.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace core {


	// ------------------------------ forward declarations ---------------------------

	template <typename V>
	class NodePathElement;
	template <typename V>
	class NodePath;


	// ------------------------------- node path elements ----------------------------

	/**
	 * A marker token for non-annotated node path elements.
	 */
	struct empty {
		empty() {}
	};

	/**
	 * A base class for node path elements providing the essential operations including
	 * the reference counting for elements.
	 */
	template <typename Derived>
	struct NodePathElementBase : public utils::Printable, public boost::equality_comparable<Derived>, public boost::less_than_comparable<Derived> {
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
		const Derived* const parent;

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
		NodePathElementBase(const NodePtr& ptr, std::size_t index, const Derived* const parent, std::size_t hash)
		    : ptr(ptr), index(index), parent(parent), depth((parent) ? parent->depth + 1 : 1), hash(hash), refCount(0) {
			if(parent) { parent->incRefCount(); }
		};

	  protected:
		/**
		 * A private destructor for path elements. Having this one private effectively prevents instances on
		 * the stack and people to invoke delete on heap allocated instances.
		 */
		~NodePathElementBase() {
			// reduce reference counter in parent node
			if(parent) { parent->decRefCount(); }
		}

	  public:
		/**
		 * Obtains the path element referencing the root node of this path.
		 * @return a pointer to the requested path element
		 */
		const Derived* getRoot() const {
			return (parent) ? (parent->getRoot()) : static_cast<const Derived*>(this);
		}

		/**
		 * Obtains the parent on the given level of this path.
		 *
		 * @param level if set to 0, it will be this node. If set to 1, it will be
		 * 				the immediate parent, 2 the parents parent and so forth. Must not
		 * 				be larger or equal the depth of this node.
		 * @return a pointer to the requested path element
		 */
		const Derived* getParent(unsigned level = 1) const {
			assert_lt(level, depth) << "Trying to access unavailable parent on address:" << *this
				<< " - node: " << dumpReadable(ptr);
			return (level == 0) ? static_cast<const Derived*>(this) : parent->getParent(level - 1);
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
			assert_gt(refCount, 0);
			int res = --refCount;
			if(refCount == 0) {
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
		 * An equality operator to compare the path ending at this node with the path ending at the
		 * given other node.
		 *
		 * @param other the path to be compared to
		 */
		bool operator==(const Derived& other) const {
			if(this == &other) { return true; }

			// quick-check hash
			if(hash != other.hash) { return false; }

			// check depth
			if(depth != other.depth) { return false; }

			// if both nodes are the root node
			if(!parent && !other.parent) {
				// compare associated pointer
				return *ptr == *other.ptr;
			}

			// compare the rest (cheap stuff first)
			return (index == other.index) && (*parent == *other.parent);
		}

		/**
		 * Realizes a lexicographical order on the address paths.
		 */
		bool operator<(const Derived& other) const {
			// quick test - shortcut
			if(this == &other) { return false; }

			// make sure this one is the shorter path
			if(depth > other.depth) { return !(other < static_cast<const Derived&>(*this)); }

			// reduce length to equal size
			if(depth < other.depth) { return (*this == *other.parent) || (*this < *(other.parent)); }

			// now the length is equally long
			assert(depth == other.depth);

			// handle root case
			if(depth == 1) {
				assert(!parent && !other.parent);
				return index < other.index || (index == other.index && *ptr < *other.ptr);
			}

			// implement lexicographical order
			if(*parent < *other.parent) { return true; }
			if(*parent != *other.parent) { return false; }
			if(index < other.index) { return true; }
			if(index != other.index) { return false; }
			return static_cast<const Derived&>(*this).isValueLessThan(other.getValue());
		}

		/**
		 * Implements the printTo function as required by the base type. Note, it is not virtual,
		 * static dispatching is utilized.
		 */
		std::ostream& printTo(std::ostream& out) const {
			return static_cast<const Derived&>(*this).printToInternal(out);
		}
	};

	/**
	 * An actual implementation of a node path element annotated by a value.
	 */
	template <typename V = empty>
	class NodePathElement : public NodePathElementBase<NodePathElement<V>> {
		/**
		 * The additional value stored compared to a base node element.
		 */
		V value;

	  public:
		/**
		 * Creates a new path element using the given values. The internal ref-counter will
		 * be set to 0.
		 *
		 * @param ptr a pointer to the node addressed by this element path
		 * @param index the index of the addressed node within its parent node.
		 * @param value the value to be attached to this element
		 * @param parent a pointer to the path element referencing the parent node - null if this element is referencing the root node.
		 */
		NodePathElement(const NodePtr& ptr, std::size_t index, const V& value, const NodePathElement<V>* parent)
		    : NodePathElementBase<NodePathElement<V>>(ptr, index, parent, computeHash(ptr, index, parent, value)), value(value) {}

		/**
		 * Obtains a reference to the value attached to this path element.
		 */
		const V& getValue() const {
			return value;
		}

		/**
		 * Compares the given value with the local value.
		 */
		bool isValueLessThan(const V& other) const {
			return value < other;
		}

		/**
		 * Implements the printTo function as required by the base type. Note, it is not virtual,
		 * static dispatching is utilized.
		 */
		std::ostream& printToInternal(std::ostream& out) const {
			if(this->parent) { return out << *(this->parent) << "-" << this->index << "/" << value; }
			return out << this->index << "/" << value;
		}

		/**
		 * Computes a hash code for the given combination of values.
		 *
		 * @param ptr the node pointer to be pointing to
		 * @param index the index of the node within its parent's child list
		 * @param the parent of the given path element.
		 * @return a proper hash value for a node based on the given values
		 */
		static std::size_t computeHash(const NodePtr& ptr, std::size_t index, const NodePathElement<V>* const parent, const V& value = V()) {
			std::size_t seed = boost::hash_value(index);
			boost::hash_combine(seed, *ptr);
			boost::hash_combine(seed, ((parent) ? parent->hash : 0));
			boost::hash_combine(seed, value);
			return seed;
		}
	};

	/**
	 * A specialization of the node-path element implementation if no information shell be attached.
	 */
	template <>
	struct NodePathElement<empty> : public NodePathElementBase<NodePathElement<empty>> {
		/**
		 * Creates a new path element using the given values.
		 *
		 * @param ptr a pointer to the node addressed by this element path
		 * @param index the index of the addressed node within its parent node.
		 * @param parent a pointer to the path element referencing the parent node - null if this element is referencing the root node.
		 */
		NodePathElement(const NodePtr& ptr, std::size_t index, const empty&, const NodePathElement<empty>* parent)
		    : NodePathElementBase<NodePathElement<empty>>(ptr, index, parent, computeHash(ptr, index, parent)) {}

		/**
		 * Implements the printTo function as required by the base type. Note, it is not virtual,
		 * static dispatching is utilized.
		 */
		std::ostream& printToInternal(std::ostream& out) const {
			if(this->parent) { return out << *(this->parent) << "-" << this->index; }
			return out << this->index;
		}

		/**
		 * Obtains a reference to the value attached to this path element.
		 */
		const empty& getValue() const {
			static const empty instance;
			return instance;
		}

		/**
		 * Compares the attached value with another value => since it is empty, they are all the same
		 */
		bool isValueLessThan(const empty& other) const {
			return false; // they are all the same
		}

		/**
		 * Computes a hash code for the given combination of values.
		 *
		 * @param ptr the node pointer to be pointing to
		 * @param index the index of the node within its parent's child list
		 * @param the parent of the given path element.
		 * @return a proper hash value for a node based on the given values
		 */
		static std::size_t computeHash(const NodePtr& ptr, std::size_t index, const NodePathElement<empty>* const parent) {
			std::size_t seed = boost::hash_value(index);
			boost::hash_combine(seed, *ptr);
			boost::hash_combine(seed, ((parent) ? parent->hash : 0));
			return seed;
		}
	};


	// ---------------------------------- node path ---------------------------------

	/**
	 * The type used to represent the path of a node to be addressed.
	 */
	template <typename V>
	class NodePath : public utils::Printable, public boost::equality_comparable<NodePath<V>>, public boost::less_than_comparable<NodePath<V>> {
		/**
		 * The last element of the path represented by this class. The elements
		 * are linked bottom up and the root element is marked by referencing null as its
		 * parent.
		 */
		const NodePathElement<V>* element;

		/**
		 * A private constructor to create a path based on a path element.
		 *
		 * @param element the element this path is pointing to
		 */
		NodePath(const NodePathElement<V>* element) : element(element) {
			if(element) { element->incRefCount(); }
		}

	  public:
		/**
		 * A default constructor for this class.
		 */
		NodePath() : element(NULL){};

		/**
		 * A constructor creating a path consisting of a single node (the root node).
		 *
		 * @param node the node the new path should consist of
		 */
		NodePath(const NodePtr& node, const V& value = V()) : element(new NodePathElement<V>(node, 0, value, NULL)) {
			element->incRefCount();
		}

		/**
		 * A copy constructor
		 */
		NodePath(const NodePath& other) : element(other.element) {
			if(element) { element->incRefCount(); }
		}

		/**
		 * A move constructor for this paths.
		 */
		NodePath(NodePath&& other) : element(other.element) {
			other.element = NULL;
		}

		/**
		 * A destructor for paths.
		 */
		~NodePath() {
			// decrease the element's ref-counter. If the ref-counter
			// is zero, the element will be deleted automatically.
			if(element) { element->decRefCount(); }
		}

		/**
		 * Obtains the node addressed by this path.
		 *
		 * @return a reference to the node addressed by this path
		 */
		const NodePtr& getAddressedNode() const {
			assert_true(isValid()) << "Unable to access invalid path!";
			return element->ptr;
		}

		/**
		 * Obtains the path element referencing the root node of this path.
		 * @return a pointer to the requested path element
		 */
		const NodePtr& getRootNode() const {
			assert_true(isValid()) << "Unable to access invalid path!";
			return element->getRoot()->ptr;
		}

		/**
		 * Obtains a path equivalent to this path, just being rooted
		 * by a different node.
		 *
		 * @param newRoot the alternative root node to be used
		 * @return the same path as this path starting from a different root node.
		 */
		NodePath switchRoot(const NodePtr& newRoot) const {
			if(getLength() <= 1) { return NodePath(newRoot); }
			return getPathToParent().switchRoot(newRoot).extendForChild(getIndex());
		}

		/**
		 * Obtains the parent on the given level of this path.
		 *
		 * @param level if set to 0, it will be this node. If set to 1, it will be
		 * 				the immediate parent, 2 the parents parent and so forth. Must not
		 * 				be larger or equal the depth of this node.
		 * @return a pointer to the requested path element
		 */
		const NodePtr& getParentNode(unsigned level = 1) const {
			return element->getParent(level)->ptr;
		}

		/**
		 * Obtains the a path consisting of the root node only.
		 *
		 * @return the requested path
		 */
		NodePath getPathToRoot() const {
			assert_true(isValid()) << "Unable to access invalid path!";
			return NodePath(element->getRoot());
		}

		/**
		 * Obtains a path ending at the given parent node.
		 *
		 * @param level if set to 0, it will be this node. If set to 1, it will be
		 * 				the immediate parent, 2 the parents parent and so forth. Must not
		 * 				be larger or equal the depth of this node.
		 * @return the requested path
		 */
		NodePath getPathToParent(unsigned level = 1) const {
			assert_lt(level, getLength()) << "Not enough levels!";
			return NodePath(element->getParent(level));
		}

		/**
		 * Obtains a path extending this path by a node referencing the given child.
		 *
		 * @param index the index of the next step
		 * @return the extended path, ending at the given child node
		 */
		NodePath extendForChild(unsigned index, const V& value = V()) const {
			assert_true(element) << "Invalid Path cannot be extended.";

			const NodePtr& cur = element->ptr;
			const NodeList& list = cur->getChildList();
			assert_lt(index, list.size()) << "Child Index out of bound!";

			return NodePath(new NodePathElement<V>(list[index], index, value, element));
		}

		/**
		 * Obtains the index of the addressed node within the parents child list.
		 */
		std::size_t getIndex() const {
			return element->index;
		}

		/**
		 * Obtains a reference to the value attached to the last element of the represented path.
		 */
		const V& getValue() const {
			return element->getValue();
		}

		/**
		 * The length of this path.
		 */
		std::size_t getLength() const {
			return element->depth;
		}

		/**
		 * Verifies whether this path is valid. A path is invalid if either it is empty or
		 * for one of the steps along the path, the referenced element is not corresponding to the
		 * indexed child within the child list.
		 */
		bool isValid() const {
			// actual test - but should never be violated
			assert_true((!element || element->isValid())) << "All paths should be valid!";

			// return relevant part of test (that the element exists)
			return element;
		}

		/**
		 * Computes a hash value for this path.
		 */
		std::size_t hash() const {
			return (element) ? element->hash : 0;
		}

		/**
		 * The copy assignment operator.
		 */
		NodePath& operator=(const NodePath& source) {
			// check for self-assignment
			if(this == &source || element == source.element) { return *this; }

			// update element pointer
			if(element) { element->decRefCount(); }
			element = source.element;
			if(element) { element->incRefCount(); }

			// return modified version of this instance
			return *this;
		}

		/**
		 * The move assignment operator.
		 */
		NodePath& operator=(NodePath&& source) {
			if(this == &source) { return *this; }

			// reduce current elements ref-counter
			if(element) { element->decRefCount(); }

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
		bool operator==(const NodePath& other) const {
			// compare for identity + compare the path element
			return (this == &other) || (element == other.element) || (element && other.element && *element == *other.element);
		}

		/**
		 * A comparison operator for two paths. Paths will be ordered lexicographical.
		 */
		bool operator<(const NodePath& other) const {
			// compare the paths - an empty path is the smallest path,
			return (!element && other.element) || (element && other.element && *element < *other.element);
		}

		/**
		 * A pointer-like conversion to a boolean.
		 */
		operator bool() const {
			return element;
		}

		/**
		 * Prints a string-representation of this instance to the given stream.
		 *
		 * @param out the stream to be printed to
		 * @return a reference to the given stream
		 */
		std::ostream& printTo(std::ostream& out) const {
			if(element) { return out << *element; }
			return out << " - empty path - ";
		}

		/**
		 * Concatenates the two paths.
		 */
		static NodePath concat(const NodePath& a, const NodePath& b) {
			if(b.getLength() == 1) { return a; }
			return concat(a, b.getPathToParent()).extendForChild(b.getIndex());
		}
	};

} // end namespace core
} // end namespace insieme

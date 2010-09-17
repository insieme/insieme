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

#include <memory>
#include <unordered_map>
#include <boost/functional/hash.hpp>

#include "hash_utils.h"
#include "functional_utils.h"
#include "string_utils.h"


namespace insieme {
namespace core {

/**
 * This class constitutes the base class for all AnnotationKey types to be used to
 * attach and address Annotation s within Annotatable objects. This key type is
 * abstract, hence only sub-classes may actually be used to address Annotations.
 *
 * @see SimpleKey<T>
 * @see StringKey
 */
class AnnotationKey : public insieme::utils::HashableImmutableData<AnnotationKey> {
protected:

	/**
	 * A simple default constructor which is protected to avoid instances of this class.
	 */
	AnnotationKey(std::size_t hashCode) : HashableImmutableData(hashCode) {};
};

/**
 * An abstract base class for any kind of annotation to be attached to a AST node or pointer.
 */
class Annotation {
public:
	/**
	 * Request a pointer to the key this annotation should be associated to. The memory of the
	 * obtained key has to be managed by the annotation. As long as the annotation is valid, the
	 * key has to be valid.
	 *
	 * NOTE: best practice would be to use static variables to represent annotation keys
	 */
	virtual const AnnotationKey* getKey() const = 0;

};


// Some type definitions for combined types required for handling annotations
typedef std::unordered_map<const AnnotationKey*, std::shared_ptr<Annotation>, hash_target<const AnnotationKey*>> AnnotationMap;
typedef std::shared_ptr<AnnotationMap> SharedAnnotationMap;

/**
 * The base class of an annotatable object. This base class is maintaining a map of annotations
 * an offers means to manipulate the set of associated annotations.
 */
class Annotatable {

	/**
	 * The internal storage for annotations. It links every key to its corresponding value.
	 * The actual register is shared among copies of this class. Hence, the register is referenced
	 * via a shared pointer.
	 *
	 * NOTE: the shared pointer cannot be initialized with NULL (thereby avoiding the initialization
	 * overhead) since updating it afterward would make copies no longer share the same register.
	 */
	mutable SharedAnnotationMap map;

public:

	/**
	 * The constructor of this class is initializing the referenced shared annotation register.
	 */
	Annotatable() : map(SharedAnnotationMap(new AnnotationMap())) {};

	/**
	 * The destructor is marked virtual since most likely derived classes will be used by client code.
	 */
	virtual ~Annotatable() {};

	/**
	 * Adds a new annotation referenced by the given shared pointer. In case another annotation is already
	 * registered based on the same key, the current version will be overridden by the newly provided value.
	 *
	 * @param annotation the annotation to be added. The key will be obtained from the given annotation.
	 */
	void addAnnotation(const std::shared_ptr<Annotation>& annotation) const {

		// check pre-condition
		assert ( annotation && "Cannot add NULL annotation!" );

		// insert new element
		auto key = annotation->getKey();
		auto value = std::make_pair(key, annotation);
		auto res = map->insert(value);

		if (!res.second) {
			// equivalent element already present => remove old and add new element
			map->erase(res.first);
			res = map->insert(value);
		}

		// check post-condition
		assert ( res.second && "Insert not successful!");
		assert ( contains(key) && "Insert not successful!");
		assert ( &*((*map->find(key)).second)==&*annotation && "Insert not successful!");
	};

	/**
	 * Obtains a pointer to an Annotation associated to this annotatable class.
	 *
	 * @tparam Key the type of key used to look up the requested element.
	 * @param key the key referencing the requested element.
	 * @return a pointer to the maintained element or NULL if there is no such element.
	 */
	template<typename Key>
	typename Key::annotation_type* getAnnotation(const Key* key) const {

		// search for entry
		auto pos = map->find(key);
		if (pos == map->end() ) {
			return NULL;
		}

		// check type
		assert (dynamic_cast<typename Key::annotation_type*>(&*(*pos).second)
				&& "Annotation Type of Key does not match actual annotation!" );

		// return pointer to result
		return static_cast<typename Key::annotation_type*>(&*(*pos).second);
	}

	/**
	 * Obtains a pointer to an Annotation associated to this annotatable class.
	 *
	 * @tparam Key the type of key used to look up the requested element.
	 * @param key the key referencing the requested element.
	 * @return a pointer to the maintained element or NULL if there is no such element.
	 */
	template<typename Key>
	typename Key::annotation_type* getAnnotation(const Key& key) const {
		return getAnnotation(&key);
	}

	/**
	 * Removes the annotation referenced by the given key from the list of maintained references.
	 *
	 * @param key a pointer to the key addressing the element to be removed.
	 */
	void remAnnotation(const AnnotationKey* key) const {
		map->erase(map->find(key));
	}

	/**
	 * Removes the annotation referenced by the given key from the list of maintained references.
	 *
	 * @param key a refernec to the key addressing the element to be removed.
	 */
	void remAnnotation(const AnnotationKey& key) const {
		remAnnotation(&key);
	}

	/**
	 * Checks whether there is an annotation referenced by the given key associated with this object.
	 *
	 * @param key a pointer to the key to be looking for
	 * @return true if found, false otherwise
	 */
	bool contains(const AnnotationKey* key) const {
		return map->find(key) != map->end();
	}

	/**
	 * Checks whether there is an annotation referenced by the given key associated with this object.
	 *
	 * @param key a reference to the key to be looking for
	 * @return true if found, false otherwise
	 */
	bool contains(const AnnotationKey& key) const {
		return contains(&key);
	}

	/**
	 * Obtains an immutable reference to the internally maintained annotations.
	 */
	const AnnotationMap& getAnnotations() const {
		return *map;
	}

	/**
	 * Replaces the currently assigned annotation map by the given annotations. This
	 * modifications will not be reflected to other copies of this instance. After the
	 * set, the maintained set of annotations will be disconnected from other copies
	 * of this pointer.
	 *
	 * @param annotations the annotations to be assigned
	 */
	void setAnnotations(const AnnotationMap& annotations) const {
		// copy annotation map ...
		map = std::make_shared<AnnotationMap>(annotations);
	}

	/**
	 * By default, annotations between copies of the same Annotated object are shared.
	 * To separated the connection between one annotated instance and a copy of it, this
	 * method can be used. Internally it copies all the annotation to a new location and
	 * updated the map pointer such that it is pointing to the new location.
	 */
	const void isolateAnnotations() const {
		// copy current annotations
		setAnnotations(getAnnotations());
	}
};

/**
 * A simple implementation of a AnnotationKey which is solely represented by a single value.
 *
 * @tparam T the type of value to be used by this key
 * @tparam Comparator the comparator to be used to compare instances of the key type
 */
template<typename T, typename AnnotationType=Annotation, typename Comparator = std::equal_to<T>>
class SimpleKey : public AnnotationKey {
public:

	/**
	 * The type of annotation referenced by this key. Since it is a simple, general key
	 * the annotation key type is
	 */
	typedef AnnotationType annotation_type;

private:

	/**
	 * The value making this key unique.
	 */
	const T value;

	/**
	 * The comparator used to compare instances of the value represented by this key.
	 */
	Comparator comparator;

public:

	/**
	 * Creates a new instance of this key type based on the given value.
	 *
	 * @param value the value to be represented by this instance
	 */
	SimpleKey(const T& value) : AnnotationKey(boost::hash<T>()(value)), value(value) {};

	/**
	 * Compares this key with another AnnotationKey.
	 *
	 * @param other the key to be compared to
	 */
	bool equals(const AnnotationKey& other) const {
		// check type of other key
		if (typeid(other) != typeid(SimpleKey<T, Comparator>)) {
			return false;
		}

		// check value
		const SimpleKey<T,Comparator>& otherKey = static_cast<const SimpleKey<T, Comparator>&>(other);
		return comparator(value, otherKey.getValue());
	}

	/**
	 * Obtains the value represented by this key.
	 *
	 * @return the value represented by this key.
	 */
	const T& getValue() const {
		return value;
	}
};


/**
 * A simple annotation key definition which is based on a string.
 */
template<typename AnnotationType = Annotation>
class StringKey : public SimpleKey<string, AnnotationType, std::equal_to<string>> {
public:

	/**
	 * The type of annotation referenced by this key. Since it is a simple, general key
	 * the annotation key type is
	 */
	typedef AnnotationType annotation_type;

	/**
	 * A default constructor for the String-Key type.
	 *
	 * @param value the value to be represented.
	 */
	StringKey(const string& value) : SimpleKey<string, AnnotationType, std::equal_to<string>>(value) {};
};

} // end namespace core
} // end namespace insieme

/**
 * Allows simple keys to be printed to an output stream.
 *
 * @tparam T the type of value maintained by the SimpleKey
 * @param out the stream to be printed to
 * @param key the key to be printed
 * @return out a reference to the handed in output stream
 */
template<typename T>
std::ostream& operator<<(std::ostream& out, const insieme::core::SimpleKey<T>& key) {
	return out << "SimpleKey(" << key.getValue() << ")";
}

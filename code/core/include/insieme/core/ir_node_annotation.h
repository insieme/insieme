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

#include "insieme/utils/annotation.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 									Node Annotations
	// **********************************************************************************

	// a forward declaration of the node annotation class and a pointer type referencing it
	class NodeAnnotation;
	typedef std::shared_ptr<NodeAnnotation> NodeAnnotationPtr;

	/**
	 * An abstract super type for all annotations being attached to nodes. In addition to the
	 * usual annotation requirements, node annotations have to support the migration between
	 * nodes during transformations.
	 */
	class NodeAnnotation : public utils::Annotation {
	  public:
		/**
		 * A method which will be invoked whenever a node with this annotation is
		 * transformed. If the annotation should be preserved, this method has to migrate
		 * itself to the given after node. During this migration, necessary modifications
		 * on the annotations may as well be applied.
		 *
		 * @param ptr the shared annotation pointer referencing this annotation within the before node
		 * @param before the node state before the transformation having this annotation attached to
		 * @param after the node state after the transformation, which might have to be updated
		 * @return true if a migration took place, false otherwise
		 */
		virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			return false;
		};

		/**
		 * A method which will be invoked whenever a node with this annotation is
		 * cloned from one node manager to another. If the annotation should be preserved,
		 * this method has to copy copy itself to the given target node. During this migration,
		 * necessary modifications on the annotations may as well be applied.
		 *
		 * By default all node annotations are copied to the target node.
		 *
		 * @param ptr the shared annotation pointer referencing this annotation within the source node
		 * @param copy the copy of the original node this annotation has been attached to
		 */
		virtual void clone(const NodeAnnotationPtr& ptr, const NodePtr& copy) const;

		/**
		 * A method which will return a list of child nodes included within this annotation
		 * to be accessible by utilities including the semantic checks.
		 *
		 * The default implementation returns an empty list.
		 *
		 * @return the list of IR nodes included within this annotation
		 */
		virtual const NodeList& getChildNodes() const {
			static const NodeList empty;
			return empty;
		}
	};


	namespace value_annotation {

		/**
		 * A utility function used by move_to_clone to conduct the actual migration without
		 * the requirement of including the definition of the NodePtr class within this file
		 * (which would result in a cyclic dependency).
		 */
		void add_annotation(const NodeAnnotationPtr& annotation, const NodePtr& target);

		// --------------- Migration ----------------------

		/**
		 * A marker type to be used for marking value annotations providing a user defined
		 * operation handling the annotation migration in case the annotated node is
		 * transformed.
		 *
		 * Values extending this interface have to implement a member function
		 *
		 * 			bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const;
		 *
		 * For details see the mirgrate method description of the NodeAnnotation class.
		 */
		struct migratable {
			// bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const;   // -- to be implemented by sub-classes!
		};

		/**
		 * A marker type to be used for marking value annotations which should be simply
		 * copied if the node it is attached to is transformed.
		 */
		struct copy_on_migration {};

		// support user defined migration operation
		template <typename V>
		typename std::enable_if<std::is_base_of<migratable, V>::value, bool>::type migrate_annotation(const NodeAnnotationPtr& ptr, const NodePtr& before,
		                                                                                              const NodePtr& after, const V& value) {
			return value.migrate(ptr, before, after);
		}

		// support copy-on-migrate option
		inline bool migrate_annotation(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after, const copy_on_migration& value) {
			add_annotation(ptr, after);
			return true;
		}

		// the default case - no migration
		template <typename V>
		typename std::enable_if<!std::is_base_of<migratable, V>::value && !std::is_base_of<copy_on_migration, V>::value, bool>::type
		migrate_annotation(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after, const V& value) {
			return false; // default case => no migration
		}


		// ---------------- Cloning -----------------------

		/**
		 * A marker type to be used for marking value annotations which should be dropped in case
		 * the annotated node is cloned to another node manager.
		 */
		struct drop_on_clone {};

		/**
		 * A marker type to be used for marking value annotations providing a user defined clone
		 * function which will be used for migrating it when cloning a node to another manager.
		 *
		 * Values extending this interface have to implement a member function
		 *
		 * 			void cloneTo(const NodePtr& target) const;
		 */
		struct cloneable {
			// void cloneTo(const NodePtr& target) const;   // -- to be implemented by sub-classes!
		};

		// the default variant for all non-specialized value types
		template <typename V>
		typename std::enable_if<!std::is_base_of<drop_on_clone, V>::value && !std::is_base_of<cloneable, V>::value>::type
		move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const V& value) {
			add_annotation(ptr, target); // default behavior => link same annotation
		}

		// support drop-on-clone option
		inline void move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const drop_on_clone& value) {
			// just do nothing ..
		}

		// support user defined migration operation
		template <typename V>
		typename std::enable_if<std::is_base_of<cloneable, V>::value>::type move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const V& value) {
			value.cloneTo(target); // let value annotation do the work
		}


		// --------------- ChildList ----------------------

		/**
		 * A marker type to be used for marking value annotations providing a user defined
		 * list of child nodes to be considered by e.g. semantic checks.
		 *
		 * Values extending this interface have to implement a member function
		 *
		 * 			const NodeList& getChildNodes() const;
		 */
		struct has_child_list {
			// const NodeList& getChildNodes() const; 		// -- to be implemented by sub-classes
		};

		// support user defined child list
		template <typename V>
		typename std::enable_if<std::is_base_of<has_child_list, V>::value, const NodeList&>::type get_child_list(const V& value) {
			return value.getChildNodes();
		}

		// the default case - no child list
		template <typename V>
		typename std::enable_if<!std::is_base_of<has_child_list, V>::value, const NodeList&>::type get_child_list(const V& value) {
			static const NodeList empty;
			return empty;
		}
	}

	/**
	 * A type trade definition allowing to determine the type of a value annotation based on the
	 * value type.
	 */
	template <typename ValueType>
	struct value_node_annotation {
		typedef utils::detail::ValueAnnotation<ValueType, core::NodeAnnotation, utils::AnnotationKey> type;
	};

	template<typename Derived>
	struct NodeAnnotationAccessHelper {

		const utils::Annotatable<NodeAnnotation>& getAnnotationContainer() const {
			return static_cast<const Derived*>(this)->getAnnotationContainer();
		}

		// -----------------------------------
		//  Forward Annotation related calls
		// -----------------------------------

		typedef typename utils::Annotatable<NodeAnnotation>::key_type KeyType;
		typedef typename utils::Annotatable<NodeAnnotation>::annotation_ptr_type annotation_ptr_type;
		typedef typename utils::Annotatable<NodeAnnotation>::annotation_map_type annotation_map_type;

		void addAnnotation(const annotation_ptr_type& annotation) const {
			getAnnotationContainer().addAnnotation(annotation);
		}

		template <typename Annotation, typename... Params>
		void addAnnotation(Params... p) const {
			getAnnotationContainer().template addAnnotation<Annotation>(p...);
		}

		const std::shared_ptr<NodeAnnotation>& getAnnotation(const utils::AnnotationKeyPtr& key) const {
			return getAnnotationContainer().getAnnotation(key);
		}

		template <typename Key>
		typename std::shared_ptr<typename Key::annotation_type> getAnnotation(const Key* key) const {
			return getAnnotationContainer().getAnnotation(key);
		}

		template <typename Key>
		typename std::shared_ptr<typename Key::annotation_type> getAnnotation(const Key& key) const {
			return getAnnotation(&key);
		}

		void remAnnotation(const KeyType* key) const {
			getAnnotationContainer().remAnnotation(key);
		}

		void remAnnotation(const KeyType& key) const {
			getAnnotationContainer().remAnnotation(key);
		}

		bool hasAnnotation(const KeyType* key) const {
			return getAnnotationContainer().hasAnnotation(key);
		}

		bool hasAnnotation(const KeyType& key) const {
			return getAnnotationContainer().hasAnnotation(key);
		}

		const annotation_map_type& getAnnotations() const {
			return getAnnotationContainer().getAnnotations();
		}

		void setAnnotations(const annotation_map_type& annotations) const {
			getAnnotationContainer().setAnnotations(annotations);
		}

		bool hasAnnotations() const {
			return getAnnotationContainer().hasAnnotations();
		}

		// -- Value Attachments ---------------------
		template <typename V>
		bool hasAttachedValue() const {
			return getAnnotationContainer().template hasAttachedValue<V>();
		}

		template <typename V>
		V& attachValue(const V& value = V()) const {
			return getAnnotationContainer().template attachValue<V>(value);
		}

		template <typename V, typename... Args>
		V& attachValue(const Args&... args) const {
			return getAnnotationContainer().template attachValue<V>(args...);
		}

		template <typename V>
		void detachValue() const {
			getAnnotationContainer().template detachValue<V>();
		}

		template <typename V>
		const V& getAttachedValue() const {
			return getAnnotationContainer().template getAttachedValue<V>();
		}
	};

} // end namespace core

namespace utils {
	namespace detail {

		/**
		 * A partial template specialization of the general ValueAnnotation template used for attaching values
		 * to annotatable objects. This template allows the value to be attached to select the mode for being
		 * migrated when cloning the underlying IR node to another node manager, specialize the migration
		 * operation (in case the annotated node is transformed) and its child list (to extend utility
		 * coverage to the annotation, e.g. for semantic checks).
		 */
		template <typename V, typename KeyType>
		class ValueAnnotation<V, core::NodeAnnotation, KeyType>
		    : public ValueAnnotationBase<V, core::NodeAnnotation, KeyType, ValueAnnotation<V, core::NodeAnnotation, KeyType>> {
		  public:
			ValueAnnotation(const V& value) : ValueAnnotationBase<V, core::NodeAnnotation, KeyType, ValueAnnotation<V, core::NodeAnnotation, KeyType>>(value) {}

			virtual ~ValueAnnotation() {}

			virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
				return core::value_annotation::migrate_annotation(
				    ptr, before, after, ValueAnnotationBase<V, core::NodeAnnotation, KeyType, ValueAnnotation<V, core::NodeAnnotation, KeyType>>::getValue());
			}

			virtual void clone(const core::NodeAnnotationPtr& ptr, const core::NodePtr& copy) const {
				core::value_annotation::move_to_clone(
				    ptr, copy, ValueAnnotationBase<V, core::NodeAnnotation, KeyType, ValueAnnotation<V, core::NodeAnnotation, KeyType>>::getValue());
			}

			virtual const core::NodeList& getChildNodes() const {
				return core::value_annotation::get_child_list(
				    ValueAnnotationBase<V, core::NodeAnnotation, KeyType, ValueAnnotation<V, core::NodeAnnotation, KeyType>>::getValue());
			}
		};

	} // end namespace detail
} // end namespace utils

} // end namespace insieme

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
		virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const { return false; };

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
	};


	namespace value_annotation {

		/**
		 * A marker type to be used for marking value annotations which should be dropped in case
		 * the annotated node is cloned to another node manager.
		 */
		struct drop_on_clone {};

		/**
		 * A marker type to be used for marking value annotations providing a user defined migration
		 * function which will be used for migrating it when cloning a node to another manager.
		 *
		 * Values extending this interface have to implement a member function
		 *
		 * 			void migrateTo(const NodePtr& target) const;
		 */
		struct migratable {
			// void migrateTo(const NodePtr& target) const;   // -- to be implemented by sub-classes!
		};

		/**
		 * A utility function used by move_to_clone to conduct the actual migration without
		 * the requirement of including the definition of the NodePtr class within this file
		 * (which would result in a cyclic dependency).
		 */
		void add_annotation(const NodeAnnotationPtr& annotation, const NodePtr& target);

		// the default variant for all non-specialized value types
		template<typename V>
		typename std::enable_if<!std::is_base_of<drop_on_clone,V>::value && !std::is_base_of<migratable,V>::value>::type
		move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const V& value) {
			add_annotation(ptr, target);		// default behavior => link same annotation
		}

		// support drop-on-clone option
		inline void move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const drop_on_clone& value) {
			// just do nothing ..
		}

		// support user defined migration operation
		template<typename V>
		typename std::enable_if<std::is_base_of<migratable,V>::value>::type
		move_to_clone(const NodeAnnotationPtr& ptr, const NodePtr& target, const V& value) {
			value.migrateTo(target);	// let value annotation do the work
		}

	}

} // end namespace core

namespace utils {
namespace detail {

	/**
	 * A partial template specialization of the general ValueAnnotation template used for attaching values
	 * to annotatable objects. This template allows the value to be attached to select the mode for being
	 * migrated when cloning the underlying IR node to another node manager.
	 */
	template<typename V, typename KeyType>
	class ValueAnnotation<V, core::NodeAnnotation, KeyType> : public ValueAnnotationBase<V,core::NodeAnnotation,KeyType,ValueAnnotation<V, core::NodeAnnotation, KeyType>> {
	public:
			ValueAnnotation(const V& value) : ValueAnnotationBase<V,core::NodeAnnotation,KeyType,ValueAnnotation<V, core::NodeAnnotation, KeyType>>(value) {}

			virtual void clone(const core::NodeAnnotationPtr& ptr, const core::NodePtr& copy) const {
				core::value_annotation::move_to_clone(ptr, copy, ValueAnnotationBase<V,core::NodeAnnotation,KeyType,ValueAnnotation<V, core::NodeAnnotation, KeyType>>::getValue());
			}
	};

} // end namespace detail
} // end namespace utils

} // end namespace insieme



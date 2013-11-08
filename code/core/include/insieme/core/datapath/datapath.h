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

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {
namespace datapath {

	/**
	 * Legacy code - a data path used to be a plain expression pointer.
	 */
	typedef ExpressionPtr DataPathPtr;

	/**
	 * The type used to represent data paths. Internally
	 * a data path is represented by an IR construct modeling
	 * the path within a data object to some referenced
	 * sub-element.
	 */
	class DataPath : public utils::Printable {

		ExpressionPtr path;

		/**
		 * The constructor to be invoked by the builder class.
		 *
		 * @param path the path to be represented by the resulting instance
		 */
		DataPath(const ExpressionPtr& path)
			: path(path) {}

	public:

		/**
		 * Creates a new data path referencing the root of a data object.
		 */
		DataPath(NodeManager& manager);

		/**
		 * Extends this data path by an access to the given member.
		 * This call is only supported if the accessed element is a
		 * struct, union or recursive type with a top-level struct/union.
		 *
		 * @param member the member to be accessed; the expresssion has to be of type identifer
		 * @return the extended data path
		 */
		DataPath member(const ExpressionPtr& member) const;

		/**
		 * Extends this data path by an access to the given member.
		 * This call is only supported if the accessed element is a
		 * struct, union or recursive type with a top-level struct/union.
		 *
		 * @param member the member to be accessed
		 * @return the extended data path
		 */
		DataPath member(const string& name) const;

		/**
		 * Extends this data path by an access to the given element.
		 * This call is only supported if the accessed element is a
		 * vector or array or a recursive type with a top-level vector/array.
		 *
		 * @param element the element to be accessed; the expression has to be an unsigned integer
		 * @return the extended data path
		 */
		DataPath element(const ExpressionPtr& element) const;

		/**
		 * Extends this data path by an access to the given element.
		 * This call is only supported if the accessed element is a
		 * vector or array or a recursive type with a top-level vector/array.
		 *
		 * @param element the element to be accessed
		 * @return the extended data path
		 */
		DataPath element(unsigned index) const;

		/**
		 * Extends this data path by an access to the given component.
		 * This call is only supported if the accessed element is a
		 * tuple or a recursive type with a top-level tuple.
		 *
		 * @param component the component to be accessed; must be an unsigned integer literal
		 * @return the extended data path
		 */
		DataPath component(const LiteralPtr& component) const;

		/**
		 * Extends this data path by an access to the given component.
		 * This call is only supported if the accessed element is a
		 * tuple or a recursive type with a top-level tuple.
		 *
		 * @param component the component to be accessed
		 * @return the extended data path
		 */
		DataPath component(unsigned index) const;

		/**
		 * Extends this data path by an access to the given parent type.
		 * This call is only supported if the accessed element is a struct
		 * exhibiting the corresponding parent class.
		 *
		 * @param parent the type of parent to be accessed
		 * @return the extended data path
		 */
		DataPath parent(const TypePtr& parent) const;

		/**
		 * A conversion operation to an ExpressionPtr.
		 */
		operator ExpressionPtr() const {
			return path;
		}

		/**
		 * A conversion operation to an StatementPtr.
		 */
		operator StatementPtr() const {
			return path;
		}

		/**
		 * A conversion operation to an NodePtr.
		 */
		operator NodePtr() const {
			return path;
		}

		/**
		 * Allows this data path to be printed in a human readable format.
		 */
		std::ostream& printTo(std::ostream& out) const;

	};


	/**
	 * A utility class supporting the creation of data paths.
	 */
	class DataPathBuilder {

		/**
		 * The path constructed by this builder.
		 */
		DataPath path;

	public:

		/**
		 * Creates a new builder instance based on the given manager
		 * using the empty path as its initial path.
		 *
		 * @param manager the manager to be used for maintaining
		 * 			the internally constructed path
		 */
		DataPathBuilder(NodeManager& manager) : path(manager) {}

		/**
		 * Creates a new builder instance based on the given initial path.
		 *
		 * @param path the path to be used as an initial path for the builder
		 */
		DataPathBuilder(const DataPath& path) : path(path) {}

		/**
		 * This function will extend the internally constructed path by
		 * an access to the given member of a struct / union.
		 *
		 * @param member the member element to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& member(const ExpressionPtr& member);

		/**
		 * This function will extend the internally constructed path by
		 * an access to the given member of a struct / union.
		 *
		 * @param member the member element to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& member(const string& member);


		/**
		 * This function will extend the internally constructed path by
		 * an access to the given element of an array / vector.
		 *
		 * @param element the element to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& element(const ExpressionPtr& element);

		/**
		 * This function will extend the internally constructed path by
		 * an access to the given element of an array / vector.
		 *
		 * @param element the element to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& element(unsigned index);


		/**
		 * This function will extend the internally constructed path by
		 * an access to the given component of a tuple.
		 *
		 * @param component the (index) of the component to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& component(const LiteralPtr& component);

		/**
		 * This function will extend the internally constructed path by
		 * an access to the given component of a tuple.
		 *
		 * @param component the (index) of the component to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& component(unsigned index);

		/**
		 * This function will extend the internally constructed path by
		 * an access to the given parent type.
		 *
		 * @param type the parent type to be accessed
		 * @return a reference to this builder to chain build-commands
		 */
		DataPathBuilder& parent(const TypePtr& type);

		/**
		 * Obtains a copy of the internally constructed data path.
		 */
		DataPath getDataPath() const {
			return path;
		}

		/**
		 * Obtains a copy of the internally constructed data path as an expression.
		 */
		DataPathPtr getPath() const {
			return path;
		}

	};


} // end namespace datapath
} // end namespace core
} // end namespace

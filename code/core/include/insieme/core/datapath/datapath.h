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
	 * A type declaration for a data path pointer. Internally,
	 * it is nothing more than a term of a given type being
	 * composed using a limit set of function symbols.
	 */
	typedef ExpressionPtr DataPathPtr;


	/**
	 * A utility class supporting the creation of data paths.
	 */
	class DataPathBuilder {

		/**
		 * The manager maintaining the internally constructed data path.
		 */
		NodeManager& manager;

		/**
		 * The path constructed by this builder.
		 */
		DataPathPtr path;

	public:

		/**
		 * Creates a new builder instance based on the given manager
		 * using the empty path as its initial path.
		 *
		 * @param manager the manager to be used for maintaining
		 * 			the internally constructed path
		 */
		DataPathBuilder(NodeManager& manager);

		/**
		 * Creates a new builder instance based on the given initial path.
		 *
		 * @param path the path to be used as an initial path for the builder
		 */
		DataPathBuilder(const DataPathPtr& path)
			: manager(path->getNodeManager()), path(path) {}

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
		 * Obtains a copy of the internally constructed data path.
		 */
		DataPathPtr getPath() const {
			return path;
		}

	};


} // end namespace datapath
} // end namespace core
} // end namespace

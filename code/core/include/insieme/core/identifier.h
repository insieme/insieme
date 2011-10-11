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

#include <string>
#include <ostream>
#include <boost/functional/hash.hpp>

#include "insieme/core/ast_node.h"

namespace insieme {
namespace core {


	/**
	 * A special kind of node representing names wherever necessary within the INSPIRE representation.
	 */
	class Identifier : public Node {

		/**
		 * The name this identifier is representing.
		 */
		const string name;

		/**
		 * Creates a new identifier based on the given name.
		 */
		Identifier(const string& name);

	public:

		/**
		 * An abstract factory method to obtain references to identifier nodes.
		 *
		 * @param manager the manager the resulting identifier should be associated to
		 * @param name the name to be represented by the identifier
		 * @return a pointer to the requested node
		 */
		static IdentifierPtr get(NodeManager& manager, const string& name);

	private:

		/**
		 * Creates a copy of this node using the given mapper.
		 *
		 * @param mapper the mapper to to be used for the copying process (will be ignored by this implementation)
		 * @return a pointer to a clone of this identifier node.
		 */
		virtual Node* createCopyUsing(NodeMapping& mapper) const {
			return new Identifier(name);
		}

	protected:

		/**
		 * Obtains an empty child list.
		 */
		virtual NodeListOpt getChildNodes() const {
			return std::make_shared<NodeList>();
		}

		/**
		 * Compares this node with the given node.
		 */
		bool equals(const Node& other) const;

	public:

		/**
		 * Prints the name of this identifier to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

		/**
		 * Obtains the name of this identifier.
		 */
		const string& getName() const {
			return name;
		}

	};

} // end namespace core
} // end namespace insieme


namespace std {
	/**
	 * Allows this type to be printed to a stream (especially useful during debugging and
	 * within test cases where equals values to be printable).
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::Identifier& identifier);
}


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

#include "insieme/core/placeholder.h"


namespace insieme {
namespace core {


	// private constructor for this placeholder node
	// TODO: use real hash seed
	Placeholder::Placeholder(char symbol) : Node(NT_Placeholder, NC_Support, symbol), symbol(symbol) {}


	PlaceholderPtr Placeholder::get(NodeManager& manager, char symbol) {
		return manager.get(Placeholder(symbol));
	}

	// --- inherited from node ---

	Node* Placeholder::createCopyUsing(NodeMapping& mapper) const {
		return new Placeholder(symbol);
	}

	Node::OptionChildList Placeholder::getChildNodes() const {
		return OptionChildList(new ChildList());
	}

	std::ostream& Placeholder::printTo(std::ostream& out) const {
		return out << symbol;
	}

	/**
	 * Compares this placeholder instance with a given node.
	 */
	bool Placeholder::equals(const Node& other) const {
		if (this == &other) {
			return true;
		}

		if (other.getNodeType() != NT_Placeholder) {
			return false;
		}

		const Placeholder &otherPlaceholder = static_cast<const Placeholder&>(other);
		return symbol == otherPlaceholder.symbol;
	}


} // end namespace core
} // end namespace insieme

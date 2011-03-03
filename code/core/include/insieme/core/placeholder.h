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

#include "insieme/core/types.h"
#include "insieme/core/expressions.h"
#include "insieme/core/statements.h"


namespace insieme {
namespace core {

	/**
	 * Within this file a special IR node is defined, which can be placed anywhere within the IR. Instances of placeholder
	 * instances can be used to represent variable parts within IR pattern matching routines.
	 */


	class Placeholder : public Node {

		/**
		 * The symbol to be used to represent this placeholder instance.
		 */
		const char symbol;

		Placeholder(char symbol);

	public:

		// factory methods

		static PlaceholderPtr get(NodeManager& manager, char symbol);

		// getter/setter

		char getSymbol();


//		// --- inherited from expression ---
//
//		virtual bool equals(const Node& stmt) const;
//		virtual bool equalsExpr(const Expression& expr) const;
//
//		// --- inherited from type ---
//
//		virtual std::ostream& printTypeTo(std::ostream& out) const;
//		virtual bool equalsType(const Type& type) const;

		// --- inherited from node ---

		virtual Node* createCopyUsing(NodeMapping& mapper) const;
		virtual OptionChildList getChildNodes() const;
		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * Implements equals for placeholders. Two placeholders are considered
		 * equal if they represent the same symbol.
		 */
		bool equals(const Node& other) const;


	};


} // end namespace core
} // end namespace insieme



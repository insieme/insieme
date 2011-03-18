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

#include <ostream>
#include <memory>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace transform {
namespace pattern {


	class Tree;
	typedef std::shared_ptr<Tree> TreePtr;


	class Tree {
		const std::vector<TreePtr> subTrees;
	public:
		template<typename... Args>
		Tree(Args && ... args) : subTrees(toVector<TreePtr>(args...)) {}

		const std::vector<TreePtr> getSubTrees() const { return subTrees; }
		virtual std::ostream& printTo(std::ostream& out) const =0;
		virtual bool operator==(const Tree& other) const =0;
	};


	class Inner : public Tree {
	public:
		template<typename... Args>
		Inner(Args && ... args) : Tree(args...) {}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "(" << join(",", getSubTrees(), print<deref<TreePtr>>()) << ")";
		}

		virtual bool operator==(const Tree& other) const {
			if (const Inner* otherInner = dynamic_cast<const Inner*>(&other)) {
				return equals(getSubTrees(), otherInner->getSubTrees(), equal_target<TreePtr>());
			}
			return false;
		}
	};


	class Leaf : public Tree {
		const char symbol;
	public:
		Leaf(char symbol) : symbol(symbol) {};

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << symbol;
		}

		virtual bool operator==(const Tree& other) const {
			if (const Leaf* otherLeaf = dynamic_cast<const Leaf*>(&other)) {
				return symbol == otherLeaf->symbol;
			}
			return false;
		}
	};


	TreePtr makeTree( char symbol );

	template<typename... Args>
	TreePtr makeTree(const Args & ... args ) {
		return std::make_shared<Inner>(args...);
	}

	std::ostream& operator<<(std::ostream& out, const Tree& tree);

	std::ostream& operator<<(std::ostream& out, const TreePtr& tree);

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

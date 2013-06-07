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

#include <boost/variant.hpp>

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/annotation.h"

namespace insieme {
namespace transform {
namespace pattern {


	class Tree;
	typedef std::shared_ptr<Tree> TreePtr;

	typedef std::vector<TreePtr> TreeList;
	typedef TreeList::const_iterator TreeListIterator;

	class Tree : public utils::Annotatable<>, public utils::Printable, boost::noncopyable {
	public:

		static const char VALUE_ID;

		typedef boost::variant<bool,int,unsigned,string> Value;

	protected:

		const char id;
		TreeList subTrees;
		Value value;

	public:

		Tree(const Value& value) : id(VALUE_ID), subTrees(), value(value) {}

		template<typename... Args>
		Tree(char id, Args && ... args) : id(id), subTrees(toVector<TreePtr>(args...)) {
			assert(id != VALUE_ID && "Value ID must not be used!");
		}

		Tree(const TreeList& children, int id) : id(id), subTrees(children) {
			assert(id != VALUE_ID && "Value ID must not be used!");
		}

		virtual ~Tree() {};

		virtual std::ostream& printTo(std::ostream& out) const;
		virtual bool operator==(const Tree& other) const;
		virtual bool operator!=(const Tree& other) const { return !(*this == other); }

		virtual const TreeList& getSubTrees() const { return subTrees; }
		virtual const TreeList& getChildList() const { return subTrees; }

		const char getId() const { return char(id); }
	};

	template<typename... Args>
	TreePtr makeTree(const Args & ... args ) {
		return std::make_shared<Tree>(0, args...);
	}

	template<typename... Args>
	TreePtr makeTree(char symbol, const Args & ... args ) {
		return std::make_shared<Tree>((int)symbol, args...);
	}

	inline TreePtr makeTree(char id, const TreeList& children) {
		return std::make_shared<Tree>(children, id);
	}

	template<typename V>
	TreePtr makeValue(const V& value) {
		return std::make_shared<Tree>(Tree::Value(value));
	}

	TreePtr parseTree(const string& tree);

	std::ostream& operator<<(std::ostream& out, const Tree& tree);

	std::ostream& operator<<(std::ostream& out, const TreePtr& tree);

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

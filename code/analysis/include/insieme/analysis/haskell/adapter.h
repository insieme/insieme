/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_node.h"

#include <boost/optional.hpp>
#include <memory>

namespace insieme {
namespace analysis {
namespace haskell {

	#include "boolean_analysis.h"

	class HSobject;
	class Tree;
	class Address;

	struct Tree {

		std::shared_ptr<HSobject> tree;
		const core::NodePtr& original;

		Tree(std::shared_ptr<HSobject> tree, const core::NodePtr& original);

		std::size_t size() const;
		void print() const;

	};

	struct Address {

		std::shared_ptr<HSobject> addr;

		Address(std::shared_ptr<HSobject> addr);

		std::size_t size() const;
		void printNode() const;
		core::NodeAddress toNodeAddress(const core::NodePtr& root) const;

	};

	class Environment {

		Environment();

	public:

		~Environment();
		Environment(const Environment&) = delete;
		void operator=(const Environment&) = delete;

		static Environment& getInstance();

		Tree passTree(const core::NodePtr& root);
		Address passAddress(const core::NodeAddress& addr, const Tree& tree);

		boost::optional<Address> findDeclr(const Address& var);
		BooleanAnalysisResult checkBoolean(const Address& expr);

	};

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

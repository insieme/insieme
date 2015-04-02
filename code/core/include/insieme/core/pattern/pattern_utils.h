/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"

namespace insieme {
namespace core {
namespace pattern {
namespace irp {
	// two utilities to collect all matches of a given pattern within a tree
	vector<core::NodePtr> collectAll(const TreePattern& pattern, const core::NodePtr& root, bool matchTypes = false);
	vector<core::NodeAddress> collectAll(const TreePattern& pattern, const core::NodeAddress& root, bool matchTypes = false);

	// same as collectAll, but also collect the associated match objects
	vector<pair<core::NodePtr, NodeMatch>> collectAllPairs(const TreePattern& pattern, const core::NodePtr& root, bool matchTypes);
	vector<pair<core::NodeAddress, AddressMatch>> collectAllPairs(const TreePattern& pattern, const core::NodeAddress& root, bool matchTypes);

	// try to match the given pattern on all instances of the given tree, 
	// calling the passed function for each match
	void matchAll(const TreePattern& pattern, const core::NodePtr& root, std::function<void(NodeMatch match)> lambda, bool matchTypes = false);
	void matchAll(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(AddressMatch match)> lambda, bool matchTypes = false);
	// call the lambda for each pair of node/address and match object in the given tree
	void matchAllPairs(const TreePattern& pattern, const core::NodePtr& root, std::function<void(core::NodePtr node, NodeMatch match)> lambda, bool matchTypes = false);
	void matchAllPairs(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda, bool matchTypes = false);
	void matchAllPairsReverse(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda, bool matchTypes=false);

	// try to match the given pattern on all instances of the given tree, 
	// calling the passed function for each match to generate a replacement
	// returns a new root with all matches replaced
	// Note: handles nested matches gracefully, but not horizontally overlapping matches
	NodePtr replaceAll(const TreePattern& pattern, const core::NodePtr& root,
		std::function<core::NodePtr(AddressMatch match)> lambda, bool matchTypes = false);

} // end namespace irp
} // end namespace pattern
} // end namespace core
} // end namespace insieme

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
	void matchAllPairs(const TreePattern& pattern, const core::NodePtr& root, std::function<void(core::NodePtr node, NodeMatch match)> lambda,
	                   bool matchTypes = false);
	void matchAllPairs(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda,
	                   bool matchTypes = false);
	void matchAllPairsReverse(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda,
	                          bool matchTypes = false);

	// try to match the given pattern on all instances of the given tree,
	// calling the passed function for each match to generate a replacement
	// returns a new root with all matches replaced
	// Note: handles nested matches gracefully, but not horizontally overlapping matches
	NodePtr replaceAll(const TreePattern& pattern, const core::NodePtr& root, std::function<core::NodePtr(AddressMatch match)> lambda, bool matchTypes = false);

	// try to match the given pattern on all instances of the given tree,
	// calling the passed function for each matching address to generate a replacement
	// returns a new root with all matches replaced
	// Note: handles nested matches gracefully, but not horizontally overlapping matches
	NodePtr replaceAllAddr(const TreePattern& pattern, const core::NodePtr& root, std::function<core::NodePtr(const NodeAddress& matchingAddr)> lambda,
	                       bool matchTypes = false);

} // end namespace irp
} // end namespace pattern
} // end namespace core
} // end namespace insieme

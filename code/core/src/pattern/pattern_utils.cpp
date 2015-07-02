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

#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_cached_visitor.h"

namespace insieme {
namespace core {
namespace pattern {
namespace irp {

	namespace {
		template<typename T>
		vector<T> collectAll(const TreePattern& pattern, const T& root, bool matchTypes) {
			// iterate through the tree and search for matches
			vector<T> res;
			// cache results of pattern evaluation
			auto clv = makeCachedLambdaVisitor([&](core::NodePtr node) { return pattern.match(node); }, matchTypes);
			core::visitDepthFirst(root, [&](const T& cur) {
				if(clv.visit(cur.template as<core::NodePtr>())) {
					res.push_back(cur);
				}
			}, true, matchTypes);

			return res;
		}

		// helpers for collectAllPairs
		template<typename T> T convert(NodeAddress a) { return a.as<T>(); }
		template<typename T> T convert(NodePtr n) { return T(n); }
		MatchOpt tmatch(const TreePattern& pattern, const NodePtr& node) {
			return pattern.matchPointer(node);
		}
		AddressMatchOpt tmatch(const TreePattern& pattern, const NodeAddress& node) {
			return pattern.matchAddress(node);
		}

		template<typename T, typename MT>
		vector<pair<T, MT>> collectAllPairs(const TreePattern& pattern, const T& root, bool matchTypes) {
			// iterate through the tree and search for matches
			vector<pair<T, MT>> res;
			// cache results of pattern evaluation
			auto clv = makeCachedLambdaVisitor([&](core::NodePtr node) { 
				return tmatch(pattern, convert<T>(node)); 
			}, matchTypes);
			core::visitDepthFirst(root, [&](const T& cur) {
				auto m = clv.visit(cur.template as<core::NodePtr>());
				if(m) {
					res.push_back(std::make_pair(cur, m.get()));
				}
			}, true, matchTypes);

			return res;
		}
	}

	vector<core::NodePtr> collectAll(const TreePattern& pattern, const core::NodePtr& root, bool matchTypes) {
		return collectAll<core::NodePtr>(pattern, root, matchTypes);
	}
	vector<core::NodeAddress> collectAll(const TreePattern& pattern, const core::NodeAddress& root, bool matchTypes) {
		return collectAll<core::NodeAddress>(pattern, root, matchTypes);
	}

	vector<pair<core::NodePtr, NodeMatch>> collectAllPairs(const TreePattern& pattern, const core::NodePtr& root, bool matchTypes) {
		return collectAllPairs<core::NodePtr, NodeMatch>(pattern, root, matchTypes);
	}
	vector<pair<core::NodeAddress, AddressMatch>> collectAllPairs(const TreePattern& pattern, const core::NodeAddress& root, bool matchTypes) {
		return collectAllPairs<core::NodeAddress, AddressMatch>(pattern, root, matchTypes);
	}

	
	void matchAll(const TreePattern& pattern, const core::NodePtr& root, std::function<void(NodeMatch match)> lambda, bool matchTypes) {
		auto&& matches = collectAllPairs(pattern, root, matchTypes);
		for(auto match : matches) lambda(match.second);
	}
	void matchAll(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(AddressMatch match)> lambda, bool matchTypes) {
		auto&& matches = collectAllPairs(pattern, root, matchTypes);
		for(auto match : matches) lambda(match.second);
	}
	void matchAllPairs(const TreePattern& pattern, const core::NodePtr& root, std::function<void(core::NodePtr node, NodeMatch match)> lambda, bool matchTypes) {
		auto&& matches = collectAllPairs(pattern, root, matchTypes);
		for(auto match : matches) lambda(match.first, match.second);
	}
	void matchAllPairs(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda, bool matchTypes) {
		auto&& matches = collectAllPairs(pattern, root, matchTypes);
		for(auto match : matches) lambda(match.first, match.second);
	}
	void matchAllPairsReverse(const TreePattern& pattern, const core::NodeAddress& root, std::function<void(core::NodeAddress addr, AddressMatch match)> lambda, bool matchTypes) {
		auto&& matches = collectAllPairs(pattern, root, matchTypes);
		for(auto it=matches.rbegin(); it!=matches.rend(); it++) lambda(it->first, it->second);
	}

	namespace {
		NodePtr callLambda(std::function<core::NodePtr(AddressMatch)> fun, AddressMatch match, const NodeAddress& addr) {
			return fun(match);
		}
		NodePtr callLambda(std::function<core::NodePtr(const NodeAddress&)> fun, AddressMatch match, const NodeAddress& addr) {
			return fun(addr);
		}

		template<typename LambdaArgType>
		NodePtr replaceAllInternal(const TreePattern& pattern, const core::NodePtr& root, std::function<core::NodePtr(LambdaArgType match)> lambda, bool matchTypes, bool passAddresses) {
			// visit in preorder and collect matches, then go through them in reverse (postorder)
			// postorder implies that we can easily handle non-overlapping results
			auto&& res = collectAllPairs<core::NodeAddress, AddressMatch>(pattern, NodeAddress(root), matchTypes);

			auto ret = root;
			while(!res.empty()) {
				auto next = res.back().first;
				auto nextMatch = res.back().second;
				res.pop_back();
				auto newAddr = next.switchRoot(ret);
				// first try unchanged
				if(nextMatch.getRoot().getAddressedNode() == newAddr.getAddressedNode()) {
					ret = core::transform::replaceAddress(root->getNodeManager(), newAddr, callLambda(lambda, nextMatch, newAddr)).getRootNode();
				} else {
					// try rematching
					AddressMatchOpt mo = pattern.matchAddress(NodeAddress(newAddr.getAddressedNode()));
					if (mo) {
						ret = core::transform::replaceAddress(root->getNodeManager(), newAddr, callLambda(lambda, mo.get(), newAddr)).getRootNode();
					}
				}
			}
			return ret;
		}
	}

	NodePtr replaceAll(const TreePattern& pattern, const core::NodePtr& root, 
			std::function<core::NodePtr(AddressMatch match)> lambda, bool matchTypes) {
		return replaceAllInternal(pattern, root, std::function<core::NodePtr(AddressMatch)>(lambda), matchTypes, false);
	}


	NodePtr replaceAllAddr(const TreePattern& pattern, const core::NodePtr& root,
		std::function<core::NodePtr(const NodeAddress& matchingAddr)> lambda, bool matchTypes) {
			return replaceAllInternal(pattern, root, 
				std::function<core::NodePtr(const NodeAddress&)>(lambda), matchTypes, true);
	}

} // end namespace irp
} // end namespace pattern
} // end namespace core
} // end namespace insieme


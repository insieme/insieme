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

#include "insieme/core/analysis/compare.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace analysis {

	namespace {
		using TagMap = utils::map::PointerMap<NodePtr, unsigned>;
		/// generates a mapping from TT/Lambda reference nodes to unsigned ints
		class TagMapGenerator : public IRVisitor<void> {
			TagMap map;
			unsigned id;

		  protected:
			void visitNode(const NodePtr& node) {
				// visit all sub-nodes
				this->visitAll(node->getChildList());
			}

			void visitRef(const NodePtr& ref) {
				if(map.find(ref) == map.end()) { map[ref] = ++id; }
				visitNode(ref);
			}

			void visitTagTypeReference(const TagTypeReferencePtr& ref) { visitRef(ref); }
			void visitLambdaReference(const LambdaReferencePtr& ref) { visitRef(ref); }

		  public:
			TagMapGenerator() : IRVisitor(true) {}
			TagMap run(const NodePtr& node) {
				map.clear();
				id = 0;
				visit(node);
				return map;
			}
		};
		
		/// compares A and B while applying maps to references
		bool equalNamelessImpl(const NodePtr& nodeA, const NodePtr& nodeB, const TagMap& mapA, const TagMap& mapB) {
			auto ntA = nodeA->getNodeType();
			auto ntB = nodeB->getNodeType();
			if(ntA != ntB) return false;

			// for references, compare mapped id
			if(ntA == NT_TagTypeReference || ntA == NT_LambdaReference) {
				return mapA.find(nodeA)->second == mapB.find(nodeB)->second;
			} 
			// compare values directly
			else if(nodeA->isValue()) {
				return nodeA == nodeB;
			}
			// else traverse further
			else {
				auto childrenA = nodeA->getChildList();
				auto childrenB = nodeB->getChildList();
				if(childrenA.size() != childrenB.size()) return false;
				for(size_t i = 0; i < childrenA.size(); ++i) {
						if(!equalNamelessImpl(childrenA[i], childrenB[i], mapA, mapB)) return false;
				}
			}
			return true;
		}
	}

	bool equalNameless(const NodePtr& nodeA, const NodePtr& nodeB) {
		if(nodeA == nodeB) return true;

		TagMapGenerator tmg;
		auto mapA = tmg.run(nodeA);
		auto mapB = tmg.run(nodeB);
		VLOG(3) << "equalNameless map A: " << mapA << "\n";
		VLOG(3) << "equalNameless map B: " << mapB << "\n";

		return equalNamelessImpl(nodeA, nodeB, mapA, mapB);
	}

} // namespace analysis
} // namespace core
} // namespace insieme

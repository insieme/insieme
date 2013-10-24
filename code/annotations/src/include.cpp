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

#include "insieme/annotations/c/include.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace annotations {
namespace c {

	using namespace insieme::core;

	/**
	 * The value annotation type to be attached to nodes to store
	 * the actual name.
	 * this tag is not migratable, this means that it gets lost while translation units merge
	 */
	struct IncludeTag : public value_annotation::migratable { 
		string include;
		IncludeTag(const string& include) : include(include) {}
		bool operator==(const IncludeTag& other) const { return include == other.include; }
		bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			if(before.isa<GenericTypePtr>() && after.isa<GenericTypePtr>()
				//For intercepted types which are represented as generic types support migration
				&& before.as<GenericTypePtr>()->getFamilyName() == after.as<GenericTypePtr>()->getFamilyName()) {
				after->attachValue(IncludeTag(include)); 
				return true;
			}
			
			if( before.isa<LiteralPtr>() && after.isa<LiteralPtr>() &&
				before.as<LiteralPtr>()->getType().isa<FunctionTypePtr>() && 
				after.as<LiteralPtr>()->getType().isa<FunctionTypePtr>() && 
				//For intercepted functions which are represented as literals with function types support migration
				(before.as<LiteralPtr>()->getType().as<FunctionTypePtr>()->getFunctionKind() == after.as<LiteralPtr>()->getType().as<FunctionTypePtr>()->getFunctionKind())
				) {
				after->attachValue(IncludeTag(include)); 
				std::cout << "IncludeTag " << before << " " << after << std::endl;
				return true;
			}

			return false;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(IncludeTag)

		typedef core::value_node_annotation<IncludeTag>::type annotation_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only include annotations supported!");
			return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue().include);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert(encoder::isEncodingOf<string>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");
			return std::make_shared<annotation_type>(IncludeTag(encoder::toValue<string>(node)));
		}
	};

	// ----------------------------------------------------

	bool hasIncludeAttached(const NodePtr& node) {
		return node->hasAttachedValue<IncludeTag>();
	}

	const string& getAttachedInclude(const NodePtr& node) {
		assert(hasIncludeAttached(node) && "Does not have a Include annotation!");
		return node->getAttachedValue<IncludeTag>().include;
	}

	void attachInclude(const NodePtr& node, const string& include) {
		node->attachValue(IncludeTag(include));
	}


} // end namespace c
} // end namespace annotations
} // end namespace insieme

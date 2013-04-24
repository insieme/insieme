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

#include "insieme/core/ir_visitor.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace core {

	template<
		typename ResultType = void,
		template<class Target> class Ptr = Pointer
	>
	class CachedVisitor : public IRVisitor<ResultType, Ptr> {

		mutable utils::cache::Cache<Ptr<const Node>, ResultType> cache;

	public:

		CachedVisitor(bool visitTypes = false)
			: IRVisitor<ResultType, Ptr>(visitTypes), cache(fun(*this, &CachedVisitor::resolve)) {}

		ResultType visit(const Ptr<const Node>& node) {
			if (!this->isVisitingTypes() && node->getNodeCategory() == NC_Type) return ResultType();
			return cache.get(node);
		}

		ResultType visit(const Ptr<const Node>& node) const {
			if (!this->isVisitingTypes() && node->getNodeCategory() == NC_Type) return ResultType();
			return cache.get(node);
		}

		ResultType operator() (const Ptr<const Node>& node) const {
			return visit(node);
		}

	protected:

		virtual ResultType resolve(const Ptr<const Node>& node) =0;
	};


	// ----- A Lambda Visitor Variant of the Cached Visitor ------


	template<typename ResType>
	struct rec_call {
		typedef CachedVisitor<ResType>& type;
	};


	template<
		typename Lambda,
		typename ResultType = typename lambda_traits<Lambda>::result_type
	>
	class CachedLambdaVisitor : public CachedVisitor<ResultType> {

	public:

		CachedLambdaVisitor(const Lambda& lambda, bool visitTypes)
			: CachedVisitor<ResultType>(visitTypes), lambda(lambda) {}

	protected:

		virtual ResultType resolve(const NodePtr& node) {
			return callLambda<ResultType>(node);
		}

	private:

		template<typename ResType>
		typename std::enable_if<lambda_traits<Lambda>::arity == 1, ResType>::type
		callLambda(const NodePtr& node) {
			return lambda(node);
		}

		template<typename ResType>
		typename std::enable_if<lambda_traits<Lambda>::arity == 2, ResType>::type
		callLambda(const NodePtr& node) {
			return lambda(node, *this);
		}

		Lambda lambda;
	};


	template<typename Lambda>
	CachedLambdaVisitor<Lambda> makeCachedLambdaVisitor(const Lambda& lambda, bool visitTypes = false) {
		return CachedLambdaVisitor<Lambda>(lambda, visitTypes);
	}


} // end namespace core
} // end namespace insieme




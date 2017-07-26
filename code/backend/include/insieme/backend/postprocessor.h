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

#include <memory>
#include <vector>

#include "insieme/utils/container_utils.h"
#include "insieme/backend/c_ast/forward_decls.h"

namespace insieme {
namespace backend {

	/**
	 * A generic interface for a post-processor to be applied to the resulting C AST.
	 * Such post-processors may be used to correct cosmetic issues with the generated code
	 * (e.g. to much brackets or casts) or to apply final optimization passes. However
	 * the later purpose is not encouraged!
	 */
	class PostProcessor {
	  public:
		/**
		 * A virtual destructor to support pointers to polymorph sub-classes.
		 */
		virtual ~PostProcessor(){};

		/**
		 * This function implements the actual post-processing step. The given code will be processed
		 * and the result will be returned. In the likely case that new nodes need to be constructed
		 * during the processing, the given manager will be used.
		 *
		 * @param manager the manager to be used to create new node instances
		 * @param code the code to be post-processed
		 * @return the result of the post-processing step.
		 */
		virtual c_ast::NodePtr process(c_ast::CNodeManager& manager, const c_ast::NodePtr& code) = 0;
	};


	/**
	 * A type definition for a shared pointer referencing a post-processor instance.
	 * This shared instance is required within connectors.
	 */
	typedef std::shared_ptr<PostProcessor> PostProcessorPtr;

	/**
	 * A generic factory method creating post-processor pointer instances.
	 */
	template <typename T, typename... E>
	PostProcessorPtr makePostProcessor(E... args) {
		return std::make_shared<T>(args...);
	}


	// -------------------------------------------------------------------------
	//  Utilities
	// -------------------------------------------------------------------------

	/**
	 * Applies the given post-processor on the given list of fragments. This will alter
	 * the given fragments.
	 *
	 * @param processor the post-processor to be applied on the given fragments.
	 * @param fragments the fragments to be processed
	 */
	void applyToAll(const PostProcessorPtr& processor, vector<c_ast::CodeFragmentPtr>& fragments);


	// -------------------------------------------------------------------------
	//  Some post-processing connectors
	// -------------------------------------------------------------------------

	/**
	 * A simple post-processing connector aggregating a sequence of post-processing steps into
	 * a single post-processing instance.
	 */
	class PostProcessingSequence : public PostProcessor {
		/**
		 * The sequence of post-processing steps to be applied when applying this post processor.
		 */
		const vector<PostProcessorPtr> processors;

	  public:
		/**
		 * A generic constructor allowing to specify an arbitrary number of post-processors.
		 */
		template <typename... P>
		PostProcessingSequence(P... processors)
		    : processors(toVector<PostProcessorPtr>(processors...)) {}

		/**
		 * Applies this post-processor on the given target code. Therefore, the internally maintained
		 * sequence of post-processing steps will be applied in order.
		 *
		 * @param manager the manager to be used to create new node instances
		 * @param code the code to be post-processed
		 * @return the result of the post-processing step.
		 */
		virtual c_ast::NodePtr process(c_ast::CNodeManager& manager, const c_ast::NodePtr& code);
	};


	// -------------------------------------------------------------------------
	//  Concrete implementations
	// -------------------------------------------------------------------------

	/**
	 * A post-processor performing no actual processing. This post-processor can be used when aiming
	 * on disabling the post-processing (according to the null-pattern).
	 */
	class NoPostProcessing : public PostProcessor {
	  public:
		virtual c_ast::NodePtr process(c_ast::CNodeManager& manager, const c_ast::NodePtr& code);
	};


} // end namespace backend
} // end namespace insieme

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

#include <memory>
#include <vector>

#include "insieme/utils/container_utils.h"
#include "insieme/core/forward_decls.h"

namespace insieme {
namespace backend {

	/**
	 * A generic interface for a pre-processor to be applied to some IR code before being
	 * transformed into target code. The generic interface defines the basic interface
	 * according to the composite and command pattern.
	 */
	class PreProcessor {

	public:

		/**
		 * A virtual destructor to support pointers to polymorph sub-classes.
		 */
		virtual ~PreProcessor() {};

		/**
		 * This function implements the actual pre-processing step. The given code will be processed
		 * and the result will be returned. In the likely case that new nodes need to be constructed
		 * during the processing, the given manager will be used.
		 *
		 * @param manager the manager to be used to create new node instances
		 * @param code the code to be pre-processed
		 * @return the result of the pre-processing step.
		 */
		virtual core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code) =0;

	};


	/**
	 * A type definition for a shared pointer referencing a preprocessor instance.
	 * This shared instance is required within connectors.
	 */
	typedef std::shared_ptr<PreProcessor> PreProcessorPtr;

	/**
	 * A generic factory method creating pre-processor pointer instances.
	 */
	template<typename T, typename ... E>
	PreProcessorPtr makePreProcessor(E ... args) {
		return std::make_shared<T>(args...);
	}

	// -------------------------------------------------------------------------
	//  Some pre-processing connectors
	// -------------------------------------------------------------------------

	/**
	 * A simple pre-processing connector aggregating a sequence of pre-processing steps into
	 * a single pre-processing instance.
	 */
	class PreProcessingSequence : public PreProcessor {

		/**
		 * The sequence of pre-processing steps to be applied when applying this pre processor.
		 */
		const vector<PreProcessorPtr> preprocessor;

	public:

		/**
		 * A generic constructor allowing to specify an arbitrary number of preprocessors.
		 */
		template<typename ... P>
		PreProcessingSequence(P ... processors)
			: preprocessor(toVector<PreProcessorPtr>(processors ...)) {}

		/**
		 * Applies this pre-processor on the given target code. Therefore, the internally maintained
		 * sequence of pre-processing steps will be applied in order.
		 *
		 * @param manager the manager to be used to create new node instances
		 * @param code the code to be pre-processed
		 * @return the result of the pre-processing step.
		 */
		virtual core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code);

	};


	// ------- concrete pre-processing step implementations ---------

	/**
	 * A pre-processor performing no actual pre-processing. This pre-processor can be used when aiming
	 * on disabling the pre-processing (according to the null-pattern).
	 */
	class NoPreProcessing : public PreProcessor {
	public:
		virtual core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * Replaces all occurrences of ITE calls with lazy-ITE calls. Lazy-ITE calls correspond to the C-equivalent,
	 * where the if / then branch is only evaluated after evaluating the boolean condition.
	 */
	class IfThenElseInlining : public PreProcessor {
	public:
		virtual core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code);
	};


} // end namespace backend
} // end namespace insieme

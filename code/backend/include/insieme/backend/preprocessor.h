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
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code) =0;

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

	/**
	 * Flags allowing to fine-tune the pre-processing actions being conducted by the
	 * basic pre-processing sequence.
	 */
	enum BasicPreprocessorFlags {
		NONE 									= 0,
		SKIP_POINTWISE_EXPANSION 				= 1,
		SKIP_GENERIC_LAMBDA_INSTANTIATION 		= 2,
		SKIP_RESTORE_GLOBALS 					= 4
	};

	/**
	 * Obtains a basic pre-processor sequence including processing steps potentially used by
	 * all backend variants. The list includes all pre-processors defined within this header file.
	 *
	 * @return a list of pre-processor instances - one of each kind
	 */
	PreProcessorPtr getBasicPreProcessorSequence(BasicPreprocessorFlags options = NONE);

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
		 * A simple constructor accepting the list of pre-processors covered by this sequence.
		 */
		PreProcessingSequence(const vector<PreProcessorPtr>& processors)
			: preprocessor(processors) {}

		/**
		 * Applies this pre-processor on the given target code. Therefore, the internally maintained
		 * sequence of pre-processing steps will be applied in order.
		 *
		 * @param manager the manager to be used to create new node instances
		 * @param code the code to be pre-processed
		 * @return the result of the pre-processing step.
		 */
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);

	};


	// ------- concrete pre-processing step implementations ---------

	/**
	 * A pre-processor performing no actual pre-processing. This pre-processor can be used when aiming
	 * on disabling the pre-processing (according to the null-pattern).
	 */
	class NoPreProcessing : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * A pre-processor replacing all initZero calls with actual instantiated, equivalent zero values.
	 */
	class InitZeroSubstitution : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * This pre-processor is restoring global variables by identifying the global struct and replacing it
	 * with a literal substitution.
	 */
	class RestoreGlobals : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * A simple pre-processor replacing pointwise operations on vectors with in-lined, equivalent code.
	 */
	class InlinePointwise : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * A pre-processor replacing generic lambdas operating on type variables with their actual instantiation
	 * based on the invocation context.
	 */
	class GenericLambdaInstantiator : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * A pre-processor making implicit vector to array conversions explicit.
	 */
	class MakeVectorArrayCastsExplicit : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

	/**
	 * Eliminates assignments to or declarations of dead variables.
	 */
	class RedundancyElimination : public PreProcessor {
	public:
		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

} // end namespace backend
} // end namespace insieme

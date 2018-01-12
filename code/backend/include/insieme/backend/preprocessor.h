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
#include "insieme/core/forward_decls.h"
#include "insieme/backend/converter.h"

namespace insieme {
namespace backend {

	/**
	 * A generic interface for a pre-processor to be applied to some IR code before being
	 * transformed into target code. The generic interface defines the basic interface
	 * according to the composite and command pattern.
	 */
	class PreProcessor : public utils::VirtualPrintable {
	  public:
		/**
		 * A virtual destructor to support pointers to polymorph sub-classes.
		 */
		virtual ~PreProcessor(){};

		/**
		 * This function implements the actual pre-processing step. The given code will be processed
		 * and the result will be returned. In the likely case that new nodes need to be constructed
		 * during the processing, the given manager will be used.
		 *
		 * @param converter the converter forming the context of this pre-processor invocation
		 * @param code the code to be pre-processed
		 * @return the result of the pre-processing step.
		 */
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code) = 0;
	};


	/**
	 * A type definition for a shared pointer referencing a preprocessor instance.
	 * This shared instance is required within connectors.
	 */
	typedef std::shared_ptr<PreProcessor> PreProcessorPtr;

	/**
	 * A generic factory method creating pre-processor pointer instances.
	 */
	template <typename T, typename... E>
	std::shared_ptr<T> makePreProcessor(E... args) {
		return std::make_shared<T>(args...);
	}

	/**
	 * Flags allowing to fine-tune the pre-processing actions being conducted by the
	 * basic pre-processing sequence.
	 */
	enum BasicPreprocessorFlags { NONE = 0, SKIP_POINTWISE_EXPANSION = 1 };

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
		template <typename... P>
		PreProcessingSequence(P... processors)
		    : preprocessor(toVector<PreProcessorPtr>(processors...)) {}

		/**
		 * A simple constructor accepting the list of pre-processors covered by this sequence.
		 */
		PreProcessingSequence(const vector<PreProcessorPtr>& processors) : preprocessor(processors) {}

		/**
		 * Applies this pre-processor on the given target code. Therefore, the internally maintained
		 * sequence of pre-processing steps will be applied in order.
		 *
		 * @param converter the converter forming the context of this pre-processor invocation
		 * @param code the code to be pre-processed
		 * @return the result of the pre-processing step.
		 */
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A generic factory function for pre-processor sequences.
	 */
	template <typename... P>
	PreProcessorPtr makePreProcessorSequence(const P&... processors) {
		return makePreProcessor<PreProcessingSequence>(processors...);
	}


	// ------- concrete pre-processing step implementations ---------

	/**
	 * A pre-processor performing no actual pre-processing. This pre-processor can be used when aiming
	 * on disabling the pre-processing (according to the null-pattern).
	 */
	class NoPreProcessing : public PreProcessor {
	  public:
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "NoPreProcessing"; }
	};

	/**
	 * A simple pre-processor replacing pointwise operations on vectors with in-lined, equivalent code.
	 */
	class InlinePointwise : public PreProcessor {
	  public:
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "InlinePointwise"; }
	};

	/**
	 * Eliminated unnecessary function pointers being passed as argument within mutual
	 * recursive functions.
	 */
	class CorrectRecVariableUsage : public PreProcessor {
	  public:
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "CorrectRecVariableUsage"; }
	};

	/**
	 * It may happen that we try to forward generic arguments to inner lambdas. This preprocessor
	 * should identify this issue and provide a correct instantiator for such lambdas.
	 */
	class RecursiveLambdaInstantiator : public PreProcessor {
	  public:
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "RecursiveLambdaInstantiator"; }
	};

	/**
	 * Determines whether defaulted methods are ever called.
	 * (because if so, they need to be generated in their respective structs even in case those structs are C-like otherwise)
	 */
	class DefaultedMemberCallMarker : public PreProcessor {
	  public:
		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "DefaultedMemberCallMarker"; }
	};

	/**
	 * Performs "interception" in the backend - or at least mimics it.
	 * TagTypes and LambdaExprs which's name start with registered prefixes get replaced by constructs which look like
	 * if the frontend had originally intercepted them.
	 */
	class BackendInterceptor : public PreProcessor {

		std::vector<std::pair<std::string, std::string>> backendInterceptions;

	  public:
		void addBackendInterception(const std::string& namePtefix, const std::string& headerToAttach);

		virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code);

		virtual std::ostream& printTo(std::ostream& out) const { return out << "BackendInterceptor"; }
	};

} // end namespace backend
} // end namespace insieme

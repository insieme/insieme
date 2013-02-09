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

#include <cassert>
#include <memory>

#include <boost/noncopyable.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/backend/c_ast/forward_decls.h"

#include "insieme/backend/variable_manager.h"

namespace insieme {
namespace backend {

	// forward declaration of preprocessor
	class PreProcessor;
	typedef std::shared_ptr<PreProcessor> PreProcessorPtr;

	class PostProcessor;
	typedef std::shared_ptr<PostProcessor> PostProcessorPtr;

	// forward declaration of involved managers
	class NameManager;
	class TypeManager;
	class VariableManager;
	class StmtConverter;
	class FunctionManager;

	class TargetCode;
	typedef std::shared_ptr<TargetCode> TargetCodePtr;

	/**
	 * The core converter class of the backend framework. A converter instance
	 * collects a list of pointers to managers handling particular parts of the backend
	 * conversion process. For each of those managers, an abstract virtual base class
	 * (interface) is defined. Individual implementations of backends may exchange the
	 * particular implementations to customize the code generation.
	 */
	class Converter : private boost::noncopyable {

		// ------- The Pre- and Post- Processors applied before the conversion -----------

		/**
		 * The pre-processor will be applied on the IR before the conversion into the
		 * target code (C AST) will be conducted. Preprocessors should be used to conduct
		 * analysis on the given IR required during the conversion process to simplify the
		 * actual conversion within the subsequent converter and manager implementations.
		 *
		 * Also, pre-processors should be used to specialize implementations for individual
		 * target platforms (sequential code, shared memory, distributed memory or OpenCL)
		 * and to conduct target specific optimizations.
		 */
		PreProcessorPtr preProcessor;

		/**
		 * The post-processor will be applied on the resulting C AST after the conversion
		 * IR <=> C AST took place. Post-processors may be used to introduce additional C
		 * constructs like instrumentation code into the resulting AST or to performe some
		 * target code based code optimizations.
		 */
		PostProcessorPtr postProcessor;

		// ------- Manager involved in the conversion process -----------

		/**
		 * The name manager is used to obtain identifiers for types, variables and functions
		 * within the generated code. This manager serves mainly cosmetic purposes.
		 */
		NameManager* nameManager;

		/**
		 * The type manager is responsible for resolving IR types and converting them into
		 * equivalent C code. Also, it is responsible for ensuring that type declarations
		 * and definitions are properly arranged within the resulting C code.
		 */
		TypeManager* typeManager;

		/**
		 * The statement (and expression) converter is responsible for converting
		 * all IR statements and expressions into equivalent C constructs. It further
		 * supports the conversion of IR programs. It therefore uses the services
		 * offered by the remaining manages and may also be used recursively by the
		 * other managers.
		 *
		 * The statement converter is the main entry point when triggering a IR to C
		 * conversion.
		 */
		StmtConverter* stmtConverter;

		/**
		 * The function manager is responsible for creating function declarations and
		 * definitions. Further, it is responsible for resolving function calls and handling
		 * functions when being passed as first class citizens within the IR. Whenever
		 * a function call, a bind or a lambda expression is encountered during the
		 * conversion process, the handling of those is being passed on to the function
		 * manager (by the statement converter).
		 */
		FunctionManager* functionManager;

		// -------- Node Managers for Source and Target Code ------------

		core::NodeManager* nodeManager;

		// NOTE: shared pointer, since it has to survive the conversion process
		c_ast::SharedCodeFragmentManager fragmentManager;

		const std::string converterName;

	public:

		/**
		 * Creates a new uninitialized converter. Before using the resulting
		 * converter, the required managers need to be initialized.
		 */
		Converter(std::string name = "Backend") :
			preProcessor(), postProcessor(), nameManager(0), typeManager(0), stmtConverter(0),
			functionManager(0), converterName(name) {}

		/**
		 * A call to this member function triggers the actual conversion process.
		 * The given IR code fragment will be converted into equivalent target code. The
		 * function may be invoked multiple times, however, the result may not be the same
		 * when being invoked using the same input multiple times (e.g. variable names
		 * may change).
		 *
		 * @param code the code to be converted
		 * @return the resulting target code
		 */
		backend::TargetCodePtr convert(const core::NodePtr& code);

		const PreProcessorPtr& getPreProcessor() const {
			assert(preProcessor);
			return preProcessor;
		}

		void setPreProcessor(PreProcessorPtr newPreProcessor) {
			preProcessor = newPreProcessor;
		}

		const PostProcessorPtr& getPostProcessor() const {
			assert(postProcessor);
			return postProcessor;
		}

		void setPostProcessor(PostProcessorPtr newPostProcessor) {
			postProcessor = newPostProcessor;
		}

		NameManager& getNameManager() const {
			assert(nameManager);
			return *nameManager;
		}

		void setNameManager(NameManager* manager) {
			nameManager = manager;
		}

		TypeManager& getTypeManager() const {
			assert(typeManager);
			return *typeManager;
		}

		void setTypeManager(TypeManager* manager) {
			typeManager = manager;
		}

		StmtConverter& getStmtConverter() const {
			assert(stmtConverter);
			return *stmtConverter;
		}

		void setStmtConverter(StmtConverter* converter) {
			stmtConverter = converter;
		}

		FunctionManager& getFunctionManager() const {
			assert(functionManager);
			return *functionManager;
		}

		void setFunctionManager(FunctionManager* manager) {
			functionManager = manager;
		}

		core::NodeManager& getNodeManager() const {
			assert(nodeManager);
			return *nodeManager;
		}

		void setNodeManager(core::NodeManager* manager) {
			nodeManager = manager;
		}

		const c_ast::SharedCodeFragmentManager& getFragmentManager() const {
			assert(fragmentManager);
			return fragmentManager;
		}

		void setFragmentManager(const c_ast::SharedCodeFragmentManager& manager) {
			fragmentManager = manager;
		}

		const c_ast::SharedCNodeManager& getCNodeManager() const;

		const std::string& getConverterName() const { return converterName; }

	};

	/**
	 * The conversion context offers a common data container which can be used to
	 * maintain conversion specific information during a single run of the conversion
	 * process. A fresh instance will be default constructed before starting a conversion
	 * and forwarded recursively throughout the translation process. It should therefore
	 * allow to implement the actual conversion routines in a stateless manner such that
	 * multiple, concurrent calls may be supportable in the future.
	 */
	class ConversionContext :  public boost::noncopyable {

		/**
		 * A reference to the converter processing the current conversion.
		 */
		const Converter& converter;

		/**
		 * A reference to a set of dependencies aggregated during the conversion.
		 * Dependencies are defining the set of code fragments to be necessarily placed
		 * before the current code fragment within the resulting output code.
		 */
		c_ast::FragmentSet dependencies;

		/**
		 * A reference to a set of requirements aggregated during the conversion.
		 * Requirements are fragments which have to be present at some point within the
		 * output code.
		 */
		c_ast::FragmentSet requirements;

		/**
		 * A variable manager maintaining the variables of the current scope.
		 */
		VariableManager variableManager;

		/**
		 * The list of include files the currently converted code fragment is
		 * depending on.
		 */
		std::set<string> includes;

	public:

		/**
		 * Creates a new, empty context instance to be used during a conversion
		 * run of the given converter.
		 *
		 * @param converter the converter which will be using the resulting context.
		 */
		ConversionContext(const Converter& converter)
			: converter(converter), dependencies(), requirements(), variableManager(), includes() {}

		const Converter& getConverter() const {
			return converter;
		}

		void addDependency(const c_ast::CodeFragmentPtr& fragment) {
			dependencies.insert(fragment);
		}

		c_ast::FragmentSet& getDependencies() {
			return dependencies;
		}

		const c_ast::FragmentSet& getDependencies() const {
			return dependencies;
		}

		void addRequirement(const c_ast::CodeFragmentPtr& fragment) {
			requirements.insert(fragment);
		}

		c_ast::FragmentSet& getRequirements() {
			return requirements;
		}

		const c_ast::FragmentSet& getRequirements() const {
			return requirements;
		}

		VariableManager& getVariableManager() {
			return variableManager;
		}

		std::set<string>& getIncludes() {
			return includes;
		}

		void addInclude(const string& include) {
			includes.insert(include);
		}

	};


} // end namespace backend
} // end namespace insieme

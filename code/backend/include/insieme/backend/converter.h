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

	// forward declaration of involved managers
	class NameManager;
	class TypeManager;
	class VariableManager;
	class StmtConverter;
	class FunctionManager;
	class ParallelManager;

	class TargetCode;
	typedef std::shared_ptr<TargetCode> TargetCodePtr;


	struct ConverterConfig {
		bool supportArrayLength;


		static ConverterConfig getDefault() {
			ConverterConfig res;
			res.supportArrayLength = false;
			return res;
		}
	};

	class Converter {

		// ------- The Preprocessor applied before the conversion -----------

		PreProcessor* preProcessor;

		// ------- Manager involved in the conversion process -----------

		NameManager* nameManager;
		TypeManager* typeManager;
		VariableManager* variableManager;
		StmtConverter* stmtConverter;
		FunctionManager* functionManager;
		ParallelManager* parallelManager;

		// -------- Node Managers for Source and Target Code ------------

		core::NodeManager* nodeManager;

		// NOTE: shared pointer, since it has to survive the conversion process
		c_ast::SharedCNodeManager cNodeManager;


		// ----------- Overall Conversion Configuration ----------------

		ConverterConfig config;


	public:

		Converter() :
			preProcessor(0), nameManager(0), typeManager(0), variableManager(0), stmtConverter(0),
			functionManager(0), parallelManager(0), config(ConverterConfig::getDefault()) {}

		Converter(const ConverterConfig& config) :
			preProcessor(0), nameManager(0), typeManager(0), variableManager(0), stmtConverter(0),
			functionManager(0), parallelManager(0), config(config) {}

		backend::TargetCodePtr convert(const core::NodePtr& code);

		PreProcessor& getPreProcessor() const {
			assert(preProcessor);
			return *preProcessor;
		}

		void setPreProcessor(PreProcessor* newPreProcessor) {
			preProcessor = newPreProcessor;
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

		ParallelManager& getParallelManager() const {
			assert(parallelManager);
			return *parallelManager;
		}

		void setParallelManager(ParallelManager* manager) {
			parallelManager = manager;
		}

		core::NodeManager& getNodeManager() const {
			assert(nodeManager);
			return *nodeManager;
		}

		void setNodeManager(core::NodeManager* manager) {
			nodeManager = manager;
		}

		const c_ast::SharedCNodeManager& getCNodeManager() const {
			assert(cNodeManager);
			return cNodeManager;
		}

		void setCNodeManager(const c_ast::SharedCNodeManager& manager) {
			cNodeManager = manager;
		}

		ConverterConfig& getConfig() {
			return config;
		}

		const ConverterConfig& getConfig() const {
			return config;
		}

		void setConfig(const ConverterConfig& newConfig) {
			config = newConfig;
		}

	};

	class ConversionContext :  public boost::noncopyable {

		/**
		 * A reference to the converter processing the current conversion.
		 */
		const Converter& converter;

		/**
		 * A reference to a set of dependencies aggregated during the conversion.
		 */
		c_ast::DependencySet dependencies;

		/**
		 * A variable manager maintaining the variables of the current scope.
		 */
		VariableManager variableManager;

		/**
		 * The list of includes depending on.
		 */
		std::set<string> includes;

	public:

		ConversionContext(const Converter& converter)
			: converter(converter), dependencies(), variableManager(), includes() {}

		const Converter& getConverter() const {
			return converter;
		}

		c_ast::DependencySet& getDependencies() {
			return dependencies;
		}

		const c_ast::DependencySet& getDependencies() const {
			return dependencies;
		}

		VariableManager& getVariableManager() {
			return variableManager;
		}

		std::set<string>& getIncludes() {
			return includes;
		}

	};


} // end namespace backend
} // end namespace insieme

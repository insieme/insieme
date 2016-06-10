/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/converter.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/logging.h"

#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/postprocessor.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"

#include "insieme/core/lang/lang.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace backend {

	Converter::Converter(core::NodeManager& nodeManager, const std::string& name, const BackendConfig& config)
	    : nodeManager(nodeManager), fragmentManager(c_ast::CodeFragmentManager::createShared()), converterName(name), config(config),
	      preProcessor(makePreProcessor<NoPreProcessing>()), postProcessor(makePostProcessor<NoPostProcessing>()),
	      nameManager(std::make_shared<SimpleNameManager>()), typeManager(std::make_shared<TypeManager>(*this)),
	      stmtConverter(std::make_shared<StmtConverter>(*this)), functionManager(std::make_shared<FunctionManager>(*this)) {}


	backend::TargetCodePtr Converter::convert(const core::NodePtr& source) {
		// -------------------------- PRE-PROCESSING ---------------------

		utils::Timer timer = insieme::utils::Timer(getConverterName() + " Preprocessing");

		assert_true(core::checks::check(source).empty())
			<< "Invalid IR passed to backend: " << core::checks::check(source);

		// pre-process program
		core::NodePtr processed = getPreProcessor()->process(*this, source);

		assert_true(core::checks::check(processed).empty())
			<< "Errors introduced by pre-processors: " << core::printer::dumpErrors(core::checks::check(processed));

		timer.stop();
		LOG(INFO) << timer;

		// -------------------------- CONVERSION -------------------------

		timer = insieme::utils::Timer(getConverterName() + " Conversions");

		// create a context
		ConversionContext context(*this, core::LambdaPtr());

		// register global names
		getNameManager().registerGlobalNames(processed);

		// convert IR node target code
		auto code = getStmtConverter().convert(context, processed);

		// create a code fragment out of it
		c_ast::CodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(fragmentManager, code);
		fragment->addDependencies(context.getDependencies());
		fragment->addRequirements(context.getRequirements());
		fragment->addIncludes(context.getIncludes());
		if(!config.additionalHeaderFiles.empty()) { fragment->addIncludes(config.additionalHeaderFiles); }

		vector<c_ast::CodeFragmentPtr> fragments = c_ast::getOrderedClosure(toVector(fragment));

		timer.stop();
		LOG(INFO) << timer;

		// ------------------------ POST-PROCESSING ----------------------

		timer = insieme::utils::Timer(getConverterName() + " Postprocessing");

		// apply post-processing passes
		applyToAll(getPostProcessor(), fragments);

		timer.stop();
		LOG(INFO) << timer;

		// --------------------------- Finalize --------------------------

		// create resulting code fragment
		return c_ast::CCode::createNew(fragmentManager, source, fragments);
	}

	namespace {

		struct PrettyPrinterPlugin : public core::printer::PrinterPlugin {
			// requires a reference to a name manager
			NameManager& nameManager;

			/**
			 * Create a new instance based on the content of the given name manager.
			 */
			PrettyPrinterPlugin(NameManager& nameManager) : nameManager(nameManager) {}


			virtual bool covers(const core::NodeAddress& node) const {
				// it is covered if it is of a certain type
				return node.isa<core::LambdaExprPtr>() || node.isa<core::TagTypePtr>();
			}

			/**
			 * A function triggered to print the given node to the given stream.
			 */
			virtual std::ostream& print(std::ostream& out, const core::NodeAddress& node, const std::function<void(const core::NodeAddress&)>&) const {
				return out << nameManager.getName(node);
			}
		};
	}


	c_ast::CommentPtr Converter::convertToComment(const core::NodePtr& node) const {
		// if not enabled, return no comment
		if(!getBackendConfig().addIRCodeAsComment) { return c_ast::CommentPtr(); }

		auto setup = core::printer::PrettyPrinter::OPTIONS_DEFAULT | core::printer::PrettyPrinter::Option::NO_LET_BINDINGS;

		// if enabled, create comment using the pretty printer infrastructure
		PrettyPrinterPlugin plugin(getNameManager());
		auto comment = toString(core::printer::PrettyPrinter(node, plugin, setup));

		// build comment
		return getCNodeManager()->create<c_ast::Comment>("\n" + comment + "\n");
	}


	const c_ast::SharedCNodeManager& Converter::getCNodeManager() const {
		assert_true(fragmentManager);
		return fragmentManager->getNodeManager();
	}


} // end namespace backend
} // end namespace insieme

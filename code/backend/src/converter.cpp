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

#include "insieme/backend/converter.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/logging.h"

#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/postprocessor.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/variable_manager.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace backend {


	backend::TargetCodePtr Converter::convert(const core::NodePtr& source) {

		// -------------------------- PRE-PROCESSING ---------------------

		// only for debugging purposes ...
//		LOG(INFO) << "\n\nBefore Preprocessed code: \n" << core::printer::PrettyPrinter(source, core::printer::PrettyPrinter::OPTIONS_DETAIL);
//		LOG(INFO) << "Semantic Checks Before Preprocessing: " << core::checks::check(source);
//		assert(core::checks::check(source).empty() && "Expected error free input program!");

		utils::Timer timer = insieme::utils::Timer(getConverterName() + " Preprocessing");

		// pre-process program
		core::NodePtr processed = getPreProcessor()->process(getNodeManager(), source);

		timer.stop();
		LOG(INFO) << timer;

		// only for debugging purposes ...
//		LOG(INFO) << "\nPreprocessed code: \n" << core::printer::PrettyPrinter(processed, core::printer::PrettyPrinter::OPTIONS_DETAIL);
//		LOG(INFO) << "Semantic Checks: " << core::checks::check(processed);
//		for_each(core::checks::check(processed).getAll(), [](const core::Message& msg) {
//			LOG(INFO) << msg << " @ " << *msg.getAddress();
//		});
//		assert(core::check(processed, core::checks::getFullCheck()).getErrors().empty() && "Errors encountered after pre-processing");

		// -------------------------- CONVERSION -------------------------

		timer = insieme::utils::Timer(getConverterName() + " Conversions");

		// create a context
		ConversionContext context(*this);

		// convert IR node target code
		auto code = getStmtConverter().convert(context, processed);

		// create a code fragment out of it
		c_ast::CodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(fragmentManager, code);
		fragment->addDependencies(context.getDependencies());
		fragment->addRequirements(context.getRequirements());
		fragment->addIncludes(context.getIncludes());

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


	const c_ast::SharedCNodeManager& Converter::getCNodeManager() const {
		assert(fragmentManager);
		return fragmentManager->getNodeManager();
	}


} // end namespace backend
} // end namespace insieme

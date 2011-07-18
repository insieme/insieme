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
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/variable_manager.h"


namespace insieme {
namespace backend {


	backend::TargetCodePtr Converter::convert(const core::NodePtr& source) {


		// conduct pre-processing
		utils::Timer timer = insieme::utils::Timer("Backend.Preprocessing");

		// TODO: make pre-processor an option

		// pre-process program
		core::NodePtr processed = getPreProcessor()->preprocess(getNodeManager(), source);

		timer.stop();
		LOG(INFO) << timer;

		timer = insieme::utils::Timer("Backend.Conversions");

		// create a context
		ConversionContext context(*this);

		// convert IR node target code
		auto code = getStmtConverter().convert(context, processed);

		// create a code fragment out of it
		c_ast::CodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(fragmentManager, code);
		fragment->addDependencies(context.getDependencies());
		fragment->addRequirements(context.getRequirements());
		fragment->addIncludes(context.getIncludes());

		// create C code
		auto res = c_ast::CCode::createNew(fragmentManager, source, fragment);

		timer.stop();
		LOG(INFO) << timer;

		// job done!
		return res;
	}


	const c_ast::SharedCNodeManager& Converter::getCNodeManager() const {
		assert(fragmentManager);
		return fragmentManager->getNodeManager();
	}


} // end namespace backend
} // end namespace insieme

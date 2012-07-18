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

#include "insieme/simple_backend/backend_convert.h"

#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/statement_converter.h"
#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/name_manager.h"
#include "insieme/simple_backend/transform/preprocessor.h"
#include "insieme/simple_backend/utils/simple_backend_utils.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include "insieme/core/printer/pretty_printer.h"


namespace insieme {
namespace simple_backend {
	
using namespace core;
using namespace insieme::utils::log;

	/**
	 * A map from Entry points to Code sections returned by ConversionContext::convert.
	 * Can be printed to any output stream
	 */
	class ConvertedCode : public backend::TargetCode {

		/**
		 * A special list of headers to be included. This fragment will always be printed
		 * before the fragment representing the actual target code.
		 */
		const std::vector<string> headers;

		/**
		 * The code fragment covering the actual target code.
		 */
		const CodeFragmentPtr code;

	public:

		/**
		 * A constructor for this class.
		 */
		ConvertedCode(const core::NodePtr& source, const std::vector<string>& headers, const CodeFragmentPtr& code)
			: TargetCode(source), headers(headers), code(code) { }

		/**
		 * This method allows to print the result to an output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {

			// print some general header information ...
			out << "// --- Generated Inspire Code ---\n";

			// print headers
			for_each(headers, [&](const string& cur) {
				out << cur << std::endl;
			});

			// add the program code
			return ::operator<<(out, this->code);
		}

	};

	
	void Converter::setMainName(const core::NodePtr& prog) {
		LOG(INFO) << "SET MAIN NAME A";
		// check for the program
		if (prog->getNodeType() != core::NT_Program) {
			return;
		}
		const core::ProgramPtr& program = static_pointer_cast<const core::Program>(prog);
		
		LOG(INFO) << "SET MAIN NAME C";
		// find entry func lambda
		const core::ExpressionPtr& mainExpr = program->getEntryPoints()[0];
		if (mainExpr->getNodeType() != core::NT_LambdaExpr) {
			return;
		}
		const core::LambdaExprPtr& main = static_pointer_cast<const core::LambdaExpr>(mainExpr);
		
		// set name of entry func
		LOG(INFO) << "SET MAIN NAME";
		getNameManager().setName(main->getLambda(), "main");
	}

	backend::TargetCodePtr Converter::convert(const core::NodePtr& prog) {

		// obtain headers
		std::vector<string> headers = stmtConverter->getHeaderDefinitions();

		// create a code fragment covering entire program
		CodeFragmentPtr code = CodeFragment::createNewDummy("Full Program");

		insieme::utils::Timer timer = insieme::utils::Timer("SimpleBackend.Preprocessing");

		// preprocess program
		NodeManager manager(prog->getNodeManager());
		core::NodePtr program = transform::preprocess(manager, prog);
		setMainName(program);

		timer.stop();
		LOG(INFO) << timer;
		timer = insieme::utils::Timer("SimpleBackend.Conversions");

		// convert code
		getStmtConverter().convert(program, code);

		timer.stop();
		LOG(INFO) << timer;

		// create resulting, converted code
		return std::make_shared<ConvertedCode>(prog, headers, code);
	}


}
}


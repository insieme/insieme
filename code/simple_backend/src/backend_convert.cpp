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

#include "insieme/core/types.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;
using namespace utils::log;

	/**
	 * A map from Entry points to Code sections returned by ConversionContext::convert.
	 * Can be printed to any output stream
	 */
	class ConvertedCode : public TargetCode {

		/**
		 * A special list of headers to be included. This fragment will always be printed
		 * before every other fragment.
		 */
		std::vector<string> headers;

		/**
		 * A map of code fragments this converted code is consisting of.
		 */
		utils::map::PointerMap<core::ExpressionPtr, CodeFragmentPtr> codeFragments;

	public:

		/**
		 * A constructor for this class.
		 */
		ConvertedCode(const core::ProgramPtr& source) : TargetCode(source) { }

		/**
		 * This method allows to print the result to an output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * Adds a header line to the generated code.
		 */
		void addHeaderLine(const string& line) {
			headers.push_back(line);
		}

		/**
		 * Adds a code fragment to the internally maintained list of fragments.
		 *
		 * @param source the source for this particular fragment
		 * @param fragment the the target code fragment to be stored
		 */
		void addFragment(const core::ExpressionPtr& source, CodeFragmentPtr& fragment) {
			codeFragments.insert(std::make_pair(source, fragment));
		}

	};


std::ostream& ConvertedCode::printTo(std::ostream& out) const {

	// print some general header information ...
	out << "// --- Generated Inspire Code ---\n";

	// print headers
	for_each(headers, [&](const string& cur) {
		out << cur << std::endl;
	});

	// add code for entry points
	for_each(getSource()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "// --- Entry Point ---\n";
		assert(this->codeFragments.find(ep) != this->codeFragments.end());
		::operator<<(out, this->codeFragments.find(ep)->second);
	});
	return out;
}

TargetCodePtr Converter::convert(const core::ProgramPtr& prog) {

	ConvertedCode* converted = new ConvertedCode(prog);

	// add headers
	auto headerList = stmtConverter->getHeaderDefinitions();
	for_each(headerList, [&](const string& cur) {
		converted->addHeaderLine(cur);
	});

	// convert the individual entry points
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {

		// create a fresh code fragment
		CodeFragmentPtr fragment = CodeFragment::createNewDummy("Full Program");

		// convert code
		getStmtConverter().convert(ep, fragment);

		// register fragment
		converted->addFragment(ep, fragment);
	});
	return TargetCodePtr(converted);
}


}
}


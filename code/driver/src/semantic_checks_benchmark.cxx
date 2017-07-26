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

#include <string>
#include <iomanip>
#include <fstream>
#include <boost/filesystem.hpp>

#include "insieme/utils/version.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/file_extensions.h"

#include "insieme/driver/utils/driver_utils.h"
#include "insieme/driver/cmd/commandline_options.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/dump/binary_dump.h"

#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/imperative_checks.h"
#include "insieme/core/checks/type_checks.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/checks/literal_checks.h"

#include "insieme/utils/timer.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace dr = insieme::driver;
namespace cmd = insieme::driver::cmd;
namespace du = insieme::driver::utils;


#define TIME_CHECK(CHECK_NAME) { std::cout << "Running check " << #CHECK_NAME << " ... "; \
	std::cout.flush(); \
	core::checks::MessageList checkResult; \
	auto time = TIME(checkResult = core::checks::check(program, core::checks::makeVisitOnce(core::checks::make_check<core::checks::CHECK_NAME>()))); \
	if(checkResult.size() != 0) std::cout << "Semantic errors encountered!\n\n" << checkResult << std::endl; \
	std::cout << "took: " << time << " seconds\n"; }


int main(int argc, char** argv) {
	std::cout << "Insieme compiler - Version: " << utils::getVersion() << "\n";

	// Step 1: parse input parameters
	std::string outFile;

	auto parser = driver::cmd::Options::getParser();
	parser.addParameter("outfile,o", outFile, std::string("a.out"), "output file");
	auto options = parser.parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	if(options.job.getFiles().size() != 1) {
		std::cout << "Please specify only one input file ...\n";
		return 1;
	}

	auto inputFile = options.job.getFiles()[0];

	auto ext = fs::extension(inputFile);

	co::NodeManager mgr;

	// read cpp, create binary dump
	if(ext == ".cpp" || ext == ".cxx") {

		std::cout << "Starting FE conversion ... ";
		std::cout.flush();
		core::ProgramPtr program;
		auto time = TIME(program = options.job.execute(mgr));
		std::cout << "done (took " << time << " seconds)\n";

		std::cout << "Creating binary dump in file " << outFile << " ... ";
		std::cout.flush();
		std::ofstream out(outFile);
		time = TIME(core::dump::binary::dumpIR(out, program));
		std::cout << "done (took " << time << " seconds)\n";

		// read binary dump, run semantic check
	} else {
		std::cout << "Reading IR from binary dump in file " << inputFile.string() << " ... ";
		std::cout.flush();
		std::ifstream in(inputFile.string());
		core::NodePtr program;
		auto time = TIME(program = core::dump::binary::loadIR(in, mgr));
		std::cout << "done (took " << time << " seconds)\n";

		std::cout << "Benchmarking execution of check ... " << std::endl;

		time = TIME(
		TIME_CHECK(DeclarationTypeCheck);
		TIME_CHECK(KeywordCheck);
		TIME_CHECK(FunctionKindCheck);
		TIME_CHECK(ParentCheck);
		TIME_CHECK(BindExprTypeCheck);
		TIME_CHECK(ExternalFunctionTypeCheck);
		TIME_CHECK(LambdaTypeCheck);
		TIME_CHECK(DeclarationStmtTypeCheck);
		TIME_CHECK(RefDeclTypeCheck);
		TIME_CHECK(IfConditionTypeCheck);
		TIME_CHECK(ForStmtTypeCheck);
		TIME_CHECK(WhileConditionTypeCheck);
		TIME_CHECK(SwitchExpressionTypeCheck);
		TIME_CHECK(InitExprTypeCheck);
		TIME_CHECK(TagTypeFieldsCheck);
		TIME_CHECK(EnumTypeCheck);
		TIME_CHECK(MemberAccessElementTypeCheck);
		TIME_CHECK(MemberAccessElementTypeInTagTypeCheck);
		TIME_CHECK(ComponentAccessTypeCheck);
		TIME_CHECK(BuiltInLiteralCheck);
		TIME_CHECK(RefCastCheck);
		TIME_CHECK(IllegalNumCastCheck);
		TIME_CHECK(IllegalNumTypeToIntCheck);
		TIME_CHECK(IllegalTypeInstantiationCheck);
		TIME_CHECK(CastCheck);
		TIME_CHECK(GenericZeroCheck);
		TIME_CHECK(ArrayTypeCheck);
		TIME_CHECK(GenericOpsCheck);

		TIME_CHECK(ConstructorTypeCheck);
		TIME_CHECK(DuplicateConstructorTypeCheck);
		TIME_CHECK(DestructorTypeCheck);
		TIME_CHECK(MemberFunctionTypeCheck);
		TIME_CHECK(DuplicateMemberFunctionCheck);
		TIME_CHECK(DuplicateMemberFieldCheck);

		TIME_CHECK(UndeclaredVariableCheck);

		// TIME_CHECK(UndefinedCheck>());
		TIME_CHECK(FreeBreakInsideForLoopCheck);
		TIME_CHECK(MissingReturnStmtCheck);
		TIME_CHECK(ValidInitExprMemLocationCheck);

		TIME_CHECK(LiteralFormatCheck);

		TIME_CHECK(ReturnTypeCheck);
		TIME_CHECK(CallExprTypeCheck);
		);

		std::cout << "done (took " << time << " seconds)\n";
	}

	return 0;
}

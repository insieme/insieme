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

#include <vector>
#include <iostream>
#include <memory>

#include <gtest/gtest.h>
#include "insieme/opencl_backend/opencl_convert.h"
#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/utils/set_utils.h"
#include "insieme/c_info/naming.h"

using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::c_info;
using namespace insieme::utils::set;
using namespace insieme::simple_backend;
using namespace insieme::backend::ocl;


#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/ocl/ocl_annotations.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/utils/logging.h"

using namespace insieme::utils::set;
using namespace insieme::utils::log;


/*ProgramPtr setupSampleProgram(ASTBuilder& build) {

	BasicGenerator typeGen(build.getNodeManager());

	TypeList printfArgType = toVector<TypePtr>(build.refType(typeGen.getChar()), typeGen.getVarList());
	TypePtr unitType = typeGen.getUnit();
	TypePtr printfType = build.functionType(printfArgType, unitType);

	auto printfDefinition = build.literal("printf", printfType);

	FunctionTypePtr voidNullaryFunctionType = build.functionType(TypeList(), unitType);

	ExpressionPtr stringLiteral = build.literal("Hello World!", typeGen.getString());
	auto invocation = build.callExpr(unitType, printfDefinition, toVector(stringLiteral));
	auto mainBody = build.compoundStmt(invocation);
	auto mainLambda = build.lambdaExpr(voidNullaryFunctionType, Lambda::ParamList(), mainBody);

	mainLambda->addAnnotation(std::make_shared<CNameAnnotation>(Identifier("main")));

	return build.createProgram(
		toSet<Program::EntryPointSet>(mainLambda)
	);
}

TEST(OpenCLBackend, Basic) {

	ASTBuilder build;
	ProgramPtr prog = setupSampleProgram(build);

	std::cout << "Start OpenCL visit\n";
	auto converted = insieme::backend::ocl::convert(prog);
	std::cout << "Converted code:\n" << *converted;
}*/
#include "insieme/frontend/ocl/ocl_host_compiler.h"

namespace fe = insieme::frontend;

TEST(OpenCLBackend, HelloCLTest) {
    NodeManager manager;
    ProgramPtr program = Program::create(manager);

	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "inputs");
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR));
//	CommandLineOptions::IncludePaths.push_back(std::string("/home/klaus/insieme/test/ocl_kernel/"));
	CommandLineOptions::Defs.push_back("INSIEME");
//	std::cout << "Converting input program '" << string(SRC_DIR) << "hello.cl" << "' to IR...\n";
	std::cout << "Converting input program '" << string(SRC_DIR) << "../../../test/ocl_kernel/runTestKernels.c" << "' to IR...\n";
	insieme::frontend::Program prog(manager);

	std::cout << SRC_DIR << std::endl;
	prog.addTranslationUnit(std::string(SRC_DIR) + "hello.cl");
//	prog.addTranslationUnit(string(SRC_DIR) + "../../../test/ocl_kernel/runTestKernels.c");
	program = prog.convert();
	std::cout << "Done.\n";

	LOG(INFO) << "Starting OpenCL host code transformations";
	fe::ocl::HostCompiler hc(program, manager);
	hc.compile();


	insieme::core::printer::PrettyPrinter pp(program);
	std::cout << "Printing the IR: " << pp;
	
	std::cout << "Start OpenCL Backend visit\n";
	auto backend = OpenCLBackend::getDefault();
	auto converted = backend->convert(program);
	std::cout << "Converted code:\n" << *converted;
}


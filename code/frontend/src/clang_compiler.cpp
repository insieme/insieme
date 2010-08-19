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

#include "clang_compiler.h"
#include "cmd_line_utils.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/DiagnosticOptions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"

#include "llvm/LLVMContext.h"
#include "llvm/System/Host.h"
#include "llvm/System/Path.h"

#include <iostream>

//#include "pragma_handler.hpp"
//#include "mpi/mpi_pragma_handler.hpp"
//#include "analysis/mpi/comm_stmt_matcher.hpp"
//#include "transformations/transformation_factory.hpp"

#include "llvm/Config/config.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/System/Signals.h"
#include "llvm/Support/Timer.h"

#include "clang/Parse/Parser.h"

#include "clang/Sema/ParseAST.h"
#include "lib/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Sema/ExternalSemaSource.h"

using namespace clang;

struct ClangCompiler::ClangCompilerImpl {
	CompilerInstance clang;
};

void InsiemeParseAST(Preprocessor &PP, ASTConsumer *Consumer,
					 ASTContext &Ctx, bool CompleteTranslationUnit) {

	Sema S(PP, Ctx, *Consumer, CompleteTranslationUnit);
	Parser P(PP, S);
	PP.EnterMainSourceFile();

	P.Initialize();
	Consumer->Initialize(Ctx);
	if (SemaConsumer *SC = dyn_cast<SemaConsumer>(Consumer))
		SC->InitializeSema(S);

	if (ExternalASTSource *External = Ctx.getExternalSource()) {
		if(ExternalSemaSource *ExternalSema = dyn_cast<ExternalSemaSource>(External))
			ExternalSema->InitializeSema(S);
		External->StartTranslationUnit(Consumer);
	}

	Parser::DeclGroupPtrTy ADecl;
	while(!P.ParseTopLevelDecl(ADecl))
		if(ADecl) Consumer->HandleTopLevelDecl(ADecl.getAsVal<DeclGroupRef>());

	Consumer->HandleTranslationUnit(Ctx);
	// S.dump();

}

ClangCompiler::ClangCompiler(const std::string& file_name) : pimpl(new ClangCompilerImpl) {
	pimpl->clang.setLLVMContext(new llvm::LLVMContext);

	// Create diagnostic client
	TextDiagnosticPrinter* diagClient = new TextDiagnosticPrinter(llvm::errs(), DiagnosticOptions());
	// diagClient->setPrefix( "insieme" );
	Diagnostic* diags = new Diagnostic(diagClient);
	pimpl->clang.setDiagnostics(diags);
	pimpl->clang.setDiagnosticClient(diagClient);

	pimpl->clang.createFileManager();
	pimpl->clang.createSourceManager();
	pimpl->clang.InitializeSourceManager(file_name);

	// A compiler invocation object has to be created in order for the diagnostic object to work
	CompilerInvocation* CI = new CompilerInvocation; // CompilerInvocation will be deleted by CompilerInstance
	CompilerInvocation::CreateFromArgs(*CI, 0, 0, pimpl->clang.getDiagnostics());
	pimpl->clang.setInvocation(CI);

	// Add the default clang system headers
	pimpl->clang.getHeaderSearchOpts().UseBuiltinIncludes;
	pimpl->clang.getHeaderSearchOpts().AddPath( "/home/motonacciu/software/llvm-2.7/lib/clang/1.1/include/",
			clang::frontend::System, true, false );
	// add user headers
	for (size_t i = 0; i < CommandLineOptions::IncludePaths.size(); ++i)
		pimpl->clang.getHeaderSearchOpts().AddPath( CommandLineOptions::IncludePaths[i],
				clang::frontend::Angled, true, false);

	TargetOptions TO;
	TO.Triple = llvm::sys::getHostTriple();
	pimpl->clang.setTarget( TargetInfo::CreateTargetInfo (pimpl->clang.getDiagnostics(), TO) );
//
//	string nameStr(File->getName());
//	string extension(nameStr.substr(nameStr.rfind('.'), string::npos));
//	bool enableCpp = extension == "cpp" || extension == "cxx" || extension == "hpp" || extension == "hxx";
//	if(CommandLineOptions::CPP) enableCpp = true;

	LangOptions& LO = pimpl->clang.getLangOpts();
	LO.GNUMode = 1;
	LO.Bool = 1;
	LO.POSIXThreads = 1;

//	if(CommandLineOptions::C99) LO.C99 = 1; 		// set c99
//
//	if(enableCpp) {
//		LO.CPlusPlus = 1; 	// set C++ 98 support
//		LO.CXXOperatorNames = 1;
//		if(Flags::CppX) LO.CPlusPlus0x = 1; // set C++0x support
//		LO.RTTI = 1;
//		LO.Exceptions = 1;
//	}

//	if(Flags::OpenCl) {
//		// OpenCL has some additional defaults.
//		LO.OpenCL = 1;
//		LO.AltiVec = 1;
//		LO.LaxVectorConversions = 1;
//	}
//
//	// set -D macros
//	for (size_t i = 0; i < Flags::D_macros.size(); ++i)
//		Clang.getPreprocessorOpts().addMacroDef(Flags::D_macros[i]);

	// Do this AFTER setting preprocessor options
	pimpl->clang.createPreprocessor();
	pimpl->clang.createASTContext();
	// Rewriter = new CodeRewriter(Clang.getSourceManager(), Clang.getLangOpts());

	pimpl->clang.getDiagnostics().getClient()->BeginSourceFile( LO, &pimpl->clang.getPreprocessor() );
}

void ClangCompiler::parse(clang::ASTConsumer& c) {
	InsiemeParseAST(pimpl->clang.getPreprocessor(), &c, pimpl->clang.getASTContext(), true);
	if( pimpl->clang.getDiagnostics().hasFatalErrorOccurred() ) {
		throw ClangParsingError();
	}
}

ClangCompiler::~ClangCompiler() {
	pimpl->clang.getDiagnostics().getClient()->EndSourceFile();
	delete pimpl;
}

InsiemeTransUnitPtr ParseSourceFile(const std::string& file_name) {
	ClangCompiler cl(file_name);
	ASTConsumer c;
	cl.parse(c);
}

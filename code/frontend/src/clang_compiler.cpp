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
#include "conversion.h"
#include "insieme_sema.h"
#include "pragma_handler.h"
// #include "programs.h"

#include "omp/omp_pragma.h"
#include "clang_config.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/DiagnosticOptions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"

#include "llvm/LLVMContext.h"
#include "llvm/System/Host.h"
#include "llvm/System/Path.h"

#include "llvm/Config/config.h"

#include "clang/Parse/Parser.h"
#include "clang/AST/DeclGroup.h"
#include "clang/Parse/ParseAST.h"

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Sema/ExternalSemaSource.h"

#include <clang/Index/TranslationUnit.h>
#include "clang/Index/DeclReferenceMap.h"
#include "clang/Index/SelectorMap.h"

#include "clang/Index/Indexer.h"
#include "clang/Index/Analyzer.h"
#include "clang/Index/CallGraph.h"

using namespace clang;
using namespace insieme::frontend;
using namespace insieme::core;

ParserProxy* ParserProxy::currParser = NULL;

Expr* ParserProxy::ParseExpression(Preprocessor& PP) {
	PP.Lex(mParser->Tok);

	Parser::ExprResult ownedResult = mParser->ParseExpression();
	Expr* result = ownedResult.takeAs<Expr> ();
	return result;
}

void ParserProxy::EnterTokenStream(Preprocessor& PP) {
	// DEBUG(ClangContext::get().getParser()->Tok.getName());
	PP.EnterTokenStream(&(CurrentToken()), 1, true, false);
}

Token& ParserProxy::ConsumeToken() {
	mParser->ConsumeAnyToken();
	// Token token = PP.LookAhead(0);
	// DEBUG(token.getName());
	// if(token.isLiteral())
	//	DEBUG( std::string(token.getLiteralData(),
	//		token.getLiteralData()+token.getLength()) );
	return CurrentToken();
}

clang::Scope* ParserProxy::CurrentScope() {
	return mParser->getCurScope();
}

Token& ParserProxy::CurrentToken() {
	return mParser->Tok;
}

namespace insieme {
namespace frontend {

struct ClangCompiler::ClangCompilerImpl {
	CompilerInstance clang;
	DiagnosticOptions diagOpts;
};

void InsiemeParseAST(Preprocessor &PP, ASTConsumer *Consumer, ASTContext &Ctx, bool CompleteTranslationUnit, PragmaList& PL) {
	InsiemeSema S(PL, PP, Ctx, *Consumer, CompleteTranslationUnit);
	Parser P(PP, S);
	PP.EnterMainSourceFile();

	P.Initialize();
	ParserProxy::init(&P);
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
	ParserProxy::discard();

	S.dump();
}

void setDiagnosticClient(clang::CompilerInstance& clang, clang::DiagnosticOptions& diagOpts) {
	TextDiagnosticPrinter* diagClient = new TextDiagnosticPrinter(llvm::errs(), diagOpts);
	Diagnostic* diags = new Diagnostic(diagClient);
	clang.setDiagnostics(diags);
}

ClangCompiler::ClangCompiler() : pimpl(new ClangCompilerImpl){
	pimpl->clang.setLLVMContext(new llvm::LLVMContext);

	setDiagnosticClient(pimpl->clang, pimpl->diagOpts);
	pimpl->clang.createFileManager();
	pimpl->clang.createSourceManager();

	// A compiler invocation object has to be created in order for the diagnostic object to work
	CompilerInvocation* CI = new CompilerInvocation; // CompilerInvocation will be deleted by CompilerInstance
	CompilerInvocation::CreateFromArgs(*CI, 0, 0, pimpl->clang.getDiagnostics());
	pimpl->clang.setInvocation(CI);

	TargetOptions TO;
	TO.Triple = llvm::sys::getHostTriple();
	pimpl->clang.setTarget( TargetInfo::CreateTargetInfo (pimpl->clang.getDiagnostics(), TO) );

	pimpl->clang.createPreprocessor();
	pimpl->clang.createASTContext();
}

ClangCompiler::ClangCompiler(const std::string& file_name) : pimpl(new ClangCompilerImpl) {
	pimpl->clang.setLLVMContext(new llvm::LLVMContext);

	// set diagnostic options for the error reporting
	pimpl->diagOpts.ShowLocation = 1;
	pimpl->diagOpts.ShowCarets = 1;
	pimpl->diagOpts.ShowColors = 1; // REMOVE FOR BETTER ERROR REPORT IN ECLIPSE
	pimpl->diagOpts.TabStop = 4;

	setDiagnosticClient(pimpl->clang, pimpl->diagOpts);

	pimpl->clang.createFileManager();
	pimpl->clang.createSourceManager();
	pimpl->clang.InitializeSourceManager(file_name);

	// A compiler invocation object has to be created in order for the diagnostic object to work
	CompilerInvocation* CI = new CompilerInvocation; // CompilerInvocation will be deleted by CompilerInstance
	CompilerInvocation::CreateFromArgs(*CI, 0, 0, pimpl->clang.getDiagnostics());
	pimpl->clang.setInvocation(CI);

	// Add default header
	pimpl->clang.getHeaderSearchOpts().AddPath( CLANG_SYSTEM_INCLUDE_FOLDER, clang::frontend::System, true, false, false);
	// add headers
	std::for_each(CommandLineOptions::IncludePaths.begin(), CommandLineOptions::IncludePaths.end(),
		[ pimpl ](std::string& curr) {
			pimpl->clang.getHeaderSearchOpts().AddPath( curr, clang::frontend::System, true, false, false);
		}
	);

	TargetOptions TO;
	TO.Triple = llvm::sys::getHostTriple();
	pimpl->clang.setTarget( TargetInfo::CreateTargetInfo (pimpl->clang.getDiagnostics(), TO) );

	std::string extension(file_name.substr(file_name.rfind('.'), std::string::npos));
	bool enableCpp = extension == "cpp" || extension == "cxx" || extension == "hpp" || extension == "hxx";

	LangOptions& LO = pimpl->clang.getLangOpts();
	LO.GNUMode = 1;
	LO.Bool = 1;
	LO.POSIXThreads = 1;

	if(CommandLineOptions::STD == "c99") LO.C99 = 1; 		// set c99

	if(enableCpp) {
		LO.CPlusPlus = 1; 	// set C++ 98 support
		LO.CXXOperatorNames = 1;
		if(CommandLineOptions::STD == "c++0x") LO.CPlusPlus0x = 1; // set C++0x support
		LO.RTTI = 1;
		LO.Exceptions = 1;
	}

	// Enable OpenCL
	LO.OpenCL = 1;
	LO.AltiVec = 1;
	LO.LaxVectorConversions = 1;

	// set -D macros
	std::for_each(CommandLineOptions::Defs.begin(), CommandLineOptions::Defs.end(), [ pimpl ](std::string& curr) {
		pimpl->clang.getPreprocessorOpts().addMacroDef(curr);
	});

	// Do this AFTER setting preprocessor options
	pimpl->clang.createPreprocessor();
	pimpl->clang.createASTContext();
	// Rewriter = new CodeRewriter(Clang.getSourceManager(), Clang.getLangOpts());

	pimpl->clang.getDiagnostics().getClient()->BeginSourceFile( LO, &pimpl->clang.getPreprocessor() );
}

ASTContext& 	ClangCompiler::getASTContext() const { return pimpl->clang.getASTContext(); }
Preprocessor& 	ClangCompiler::getPreprocessor() const { return pimpl->clang.getPreprocessor(); }
Diagnostic& 	ClangCompiler::getDiagnostics() const { return pimpl->clang.getDiagnostics(); }
SourceManager& 	ClangCompiler::getSourceManager() const { return pimpl->clang.getSourceManager(); }

ClangCompiler::~ClangCompiler() {
	pimpl->clang.getDiagnostics().getClient()->EndSourceFile();
	delete pimpl;
}

/**
 * A translation unit contains informations about the compiler (needed to keep alive object instantiated by clang),
 * and the insieme IR which has been generated from the source file.
 */
class TranslationUnitImpl: public frontend::TranslationUnit, public clang::idx::TranslationUnit {
	std::shared_ptr<clang::idx::DeclReferenceMap>   	mDeclRefMap;
	std::shared_ptr<clang::idx::SelectorMap>		   	mSelMap;

public:
	TranslationUnitImpl(const std::string& file_name, const insieme::core::ProgramPtr& prog, const core::SharedNodeManager& mgr):
			frontend::TranslationUnit(file_name) {
		// conversion::IRConsumer cons(mClang, prog, mgr, mPragmaList, doConversion);

		// register 'omp' pragmas
		omp::registerPragmaHandlers( mClang.getPreprocessor() );

		// register 'test' pragma
		mClang.getPreprocessor().AddPragmaHandler(
				PragmaHandlerFactory::CreatePragmaHandler<TestPragma>(mClang.getPreprocessor().getIdentifierInfo("test"), tok::string_literal["expected"] >> tok::eom));

		clang::ASTConsumer emptyCons;
		InsiemeParseAST(mClang.getPreprocessor(), &emptyCons, mClang.getASTContext(), true, mPragmaList);

		if( mClang.getDiagnostics().hasErrorOccurred() ) {
			// errors are always fatal!
			throw ClangParsingError(file_name);
		}

		// the translation unit has been correctly parsed
		mDeclRefMap = std::make_shared<clang::idx::DeclReferenceMap>( mClang.getASTContext() );
		mSelMap = std::make_shared<clang::idx::SelectorMap>( mClang.getASTContext() );
	}

	clang::Preprocessor& getPreprocessor() { return getCompiler().getPreprocessor(); }
	clang::ASTContext& getASTContext() { return getCompiler().getASTContext(); }
	clang::Diagnostic& getDiagnostic() { return getCompiler().getDiagnostics(); }

	clang::idx::DeclReferenceMap& getDeclReferenceMap() { assert(mDeclRefMap); return *mDeclRefMap; }
	clang::idx::SelectorMap& getSelectorMap() { assert(mSelMap); return *mSelMap; }
};

struct Program::ProgramImpl {
	TranslationUnitSet tranUnits;

	clang::idx::Program  mProg;
	clang::idx::Indexer  mIdx;
	clang::idx::Analyzer mAnalyzer;

	clang::CallGraph mCallGraph;

	ProgramImpl() : mIdx(mProg), mAnalyzer(mProg, mIdx), mCallGraph(mProg) { }
};

Program::Program(const core::SharedNodeManager& mgr): pimpl( new ProgramImpl() ), mMgr(mgr), mProgram( core::Program::create(*mgr) ) { }

void Program::addTranslationUnit(const std::string& file_name) {
	frontend::TranslationUnitImpl* tuImpl = new frontend::TranslationUnitImpl(file_name, mProgram, mMgr);
	pimpl->tranUnits.insert( TranslationUnitPtr(tuImpl) );
	pimpl->mIdx.IndexAST( dynamic_cast<clang::idx::TranslationUnit*>(tuImpl) );
	pimpl->mCallGraph.addTU( tuImpl->getASTContext() );
	// update the program
	// mProgram = tuImpl->getProgram();
}

const Program::TranslationUnitSet& Program::getTranslationUnits() const { return pimpl->tranUnits; }

void Program::dumpCallGraph() const { return pimpl->mCallGraph.dump(); }

const core::ProgramPtr& Program::convert() {

	for(Program::TranslationUnitSet::const_iterator it = pimpl->tranUnits.begin(), end = pimpl->tranUnits.end(); it != end; ++it) {
		const ClangCompiler& comp = (*it)->getCompiler();
		const PragmaList& pList = (*it)->getPragmaList();

		conversion::IRConverter conv(comp, mProgram, mMgr, pList);
		clang::DeclContext* declRef = clang::TranslationUnitDecl::castToDeclContext( comp.getASTContext().getTranslationUnitDecl() );

		conv.handleTopLevelDecl(declRef);

		mProgram = conv.getProgram();
	}

	return mProgram;
}
} // End fronend namespace
} // End insieme namespace

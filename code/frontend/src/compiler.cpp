/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/compiler.h"

#include <iostream>

#include <boost/filesystem.hpp>

#include "insieme/frontend/clang.h"
#include "insieme/frontend/sema.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"


using namespace clang;
using namespace insieme::frontend;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//     PARSER
//////////////////////////////////////////////////////////////////////////////////////////////////////

ParserProxy* ParserProxy::currParser = NULL;

clang::Expr* ParserProxy::ParseExpression(clang::Preprocessor& PP) {
	PP.Lex(mParser->Tok);
	
	ExprResult ownedResult = mParser->ParseExpression();
	Expr* result = ownedResult.getAs<Expr>();
	return result;
}

void ParserProxy::EnterTokenStream(clang::Preprocessor& PP) {
	PP.EnterTokenStream(&(CurrentToken()), 1, true, false);
}

Token& ParserProxy::ConsumeToken() {
	mParser->ConsumeAnyToken();
	// Token token = PP.LookAhead(0);
	return CurrentToken();
}

clang::Scope* ParserProxy::CurrentScope() {
	return mParser->getCurScope();
}

Token& ParserProxy::CurrentToken() {
	return mParser->Tok;
}

namespace {

void printHeader(clang::HeaderSearchOptions& ho) {
	std::cout << "++++++++++++++++++++++++++++++++++++++++++++++++++"<<std::endl;
	for(clang::HeaderSearchOptions::Entry path : ho.UserEntries) {
		std::cout << path.Path << std::endl;
	}
	std::cout << "sysroot: " << ho.Sysroot << std::endl;
	std::cout << "resourceDir: " << ho.ResourceDir << std::endl;
	
	for(clang::HeaderSearchOptions::SystemHeaderPrefix path : ho.SystemHeaderPrefixes) {
		std::cout << path.Prefix << std::endl;
	}
	if(ho.UseBuiltinIncludes) {
		std::cout << "built in" <<std::endl;
	}
	if(ho.UseStandardSystemIncludes) {
		std::cout << "UseStandardSystemIncludes" <<std::endl;
	}
	if(ho.UseStandardCXXIncludes) {
		std::cout << "UseStandardCXXIncludes" <<std::endl;
	}
	std::cout << "++++++++++++++++++++++++++++++++++++++++++++++++++"<<std::endl;
}


void setDiagnosticClient(clang::CompilerInstance& clang, bool printDiagToConsole) {

	// NOTE: the TextDiagnosticPrinter within the set DiagnosticClient takes over ownership of the printer object!
	clang::DiagnosticOptions* options = new clang::DiagnosticOptions();
	
	// set diagnostic options for the error reporting
	options->ShowLocation = 1;
	options->ShowCarets = 1;
	options->ShowColors = 1; // REMOVE FOR BETTER ERROR REPORT IN ECLIPSE
	options->TabStop = 4;
	
	DiagnosticConsumer* diagClient;
	if(printDiagToConsole) {
		diagClient = new TextDiagnosticPrinter(llvm::errs(), options);
	}
	else {
		diagClient = new IgnoringDiagConsumer();
	}
	// cppcheck-suppress exceptNew
	
	// check why, it might be a double insert in list, or a isolated delete somewhere
	DiagnosticsEngine* diags = new DiagnosticsEngine(llvm::IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()),
	        options, diagClient, true);
	diags->setSuppressSystemWarnings(true);
	clang.setDiagnostics(diags);
}

} // end anonymous namespace

namespace insieme {
namespace frontend {

//////////////////////////////////////////////////////////////////////////////////////////////////////
//     COMPILER
//////////////////////////////////////////////////////////////////////////////////////////////////////

struct ClangCompiler::ClangCompilerImpl {
	CompilerInstance clang;
	std::shared_ptr<TargetOptions> TO;
	bool m_isCXX;
	ClangCompilerImpl() : clang(), TO(new TargetOptions()), m_isCXX(false) {}
};

ClangCompiler::ClangCompiler(const ConversionSetup& config, const path& file) : pimpl(new ClangCompilerImpl), config(config) {
	//assert_false(is_obj);
	// NOTE: the TextDiagnosticPrinter within the set DiagnosticClient takes over ownership of the diagOpts object!
	setDiagnosticClient(pimpl->clang, config.hasOption(ConversionJob::PrintDiag));
	
	pimpl->clang.createFileManager();
	pimpl->clang.createSourceManager(pimpl->clang.getFileManager());
	
	
	// A compiler invocation object has to be created in order for the diagnostic object to work
	CompilerInvocation* CI = new CompilerInvocation; // CompilerInvocation will be deleted by CompilerInstance
	CompilerInvocation::CreateFromArgs(*CI, 0, 0, pimpl->clang.getDiagnostics());
	
	pimpl->clang.setInvocation(CI);
	
	
	//******************** TAKE CARE OF ORDER OF INCLUDE PATHS *************//
	//first user-provided, than our openmp replacement, then default-path
	
	//setup headers
	pimpl->clang.getHeaderSearchOpts().UseBuiltinIncludes = 0;
	pimpl->clang.getHeaderSearchOpts().UseStandardSystemIncludes = 1;  // Includes system includes, usually  /usr/include
	pimpl->clang.getHeaderSearchOpts().UseStandardCXXIncludes = 0;
	
	// ******************** FRONTEND PLUGIN ********************
	// this must be the first call of addpath otherwise
	// the user kidnapped header files won't be recognized
	for(auto extension : config.getExtensions()) {
		for(auto kidnappedHeader : extension->getKidnappedHeaderList()) {
			pimpl->clang.getHeaderSearchOpts().AddPath(kidnappedHeader.string(), clang::frontend::System, false, false);
		}
	}
	
	if(config.hasOption(ConversionJob::WinCrossCompile)) {
		pimpl->TO->Triple = llvm::Triple("x86_64", "pc", "win32").getTriple();
	}
	else {
		pimpl->TO->Triple = llvm::Triple("x86_64", "pc", "linux").getTriple();
	}
	
	pimpl->clang.setTarget(TargetInfo::CreateTargetInfo(pimpl->clang.getDiagnostics(), pimpl->TO));
	LangOptions& LO = pimpl->clang.getLangOpts();
	
	// add user provided headers
	for(const path& cur : config.getIncludeDirectories()) {
		this->pimpl->clang.getHeaderSearchOpts().AddPath(cur.string(), clang::frontend::System, false, false);
	}
	
	// ******************** FRONTEND PLUGIN ********************
	// ADD INJECTED HEADERS
	for(auto extension : config.getExtensions()) {
		for(auto header : extension->getInjectedHeaderList()) {
			string hp = header;
			// if the header path is relative, build an absolute one dependent on the source file location
			if(boost::filesystem::path(hp).is_relative()) {
				hp = boost::filesystem::absolute(header, file.parent_path()).string();
			}
			this->pimpl->clang.getPreprocessorOpts().Includes.push_back(hp);
		}
	}
	
	// set -D macros
	for(const std::pair<string,string>& cur : config.getDefinitions()) {
		string def = cur.first;
		if(!cur.second.empty()) {
			def = def + "=" + cur.second;
		}
		this->pimpl->clang.getPreprocessorOpts().addMacroDef(def);
	}
	
	// ******************** FRONTEND PLUGIN ********************
	// ADD FRONTEND PLUGIN PROVIDED MACRO DEFINITIONS
	for(auto extension : config.getExtensions()) {
		for(auto it = extension->getMacroList().cbegin(); it != extension->getMacroList().cend(); ++it) {
			string def = (*it).first;
			if(!(*it).second.empty()) {
				def = def + "=" + (*it).second;
			}
			this->pimpl->clang.getPreprocessorOpts().addMacroDef(def);
		}
	}
	
	/*** VECTOR EXTENSION STUFF ***/
	// Enable OpenCL
	// LO.OpenCL = 1;
	LO.AltiVec = 1;
	LO.LaxVectorConversions = 1;
	
	// to eneable clang to parse gcc-builtins we need the hacked builtinheaders
	//	+	for some not builtins which are NOT supported by CLANG we give the signature (taken from GCC) as extern
	//		functiondefinition
	//	+	for some builtins with differeing signature (currently storelps/storehps/movntq) we hack the
	//		intrinsic to use depending on the used compiler the correct casts
	this->pimpl->clang.getHeaderSearchOpts().AddPath(FRONTEND_TEST_DIR "../include/insieme/frontend/builtin_headers/",	clang::frontend::System, false, false);
	/*** VECTOR EXTENSION STUFF END ***/
	
	pimpl->m_isCXX = false;
	if(config.getStandard() == ConversionSetup::C99) {
		//set default values for C --
		//langStandard is defined in include/clang/Frontend/LangStandards.de
		CompilerInvocation::setLangDefaults(LO, clang::IK_C, clang::LangStandard::lang_gnu99);
	}
	
	if(config.getStandard() == ConversionSetup::Auto && config.isCxx(file)) {
		pimpl->m_isCXX = true;
	}
	if(config.getStandard() == ConversionSetup::Cxx03 ||
	        config.getStandard() == ConversionSetup::Cxx98 ||
	        config.getStandard() == ConversionSetup::Cxx11) {
		pimpl->m_isCXX = true;
	}
	
	if(pimpl->m_isCXX) {
		// set cxx standard to c++98
		if(config.getStandard() == ConversionSetup::Cxx11) {
			CompilerInvocation::setLangDefaults(LO, clang::IK_CXX, clang::LangStandard::lang_cxx11);
		}
		else if(config.getStandard() == ConversionSetup::Cxx98) {
			CompilerInvocation::setLangDefaults(LO, clang::IK_CXX, clang::LangStandard::lang_cxx98);
		}
		else {
			CompilerInvocation::setLangDefaults(LO, clang::IK_CXX, clang::LangStandard::lang_cxx03);
		}
		
		// use the cxx header of the backend c++ compiler
		pimpl->clang.getHeaderSearchOpts().UseBuiltinIncludes = 1;
		pimpl->clang.getHeaderSearchOpts().UseStandardCXXIncludes = 0;
		pimpl->clang.getHeaderSearchOpts().UseStandardSystemIncludes = 0;
		
		this->pimpl->clang.getPreprocessorOpts().UsePredefines = true;
		
		LO.CPlusPlus = 1;
		LO.Exceptions = 1;
		LO.CXXExceptions = 1;
		LO.Bool = 1;
	}
	else {
		LO.CPlusPlus = 0;
	}
	
	for(std::string curr : insieme::utils::compiler::getDefaultCIncludePaths()) {
		pimpl->clang.getHeaderSearchOpts().AddPath(curr, clang::frontend::System,  false, false);
	}
	for(const path& cur : config.getSystemHeadersDirectories()) {
		pimpl->clang.getHeaderSearchOpts().AddPath(cur.string(), clang::frontend::System,  false, false);
	}
	
	// Do this AFTER setting preprocessor options
	pimpl->clang.createPreprocessor(clang::TranslationUnitKind::TU_Complete);
	pimpl->clang.createASTContext();
	
	//FIXME why is this needed?
	getPreprocessor().getBuiltinInfo().InitializeBuiltins(
	    getPreprocessor().getIdentifierTable(),
	    getPreprocessor().getLangOpts()
	);
	
	//pimpl->clang.getDiagnostics().getClient()->BeginSourceFile( LO, &pimpl->clang.getPreprocessor() );
	const FileEntry* fileID = pimpl->clang.getFileManager().getFile(file.string());
	if(!fileID) {
		std::cerr << " file: " << file.string() << " does not exist" << std::endl;
		throw  ClangParsingError(file);
	}
	
	pimpl->clang.getSourceManager().setMainFileID(pimpl->clang.getSourceManager().createFileID(fileID, SourceLocation(), SrcMgr::C_User));
	
	pimpl->clang.getDiagnosticClient().BeginSourceFile(
	    pimpl->clang.getLangOpts(),
	    &pimpl->clang.getPreprocessor());
	    
	if(VLOG_IS_ON(2)) {
		printHeader(getPreprocessor().getHeaderSearchInfo().getHeaderSearchOpts());
	}
}

ASTContext& 		ClangCompiler::getASTContext()    const {
	return pimpl->clang.getASTContext();
}
Preprocessor& 		ClangCompiler::getPreprocessor()  const {
	return pimpl->clang.getPreprocessor();
}
DiagnosticsEngine& 	ClangCompiler::getDiagnostics()   const {
	return pimpl->clang.getDiagnostics();
}
SourceManager& 		ClangCompiler::getSourceManager() const {
	return pimpl->clang.getSourceManager();
}
TargetInfo& 		ClangCompiler::getTargetInfo()    const {
	return pimpl->clang.getTarget();
}
bool				ClangCompiler::isCXX()			  const {
	return pimpl->m_isCXX;
}

ClangCompiler::~ClangCompiler() {
	pimpl->clang.getDiagnostics().getClient()->EndSourceFile();
	delete pimpl;
	//sema object of pimpl will be deleted by the InsiemeSema pimpl
}

} // End fronend namespace
} // End insieme namespace

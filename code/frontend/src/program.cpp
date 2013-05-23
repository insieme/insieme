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

#include "insieme/frontend/program.h"

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <llvm/Support/FileSystem.h>
#include <clang/Serialization/ASTWriter.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/indexer.h"
#include "insieme/frontend/utils/functionDependencyGraph.h"
#include "insieme/frontend/utils/interceptor.h"

#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/cilk/cilk_pragma.h"
#include "insieme/frontend/mpi/mpi_pragma.h"
#include "insieme/frontend/mpi/mpi_sema.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/DeclGroup.h"

#include "clang/Analysis/CFG.h"

#include "clang/Parse/Parser.h"
#include "clang/Parse/ParseAST.h"

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Sema/ExternalSemaSource.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/container_utils.h"


using namespace insieme;
using namespace insieme::core;
using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace clang;

namespace {

// Instantiate the clang parser and sema to build the clang AST. Pragmas are stored during the parsing
///
void parseClangAST(ClangCompiler &comp, clang::ASTConsumer *Consumer, bool CompleteTranslationUnit, PragmaList& PL, InsiemeSema& sema, bool dumpCFG) {

	Parser P(comp.getPreprocessor(), sema, false);  // do not skip function bodies
	comp.getPreprocessor().EnterMainSourceFile();

	ParserProxy::init(&P);
	P.Initialize();	  //FIXME
	Consumer->Initialize(comp.getASTContext());
	if (SemaConsumer *SC = dyn_cast<SemaConsumer>(Consumer))
		SC->InitializeSema(sema);

	if (ExternalASTSource *External = comp.getASTContext().getExternalSource()) {
		if(ExternalSemaSource *ExternalSema = dyn_cast<ExternalSemaSource>(External))
			ExternalSema->InitializeSema(sema);
		External->StartTranslationUnit(Consumer);
	}

	Parser::DeclGroupPtrTy ADecl;
	while(!P.ParseTopLevelDecl(ADecl))
		if(ADecl) Consumer->HandleTopLevelDecl(ADecl.getAsVal<DeclGroupRef>());

	Consumer->HandleTranslationUnit(comp.getASTContext());
	ParserProxy::discard();  // FIXME

	// PRINT THE CFG from CLANG just for debugging purposes for the C++ frontend
	if(dumpCFG) {
		clang::DeclContext* dc = comp.getASTContext().getTranslationUnitDecl();
		std::for_each(dc->decls_begin(), dc->decls_end(), [&] (const clang::Decl* d) {
			if (const clang::FunctionDecl* func_decl = llvm::dyn_cast<const clang::FunctionDecl> (d)) {
				if( func_decl->hasBody() ) {
					clang::CFG::BuildOptions bo;
					bo.AddInitializers = true;
					bo.AddImplicitDtors = true;
					clang::CFG* cfg = clang::CFG::buildCFG(func_decl, func_decl->getBody(), &comp.getASTContext(), bo);
					assert(cfg);
					std::cerr << "~~~ Function: "  << func_decl->getNameAsString() << " ~~~~~" << std::endl;
					cfg->dump(comp.getPreprocessor().getLangOpts(), true);
				}
			}
		});
	}
}

///  A translation unit contains informations about the compiler (needed to keep alive object instantiated by clang),
///  and the insieme IR which has been generated from the source file.
class TranslationUnitImpl: public insieme::frontend::TranslationUnit{
    insieme::frontend::InsiemeSema mSema;
public:
	TranslationUnitImpl(const ConversionJob& job):
		insieme::frontend::TranslationUnit(job),
		mSema(mPragmaList, mClang.getPreprocessor(), mClang.getASTContext(), true) {

    	// register 'omp' pragmas
		omp::registerPragmaHandlers( mClang.getPreprocessor() );

		//register 'cilk' pragmas
		cilk::registerPragmaHandlers( mClang.getPreprocessor() );

		// register 'test' pragma
		TestPragma::registerPragmaHandler( mClang.getPreprocessor() );

		// register 'insieme' pragma
		InsiemePragma::registerPragmaHandler( mClang.getPreprocessor() );

		// register 'mpi' pragma
		mpi::registerPragmaHandler( mClang.getPreprocessor() );

		//  FIXME: preprocess here or in indexer?
		//clang::ASTConsumer emptyCons;
		clang::ASTConsumer emptyCons;
		//insieme::frontend::utils::indexerASTConsumer consumer(indexer,
	//									dynamic_cast<insieme::frontend::TranslationUnit*>(this));
        if(!boost::algorithm::ends_with(job.getFile(),".o")) {
            parseClangAST(mClang, &emptyCons, true, mPragmaList, mSema, job.hasOption(ConversionJob::DumpCFG));

            if( mClang.getDiagnostics().hasErrorOccurred() ) {
                // errors are always fatal!
                throw ClangParsingError(mFileName);
            }
        }

	}

	// getters
	clang::Preprocessor& getPreprocessor() { return getCompiler().getPreprocessor(); }
	const clang::Preprocessor& getPreprocessor() const { return getCompiler().getPreprocessor(); }

	clang::ASTContext& getASTContext() { return getCompiler().getASTContext(); }
	const clang::ASTContext& getASTContext() const { return getCompiler().getASTContext(); }

	clang::DiagnosticsEngine& getDiagnostic() { return getCompiler().getDiagnostics(); }
	const clang::DiagnosticsEngine& getDiagnostic() const { return getCompiler().getDiagnostics(); }

	void storeUnit(const std::string& output_file) {
        //if no output_file is specified the current file_name is taken and modified to .o extension
        std::string raw_name = output_file;
        if(output_file.size()==0) {
            size_t lastslash = getFileName().find_last_of("/")+1;
            size_t lastdot = getFileName().find_last_of(".");
            if (lastdot == std::string::npos)
                raw_name = getFileName();
            raw_name = getFileName().substr(lastslash, lastdot-lastslash);
            raw_name += ".o";
        }
        llvm::SmallString<128> TempPath;
        TempPath = raw_name;
        TempPath += "-%%%%%%%%";
        int fd;
        llvm::sys::fs::unique_file(TempPath.str(), fd, TempPath,
                                            false);
        llvm::raw_fd_ostream Out(fd, true);
        llvm::SmallString<128> Buffer;
        llvm::BitstreamWriter Stream(Buffer);
        clang::ASTWriter Writer(Stream);
        Writer.WriteAST(mSema, std::string(), 0, "", false);
        if (!Buffer.empty())
            Out.write(Buffer.data(), Buffer.size());
        Out.close();
        llvm::sys::fs::rename(TempPath.str(), raw_name);
    }

};
} // end anonymous namespace

namespace insieme {
namespace frontend {

struct Program::ProgramImpl {
	utils::Indexer mIdx;
	TranslationUnitSet tranUnits;
	const vector<boost::filesystem::path> stdLibDirs;
	utils::Interceptor interceptor;
	utils::FunctionDependencyGraph funcDepGraph;

	ProgramImpl(core::NodeManager& mgr, const vector<string>& stdLibDirs) :
		mIdx(),
		stdLibDirs(::transform(stdLibDirs, [](const string& path) { return boost::filesystem::canonical(path); } )),
		interceptor(mgr, mIdx, this->stdLibDirs),  funcDepGraph(mIdx,interceptor) {}
};

Program::Program(core::NodeManager& mgr, const ConversionJob& job):
	pimpl( new ProgramImpl(mgr, job.getStdLibIncludeDirectories()) ), mMgr(mgr), mProgram( core::Program::get(mgr) ), config(job) { }

Program::~Program() { delete pimpl; }

utils::Interceptor& Program::getInterceptor() const { return pimpl->interceptor; }
utils::Indexer& Program::getIndexer() const { return pimpl->mIdx; }
utils::FunctionDependencyGraph& Program::getCallGraph() const {return pimpl->funcDepGraph; }
const vector<boost::filesystem::path>& Program::getStdLibDirs() const { return pimpl->stdLibDirs; }

void Program::setupInterceptor() {
	if(!config.getIntercepterConfigFile().empty()) {
		//if we have a interceptor config file we use this to setup the interceptor
		pimpl->interceptor.loadConfigFile(config.getIntercepterConfigFile());
	}

	//by default we intercept "std::.*" and "__gnu_cxx::.*" -- set in the ctor
}

void Program::analyzeFuncDependencies() {
	VLOG(1) << " ************* Analyze function dependencies (recursion)***************";
	auto elem = getIndexer().begin();
	auto end = getIndexer().end();
	for (; elem != end; ++elem){
		if (llvm::isa<clang::FunctionDecl>(*elem)){
			const clang::FunctionDecl* funcDecl = llvm::cast<clang::FunctionDecl>(*elem);
			if( !(getInterceptor().isIntercepted(funcDecl)) ) {
				//if the funcDecl was intercepted we ignore it for the funcDepAnalysis
				pimpl->funcDepGraph.addNode(funcDecl);
			}
		}
	}
	VLOG(1) << " ************* Analyze function dependencies DONE***************";
	if (VLOG_IS_ON(2)){
		dumpCallGraph();
	}
}

void Program::dumpCallGraph() const {
	pimpl->funcDepGraph.print(std::cout);
}

void Program::storeTranslationUnits(const string& output_file) {
    for(TranslationUnitPtr tu : pimpl->tranUnits) {
        static_cast<TranslationUnitImpl *>(tu.get())->storeUnit(output_file);
    }
}

TranslationUnit& Program::addTranslationUnit(const ConversionJob& job) {
	assert(job.getFiles().size() == 1 && "Can only cover a single file!");

	TranslationUnitImpl* tuImpl = new TranslationUnitImpl(job);

	pimpl->mIdx.indexTU(tuImpl);

	pimpl->tranUnits.insert( TranslationUnitPtr(tuImpl) /* the shared_ptr will take care of cleaning the memory */);
	return *tuImpl;
}

const Program::TranslationUnitSet& Program::getTranslationUnits() const { return pimpl->tranUnits; }

Program::PragmaIterator Program::pragmas_begin() const {
	auto filtering = [](const Pragma&) -> bool { return true; };
	return Program::PragmaIterator(pimpl->tranUnits, filtering);
}

Program::PragmaIterator Program::pragmas_begin(const Program::PragmaIterator::FilteringFunc& func) const {
	return Program::PragmaIterator(pimpl->tranUnits, func);
}

Program::PragmaIterator Program::pragmas_end() const {
	return Program::PragmaIterator(pimpl->tranUnits.end());
}

bool Program::PragmaIterator::operator!=(const PragmaIterator& iter) const {
	return (tuIt != iter.tuIt); // FIXME also compare the pragmaIt value
}

void Program::PragmaIterator::inc(bool init) {
	while(tuIt != tuEnd) {
		if(init)	pragmaIt = (*tuIt)->getPragmaList().begin();
		// advance to the next pragma if there are still pragmas in the
		// current translation unit
		if(!init && pragmaIt != (*tuIt)->getPragmaList().end()) { ++pragmaIt; }

		if(pragmaIt != (*tuIt)->getPragmaList().end() && filteringFunc(**pragmaIt)) {
			return;
		}
		// advance to the next translation unit
		++tuIt;
		if(tuIt != tuEnd)
			pragmaIt = (*tuIt)->getPragmaList().begin();
	}
}

std::pair<PragmaPtr, TranslationUnitPtr> Program::PragmaIterator::operator*() const {
	assert(tuIt != tuEnd && pragmaIt != (*tuIt)->getPragmaList().end());
	return std::pair<PragmaPtr, TranslationUnitPtr>(*pragmaIt, *tuIt);
}

namespace {

/**
 * Loops through an IR AST which contains OpenCL, OpenMP and MPI annotations.
 * Those annotations will be translated to parallel constructs
 */
core::ProgramPtr addParallelism(core::ProgramPtr& prog, core::NodeManager& mgr, bool tagMPI) {

	// OpenCL frontend
	ocl::Compiler oclCompiler(prog, mgr);
	prog = oclCompiler.lookForOclAnnotations();

	// MPI frontend
	prog = mpi::handleMPICalls(prog, tagMPI);

	//ocl::Compiler oclCompiler(prog, mgr);
	//prog= oclCompiler.lookForOclAnnotations();
	//ocl::HostCompiler hc(prog, mgr);
	//hc.compile();

	return prog;
}

} // end anonymous namespace

const core::ProgramPtr& Program::convert() {
	// We check for insieme pragmas in each translation unit
	bool insiemePragmaFound = false;
	bool isCXX = any(pimpl->tranUnits, [](const TranslationUnitPtr& curr) { return curr->getCompiler().isCXX(); } );

	setupInterceptor();

	analyzeFuncDependencies();

	std::shared_ptr<conversion::ASTConverter> astConvPtr;
	if(isCXX) {
		astConvPtr = std::make_shared<conversion::CXXASTConverter>(mMgr, *this);
	} else {
		astConvPtr = std::make_shared<conversion::ASTConverter>(mMgr, *this);
	}

	// filters all the pragma across all the compilation units which are of type insieme::mark
	auto pragmaMarkFilter = [](const pragma::Pragma& curr) -> bool { return curr.getType() == "insieme::mark"; };

	for(Program::PragmaIterator pit = pragmas_begin(pragmaMarkFilter), pend = pragmas_end(); pit != pend; ++pit) {
		insiemePragmaFound = true;

		const pragma::Pragma& insiemePragma = *(*pit).first;

		if(insiemePragma.isDecl()) {
			// this is a declaration, if it's a function add it to the entry points of the program
			const clang::FunctionDecl* funcDecl = dyn_cast<const clang::FunctionDecl>(insiemePragma.getDecl());
			assert(funcDecl && "Pragma insieme only valid for function declarations.");

			mProgram = astConvPtr->handleFunctionDecl(funcDecl);
		} else {
			// insieme pragma associated to a statement, in this case we convert the body
			// and create an anonymous lambda expression to enclose it
			const clang::Stmt* body = insiemePragma.getStatement();
			assert(body && "Pragma matching failed!");
			core::CallExprPtr callExpr = astConvPtr->handleBody(body, *(*pit).second);
			mProgram = core::Program::addEntryPoint(mMgr, mProgram, callExpr);
		}
	}

	if(!insiemePragmaFound) {
		mProgram = astConvPtr->handleMainFunctionDecl();
	}

	LOG(INFO) << "=== Adding Parallelism to sequential IR ===";
	insieme::utils::Timer convertTimer("Frontend.AddParallelism ");
	mProgram = addParallelism(mProgram, mMgr, config.hasOption(ConversionJob::TAG_MPI));
	convertTimer.stop();
	LOG(INFO) << convertTimer;

	return mProgram;
}

} // end frontend namespace
} // end insieme namespace


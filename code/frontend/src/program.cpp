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
void parseClangAST(ClangCompiler &comp, clang::ASTConsumer *Consumer, bool CompleteTranslationUnit, PragmaList& PL) {

	InsiemeSema S(PL, comp.getPreprocessor(), comp.getASTContext(), *Consumer, CompleteTranslationUnit);

	//Parser P(comp.getPreprocessor(), S); // clang [3.0]
	Parser P(comp.getPreprocessor(), S, false);  // do not skip function bodies
	comp.getPreprocessor().EnterMainSourceFile();  

	P.Initialize();	  //FIXME
	ParserProxy::init(&P);
	Consumer->Initialize(comp.getASTContext());
	if (SemaConsumer *SC = dyn_cast<SemaConsumer>(Consumer))
		SC->InitializeSema(S);

	if (ExternalASTSource *External = comp.getASTContext().getExternalSource()) {
		if(ExternalSemaSource *ExternalSema = dyn_cast<ExternalSemaSource>(External))
			ExternalSema->InitializeSema(S);
		External->StartTranslationUnit(Consumer);
	}

	Parser::DeclGroupPtrTy ADecl;
	while(!P.ParseTopLevelDecl(ADecl))
		if(ADecl) Consumer->HandleTopLevelDecl(ADecl.getAsVal<DeclGroupRef>());

	Consumer->HandleTranslationUnit(comp.getASTContext());
	ParserProxy::discard();  // FIXME

	S.dump();

	// PRINT THE CFG from CLANG just for debugging purposes for the C++ frontend
	if(CommandLineOptions::ClangCFGDump) {
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
					// clang [3.0 ]cfg->dump(comp.getPreprocessor().getLangOptions());
					cfg->dump(comp.getPreprocessor().getLangOpts(), true);
				}
			}
		});
	}
	//////////////////////////////////////////////////////////////////////////////
}

///  A translation unit contains informations about the compiler (needed to keep alive object instantiated by clang),
///  and the insieme IR which has been generated from the source file.
// clang [3.0]class TranslationUnitImpl: public insieme::frontend::TranslationUnit, public clang::idx::TranslationUnit {
class TranslationUnitImpl: public insieme::frontend::TranslationUnit{

	// clang [3.0]
	//std::shared_ptr<clang::idx::DeclReferenceMap>   	mDeclRefMap;
	//std::shared_ptr<clang::idx::SelectorMap>		   	mSelMap;

public:
	TranslationUnitImpl(const std::string& file_name):
		insieme::frontend::TranslationUnit(file_name) {
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
		parseClangAST(mClang, &emptyCons, true, mPragmaList);

		if( mClang.getDiagnostics().hasErrorOccurred() ) {
			// errors are always fatal!
			throw ClangParsingError(file_name);
		}
	}

	// getters
	clang::Preprocessor& getPreprocessor() { return getCompiler().getPreprocessor(); }
	const clang::Preprocessor& getPreprocessor() const { return getCompiler().getPreprocessor(); }

	clang::ASTContext& getASTContext() { return getCompiler().getASTContext(); }
	const clang::ASTContext& getASTContext() const { return getCompiler().getASTContext(); }

	clang::DiagnosticsEngine& getDiagnostic() { return getCompiler().getDiagnostics(); }
	const clang::DiagnosticsEngine& getDiagnostic() const { return getCompiler().getDiagnostics(); }
};
} // end anonymous namespace

namespace insieme {
namespace frontend {

struct Program::ProgramImpl {
	utils::Indexer mIdx;
	TranslationUnitSet tranUnits;
	utils::Interceptor interceptor;
	utils::FunctionDependencyGraph funcDepGraph;
		
	ProgramImpl(core::NodeManager& mgr) : mIdx(), interceptor(mgr, mIdx),  funcDepGraph(mIdx,interceptor) {}
};

Program::Program(core::NodeManager& mgr):
	pimpl( new ProgramImpl(mgr) ), mMgr(mgr), mProgram( core::Program::get(mgr) ) { }

Program::~Program() { delete pimpl; }

utils::Interceptor& Program::getInterceptor() const { return pimpl->interceptor; }
utils::Indexer& Program::getIndexer() const { return pimpl->mIdx; }
utils::FunctionDependencyGraph& Program::getCallGraph() const {return pimpl->funcDepGraph; }

void Program::intercept(std::string fileName) {
	pimpl->interceptor.loadConfigFile(fileName);
	pimpl->interceptor.intercept();
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

TranslationUnit& Program::addTranslationUnit(const std::string& file_name) {
	TranslationUnitImpl* tuImpl = new TranslationUnitImpl(file_name);

	pimpl->mIdx.indexTU(tuImpl);

	pimpl->tranUnits.insert( TranslationUnitPtr(tuImpl) /* the shared_ptr will take care of cleaning the memory */);
	return *tuImpl;
}

TranslationUnit& Program::createEmptyTranslationUnit() {
	TranslationUnit* tuImpl = new TranslationUnit;
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
core::ProgramPtr addParallelism(core::ProgramPtr& prog, core::NodeManager& mgr) {

	// OpenCL frontend 
	ocl::Compiler oclCompiler(prog, mgr);
	prog = oclCompiler.lookForOclAnnotations();

	// MPI frontend
	prog = mpi::handleMPICalls(prog);

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

	if(!CommandLineOptions::Intercept.empty()) {
		intercept(CommandLineOptions::Intercept);
	}

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
	mProgram = addParallelism(mProgram, mMgr);
	convertTimer.stop();
	LOG(INFO) << convertTimer;

	return mProgram;
}

} // end frontend namespace
} // end insieme namespace


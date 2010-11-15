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

#include "insieme/frontend/pragma_handler.h"
#include "insieme/frontend/conversion.h"
#include "insieme/frontend/insieme_pragma.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/DeclGroup.h"

#include "clang/Index/Indexer.h"
#include "clang/Index/Analyzer.h"
#include "clang/Index/CallGraph.h"
#include "clang/Index/TranslationUnit.h"
#include "clang/Index/DeclReferenceMap.h"
#include "clang/Index/SelectorMap.h"

#include "clang/Parse/Parser.h"
#include "clang/Parse/ParseAST.h"

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "clang/Sema/ExternalSemaSource.h"

#include "insieme/utils/set_utils.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::frontend;
using namespace clang;

namespace {

void parseClangAST(clang::Preprocessor &PP, clang::ASTConsumer *Consumer, clang::ASTContext &Ctx, bool CompleteTranslationUnit, PragmaList& PL) {
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

/**
 * A translation unit contains informations about the compiler (needed to keep alive object instantiated by clang),
 * and the insieme IR which has been generated from the source file.
 */
class TranslationUnitImpl: public insieme::frontend::TranslationUnit, public clang::idx::TranslationUnit {
	std::shared_ptr<clang::idx::DeclReferenceMap>   	mDeclRefMap;
	std::shared_ptr<clang::idx::SelectorMap>		   	mSelMap;

public:
	TranslationUnitImpl(const std::string& file_name):
		insieme::frontend::TranslationUnit(file_name) {
		// register 'omp' pragmas
		omp::registerPragmaHandlers( mClang.getPreprocessor() );

		// register 'test' pragma
		TestPragma::registerPragmaHandler(mClang.getPreprocessor());

		// register 'insieme' pragma
		InsiemePragma::registerPragmaHandler(mClang.getPreprocessor());

		clang::ASTConsumer emptyCons;
		parseClangAST(mClang.getPreprocessor(), &emptyCons, mClang.getASTContext(), true, mPragmaList);

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
} // end anonymous namespace

namespace insieme {
namespace frontend {

struct Program::ProgramImpl {
	TranslationUnitSet tranUnits;

	clang::idx::Program  mProg;
	clang::idx::Indexer  mIdx;
	clang::idx::Analyzer mAnalyzer;

	clang::CallGraph mCallGraph;

	ProgramImpl() : mIdx(mProg), mAnalyzer(mProg, mIdx), mCallGraph(mProg) { }
};

Program::Program(core::NodeManager& mgr): pimpl( new ProgramImpl() ), mMgr(mgr), mProgram( core::Program::create(mgr) ) { }

TranslationUnit& Program::addTranslationUnit(const std::string& file_name) {
	TranslationUnitImpl* tuImpl = new TranslationUnitImpl(file_name);
	pimpl->tranUnits.insert( TranslationUnitPtr(tuImpl) /* the shared_ptr will take care of cleaning the memory */);
	pimpl->mIdx.IndexAST( dynamic_cast<clang::idx::TranslationUnit*>(tuImpl) );
	pimpl->mCallGraph.addTU( tuImpl->getASTContext() );
	return *tuImpl;
}

TranslationUnit& Program::createEmptyTranslationUnit() {
	TranslationUnit* tuImpl = new TranslationUnit;
	pimpl->tranUnits.insert( TranslationUnitPtr(tuImpl) /* the shared_ptr will take care of cleaning the memory */);
	return *tuImpl;
}

const Program::TranslationUnitSet& Program::getTranslationUnits() const { return pimpl->tranUnits; }

clang::idx::Program& Program::getClangProgram() { return pimpl->mProg; }
clang::idx::Indexer& Program::getClangIndexer() { return pimpl->mIdx; }

void Program::dumpCallGraph() const { return pimpl->mCallGraph.dump(); }

const TranslationUnit& Program::getTranslationUnit(const clang::idx::TranslationUnit* tu) {
	return *dynamic_cast<const TranslationUnit*>(reinterpret_cast<const TranslationUnitImpl*>(tu));
}

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
 * Loops through an IR AST which contains OpenCL, OpenMP and MPI annotations. Those annotations will be translated to parallel constructs
 */
core::ProgramPtr addParallelism(const core::ProgramPtr& prog, core::NodeManager& mgr) {
    ocl::Compiler oclCompiler(prog, mgr);
    return oclCompiler.lookForOclAnnotations();
}

} // end anonymous namespace

const core::ProgramPtr& Program::convert() {
	bool insiemePragmaFound = false;
	// We check for insieme pragmas in each translation unit
	conversion::ASTConverter conv(mMgr, *this);

	// filters all the pragma across all the compilation units which are of type insieme::mark
	auto pragmaMarkFilter = [](const Pragma& curr) -> bool { return curr.getType() == "insieme::mark"; };

	for(Program::PragmaIterator pit = pragmas_begin(pragmaMarkFilter), pend = pragmas_end(); pit != pend; ++pit) {
		insiemePragmaFound = true;
		const Pragma& insiemePragma = *(*pit).first;

		if(insiemePragma.isDecl()) {
			// this is a declaration, if it's a function add it to the entry points of the program
			const clang::FunctionDecl* funcDecl = dyn_cast<const clang::FunctionDecl>(insiemePragma.getDecl());
			assert(funcDecl && "Pragma insieme only valid for function declarations.");

			mProgram = conv.handleFunctionDecl(funcDecl);
		} else {
			// insieme pragma associated to a statement, in this case we convert the body
			// and create an anonymous lambda expression to enclose it
			const clang::Stmt* body = insiemePragma.getStatement();
			assert(body && "Pragma matching failed!");
			core::LambdaExprPtr&& lambdaExpr = conv.handleBody(body, *(*pit).second);
			mProgram = core::Program::addEntryPoint(mMgr, mProgram, lambdaExpr);
		}
	}

	if(!insiemePragmaFound) {
		// We start the conversion from the main function and then visit all the
		// called functions according to the callgraph of the input program.
		clang::CallGraphNode* main = pimpl->mCallGraph.getRoot();
		mProgram = conv.handleFunctionDecl(dyn_cast<const FunctionDecl>(pimpl->mCallGraph.getDecl(main)), true);
	}
	mProgram = addParallelism(mProgram, mMgr);
	return mProgram;
}

} // end frontend namespace
} // end insieme namespace


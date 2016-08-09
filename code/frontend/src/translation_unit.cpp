/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/translation_unit.h"

#include <regex>

#include "insieme/frontend/clang.h"
#include "insieme/frontend/pragma/handler.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace clang;

namespace {

    /**
     * this class guarantees release of resources when using the parserproxy.
     * TODO: Idealy ParserProxy should dissapear or turn into something which is
     * more standard.
     */
    struct ParserProxyRAII {
            ParserProxyRAII(clang::Parser* p){
                ParserProxy::init(p);
                p->Initialize();
            }
            ParserProxyRAII(const ParserProxyRAII&) = delete;
            ParserProxyRAII& operator=(const ParserProxyRAII&) = delete;
            ~ParserProxyRAII(){
		        ParserProxy::discard();
            }
    };

	// Instantiate the clang parser and sema to build the clang AST. Pragmas are stored during the parsing
	///
	void parseClangAST(ClangCompiler& comp, clang::ASTConsumer* Consumer, InsiemeSema& sema) {
		Parser P(comp.getPreprocessor(), sema, false); // do not skip function bodies
		comp.getPreprocessor().EnterMainSourceFile();

        // guarantee that resources are relased after use
        ParserProxyRAII guard(&P);

		Consumer->Initialize(comp.getASTContext());
		if(SemaConsumer* SC = dyn_cast<SemaConsumer>(Consumer)) { SC->InitializeSema(sema); }

		if(ExternalASTSource* External = comp.getASTContext().getExternalSource()) {
			if(ExternalSemaSource* ExternalSema = dyn_cast<ExternalSemaSource>(External)) { ExternalSema->InitializeSema(sema); }
			External->StartTranslationUnit(Consumer);
		}

		Parser::DeclGroupPtrTy ADecl;
		while(!P.ParseTopLevelDecl(ADecl)) {
			if(ADecl) { Consumer->HandleTopLevelDecl(ADecl.getPtrAs<DeclGroupRef>()); }
		}
		Consumer->HandleTranslationUnit(comp.getASTContext());
	}

} // end anonymous namespace

namespace insieme {
namespace frontend {

	TranslationUnit::TranslationUnit(NodeManager& mgr, const path& file, const ConversionSetup& setup)
	    : mMgr(mgr), mFileName(file), setup(setup), mClang(setup, file), mSema(mPragmaList, mClang.getPreprocessor(), mClang.getASTContext(), emptyCons, true) {

		// check for frontend extensions pragma handlers
		// and add user provided pragmas to be handled
		// by insieme
		std::map<std::string, clang::PragmaNamespace*> pragmaNamespaces;
		for(auto extension : setup.getExtensions()) {
			for(auto ph : extension->getPragmaHandlers()) {
				std::string name = ph->getName();
				// if the pragma namespace is not registered already
				// create and register it and store it in the map of pragma namespaces
				if(pragmaNamespaces.find(name) == pragmaNamespaces.end()) {
					pragmaNamespaces[name] = new clang::PragmaNamespace(name);
					mClang.getPreprocessor().AddPragmaHandler(pragmaNamespaces[name]);
				}
				// add the user provided pragma handler
				pragmaNamespaces[name]->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<pragma::Pragma>(
				    mClang.getPreprocessor().getIdentifierInfo(ph->getKeyword()), *ph->getToken(), ph->getName(), ph->getFunction()));
			}
		}

		parseClangAST(mClang, &emptyCons, mSema);

		// all pragmas should now have either a decl or a stmt attached.
		// it can be the case that a pragma is at the end of a file
		// and therefore not attached to anything. Find these cases
		// and attach the pragmas to the translation unit declaration.
		for(auto cur : mPragmaList) {
			if(!cur->isStatement() && !cur->isDecl()) { cur->setDecl(getCompiler().getASTContext().getTranslationUnitDecl()); }
		}

		assert_false(mClang.getDiagnostics().hasErrorOccurred()) << "Clang could not parse input file " << mFileName << ", please check the error report";

		if(setup.hasOption(ConversionSetup::DumpClangAST)) {
			const std::string filter = setup.getClangASTDumpFilter();
			auto tuDecl = getASTContext().getTranslationUnitDecl();
			// if nothing defined print the whole context
			if(filter.empty()) {
				tuDecl->dumpColor();
			} else {
				// else filter out the right function decls
				auto declCtx = clang::TranslationUnitDecl::castToDeclContext(tuDecl);
				// iterate through the declarations inside and print (maybe)
				for(auto it = declCtx->decls_begin(); it != declCtx->decls_end(); ++it) {
					if(const clang::FunctionDecl* funDecl = llvm::dyn_cast<clang::FunctionDecl>(*it)) {
						if(std::regex_match(funDecl->getNameAsString(), std::regex(filter))) { funDecl->dumpColor(); }
					}
				}
			}
		}
	}

	TranslationUnit::PragmaIterator TranslationUnit::pragmas_begin() const {
		auto filtering = [](const Pragma&) -> bool { return true; };
		return TranslationUnit::PragmaIterator(getPragmaList(), filtering);
	}

	TranslationUnit::PragmaIterator TranslationUnit::pragmas_begin(const TranslationUnit::PragmaIterator::FilteringFunc& func) const {
		return TranslationUnit::PragmaIterator(getPragmaList(), func);
	}

	TranslationUnit::PragmaIterator TranslationUnit::pragmas_end() const {
		return TranslationUnit::PragmaIterator(getPragmaList().end());
	}

	bool TranslationUnit::PragmaIterator::operator!=(const PragmaIterator& iter) const {
		return (pragmaIt != iter.pragmaIt);
	}

	void TranslationUnit::PragmaIterator::inc() {
		while(pragmaIt != pragmaItEnd) {
			++pragmaIt;

			if(pragmaIt != pragmaItEnd && filteringFunc(**pragmaIt)) { return; }
		}
	}

	PragmaPtr TranslationUnit::PragmaIterator::operator*() const {
		assert(pragmaIt != pragmaItEnd);
		return *pragmaIt;
	}

} // end frontend namespace
} // end insieme namespace

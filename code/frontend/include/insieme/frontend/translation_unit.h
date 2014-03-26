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

#pragma once

#include <set>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>

#include "insieme/core/ir_program.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/compiler.h"

#include "insieme/utils/logging.h"

#include "insieme/frontend/sema.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include "clang/AST/ASTConsumer.h"
#pragma GCC diagnostic pop


namespace insieme {
namespace frontend {

namespace pragma {
	class Pragma;
	typedef std::shared_ptr<Pragma> PragmaPtr;
	typedef std::vector<PragmaPtr> PragmaList;
}

// ------------------------------------ TranslationUnit ---------------------------
/**
 * A translation unit contains informations about the compiler (needed to keep alive object
 * instantiated by clang), and the pragmas encountred during the processing of the translation unit.
 */
class TranslationUnit: public boost::noncopyable {

	insieme::core::NodeManager& mMgr;
	path 					mFileName;
	const ConversionSetup	setup;
	ClangCompiler			mClang;
	insieme::frontend::pragma::PragmaList 		mPragmaList;

	//needed for for setting up the clang-compiler
	clang::ASTConsumer emptyCons;
    insieme::frontend::InsiemeSema mSema;

	//FIXME: needed for what? maybe for ParserProxy
	friend class ::TypeConversion_FileTest_Test;
	friend class ::StmtConversion_FileTest_Test;

public:
	TranslationUnit(insieme::core::NodeManager& mgr, const path& file,  const ConversionSetup& setup = ConversionSetup());

	class PragmaIterator : public std::iterator<std::input_iterator_tag,insieme::frontend::pragma::PragmaPtr> {
	public:
		typedef std::function<bool (const pragma::Pragma&)> FilteringFunc;
		typedef insieme::frontend::pragma::PragmaList::const_iterator pragma_iter;
	private:
		insieme::frontend::pragma::PragmaList::const_iterator pragmaIt, pragmaItEnd;
		FilteringFunc filteringFunc;

		// creates end iter
		PragmaIterator(const pragma_iter& end) : pragmaIt(end), pragmaItEnd(end) { }
		PragmaIterator(const pragma::PragmaList& list, const FilteringFunc& filteringFunc)
			: pragmaIt(list.begin()), pragmaItEnd(list.end()), filteringFunc(filteringFunc) { }

		void inc();

		friend class TranslationUnit;
	public:
		bool operator!=(const PragmaIterator& iter) const;
		bool operator==(const PragmaIterator& iter) const { return !(*this != iter); }
		pragma::PragmaPtr operator*() const;
		PragmaIterator& operator++() { inc(); return *this; }
	};

	/**
	 * Returns the list of registered pragmas across the translation units
	 */
	PragmaIterator pragmas_begin() const;
	PragmaIterator pragmas_begin(const PragmaIterator::FilteringFunc& func) const;
	PragmaIterator pragmas_end() const;

	/**
	 * Returns a list of pragmas defined in the translation unit
	 */
	const pragma::PragmaList& getPragmaList() const { return mPragmaList; }

	const ClangCompiler& getCompiler() const { return mClang; }

	const path& getFileName() const { return mFileName; }
	
	bool isCxx() const { return getCompiler().isCXX(); }

	// getters
	clang::Preprocessor& getPreprocessor() { return getCompiler().getPreprocessor(); }
	const clang::Preprocessor& getPreprocessor() const { return getCompiler().getPreprocessor(); }

	clang::ASTContext& getASTContext() { return getCompiler().getASTContext(); }
	const clang::ASTContext& getASTContext() const { return getCompiler().getASTContext(); }

	clang::DiagnosticsEngine& getDiagnostic() { return getCompiler().getDiagnostics(); }
	const clang::DiagnosticsEngine& getDiagnostic() const { return getCompiler().getDiagnostics(); }

};

} // end frontend namespace
} // end insieme namespace

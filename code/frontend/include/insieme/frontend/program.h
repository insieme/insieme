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


namespace insieme {
namespace frontend {

namespace analysis {
	class GlobalVarCollector;
}

namespace utils {
	class Interceptor;
	class Indexer;
	class FunctionDependencyGraph;
}

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
protected:
	path 					mFileName;
	ClangCompiler			mClang;
	insieme::frontend::pragma::PragmaList 		mPragmaList;

public:
	TranslationUnit(const ConversionSetup& setup, const path& file)
    : mFileName(file), mClang(setup, file) { }

	/**
	 * Returns a list of pragmas defined in the translation unit
	 */
	const pragma::PragmaList& getPragmaList() const {
		return mPragmaList;
	}

	const ClangCompiler& getCompiler() const {
		return mClang;
	}

	const path& getFileName() const {
		return mFileName;
	}

};


// ------------------------------------ Program ---------------------------
/**
 * A program is made of a set of compilation units, we need to keep this object so we can create a
 * complete call graph and thus determine which part of the input program should be handled by
 * insieme and which should be kept as the original.
 */
class Program: public boost::noncopyable {

	class ProgramImpl;
	typedef ProgramImpl* ProgramImplPtr;
	ProgramImplPtr pimpl;

	// Reference to the NodeManager used to convert the translation units into IR code
	insieme::core::NodeManager& mMgr;

	const ConversionSetup config;

	friend class ::TypeConversion_FileTest_Test;
	friend class ::StmtConversion_FileTest_Test;
public:

	Program(core::NodeManager& mgr, const path& file, const ConversionSetup& setup = ConversionSetup());

	~Program();

	const ClangCompiler& getCompiler() const;

	utils::Interceptor& getInterceptor() const;

	const vector<boost::filesystem::path>& getStdLibDirs() const;

	// TODO: eliminate this => setup should be parsed in the driver and the result should be stored in the conversion setup
	void setupInterceptor();

	bool isCxx() const { return config.getStandard() == ConversionSetup::Cxx03; }

	const pragma::PragmaList& getPragmaList() const;

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

		friend class Program;
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

};

} // end frontend namespace
} // end insieme namespace

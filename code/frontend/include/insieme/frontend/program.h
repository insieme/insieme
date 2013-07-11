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
	std::string 			mFileName;
	ClangCompiler			mClang;
	insieme::frontend::pragma::PragmaList 		mPragmaList;

public:
	TranslationUnit(const ConversionJob& job)
    : mFileName(job.getFile()), mClang(job,boost::algorithm::ends_with(job.getFile(),".o")) {
		assert(job.getFiles().size() == 1u && "Only a single file per translation unit allowed!");
    }

	/**
	 * Returns a list of pragmas defined in the translation unit
	 */
	const pragma::PragmaList& getPragmaList() const {
		return mPragmaList;
	}

	const ClangCompiler& getCompiler() const {
		return mClang;
	}

	const std::string& getFileName() const {
		return mFileName;
	}

};

typedef std::shared_ptr<TranslationUnit> TranslationUnitPtr;

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

	// The IR program node containing the converted IR
	insieme::core::ProgramPtr mProgram;

	const ConversionJob config;

	friend class ::TypeConversion_FileTest_Test;
	friend class ::StmtConversion_FileTest_Test;
public:
	typedef std::vector<TranslationUnitPtr> TranslationUnitList;
	Program(core::NodeManager& mgr, const ConversionJob& job = ConversionJob());

	~Program();

	utils::Interceptor& getInterceptor() const;
	utils::Indexer& getIndexer() const;
	analysis::GlobalVarCollector& getGlobalCollector() const;

	utils::FunctionDependencyGraph& getCallGraph() const;
	const vector<boost::filesystem::path>& getStdLibDirs() const;
	void setupInterceptor();
	void analyzeFuncDependencies();
	void dumpCallGraph() const;

    /**
     * Store ASTContext(s) of translation unit(s) to output file(s)
     *
     * @param output file name
     */
     void storeTranslationUnits(const string& output_file);

	/**
	 * Add a single file to the program.
	 *
	 * @param job a job covering a single translation unit (one input file only!)
	 */
	TranslationUnit& addTranslationUnit(const ConversionJob& job);

	/**
	 * Add multiple files to the program
	 */
	void addTranslationUnits(const ConversionJob& job) {
		for(const auto& cur : job.getFiles()) {
			ConversionJob single = job;
			single.setFiles(toVector(cur));
			addTranslationUnit(single);
		}
	}

	// convert the program into the IR representation
	const insieme::core::ProgramPtr& convert();

	const insieme::core::ProgramPtr& getProgram() const { return mProgram; }

	/**
	 * Returns a list of parsed translation units
	 */
	const TranslationUnitList& getTranslationUnits() const;

	class PragmaIterator: public
				std::iterator<
						std::input_iterator_tag,
						std::pair<insieme::frontend::pragma::PragmaPtr, TranslationUnitPtr>
				>
	{
	public:
		typedef std::function<bool (const pragma::Pragma&)> FilteringFunc;

	private:
		TranslationUnitList::const_iterator tuIt, tuEnd;
		insieme::frontend::pragma::PragmaList::const_iterator pragmaIt;
		FilteringFunc filteringFunc;

		// creates end iter
		PragmaIterator(const TranslationUnitList::const_iterator& tend) : tuIt(tend), tuEnd(tend) { }
		PragmaIterator(const TranslationUnitList& tu, const FilteringFunc& filteringFunc):
			tuIt(tu.begin()), tuEnd(tu.end()), filteringFunc(filteringFunc) { inc(true); }

		void inc(bool init);

		friend class Program;
	public:
		bool operator!=(const PragmaIterator& iter) const;
		bool operator==(const PragmaIterator& iter) const { return !(*this != iter); }
		std::pair<pragma::PragmaPtr, TranslationUnitPtr> operator*() const;
		PragmaIterator& operator++() { inc(false); return *this; }
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

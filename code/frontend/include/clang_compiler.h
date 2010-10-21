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

#include "program.h"

#include <memory>
#include <string>
#include <vector>
#include <exception>
#include <stdexcept>
#include <cassert>

#include <boost/utility.hpp>

// forward declarations
namespace clang {
class ASTContext;
class ASTConsumer;
class Preprocessor;
class Diagnostic;
class SourceManager;
class Parser;
class Token;
class Scope;
class Expr;
} // end clang namespace

// ------------------------------------ ParserProxy ---------------------------

/**
 * This is a proxy class which enables the access to internal clang features, i.e. Parser.
 * The main scope of this class is to handle the parsing of pragma(s) of the input file
 */
class ParserProxy {
	static ParserProxy* currParser;
	clang::Parser* mParser;

	ParserProxy(clang::Parser* parser): mParser(parser) { }
public:

	/**
	 * Initialize the proxy with the parser used to parse the current translation unit,
	 * call this method with a NULL parser causes an assertion.
	 */
	static void init(clang::Parser* parser=NULL) {
		assert(parser && "ParserProxy cannot be initialized with a NULL parser");
		currParser = new ParserProxy(parser);
	}

	/**
	 * the discard method is called when the Parser is no longer valid.
	 */
	static void discard() { currParser = NULL; }

	/**
	 * Returns the current parser, if not initialized an assertion is thrown.
	 */
	static ParserProxy& get() {
		assert(currParser && "Parser proxy not initialized.");
		return *currParser;
	}

	/**
	 * Parse an expression using the clang parser starting from the current token
	 */
	clang::Expr* ParseExpression(clang::Preprocessor& PP);
	void EnterTokenStream(clang::Preprocessor& PP);
	/**
	 * Consumes the current token (by moving the input stream pointer) and returns a reference to it
	 */
	clang::Token& ConsumeToken();
	clang::Scope* CurrentScope();
	/**
	 * Returns the last consumed token without advancing in the input stream
	 */
	clang::Token& CurrentToken();
	clang::Parser* getParser() const { return mParser; }
};

namespace insieme {

namespace core {
class Program;
}

namespace frontend {
/**
 * Used to report a parsing error occurred during the parsing of the input file
 */
struct ClangParsingError: public std::logic_error {
	ClangParsingError(const std::string& file_name): std::logic_error(file_name) { }
};

// ------------------------------------ ClangCompiler ---------------------------

/**
 * ClangCompiler is a wrapper class for the Clang compiler main interfaces. The main goal is to hide implementation
 * details to the client.
 */
class ClangCompiler: boost::noncopyable {
	struct ClangCompilerImpl;

	ClangCompilerImpl* pimpl;
	friend class InsiemeTransUnit;
public:
	/**
	 * Creates an empty compiler instance, usefull for test cases
	 */
	ClangCompiler();

	/**
	 * Creates a compiler instance from an input file
	 */
	ClangCompiler(const std::string& file_name);
	clang::ASTContext& getASTContext() const;
	clang::SourceManager& getSourceManager() const;
	clang::Preprocessor& getPreprocessor() const;
	clang::Diagnostic& getDiagnostics() const;
	~ClangCompiler();
};

// Forward declaration for Pragma
class Pragma;
typedef std::shared_ptr<Pragma> PragmaPtr;
typedef std::vector<PragmaPtr> 	PragmaList;

// ------------------------------------ TranslationUnit ---------------------------

/**
 * A translation unit contains informations about the compiler (needed to keep alive object instantiated by clang),
 * and the pragmas encountred during the processing of the translation unit.
 */
class TranslationUnit: public boost::noncopyable {
protected:
	std::string 	mFileName;
	ClangCompiler	mClang;
	PragmaList 		mPragmaList;
public:
	TranslationUnit(const std::string& fileName): mFileName(fileName), mClang(fileName) { }
	/**
	 * Returns a list of pragmas defined in the translation unit
	 */
	const PragmaList& getPragmaList() const { return mPragmaList; }
	const ClangCompiler& getCompiler() const { return mClang; }
	const std::string& getFileName() const { return mFileName; }
};

typedef std::shared_ptr<TranslationUnit> TranslationUnitPtr;

// ------------------------------------ Program ---------------------------
/**
 * A program is made of a set of compilation units, we need to keep this object so we can create a complete call graph and thus
 * determine which part of the input program should be handled by insieme and which should be kept as the original.
 */
class Program: public boost::noncopyable {

	// Implements the pimpl pattern so we don't need to introduce an explicit dependency to Clang headers
	class ProgramImpl;
	typedef std::shared_ptr<ProgramImpl> ProgramImplPtr;
	ProgramImplPtr pimpl;

	// Reference to the NodeManager used to convert the translation units into IR code
	const core::SharedNodeManager& mMgr;

	// The IR program node containing the converted IR
	core::ProgramPtr mProgram;

private:
	// Expand various annotations to corresponding parallel constructs
	const core::ProgramPtr& addParallelism();

public:
	typedef std::set<TranslationUnitPtr> TranslationUnitSet;

	Program(const core::SharedNodeManager& mgr);

	/**
	 * Add a single file to the program
	 */
	void addTranslationUnit(const std::string& fileName);

	/**
	 * Add multiple files to the program
	 */
	void addTranslationUnits(const std::vector<std::string>& fileNames) {
		std::for_each(fileNames.begin(), fileNames.end(), [ this ](const std::string& fileName){ this->addTranslationUnit(fileName); });
	}

	// convert the program into the IR representation
	const core::ProgramPtr& convert();

	const core::ProgramPtr& getProgram() const { return mProgram; }

	/**
	 * Returns a list of parsed translation units
	 */
	const TranslationUnitSet& getTranslationUnits() const;

	void dumpCallGraph() const;
};

} // End frontend namespace
} // End insieme namespace


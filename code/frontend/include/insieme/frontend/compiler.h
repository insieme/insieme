/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <iostream>

#include <memory>
#include <string>
#include <vector>
#include <exception>
#include <stdexcept>
#include <cassert>

#include <boost/utility.hpp>
#include <fstream>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include "insieme/frontend/frontend.h"

// forward declarations
namespace clang {
	class ASTUnit;
	class ASTContext;
	class ASTConsumer;
	class Preprocessor;
	class DiagnosticsEngine;
	class SourceManager;
	class Parser;
	class Token;
	class Scope;
	class Expr;
	class TargetInfo;
	class Sema;
	class FileSystemOptions;

	namespace idx {
		class Program;
		class Indexer;
	}
} // end clang namespace

class TypeConversion_FileTest_Test;
class StmtConversion_FileTest_Test;

// ------------------------------------ ParserProxy ---------------------------
/**
 * This is a proxy class which enables the access to internal clang features, i.e. Parser.
 * The main scope of this class is to handle the parsing of pragma(s) of the input file
 */
class ParserProxy {
	static ParserProxy* currParser;
	clang::Parser* mParser;

	ParserProxy(clang::Parser* parser) : mParser(parser) {}

  public:
	/**
	 * Initialize the proxy with the parser used to parse the current translation unit,
	 * call this method with a NULL parser causes an assertion.
	 */
	static void init(clang::Parser* parser = NULL) {
		assert_true(parser) << "ParserProxy cannot be initialized with a NULL parser";
		currParser = new ParserProxy(parser);
	}

	/**
	 * the discard method is called when the Parser is no longer valid.
	 */
	static void discard() {
		delete currParser;
		currParser = NULL;
	}

	/**
	 * Returns the current parser, if not initialized an assertion is thrown.
	 */
	static ParserProxy& get() {
		assert_true(currParser) << "Parser proxy not initialized.";
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
	clang::Parser* getParser() const {
		return mParser;
	}
};

namespace insieme {
namespace frontend {

	class InsiemeSema;

	namespace pragma {
		// forward declarations for pragma
		class Pragma;
		typedef std::shared_ptr<Pragma> PragmaPtr;
		typedef std::vector<PragmaPtr> PragmaList;

		class MatchMap;
	} // end pragma namespace

	// ------------------------------------ ClangCompiler ---------------------------
	/**
	 * ClangCompiler is a wrapper class for the Clang compiler main interfaces. The main goal is to hide implementation
	 * details to the client.
	 */
	class ClangCompiler : boost::noncopyable {
		struct ClangCompilerImpl;

		ClangCompilerImpl* pimpl;

	  public:
		/**
		 * Creates a compiler instance from the given conversion job.
		 */
		ClangCompiler(const ConversionSetup& config, const path& file);

		/**
		 * Returns clang's ASTContext
		 * @return
		 */
		clang::ASTContext& getASTContext() const;

		/**
		 * Returns clang's SourceManager
		 * @return
		 */
		clang::SourceManager& getSourceManager() const;

		/**
		 * Returns clang's Prepocessor
		 * @return
		 */
		clang::Preprocessor& getPreprocessor() const;

		/**
		 * Returns clang's Diagnostics
		 * @return
		 */
		clang::DiagnosticsEngine& getDiagnostics() const;

		/**
		 * Returns clang's TargetInfo
		 * @return
		 */
		clang::TargetInfo& getTargetInfo() const;

		/**
		 * Determines whether the represented translation unit
		 * is based on C or C++.
		 */
		bool isCXX() const;

		~ClangCompiler();
	};

} // End frontend namespace
} // End insieme namespace

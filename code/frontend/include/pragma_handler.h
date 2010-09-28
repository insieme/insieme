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

#include "clang_compiler.h"
#include "pragma_matcher.h"
#include "insieme_sema.h"

#include <memory>
#include <sstream>
#include <map>

#include <clang/Basic/SourceLocation.h>
#include <clang/Lex/Pragma.h>

#include "clang/Parse/Parser.h"

#include <glog/logging.h>

// forward declaration
namespace clang {
class Stmt;
class Decl;
class Expr;
}

namespace insieme {
namespace frontend {

// ------------------------------------ Pragma ---------------------------

/**
 * Defines a generic pragma which contains the location (start,end), and the target node
 */
class Pragma {
	/**
	 * Attach the pragma to a statement. If the pragma is already bound to a statement or location, a call to this method will produce an error.
	 */
	void setStatement(clang::Stmt const* stmt);

	/**
	 * Attach the pragma to a declaration. If the pragma is already bound to a statement or location, a call to this method will produce an error.
	 */
	void setDecl(clang::Decl const* decl);

	friend class InsiemeSema;
public:
	typedef llvm::PointerUnion<clang::Stmt const*, clang::Decl const*> PragmaTarget;

	/**
	 * Creates an empty pragma starting from source location startLoc and ending ad endLoc.
	 */
	Pragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type) :
		mStartLoc(startLoc), mEndLoc(endLoc), mType(type) { }

	/**
	 * Creates a pragma starting from source location startLoc and ending ad endLoc by passing the content of the map which associates, for each
	 * key defined in the pragma_matcher, the relative parsed list of values
	 *
	 */
	Pragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type,
			MatchMap const& mmap) :
		mStartLoc(startLoc), mEndLoc(endLoc), mType(type) { }

	const clang::SourceLocation& getStartLocation() const { return mStartLoc; }
	const clang::SourceLocation& getEndLocation() const { return mEndLoc; }
	/**
	 * Returns a string which identifies the pragma
	 */
	const std::string& getType() const { return mType; }

	clang::Stmt const* getStatement() const;
	clang::Decl const* getDecl() const;

	/**
	 * Returns true if the AST node associated to this pragma is a statement (clang::Stmt)
	 */
	bool isStatement() const { return mTargetNode.is<clang::Stmt const*> ();	}

	/**
	 * Returns true if the AST node associated to this pragma is a declaration (clang::Decl)
	 */
	bool isDecl() const { return mTargetNode.is<clang::Decl const*> (); }

	/**
	 * Writes the content of the pragma to standard output
	 */
	virtual void dump(std::ostream& out, const clang::SourceManager& sm) const;

	/**
	 * Returns a string representation of the pragma
	 */
	std::string toStr(const clang::SourceManager& sm) const;

	virtual ~Pragma() {	}

private:
	clang::SourceLocation mStartLoc, mEndLoc;
	std::string mType;
	PragmaTarget mTargetNode;
};

// ------------------------------------ BasicPragmaHandler<T> ---------------------------

/**
 * Defines a generic pragma handler which uses the pragma_matcher. Pragmas which are syntactically correct are then instantiated and associated with the
 * following node (i.e. a Stmt or Declaration). If an error occurs, the error message is printed out showing the location and the list of tokens the parser
 * was expecting at that location.
 */
template<class T>
class BasicPragmaHandler: public clang::PragmaHandler {
	node* pragma_matcher;
	std::string base_name;

public:
	BasicPragmaHandler(clang::IdentifierInfo* name, node const& pragma_matcher, std::string const& base_name = std::string()) :
		PragmaHandler(name->getName().str()), pragma_matcher(pragma_matcher.copy()), base_name(base_name) {
	}

	void HandlePragma(clang::Preprocessor& PP, clang::Token &FirstToken) {
		// '#' symbol is 1 position before
		clang::SourceLocation startLoc = ParserProxy::get().CurrentToken().getLocation().getFileLocWithOffset(-1);

		MatchMap mmap;
		ParserStack errStack;

		if (pragma_matcher->MatchPragma(PP, mmap, errStack)) {
			// the pragma type is formed by concatenation of the base_name and identifier, for example the type for the pragma:
			// #pragma omp barrier
			// will be "omp::barrier", the string is passed to the pragma constructur which store the value
			std::ostringstream pragma_name;
			pragma_name << base_name;
			if(!getName().empty())
				pragma_name << "::" << getName().str();

			clang::SourceLocation endLoc = ParserProxy::get().CurrentToken().getLocation();
			// the pragma has been successfully parsed, now we have to instantiate the correct type which is associated to this pragma (T) and
			// pass the matcher map in order for the pragma to initialize his internal representation. The framework will then take care of
			// associating the pragma to the following node (i.e. a statement or a declaration).
			static_cast<InsiemeSema&>(ParserProxy::get().getParser()->getActions()).ActOnPragma<T>(pragma_name.str(), mmap, startLoc, endLoc);
		} else {
			// In case of error, we report it to the console using the clang Diagnostics.
			ErrorReport(PP, startLoc, errStack);
			PP.DiscardUntilEndOfDirective();
		}
	}

	~BasicPragmaHandler() {	delete pragma_matcher; }
};

// ------------------------------------ PragmaHandlerFactory ---------------------------

struct PragmaHandlerFactory {

	template<class T>
	static clang::PragmaHandler* CreatePragmaHandler(clang::IdentifierInfo* name, node const& re, const std::string& base_name = std::string()) {
		return new BasicPragmaHandler<T> (name, re, base_name);
	}
};

/**
 * Pragma used for testing purposes
 */
class TestPragma: public Pragma {
	std::string expected;
public:
	TestPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& type, MatchMap const& mmap);
	std::string getExpected() const { return expected; }
};

} // End frontend namespace
} // End insieme namespace



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

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "clang_compiler.h"
#include "pragma_matcher.h"
#include "insieme_sema.h"

#include <clang/Basic/SourceLocation.h>
#include <clang/Lex/Pragma.h>

#include "clang/Parse/Parser.h"

#include <memory>
#include <sstream>
#include <map>

// forward declaration
namespace clang {
class Stmt;
class Decl;
class Expr;
}

namespace insieme {
namespace frontend {

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

template<class T>
class BasicPragmaHandler: public clang::PragmaHandler {
	std::string base_name;
	node* reg_exp;

public:
	BasicPragmaHandler(std::string const& base_name, clang::IdentifierInfo* name, node const& reg_exp) :
		PragmaHandler(name), base_name(base_name), reg_exp(reg_exp.copy()) {
	}

	void HandlePragma(clang::Preprocessor& PP, clang::Token &FirstToken) {
		// DEBUG("PRAGMA HANDLER: " << 
		// std::string(getName()->getNameStart(),
		//	   		   getName()->getNameStart()+getName()->getLength()) );
		// ParserProxy::CurrentToken().getName().setKind(FirstToken);

		clang::Token saveTok = ParserProxy::get().CurrentToken();
		// '#' symbol is 1 position before
		clang::SourceLocation startLoc = ParserProxy::get().CurrentToken().getLocation().getFileLocWithOffset(-1);

		MatcherResult MR = reg_exp->match(PP);
		if (MR.first) {
			// act on pragma
			// DEBUG(ParserProxy::CurrentToken().getName());
			std::ostringstream pragma_name;
			pragma_name << base_name;
			if (getName())
				pragma_name << "::" << std::string(getName()->getNameStart(), getName()->getNameStart() + getName()->getLength());

			clang::SourceLocation endLoc = ParserProxy::get().CurrentToken().getLocation();
			static_cast<InsiemeSema&>(ParserProxy::get().getParser()->getActions()).ActOnPragma<T>(pragma_name.str(), MR.second, startLoc, endLoc);
		} else {
			// TODO: REPORT ERROR
			PP.DiscardUntilEndOfDirective();
		}
	}

	~BasicPragmaHandler() {	delete reg_exp;	}
};

struct PragmaHandlerFactory {

	template<class T>
	static clang::PragmaHandler* CreatePragmaHandler(const char* base_name, clang::IdentifierInfo* name, node const& re) {
		return new BasicPragmaHandler<T> (base_name, name, re);
	}
};

} // End frontend namespace
} // End insieme namespace



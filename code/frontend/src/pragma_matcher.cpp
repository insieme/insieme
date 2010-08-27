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

#include "pragma_matcher.h"
#include "utils/source_locations.h"

#include <clang/Lex/Preprocessor.h>
#include <clang/Parse/Parser.h>
#include <clang/AST/Expr.h>
#include "lib/Sema/Sema.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include <llvm/Support/raw_ostream.h>

#include <glog/logging.h>
#include <boost/algorithm/string/join.hpp>

using namespace clang;
using namespace insieme::frontend;

#include <sstream>

namespace insieme {
namespace frontend {

// ------------------------------------ ValueUnion ---------------------------
ValueUnion::~ValueUnion() {
	if(ptrOwner && is<clang::Stmt*>()) {
		assert(clangCtx && "Invalid ASTContext associated with this element.");
		clangCtx->Deallocate(get<Stmt*>());
	}
	if(ptrOwner && is<std::string*>())
		delete get<std::string*>();
}

std::string ValueUnion::toStr() const {
	std::string ret;
	llvm::raw_string_ostream rs(ret);
	if(is<Stmt*>())
		get<Stmt*>()->printPretty(rs, *clangCtx, 0, clang::PrintingPolicy(clangCtx->getLangOptions()));
	else
		rs << *get<std::string*>();
	return rs.str();
}

void reportRecord(std::ostream& ss, std::vector<ParsingError>& errs, clang::SourceManager& srcMgr) {

	std::vector<std::string> list;
	std::transform(errs.begin(), errs.end(), back_inserter(list),
		[](const ParsingError& pe) { return pe.expected; });

	ss << boost::join(list, " | ");

	ss << " @ (" << frontend::util::Line(errs[0].loc, srcMgr) << ":" << frontend::util::Column(errs[0].loc, srcMgr) << ")";

	ss << std::endl;
}

void ErrorReport(clang::Preprocessor& pp, clang::SourceLocation& pragmaLoc, ErrorStack& errStack) {
	TextDiagnosticPrinter &tdc = (TextDiagnosticPrinter&) *pp.getDiagnostics().getClient();

	std::string str;
	llvm::raw_string_ostream sstr(str);
	pragmaLoc.print(sstr, pp.getSourceManager());
	std::ostringstream ss;
	ss << sstr.str() << ": ";
	ss << "error: expected ";

	size_t err, ferr = errStack.getFirstRecord();
	err = ferr;
	bool first = true;
	do {
		if(!errStack.errs[err].empty()) {
			if(!first)
				ss << "\tor: ";
			reportRecord(ss, errStack.errs[err], pp.getSourceManager());
			if(first) {
				first = false;
			}
		}
		err++;
	} while(err < errStack.errs.size());
	llvm::errs() << ss.str();
	tdc.EmitCaretDiagnostic(errStack.errs[ferr].begin()->loc, NULL, 0, pp.getSourceManager(), 0, 0, 80);
}

// ------------------------------------ node ---------------------------

concat node::operator>>(node const& n) const { return concat(*this, n); }
star node::operator*() const { return star(*this); }
choice node::operator|(node const& n) const { return choice(*this, n); }
option node::operator!() const { return option(*this); }

bool concat::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	int id = errStack.openRecord();
	PP.EnableBacktrackAtThisPos();
	if (first->match(PP, mmap, errStack, id)) {
		errStack.discardPrevRecords(id);
		id = errStack.openRecord();
		if(second->match(PP, mmap, errStack, id)) {
			PP.CommitBacktrackedTokens();
			errStack.discardRecord(id);
			return true;
		}
	}
	PP.Backtrack();
	return false;
}

bool star::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	while (getNode()->match(PP, mmap, errStack, recID))
		;
	return true;
}

bool choice::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	int id = errStack.openRecord();
	PP.EnableBacktrackAtThisPos();
	if (first->match(PP, mmap, errStack, id)) {
		PP.CommitBacktrackedTokens();
		errStack.discardRecord(id);
		return true;
	}
	PP.Backtrack();
	PP.EnableBacktrackAtThisPos();
	if (second->match(PP, mmap, errStack, id)) {
		PP.CommitBacktrackedTokens();
		errStack.discardRecord(id);
		return true;
	}
	PP.Backtrack();
	return false;
}

bool option::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	PP.EnableBacktrackAtThisPos();
	if (getNode()->match(PP, mmap, errStack, recID)) {
		PP.CommitBacktrackedTokens();
		return true;
	}
	PP.Backtrack();
	return true;
}

bool expr_p::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	// ClangContext::get().getParser()->Tok.setKind(*firstTok);
	PP.EnableBacktrackAtThisPos();
	Expr* result = ParserProxy::get().ParseExpression(PP);

	if (result) {
		PP.CommitBacktrackedTokens();
		ParserProxy::get().EnterTokenStream(PP);
		PP.LookAhead(1); // THIS IS CRAZY BUT IT WORKS
		if (getMapName().size())
			mmap[getMapName()].push_back(
					ValueUnionPtr(new ValueUnion(result, &static_cast<clang::Sema&>(ParserProxy::get().getParser()->getActions()).Context))
			);
		return true;
	}
	PP.Backtrack();
	ParsingError pe;
	pe.expected = "expr";
	pe.loc = ParserProxy::get().CurrentToken().getLocation();
	errStack.addExpected(recID,pe);
	return false;
}

bool kwd::match(Preprocessor& PP, MatchMap& mmap, ErrorStack& errStack, size_t recID) const {
	clang::Token& token = ParserProxy::get().ConsumeToken();
	if (token.is(clang::tok::identifier) && ParserProxy::get().CurrentToken().getIdentifierInfo()->getName() == kw) {
		if(isAddToMap() && getMapName().empty())
			mmap[kw];
		else if(isAddToMap())
			mmap[getMapName()].push_back( ValueUnionPtr(new ValueUnion( kw )) );
		return true;
	}
	ParsingError pe;
	pe.expected = "\'" + kw + "\'";
	pe.loc = token.getLocation();
	errStack.addExpected(recID,pe);
	return false;
}

std::string TokenToStr(clang::tok::TokenKind token) {
	const char *name = clang::tok::getTokenSimpleSpelling(token);
	if(name)
		return std::string(name);
	else
		return std::string(clang::tok::getTokenName(token));
}

std::string TokenToStr(const clang::Token& token) {
	if (token.isLiteral()) {
		return std::string(token.getLiteralData(), token.getLiteralData() + token.getLength());
	} else {
		return TokenToStr(token);
	}
}

void AddToMap(clang::tok::TokenKind tok, Token const& token, std::string const& map_str, MatchMap& mmap) {
	if (!map_str.size())
		return;
	Action& A = ParserProxy::get().getParser()->getActions();
	switch (tok) {
	case clang::tok::numeric_constant:
		mmap[map_str].push_back(
				ValueUnionPtr(new ValueUnion(A.ActOnNumericConstant(token).takeAs<IntegerLiteral>(), &static_cast<clang::Sema&>(A).Context)) );
		break;
	case clang::tok::identifier: {
		UnqualifiedId Name;
		CXXScopeSpec ScopeSpec;
		Name.setIdentifier(token.getIdentifierInfo(), token.getLocation());

		mmap[map_str].push_back(
				ValueUnionPtr(new ValueUnion(A.ActOnIdExpression(ParserProxy::get().CurrentScope(), ScopeSpec, Name, false, false).takeAs<Stmt>(),
						&static_cast<clang::Sema&>(A).Context))
				);
		break;
	}
	default: {
		mmap[map_str].push_back( ValueUnionPtr(new ValueUnion(TokenToStr(token))) );
		break;
	}
	}
}

} // End frontend namespace
} // End insieme namespace

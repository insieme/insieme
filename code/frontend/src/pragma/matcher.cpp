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

#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/utils/string_utils.h"
#include "insieme/frontend/convert.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/Lex/Preprocessor.h>
#include <clang/Parse/Parser.h>
#include "clang/Sema/Sema.h"
#include <clang/Sema/Lookup.h>
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Basic/Diagnostic.h"
#include <clang/AST/Expr.h>
#include <clang/AST/ASTContext.h>
#pragma GCC diagnostic pop

#include <llvm/Support/raw_ostream.h>

//#include <glog/logging.h>
#include "insieme/utils/logging.h"
#include <boost/algorithm/string/join.hpp>


using namespace clang;
using namespace insieme::frontend;
using namespace insieme::frontend::pragma;

#include <sstream>

namespace {

void reportRecord( std::ostream& 					ss,
				   ParserStack::LocErrorList const& errs,
				   clang::SourceManager& 			srcMgr )
{
	std::vector<std::string> list;
	std::transform(errs.begin(), errs.end(), back_inserter(list),
			[](const ParserStack::Error& pe) { return pe.expected; }
		);

	ss << boost::join(list, " | ");
	ss << std::endl;
}

} // end anonymous namespace

namespace insieme {
namespace frontend {
namespace pragma {

// ------------------------------------ ValueUnion ---------------------------
ValueUnion::~ValueUnion() {
	if ( ptrOwner && is<clang::Stmt*>() ) {
		assert(clangCtx && "Invalid ASTContext associated with this element.");
		clangCtx->Deallocate(get<Stmt*>());
	}
	if(ptrOwner && is<std::string*>())
		delete get<std::string*>();
}

std::string ValueUnion::toStr() const {
	std::string ret;
	llvm::raw_string_ostream rs(ret);
	if ( is<Stmt*>() ) {
		// [3.0] get<Stmt*>()->printPretty(rs, *clangCtx, 0, clang::PrintingPolicy(clangCtx->getLangOptions()));
		get<Stmt*>()->printPretty(rs, 0, clangCtx->getPrintingPolicy());
	} else {
		rs << *get<std::string*>();
	}
	return rs.str();
}

std::ostream& ValueUnion::printTo(std::ostream& out) const {
	return out << toStr();
}

MatchMap::MatchMap(const MatchMap& other) {

	std::for_each(other.cbegin(), other.cend(), [ this ](const MatchMap::value_type& curr) {
		(*this)[curr.first] = ValueList();
		ValueList& currList = (*this)[curr.first];

		std::for_each(curr.second.cbegin(), curr.second.cend(), [ &currList ](const ValueList::value_type& elem) {
			currList.push_back( ValueUnionPtr( new ValueUnion(*elem, true, elem->isExpr()) ) );
		});
	});
}

std::ostream& MatchMap::printTo(std::ostream& out) const {
	for_each(begin(), end(), [&] ( const MatchMap::value_type& cur ) {
				out << "KEY: '" << cur.first << "' -> ";
				out << "[" << join(", ", cur.second,
					[](std::ostream& out, const ValueUnionPtr& cur){ out << *cur; } ) << "]";
				out << std::endl;
			});
	return out;
}

bool ValueUnion::isExpr() const {
    return isExp;
}

// ------------------------------------ MatchObject ---------------------------
core::ExpressionPtr MatchObject::getExpr(const ValueUnionPtr& p, conversion::Converter& fact) {
    clang::Stmt* stmt = p->get<clang::Stmt*>();
    if(auto expr = llvm::dyn_cast<clang::Expr>(stmt)) {
        core::ExpressionPtr&& varExpr = fact.convertExpr( expr );
        assert(varExpr && "Conversion a to Insieme node failed!");
        return varExpr;
    }
    assert(false && "expression used in pragma seems to be no expression");
}

core::VariablePtr MatchObject::getVar(const ValueUnionPtr& p, conversion::Converter& fact) {
    clang::Stmt* varIdent = p->get<clang::Stmt*>();
    assert(varIdent && "Clause not containing var exps");
    clang::DeclRefExpr* refVarIdent = llvm::dyn_cast<clang::DeclRefExpr>(varIdent);
    assert(refVarIdent && "Clause not containing a DeclRefExpr");
    core::ExpressionPtr&& varExpr = fact.convertExpr( refVarIdent );
    assert(varExpr.isa<core::VariablePtr>() && "variable used in pragma cannot seems to be no variable");
    return varExpr.as<core::VariablePtr>();
}


void MatchObject::cloneFromMatchMap(const MatchMap& mmap, conversion::Converter& fact) {
    if(called) {
        return;
    } else {
        for(auto m : mmap) {
            for(unsigned i=0; i<m.second.size(); i++) {
                if(!m.second[i]->isExpr()) {
                    varList[m.first].push_back(getVar(m.second[i], fact));
                } else {
                    exprList[m.first].push_back(getExpr(m.second[i], fact));
                }
            }
        }
        called = true;
    }
}

// ------------------------------------ ParserStack ---------------------------

size_t ParserStack::openRecord() {
	mRecords.push_back( LocErrorList() );
	return mRecordId++;
}

void ParserStack::addExpected(size_t recordId, const Error& pe) { mRecords[recordId].push_back(pe); }

void ParserStack::discardRecord(size_t recordId) { mRecords[recordId] = LocErrorList(); }

size_t ParserStack::getFirstValidRecord() {
	for ( size_t i=0; i<mRecords.size(); ++i )
		if ( !mRecords[i].empty() ) return i;
	assert(false);
	return 0;
}

void ParserStack::discardPrevRecords(size_t recordId) {
	//TODO: Recheck this Visual Studio 2010 fix!
	std::for_each(mRecords.begin(), mRecords.begin()+recordId, [](ParserStack::LocErrorList& cur) {
		cur = ParserStack::LocErrorList();
	} );
}

const ParserStack::LocErrorList& ParserStack::getRecord(size_t recordId) const { return mRecords[recordId]; }


/**
 * This function is used to report an error occurred during the pragma matching. Clang utilities are used
 * to report the carret location of the error.
 */
void errorReport(clang::Preprocessor& pp, clang::SourceLocation& pragmaLoc, ParserStack& errStack) {
	using namespace insieme::frontend::utils;

	std::string str;
	llvm::raw_string_ostream sstr(str);
	pragmaLoc.print(sstr, pp.getSourceManager());
	std::ostringstream ss;
	ss << sstr.str() << ": ";
	ss << "error: expected ";

	size_t err, ferr = errStack.getFirstValidRecord();
	err = ferr;
	SourceLocation errLoc = errStack.getRecord(err).front().loc;
	ss << "at location (" << Line(errLoc, pp.getSourceManager()) << ":" << Column(errLoc, pp.getSourceManager()) << ") ";
	bool first = true;
	do {
		if ( !errStack.getRecord(err).empty() && errStack.getRecord(err).front().loc == errLoc ) {
			!first && ss << "\t";

			reportRecord(ss, errStack.getRecord(err), pp.getSourceManager());
			first = false;
		}
		err++;
	} while(err < errStack.stackSize());

	pp.Diag(errLoc, pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Error, ss.str()));
}

// ------------------------------------ node ---------------------------
concat node::operator>>(node const& n) const { return concat(*this, n); }
star node::operator*() const { return star(*this); }
choice node::operator|(node const& n) const { return choice(*this, n); }
option node::operator!() const { return option(*this); }

std::ostream& node::printTo(std::ostream& out) const {
	return out << "node";
}

std::ostream& star::printTo(std::ostream& out) const {
	return out << "star(" << *getNode() << ")";
}

std::ostream& concat::printTo(std::ostream& out) const {
	return out << "(" << *first << ", " << *second << ")";
}

std::ostream& option::printTo(std::ostream& out) const {
	return out << "option(" << *getNode() << ")";
}

template<clang::tok::TokenKind T>
std::ostream& Tok<T>::printTo(std::ostream& out) const {
	if(tok.empty())
		return out << clang::tok::getTokenName(T);
	else
		return out << "Tok(" << clang::tok::getTokenName(T) << ": " << tok << ")";
}

std::ostream& kwd::printTo(std::ostream& out) const {
	return out << "kwd(" << kw << ")";
}

bool concat::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
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

bool star::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
	while (getNode()->match(PP, mmap, errStack, recID))
		;
	return true;
}

bool choice::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
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

bool option::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
	PP.EnableBacktrackAtThisPos();
	if (getNode()->match(PP, mmap, errStack, recID)) {
		PP.CommitBacktrackedTokens();
		return true;
	}
	PP.Backtrack();
	return true;
}

bool expr_p::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
	// ClangContext::get().getParser()->Tok.setKind(*firstTok);
	PP.EnableBacktrackAtThisPos();
	Expr* result = ParserProxy::get().ParseExpression(PP);

	if (result) {
		PP.CommitBacktrackedTokens();
		ParserProxy::get().EnterTokenStream(PP);
		PP.LookAhead(1); // THIS IS CRAZY BUT IT WORKS
		if (getMapName().size())
			mmap[getMapName()].push_back( ValueUnionPtr(
				new ValueUnion(result, &static_cast<clang::Sema&>(ParserProxy::get().getParser()->getActions()).Context, true)
			));
		return true;
	}
	PP.Backtrack();
	errStack.addExpected(recID, ParserStack::Error("expr", ParserProxy::get().CurrentToken().getLocation()));
	return false;
}

bool kwd::match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
	clang::Token& token = ParserProxy::get().ConsumeToken();
	if (token.getIdentifierInfo() && token.getIdentifierInfo()->getName() == kw) {
		if(isAddToMap() && getMapName().empty())
			mmap[kw];
		else if(isAddToMap())
			mmap[getMapName()].push_back( ValueUnionPtr(new ValueUnion( kw )) );
		return true;
	}
	errStack.addExpected(recID, ParserStack::Error("\'" + kw + "\'", token.getLocation()));
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
		return TokenToStr(token.getKind());
	}
}

void AddToMap(clang::tok::TokenKind tok, Token const& token, bool resolve, std::string const& map_str, MatchMap& mmap) {
	if (!map_str.size()) { return; }

	Sema& A = ParserProxy::get().getParser()->getActions();

	// HACK: FIXME
	// this hacks make it possible that if we have a token and we just want its string value
	// we do not invoke clang semantics action on it.
	if (!resolve) {
		if (tok == clang::tok::identifier) {
			UnqualifiedId Name;
			Name.setIdentifier(token.getIdentifierInfo(), token.getLocation());
			mmap[map_str].push_back(
				ValueUnionPtr(new ValueUnion(
					std::string(
						Name.Identifier->getNameStart(),
						Name.Identifier->getLength()
					)
				))
			);
			return;
		}
		mmap[map_str].push_back( ValueUnionPtr(new ValueUnion(TokenToStr(token))) );
		return ;
	}

	// We want to use clang sema to actually get the Clang node which is found out of this
	// identifier
	switch (tok) {
	case clang::tok::numeric_constant:
		mmap[map_str].push_back(ValueUnionPtr(
			new ValueUnion(A.ActOnNumericConstant(token).takeAs<IntegerLiteral>(), &static_cast<clang::Sema&>(A).Context))
		);
		break;
	case clang::tok::identifier: {
		UnqualifiedId Name;
		CXXScopeSpec ScopeSpec;
		Name.setIdentifier(token.getIdentifierInfo(), token.getLocation());

		// look up the identifier name
		LookupResult res(A, clang::DeclarationName(token.getIdentifierInfo()), token.getLocation(), clang::Sema::LookupOrdinaryName);
		if (!A.LookupName(res, ParserProxy::get().CurrentScope(), false)) {
			// TODO: Identifier could not be resolved => report error!
			assert(false && "Unable to obtain declaration of identifier!");
		}
		auto varDecl = res.getAsSingle<clang::VarDecl>();

		mmap[map_str].push_back(
			ValueUnionPtr(
				new ValueUnion(
					// [3.0] A.ActOnIdExpression(ParserProxy::get().CurrentScope(), ScopeSpec, Name, false, false).takeAs<Stmt>(),
					new (A.Context) clang::DeclRefExpr(varDecl, false, varDecl->getType(), VK_LValue, varDecl->getLocation()),
					&A.Context
				)
			));
		break;
	}
	default: {
		mmap[map_str].push_back( ValueUnionPtr(new ValueUnion(TokenToStr(token))) );
		break;
	}
	}
}

} // end pragma namespace
} // End frontend namespace
} // End insieme  namespace

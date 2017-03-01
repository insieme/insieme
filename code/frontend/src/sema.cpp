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
#include "insieme/frontend/sema.h"

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/utils/source_locations.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Stmt.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/Sema.h"

#include "insieme/utils/logging.h"

using namespace clang;
using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::frontend::utils;
using namespace insieme::utils::log;

typedef std::list<PragmaPtr> PendingPragmaList;

namespace {

	// Returns true if the source location SL is inside the range defined by SR
	bool isInsideRange(SourceRange SR, SourceLocation SL, SourceManager const& sm) {
		return Line(SR, sm).first <= Line(SL, sm) && Line(SR, sm).second > Line(SL, sm);
	}

	// Returns true if the source location SL is after the range defined by SR
	bool isAfterRange(SourceRange SR, SourceLocation SL, clang::SourceManager const& sm) {
		return Line(SR, sm).second <= Line(SL, sm);
	}

	void EraseMatchedPragmas(PendingPragmaList& pending, PragmaList& matched) {
		for(PragmaList::iterator I = matched.begin(), E = matched.end(); I != E; ++I) {
			std::list<PragmaPtr>::iterator it = std::find(pending.begin(), pending.end(), *I);
			assert(it != pending.end() && "Current matched pragma is not in the list of pending pragmas");
			pending.erase(it);
		}
	}

	/**
	 * Given a range, the PragmaFilter returns the pragmas with are defined between that range.
	 */
	class PragmaFilter {
		SourceRange bounds;
		SourceManager const& sm;
		PendingPragmaList::reverse_iterator I, E;

		void inc(bool first) {
			while(first && I != E && isAfterRange(bounds, (*I)->getStartLocation(), sm)) {
				++I;
			}

			if(!first) { ++I; }
		}

	  public:
		PragmaFilter(SourceRange const& bounds, SourceManager const& sm, PendingPragmaList& pragma_list)
		    : bounds(bounds), sm(sm), I(pragma_list.rbegin()), E(pragma_list.rend()) {
			inc(true);
		}

		void operator++() {
			inc(false);
		}

		PragmaPtr operator*() const {
			if(I == E) { return PragmaPtr(); }

			if(isInsideRange(bounds, (*I)->getStartLocation(), sm)) { return *I; }

			return PragmaPtr();
		}

		bool operator!=(PragmaFilter const& other) const {
			return I == other.I;
		}
	};
} // end anonymous namespace

namespace insieme {
namespace frontend {

	struct InsiemeSema::InsiemeSemaImpl {
		PragmaList& pragma_list;
		PendingPragmaList pending_pragma;

		InsiemeSemaImpl(PragmaList& pragma_list) : pragma_list(pragma_list) {}
	};

	InsiemeSema::InsiemeSema(PragmaList& pragma_list, clang::Preprocessor& pp, clang::ASTContext& ctx, clang::ASTConsumer& ast_consumer,
	                         bool CompleteTranslationUnit, clang::CodeCompleteConsumer* CompletionConsumer)
	    : clang::Sema(pp, ctx, ast_consumer, clang::TU_Complete, CompletionConsumer), pimpl(new InsiemeSemaImpl(pragma_list)), isInsideFunctionDef(false) {}

	InsiemeSema::~InsiemeSema() {
		delete pimpl;
	}

	/*
	 * The function search for the character c in the input stream backwards. The assumption is the
	 * character will be in the input stream so no termination condition is needed.
	 */
	const char* strbchr(const char* pos, const char* begin, char c) {
		while(pos != begin && *pos != c) {
			pos--;
		}
		return pos;
	}

	clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R, llvm::ArrayRef<clang::Stmt*> Elts, bool isStmtExpr) {
		// we parse the original code segment, within the original locations
		StmtResult&& ret = Sema::ActOnCompoundStmt(L, R, std::move(Elts), isStmtExpr);
		clang::CompoundStmt* CS = cast<clang::CompoundStmt>(ret.get());

		// This is still buggy with Clang 3.6.2:
		// when pragmas are just after the beginning of a compound stmt, example:
		// {
		// 		#pragma xxx
		// 		...
		// }
		// the location of the opening bracket is wrong because of a bug in the clang parser.
		//
        // FIXME: THIS IS A DIRTY HACK AND SHOLD BE ADDRESSED
        // the problem is not clang as was always blamed, our plagma matcher leaves the
        // lexer in an unstable state. Lexer Tokens are used by reference and each lookahead alter
        // the Lexer state. Once pragma has been consumed, the lexer is not returned to the rightfull
        // position.
        // Looking for the the brackets here is error prone. There is a need to deal with macro expansions and
        // wold require of a more sofisticated infrastructure. Like the one already provided by clang preprocessor.
        // Therefore here we sould assert that the source locations(L and R) point EXACTLY to { and } respectively,
        // and solve this issue in the pragma lexer. (I have seen it working, but without the Raw string in C mode).

		enum { MacroIDBit = 1U << 31 }; // from clang/Basic/SourceLocation.h for use with cpp classes
		{
			SourceLocation&& leftBracketLoc = SourceMgr.getImmediateSpellingLoc(L);
			std::pair<FileID, unsigned>&& locInfo = SourceMgr.getDecomposedLoc(leftBracketLoc);
			llvm::StringRef&& buffer = SourceMgr.getBufferData(locInfo.first);
			const char* strData = buffer.begin() + locInfo.second;
			char const* lBracePos = strbchr(strData, buffer.begin(), '{');

			// We know the location of the left bracket, we overwrite the value of L with the correct location
			// but only if the location is valid as in getFileLocWithOffset() in SourceLocation
			if((((leftBracketLoc.getRawEncoding() & ~MacroIDBit) + (lBracePos - strData)) & MacroIDBit) == 0) {
				L = leftBracketLoc.getLocWithOffset(lBracePos - strData);
			}
		}

		// For the right bracket, we start at the final statement in the compound
		//   (or its start if it is empty) and search forward until we find the first "}"
		// Otherwise, cases such as this:
		//
		// {
		//    bla();
		// }
		// #pragma test expect_ir(R"( {} )")
		//
		// will be broken

		{
			SourceLocation rightBracketLoc;
			if(CS->size() == 0) {
				rightBracketLoc = SourceMgr.getImmediateSpellingLoc(L);
			} else {
				rightBracketLoc = SourceMgr.getImmediateSpellingLoc(CS->body_back()->getLocEnd());
			}
			std::pair<FileID, unsigned>&& locInfo = SourceMgr.getDecomposedLoc(rightBracketLoc);
			llvm::StringRef buffer = SourceMgr.getBufferData(locInfo.first);
			const char* strData = buffer.begin() + locInfo.second;
			char const* rBracePos = strchr(strData, '}');

			// We know the location of the right bracket, we overwrite the value of R with the correct location
			if((((rightBracketLoc.getRawEncoding() & ~MacroIDBit) + (rBracePos - strData)) & MacroIDBit) == 0) {
				R = rightBracketLoc.getLocWithOffset(rBracePos - strData);
			}
		}

		// the source range we inspect is defined by the new source locations,
		// this fix the problem with boundaries jumping to the beginning of the file in
		// the macro expansions:
		//
		//	#define F(x) { }
		//
		//		...
		//
		//		F(r)    // <-this statement will jum to the macro location
		//
		PragmaList matched;
		SourceRange SR(L, R);

		// for each of the pragmas in the range between brackets
		for(PragmaFilter&& filter = PragmaFilter(SR, SourceMgr, pimpl->pending_pragma); *filter; ++filter) {
			PragmaPtr P = *filter;

			unsigned int pragmaStart = utils::Line(P->getStartLocation(), SourceMgr);
			unsigned int pragmaEnd = utils::Line(P->getEndLocation(), SourceMgr);

			bool found = false;
			// problem with first pragma, compound start is delayed until fist usable line (first stmt)
			if(CS->size() > 0) {
				for(clang::CompoundStmt::body_iterator it = CS->body_begin(); it != CS->body_end(); ++it) {
					unsigned int stmtStart = (Line((*it)->getLocStart(), SourceMgr));

					if((pragmaEnd <= stmtStart)) {
						// ACHTUNG: if the node is a nullStmt, and is not at the end of the compound (in
						// which case is most probably ours) we can not trust it. semantics wont change,
						// we move one more. (BUG: nullStmt followed by pragmas, the source begin is
						// postponed until next stmt) this makes pragmas to be attached to a previous
						// stmt
						if(!llvm::isa<clang::NullStmt>(*it)) {
							// this pragma is attached to the current stmt
							P->setStatement(*it);
							matched.push_back(P);
							found = true;
							break;
						}
					}
				}
			}
			if(!found && pragmaStart <= utils::Line(R, SourceMgr)) {
				// this is a de-attached pragma (barrier i.e.) at the end of the compound
				// we need to create a fake NullStmt ( ; ) to attach this
				Stmt** stmts = new Stmt*[CS->size() + 1];

				ArrayRef<clang::Stmt*> stmtList(stmts, CS->size() + 1);

				clang::CompoundStmt* newCS =
				    new(Context) clang::CompoundStmt(Context, stmtList, CS->getSourceRange().getBegin(), CS->getSourceRange().getEnd());

				std::copy(CS->body_begin(), CS->body_end(), newCS->body_begin());
				std::for_each(CS->body_begin(), CS->body_end(), [&](Stmt*& curr) { this->Context.Deallocate(curr); });
				newCS->setLastStmt(new(Context) NullStmt(SourceLocation()));

				P->setStatement(*newCS->body_rbegin());
				matched.push_back(P);

				// transfer the ownership of the statement
				clang::CompoundStmt* oldStmt = ret.getAs<clang::CompoundStmt>();
				oldStmt->setStmts(Context, NULL, 0);
				ret = newCS;
				CS = newCS;

				// destroy the old compound stmt
				Context.Deallocate(oldStmt);
				delete[] stmts;
			}
		}
		// remove matched pragmas
		EraseMatchedPragmas(pimpl->pending_pragma, matched);

		return std::move(ret);
	}

	void InsiemeSema::matchStmt(clang::Stmt* S, const clang::SourceRange& bounds, const clang::SourceManager& sm, PragmaList& matched) {
		for(PragmaFilter filter(bounds, sm, pimpl->pending_pragma); *filter; ++filter) {
			PragmaPtr&& P = *filter;

			P->setStatement(S);
			matched.push_back(P);
		}
	}

	clang::StmtResult InsiemeSema::ActOnIfStmt(clang::SourceLocation IfLoc, clang::Sema::FullExprArg CondVal, clang::Decl* CondVar, clang::Stmt* ThenVal,
	                                           clang::SourceLocation ElseLoc, clang::Stmt* ElseVal) {
		VLOG(2) << "{InsiemeSema}: ActOnIfStmt()";
		clang::StmtResult ret = Sema::ActOnIfStmt(IfLoc, CondVal, CondVar, std::move(ThenVal), ElseLoc, std::move(ElseVal));

		clang::IfStmt* ifStmt = static_cast<clang::IfStmt*>(ret.get());
		PragmaList matched;

		matchStmt(ifStmt->getThen(), SourceRange(IfLoc, IfLoc), SourceMgr, matched);
		// is there any pragmas to be associated with the 'then' statement of this if?
		if(!isa<clang::CompoundStmt>(ifStmt->getThen())) {
			// if there is no compound stmt, check the then part
			matchStmt(ifStmt->getThen(), SourceRange(IfLoc, ThenVal->getLocEnd()), SourceMgr, matched);
		}
		EraseMatchedPragmas(pimpl->pending_pragma, matched);
		matched.clear();

		// is there any pragmas to be associated with the 'else' statement of this if?
		if(ifStmt->getElse() && !isa<clang::CompoundStmt>(ifStmt->getElse())) {
			matchStmt(ifStmt->getElse(), SourceRange(ElseLoc, ifStmt->getSourceRange().getEnd()), SourceMgr, matched);
		}

		EraseMatchedPragmas(pimpl->pending_pragma, matched);
		return ret;
	}

	clang::StmtResult InsiemeSema::ActOnForStmt(clang::SourceLocation ForLoc, clang::SourceLocation LParenLoc, clang::Stmt* First,
	                                            clang::Sema::FullExprArg Second, clang::Decl* SecondVar, clang::Sema::FullExprArg Third,
	                                            clang::SourceLocation RParenLoc, clang::Stmt* Body) {
		VLOG(2) << "{InsiemeSema}: ActOnForStmt()" << std::endl;
		clang::StmtResult ret = Sema::ActOnForStmt(ForLoc, LParenLoc, std::move(First), Second, SecondVar, Third, RParenLoc, std::move(Body));

		clang::ForStmt* forStmt = (clang::ForStmt*)ret.get();
		PragmaList matched;
		if(!isa<clang::CompoundStmt>(forStmt->getBody())) { matchStmt(forStmt->getBody(), forStmt->getSourceRange(), SourceMgr, matched); }
		EraseMatchedPragmas(pimpl->pending_pragma, matched);
		matched.clear();

		return ret;
	}

	clang::Decl* InsiemeSema::ActOnStartOfFunctionDef(clang::Scope* FnBodyScope, clang::Declarator& D) {
		isInsideFunctionDef = true;
		return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
	}

	clang::Decl* InsiemeSema::ActOnStartOfFunctionDef(clang::Scope* FnBodyScope, clang::Decl* D) {
		isInsideFunctionDef = true;
		return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
	}

	clang::Decl* InsiemeSema::ActOnFinishFunctionBody(clang::Decl* Decl, clang::Stmt* Body) {
		VLOG(2) << "{InsiemeSema}: ActOnFinishFunctionBody()";
		clang::Decl* ret = Sema::ActOnFinishFunctionBody(Decl, std::move(Body));
		// We are sure all the pragmas inside the function body have been matched

		FunctionDecl* FD = dyn_cast<FunctionDecl>(ret);

		if(!FD) { return ret; }

		PragmaList matched;
		std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

		while(I != E && isAfterRange(FD->getSourceRange(), (*I)->getStartLocation(), SourceMgr)) {
			++I;
		}

		while(I != E) {
			unsigned int pragmaEnd = utils::Line((*I)->getEndLocation(), SourceMgr);
			unsigned int declBegin = utils::Line(ret->getSourceRange().getBegin(), SourceMgr);

			if(pragmaEnd <= declBegin) {
				(*I)->setDecl(FD);
				matched.push_back(*I);
			}
			++I;
		}
		EraseMatchedPragmas(pimpl->pending_pragma, matched);
		isInsideFunctionDef = false;

		return ret;
	}

	clang::Decl* InsiemeSema::ActOnDeclarator(clang::Scope* S, clang::Declarator& D) {
		VLOG(2) << "{InsiemeSema}: ActOnDeclarator()";

		clang::Decl* ret = Sema::ActOnDeclarator(S, D);
		if(isInsideFunctionDef) { return ret; }

		PragmaList matched;
		std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

		while(I != E && isAfterRange(ret->getSourceRange(), (*I)->getStartLocation(), SourceMgr)) {
			++I;
		}

		while(I != E) {
			(*I)->setDecl(ret);
			matched.push_back(*I);
			++I;
		}

		EraseMatchedPragmas(pimpl->pending_pragma, matched);
		return ret;
	}

	void InsiemeSema::ActOnTagFinishDefinition(clang::Scope* S, clang::Decl* TagDecl, clang::SourceLocation RBraceLoc) {
		VLOG(2) << "{InsiemeSema}: ActOnTagFinishDefinition()";

		Sema::ActOnTagFinishDefinition(S, TagDecl, RBraceLoc);
		if(isInsideFunctionDef) { return; }

		PragmaList matched;
		std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

		while(I != E && isAfterRange(TagDecl->getSourceRange(), (*I)->getStartLocation(), SourceMgr)) {
			++I;
		}

		while(I != E) {
			(*I)->setDecl(TagDecl);
			matched.push_back(*I);
			++I;
		}
		EraseMatchedPragmas(pimpl->pending_pragma, matched);
	}

	void InsiemeSema::addPragma(PragmaPtr P) {
		pimpl->pragma_list.push_back(P);
		pimpl->pending_pragma.push_back(P);
	}

} // End frontend namespace
} // End insieme namespace

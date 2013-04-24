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

#include "insieme/frontend/sema.h"

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/utils/source_locations.h"

#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/Sema.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Decl.h"
#include "clang/AST/ASTContext.h"

#include "insieme/utils/logging.h"

//FIXME debug
#include "insieme/frontend/pragma/insieme.h"

using namespace clang;
using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::frontend::utils;
using namespace insieme::utils::log;

typedef std::list<PragmaPtr> PendingPragmaList;

namespace {

// It returns true if the source location SL is inside the range defined by SR
bool isInsideRange(SourceRange SR, SourceLocation SL, SourceManager const& sm) {

	//LOG(INFO) << "Is after RANGE(" << Line(SR, sm).first << ", " << Line(SR, sm).second << ")";
	//LOG(INFO) << "Is after RANGE(" << Line(SL, sm) << ")";

	return Line(SR, sm).first <= Line(SL, sm) && Line(SR, sm).second > Line(SL, sm);
}

// It returns true if the source location SL is after the range defined by SR
bool isAfterRange(SourceRange SR, SourceLocation SL, clang::SourceManager const& sm) {
	return Line(SR, sm).second <= Line(SL, sm);
}

void EraseMatchedPragmas(PendingPragmaList& pending, PragmaList& matched) {
	for ( PragmaList::iterator I = matched.begin(), E = matched.end(); I != E; ++I ) {
		std::list<PragmaPtr>::iterator it = std::find(pending.begin(), pending.end(), *I);
		assert(it != pending.end() && "Current matched pragma is not in the list of pending pragmas");
		pending.erase(it);
	}
} // end anonymous namespace 

/**
 * Given a range, the PragmaFilter returns the pragmas with are defined between that range.
 */
class PragmaFilter {
	SourceRange bounds;
	SourceManager const& sm;
	PendingPragmaList::reverse_iterator I, E;

	void inc(bool first) {
		while ( first && I != E && isAfterRange(bounds, (*I)->getStartLocation(), sm) )
			++I;

		if (!first)	++I;
	}

public:
	PragmaFilter(SourceRange const& bounds, SourceManager const& sm, PendingPragmaList& pragma_list) :
		bounds(bounds), sm(sm), I(pragma_list.rbegin()), E(pragma_list.rend()) { inc(true); }

	void operator++() {	inc(false); }

	PragmaPtr operator*() const {
		if ( I == E ) return PragmaPtr();

		if ( isInsideRange(bounds, (*I)->getStartLocation(), sm)) {
			return *I;
		}

		return PragmaPtr();
	}

	bool operator!=(PragmaFilter const& other) const { return I == other.I; }
};

} // End empty namespace

namespace insieme {
namespace frontend {

struct InsiemeSema::InsiemeSemaImpl {
	PragmaList& pragma_list;
	PendingPragmaList pending_pragma;

	InsiemeSemaImpl(PragmaList& pragma_list) :	pragma_list(pragma_list) {	}
};

InsiemeSema::InsiemeSema(
		PragmaList& 					pragma_list, 
		clang::Preprocessor& 			pp, 
		clang::ASTContext& 				ctx,
		clang::ASTConsumer& 			consumer, 
		bool 							CompleteTranslationUnit,
		clang::CodeCompleteConsumer* 	CompletionConsumer) 
:
	clang::Sema(pp, ctx, consumer, clang::TU_Complete, CompletionConsumer),
	pimpl(new InsiemeSemaImpl(pragma_list)),
	isInsideFunctionDef(false) { }

InsiemeSema::~InsiemeSema() { delete pimpl; }

/*
 * The function search for the character c in the input stream backwards. The assumption is the
 * character will be in the input stream so no termination condition is needed.
 */
const char* strbchr(const char* stream, char c) {
	// soon or later we are going to find the char we are looking for, no need for further termination condition
	while ( *stream != c ) {
		stream--;
	}
	return stream;
}

clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R,
												 clang::MultiStmtArg Elts, bool isStmtExpr) {

	// we parse the original code segment, within the original locations
	StmtResult&& ret = Sema::ActOnCompoundStmt(L, R, std::move(Elts), isStmtExpr);
	clang::CompoundStmt* CS = cast<CompoundStmt>(ret.get());

//	std::cout << "{InsiemeSema}: ActOnCompoundStmt()\n";
//	std::cout << "LEFT  { line: " << utils::Line(L, SourceMgr) << " col: " << utils::Line(L,SourceMgr) << std::endl;
//	std::cout << "RIGHT } line: " << utils::Line(R, SourceMgr) << " col: " << utils::Line(R,SourceMgr) << std::endl;


	// FIXME: check if this is actualy needed anymore
	/// when pragmas are just after the beginning of a compound stmt, example:
	/// {
	/// 		#pragma xxx
	/// 		...
	/// }
	/// the location of the opening bracket is wrong because of a bug in the clang parser.
	///
	/// We solve the problem by searching for the bracket in the input stream and overwrite
	/// the value of L (which contains the wrong location) with the correct value.
	
	
	enum {MacroIDBit = 1U << 31}; // from clang/Basic/SourceLocation.h for use with cpp classes
	{
		SourceLocation&& leftBracketLoc = SourceMgr.getImmediateSpellingLoc(L);
		std::pair<FileID, unsigned>&& locInfo = SourceMgr.getDecomposedLoc(leftBracketLoc);
		llvm::StringRef&& buffer = SourceMgr.getBufferData(locInfo.first);
		const char *strData = buffer.begin() + locInfo.second;
		char const* lBracePos = strbchr(strData, '{');

		// We know the location of the left bracket, we overwrite the value of L with the correct location
		// but only if the location is valid as in getFileLocWithOffset() in SourceLocation
		if((((leftBracketLoc.getRawEncoding() & ~MacroIDBit)+(lBracePos - strData)) & MacroIDBit)==0){
			L = leftBracketLoc.getLocWithOffset(lBracePos - strData);
		}
	}
	// the same is done for the right bracket
	{
		SourceLocation&& rightBracketLoc = SourceMgr.getImmediateSpellingLoc(R);
		std::pair<FileID, unsigned>&& locInfo = SourceMgr.getDecomposedLoc(rightBracketLoc);
		llvm::StringRef&& buffer = SourceMgr.getBufferData(locInfo.first);
		const char *strData = buffer.begin() + locInfo.second;
		char const* rBracePos = strbchr(strData, '}');

		// We know the location of the right bracket, we overwrite the value of R with the correct location
		if((((rightBracketLoc.getRawEncoding() & ~MacroIDBit)+(rBracePos - strData)) & MacroIDBit)==0){
			R = rightBracketLoc.getLocWithOffset(rBracePos - strData);
		}
	}

//	std::cout << "corrected LEFT  { line: " << utils::Line(L, SourceMgr) << " col: " << utils::Line(L,SourceMgr) << std::endl;
//	std::cout << "corrected RIGHT } line: " << utils::Line(R, SourceMgr) << " col: " << utils::Line(R,SourceMgr) << std::endl;

	// the source range we inspect is defined by the new source locations,
	// this fix the problem with bonduaries jumping to the begining of the file in 
	// the macro expanisons:
	//
	//	#define F(x) { }
	//
	//		...
	//
	//		F(r)    // <-this statement will jum to the macro location
	//
	PragmaList matched;
	SourceRange SR(L,R);

	// for each of the pragmas in the range between brackets
	for ( PragmaFilter&& filter = PragmaFilter(SR, SourceMgr, pimpl->pending_pragma); *filter; ++filter ) {
		PragmaPtr P = *filter;
		// iterate throug statements of the compound in reverse order 

		unsigned int pragmaStart = utils::Line(P->getStartLocation(), SourceMgr);
		unsigned int pragmaEnd	 = utils::Line(P->getEndLocation(),   SourceMgr);
		unsigned int pragmaSize  = pragmaEnd-pragmaStart +1; // lines pragma uses

//		std::cout << "Match Pragma: " << pragmaStart  << " , " << pragmaEnd << std::endl;

		bool found =false;
		// problem with first pragma, compound start is delayed until fist usable line (first stmt)
		if (CS->size()>0){
			unsigned int lastEnd = (Line((*(CS->body_begin()))->getLocStart(), SourceMgr));
			for (CompoundStmt::body_iterator it = CS->body_begin(); it != CS->body_end(); ++it){

				unsigned int stmtStart = (Line((*it)->getLocStart(), SourceMgr));
				unsigned int stmtEnd   = (Line((*it)->getLocEnd(), SourceMgr));
//				(*it)->dump();
//				std::cout << "lastEnd: " << lastEnd << std::endl;
//				std::cout << "   vs stmt: " << stmtStart  << " -> " << stmtEnd << std::endl;

				if ( (pragmaEnd < stmtStart)){
					// ACHTUNG: if the node is a nullStmt, and is not at the end of the compound (in
					// wich case is most problably ours) we can not trust it. semantics wont change,
					// we move one more. (BUG: nulstmt followed by pragmas, the souce begin is
					// postponed until next stmt) this makes pragmas to be attached to a previous
					// stmt
					if (!llvm::isa<clang::NullStmt>(*it)){
						
						// this pragma is attached to the current stmt
						P->setStatement(*it);
						matched.push_back(P);
						
//						std::cout << " ## attached\n" << std::endl;
						found =true;
						break;
					}
				}

				lastEnd = stmtEnd;
			} 
		}
		if(!found && pragmaStart <= utils::Line(R, SourceMgr)){
			// this is a de-attached pragma (barrier i.e.) at the end of the compound 
			// we need to create a fake NullStmt ( ; ) to attach this
			Stmt** stmts = new Stmt*[CS->size() + 1];

			CompoundStmt* newCS =
					new (Context) CompoundStmt(Context, stmts, CS->size()+1, CS->getSourceRange().getBegin(),
							CS->getSourceRange().getEnd()
						);

			std::copy(CS->body_begin(), CS->body_end(), newCS->body_begin());
			std::for_each(CS->body_begin(), CS->body_end(), [&] (Stmt*& curr) { this->Context.Deallocate(curr); });
			newCS->setLastStmt( new (Context) NullStmt(SourceLocation()) );
			P->setStatement( *newCS->body_rbegin() );
			matched.push_back(P);

			// transfer the ownership of the statement
			CompoundStmt* oldStmt = ret.takeAs<CompoundStmt>();
			oldStmt->setStmts(Context, NULL, 0);
			ret = newCS;
			CS = newCS;

			// destroy the old compound stmt
			Context.Deallocate(oldStmt);
			delete[] stmts;
			
//			std::cout << "### de-attached pragma " << utils::Line(P->getEndLocation(), SourceMgr) << std::endl <<std::endl;
		}
	}
//	std::cout << matched.size()<< " pragmas withing locations" << std::endl << std::endl;

	// remove matched pragmas
	EraseMatchedPragmas(pimpl->pending_pragma, matched);

	return std::move(ret);
}

void InsiemeSema::matchStmt(clang::Stmt* S, const clang::SourceRange& bounds, const clang::SourceManager& sm,
							PragmaList& matched) {
	for ( PragmaFilter filter(bounds, sm,  pimpl->pending_pragma); *filter; ++filter ) {
		PragmaPtr&& P = *filter;
		
		P->setStatement(S);
		matched.push_back(P);
	}
}

clang::StmtResult
InsiemeSema::ActOnIfStmt(clang::SourceLocation IfLoc, clang::Sema::FullExprArg CondVal, clang::Decl* CondVar,
		clang::Stmt* ThenVal, clang::SourceLocation ElseLoc, clang::Stmt* ElseVal) {
	// VLOG(2) << "{InsiemeSema}: ActOnIfStmt()";
	clang::StmtResult ret =
			Sema::ActOnIfStmt(IfLoc, CondVal, CondVar, std::move(ThenVal), ElseLoc, std::move(ElseVal));

	IfStmt* ifStmt = static_cast<IfStmt*>( ret.get() );
	PragmaList matched;

	matchStmt(ifStmt->getThen(), SourceRange(IfLoc, IfLoc), SourceMgr, matched);
	// is there any pragmas to be associated with the 'then' statement of this if?
	if ( !isa<CompoundStmt> (ifStmt->getThen()) ) {
		// if there is no compound stmt, check the then part 
		matchStmt(ifStmt->getThen(), SourceRange(IfLoc, ThenVal->getLocEnd()), SourceMgr, matched);
	}
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	matched.clear();

	// is there any pragmas to be associated with the 'else' statement of this if?
	if ( ifStmt->getElse() && !isa<CompoundStmt> (ifStmt->getElse()) ) {
		matchStmt(ifStmt->getElse(), SourceRange(ElseLoc, ifStmt->getSourceRange().getEnd()), SourceMgr, matched);
	}

	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	return std::move(ret);
}

clang::StmtResult
InsiemeSema::ActOnForStmt(clang::SourceLocation ForLoc, clang::SourceLocation LParenLoc, clang::Stmt* First,
		clang::Sema::FullExprArg Second, clang::Decl* SecondVar, clang::Sema::FullExprArg Third,
		clang::SourceLocation RParenLoc, clang::Stmt* Body) {
	// VLOG(2) << "{InsiemeSema}: ActOnForStmt()" << std::endl;
	clang::StmtResult ret =
		Sema::ActOnForStmt(ForLoc, LParenLoc, std::move(First), Second, SecondVar, Third, RParenLoc, std::move(Body));

	ForStmt* forStmt = (ForStmt*) ret.get();
	PragmaList matched;
	if ( !isa<CompoundStmt> (forStmt->getBody()) ) {
		matchStmt(forStmt->getBody(), forStmt->getSourceRange(), SourceMgr, matched);
	}
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	matched.clear();

	return std::move(ret);
}

clang::Decl* InsiemeSema::ActOnStartOfFunctionDef(clang::Scope *FnBodyScope, clang::Declarator &D) {
	isInsideFunctionDef = true;
	return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
}

clang::Decl* InsiemeSema::ActOnStartOfFunctionDef(clang::Scope *FnBodyScope, clang::Decl* D) {
	isInsideFunctionDef = true;
	return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
}

clang::Decl* InsiemeSema::ActOnFinishFunctionBody(clang::Decl* Decl, clang::Stmt* Body) {
	// VLOG(2) << "{InsiemeSema}: ActOnFinishFunctionBody()";
	clang::Decl* ret = Sema::ActOnFinishFunctionBody(Decl, std::move(Body));
	// We are sure all the pragmas inside the function body have been matched

	FunctionDecl* FD = dyn_cast<FunctionDecl>(ret);

//	std::cout << "\nfunc in: " ;
//	FD->getSourceRange().getBegin().dump(SourceMgr);
//	std::cout << std::endl;

	if (!FD) { return ret; }

	PragmaList matched;
	std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

	while ( I != E && isAfterRange(FD->getSourceRange(), (*I)->getStartLocation(), SourceMgr) ) {
		++I;
	}

	while ( I != E ) {
		unsigned int pragmaEnd	 = utils::Line((*I)->getEndLocation(),   SourceMgr);
		unsigned int declBegin	 = utils::Line(ret->getSourceRange().getBegin(),   SourceMgr);

//		std::cout << "pragma ends: " <<pragmaEnd << std::endl;
//		std::cout << "Decl begins: " <<declBegin << std::endl;

		if(pragmaEnd <=declBegin) {
			(*I)->setDecl(FD);
			matched.push_back(*I);
		}
		++I;
	}
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	isInsideFunctionDef = false;

	return ret;
}

//clang::StmtResult
//InsiemeSema::ActOnDeclStmt(clang::Sema::DeclGroupPtrTy 	Decl,
//						   SourceLocation 				StartLoc,
//						   SourceLocation 				EndLoc)
//{
//	DLOG(INFO) << "{InsiemeSema}: ActOnDeclStmt()";
//	clang::StmtResult ret = Sema::ActOnDeclStmt(Decl, StartLoc, EndLoc);
//	DeclStmt* DS = (DeclStmt*) ret.get();
//
//	DS->dump();
//
//	assert(isa<DeclStmt>(DS));
//
//	PragmaList matched;
//	std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(),
//										 E = pimpl->pending_pragma.rend();
//
//	while(I != E && isAfterRange(DS->getSourceRange(),(*I)->getStartLocation(), SourceMgr))
//		++I;
//
//	while(I != E){
//		(*I)->setStatement(DS);
//		matched.push_back(*I);
//		++I;
//	}
//	EraseMatchedPragmas(pimpl->pragma_list, pimpl->pending_pragma, matched);
//	return std::move(ret);
//}

clang::Decl* InsiemeSema::ActOnDeclarator(clang::Scope *S, clang::Declarator &D) {
	// VLOG(2) << "{InsiemeSema}: ActOnDeclarator()";

	clang::Decl* ret = Sema::ActOnDeclarator(S, D);
	if ( isInsideFunctionDef ) {
		return ret;
	}

//	std::cout << utils::Line(ret->getSourceRange().getBegin(), SourceMgr) << ":" <<
//				  utils::Column(ret->getSourceRange().getBegin(), SourceMgr) << ", " <<
//	  			  utils::Line(ret->getSourceRange().getEnd(), SourceMgr) << ":" <<
//				  utils::Column(ret->getSourceRange().getEnd(), SourceMgr) << std::endl;

	PragmaList matched;
	std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

	while ( I != E && isAfterRange(ret->getSourceRange(), (*I)->getStartLocation(), SourceMgr) ) {
		++I;
	}

	while ( I != E ) {
		(*I)->setDecl(ret);
		matched.push_back(*I);
		++I;
	}

	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	return ret;
}

void InsiemeSema::ActOnTagFinishDefinition(clang::Scope* S, clang::Decl* TagDecl, clang::SourceLocation RBraceLoc) {
	// VLOG(2) << "{InsiemeSema}: ActOnTagFinishDefinition()";

	Sema::ActOnTagFinishDefinition(S, TagDecl, RBraceLoc);
	PragmaList matched;
	std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(), E = pimpl->pending_pragma.rend();

	while ( I != E && isAfterRange(TagDecl->getSourceRange(), (*I)->getStartLocation(), SourceMgr) ) {
		++I;
	}

	while ( I != E ) {
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

void InsiemeSema::dump() {
//Visual Studios 2010 fix: Release mode (without debug) evaluates DLOG(INFO) to "(void) 0"
	//if(VLOG_IS_ON(2)) {
	//	VLOG(2) << "{InsiemeSema}:\nRegistered Pragmas: " << pimpl->pragma_list.size() << std::endl;
	//	std::for_each(pimpl->pragma_list.begin(), pimpl->pragma_list.end(),
	//			[ this ](const PragmaPtr& pragma) { pragma->dump(LOG_STREAM(INFO), this->SourceMgr); }
	//		);
	//}
}

} // End frontend namespace
} // End insieme namespace

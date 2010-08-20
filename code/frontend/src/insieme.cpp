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

#include "insieme.h"

#include "pragma_handler.h"
#include "utils/source_locations.h"

#include "clang/Lex/Preprocessor.h"

#include "clang/Parse/Parser.h"

using namespace clang;

//~~~~~~~~~~~~~~~~~~~~~~~~~~ PRAGMA PROXY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
Expr* ParserProxy::ParseExpression(Preprocessor& PP){
	PP.Lex(mParser->Tok);

	Parser::OwningExprResult ownedResult = mParser->ParseExpression();
	Expr* result = ownedResult.takeAs<Expr>();
	return result;
}

void ParserProxy::EnterTokenStream(Preprocessor& PP){
	// DEBUG(ClangContext::get().getParser()->Tok.getName());
	PP.EnterTokenStream(&(CurrentToken()), 1, true, false);
}

Token& ParserProxy::ConsumeToken(){
	mParser->ConsumeAnyToken();
	// Token token = PP.LookAhead(0);
	// DEBUG(token.getName());
	// if(token.isLiteral())
	//	DEBUG( std::string(token.getLiteralData(),
	//		token.getLiteralData()+token.getLength()) );
	return CurrentToken();
}

clang::Scope* ParserProxy::CurrentScope(){
	return mParser->CurScope;
}

Token& ParserProxy::CurrentToken(){
	return mParser->Tok;
}

struct InsiemeSema::InsiemeSemaImpl{
	PragmaList&					pragma_list;
	std::list<PragmaPtr>		pending_pragma;
	
	InsiemeSemaImpl(PragmaList& pragma_list): pragma_list(pragma_list){ }
};

InsiemeSema::InsiemeSema(PragmaList& 					pragma_list, 
						 clang::Preprocessor&			pp, 
						 clang::ASTContext&				ctxt, 
						 clang::ASTConsumer&			consumer, 
						 bool 							CompleteTranslationUnit, 
						 clang::CodeCompleteConsumer*	CompletionConsumer):
				
	Sema::Sema(pp,ctxt,consumer,CompleteTranslationUnit,CompletionConsumer), 
	pimpl(new InsiemeSemaImpl(pragma_list)), isInsideFunctionDef(false){ }
		
	
bool isInsideRange(clang::SourceRange SR, clang::SourceLocation SL, clang::SourceManager const& sm){
	// DEBUG("(" << sloc::Line(SR).first << ", " << sloc::Line(SR).second << ") <- " << sloc::Line(SL));
	return sloc::Line(SR,sm).first <= sloc::Line(SL,sm) && sloc::Line(SR,sm).second > sloc::Line(SL,sm);
}	

bool isAfterRange(SourceRange SR, SourceLocation SL, clang::SourceManager const& sm){
	return  sloc::Line(SR,sm).second <= sloc::Line(SL,sm);
}	

void EraseMatchedPragmas(std::list<PragmaPtr>& pending, PragmaList& matched){
	for(PragmaList::iterator I = matched.begin(), E = matched.end();
		I != E; ++I)
	{
		std::list<PragmaPtr>::iterator it =
			std::find(pending.begin(), pending.end(), *I);
		assert(it != pending.end());
		pending.erase(it);
	}
}

class PragmaFilter{
	SourceRange bounds;
	SourceManager const& sm;
	std::list<PragmaPtr>::reverse_iterator I, E;
	
	void inc(bool first){
		while(first && I != E && isAfterRange(bounds, (*I)->getStartLocation(), sm))
			++I;
		if(!first) 	++I;
	}
	
public:
	PragmaFilter(SourceRange const& bounds, SourceManager const& sm, std::list<PragmaPtr>& pragma_list):
		bounds(bounds), sm(sm), I( pragma_list.rbegin() ), E( pragma_list.rend() )
	{ inc(true); }
			
	void operator++(){ inc(false); }
	
	PragmaPtr operator*() const{
		if(I == E) return PragmaPtr();
		if(isInsideRange(bounds, (*I)->getStartLocation(), sm))	return *I;
		return PragmaPtr();
	}
	
	bool operator!=(PragmaFilter const& other) const{
		return I == other.I;
	}
};


clang::Sema::OwningStmtResult 
InsiemeSema::ActOnCompoundStmt(SourceLocation 				L, 
							   SourceLocation 				R, 
							   clang::Sema::MultiStmtArg 	Elts, 
							   bool 						isStmtExpr)
{
	// DEBUG("{InsiemeSema}: ActOnCompoundStmt()");
	clang::Sema::OwningStmtResult ret = Sema::ActOnCompoundStmt(L, R, clang::move(Elts), isStmtExpr);
	CompoundStmt* CS = (CompoundStmt*) ret.get();
	Stmt* Prev = NULL;
	
	PragmaList matched;
	
	SourceRange SR(CS->getLBracLoc(), CS->getRBracLoc());
	for(PragmaFilter filter = PragmaFilter(SR, SourceMgr, pimpl->pending_pragma);
		*filter; ++filter)
	{
		PragmaPtr P = *filter;
		for(CompoundStmt::reverse_body_iterator I = CS->body_rbegin(), E = CS->body_rend(); I != E; ){
			Prev = *I;
			++I;
			
			if(I != E && sloc::Line((*I)->getLocStart(), SourceMgr) < sloc::Line(P->getEndLocation(), SourceMgr)){
				if(sloc::Line(Prev->getLocStart(), SourceMgr) >= sloc::Line(P->getEndLocation(), SourceMgr)){
					P->setStatement(Prev);
					matched.push_back(P);
				}else{
					// add a ';'
					Stmt** stmts = new Stmt*[CS->size() + 1];
					
					
					CompoundStmt* newCS = new (Context) CompoundStmt(Context, stmts, CS->size() + 1,
										 CS->getSourceRange().getBegin(), CS->getSourceRange().getEnd());
					
					std::copy(CS->body_begin(), CS->body_end(), newCS->body_begin());
					CompoundStmt::body_iterator it = newCS->body_begin();
					for(size_t i=0; i<CS->size(); ++i, ++it)	;
					*it = new (Context) NullStmt(SourceLocation());
					
					P->setStatement(*it);
					matched.push_back(P);
					
					CompoundStmt* oldStmt = ret.takeAs<CompoundStmt>();
					ret = newCS;
					CS = newCS;
					
					free(oldStmt); // we have to make sure the old CompoundStmt is removed but not
								   // the statements inside it
					delete[] stmts;
				}
				break;
			}
			if(I == E && sloc::Line(Prev->getLocStart(), SourceMgr) > sloc::Line(P->getEndLocation(), SourceMgr)) {
				P->setStatement(Prev);
				matched.push_back(P);
				break;
			}
		}
	}
	
	// remove mathced pragmas
	EraseMatchedPragmas(pimpl->pending_pragma, matched);

	return clang::move(ret);
}

void MatchStmt(Stmt* S, SourceRange bounds, SourceManager const& sm, std::list<PragmaPtr>& pending, PragmaList& matched){
	
	for(PragmaFilter filter = PragmaFilter(bounds, sm, pending);
		*filter; ++filter)
	{
		PragmaPtr P = *filter;
		
		P->setStatement(S);
		matched.push_back(P);
	}
}

clang::Sema::OwningStmtResult 
InsiemeSema::ActOnIfStmt(SourceLocation 			IfLoc, 
						 clang::Sema::FullExprArg 	CondVal, 
						 clang::Sema::DeclPtrTy 	CondVar, 
						 clang::Sema::StmtArg 		ThenVal, 
						 SourceLocation 			ElseLoc, 
						 clang::Sema::StmtArg 		ElseVal)
{
	// DEBUG("{InsiemeSema}: ActOnIfStmt()");
	clang::Sema::OwningStmtResult ret = 
			Sema::ActOnIfStmt(IfLoc,CondVal,CondVar, clang::move(ThenVal), ElseLoc, clang::move(ElseVal));
	
	IfStmt* ifStmt = (IfStmt*) ret.get();
	PragmaList matched;
	
	// is there any pragmas to be associated with the 'then' statement of this if?
	if( !isa<CompoundStmt>(ifStmt->getThen()) )
		MatchStmt(ifStmt->getThen(), SourceRange(IfLoc, ElseLoc), SourceMgr, pimpl->pending_pragma, matched);
	
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	matched.clear();
	
	// is there any pragmas to be associated with the 'else' statement of this if?
	if( ifStmt->getElse() && !isa<CompoundStmt>(ifStmt->getElse()) )
		MatchStmt(ifStmt->getElse(), SourceRange(ElseLoc, ifStmt->getSourceRange().getEnd()), SourceMgr, pimpl->pending_pragma, matched);
	
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	return clang::move(ret);
}

clang::Sema::OwningStmtResult
InsiemeSema::ActOnForStmt(SourceLocation 			ForLoc, 
						 SourceLocation 			LParenLoc, 
						 clang::Sema::StmtArg 		First, 
						 clang::Sema::FullExprArg 	Second, 
						 clang::Sema::DeclPtrTy 	SecondVar, 
						 clang::Sema::FullExprArg 	Third, 
						 SourceLocation 			RParenLoc, 
						 clang::Sema::StmtArg 		Body)
{
	// DEBUG("{InsiemeSema}: ActOnForStmt()");
	clang::Sema::OwningStmtResult ret =
		Sema::ActOnForStmt(ForLoc, LParenLoc, clang::move(First), Second, SecondVar, Third, RParenLoc, clang::move(Body));
	
	ForStmt* forStmt = (ForStmt*) ret.get();
	PragmaList matched;
	if( !isa<CompoundStmt>(forStmt->getBody()) )
		MatchStmt(forStmt->getBody(), forStmt->getSourceRange(), SourceMgr, pimpl->pending_pragma, matched);
	
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	matched.clear();
	
	return clang::move(ret);
}

clang::Sema::DeclPtrTy 
InsiemeSema::ActOnStartOfFunctionDef(Scope *FnBodyScope, Declarator &D){
	isInsideFunctionDef=true;
	return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
}

clang::Sema::DeclPtrTy 
InsiemeSema::ActOnStartOfFunctionDef(Scope *FnBodyScope, DeclPtrTy D){
	isInsideFunctionDef=true;
	return Sema::ActOnStartOfFunctionDef(FnBodyScope, D);
}

clang::Sema::DeclPtrTy 
InsiemeSema::ActOnFinishFunctionBody(clang::Sema::DeclPtrTy 	Decl, 
									 clang::Sema::StmtArg 		Body)
{	
	// DEBUG("{InsiemeSema}: ActOnFinishFunctionBody()");
	DeclPtrTy ret = Sema::ActOnFinishFunctionBody(Decl, clang::move(Body));
	// We are sure all the pragmas inside the function body have been matched
	
	FunctionDecl* FD = ret.getAs<FunctionDecl>();
	assert(isa<FunctionDecl>(FD));
	
	PragmaList matched;
	std::list<PragmaPtr>::reverse_iterator I = pimpl->pending_pragma.rbegin(),
										   E = pimpl->pending_pragma.rend();
	
	while(I != E && isAfterRange(FD->getSourceRange(),(*I)->getStartLocation(), SourceMgr))
		++I;
	
	while(I != E){
		(*I)->setDecl(FD);
		matched.push_back(*I);
		++I;
	}
	EraseMatchedPragmas(pimpl->pending_pragma, matched);
	isInsideFunctionDef=false;
	return ret;
}


//clang::Sema::OwningStmtResult 
//InsiemeSema::ActOnDeclStmt(clang::Sema::DeclGroupPtrTy 	Decl, 
//						   SourceLocation 				StartLoc, 
//						   SourceLocation 				EndLoc)
//{
//	DEBUG("{InsiemeSema}: ActOnDeclStmt()");
//	clang::Sema::OwningStmtResult ret = Sema::ActOnDeclStmt(Decl, StartLoc, EndLoc);
//	DeclStmt* DS = (DeclStmt*) ret.get();
//	
//	DS->dump();
//	
//	assert(isa<DeclStmt>(DS));
//	
//	std::vector<Pragma*> matched;
//	std::list<Pragma*>::reverse_iterator I = pimpl->pending_pragma.rbegin(), 
//										 E = pimpl->pending_pragma.rend();
//	
//	while(I != E && isAfterRange(DS->getSourceRange(),(*I)->getStartLocation()))		
//		++I;
//	
//	while(I != E){
//		(*I)->setStatement(DS);
//		matched.push_back(*I);
//		++I;
//	}
//	EraseMatchedPragmas(pimpl->pending_pragma, matched);
//	return clang::move(ret);
//}

clang::Sema::DeclPtrTy 
InsiemeSema::ActOnDeclarator(Scope *S, Declarator &D){
	// DEBUG("{InsiemeSema}: ActOnDeclarator()");
	clang::Sema::DeclPtrTy ret = Sema::ActOnDeclarator(S,D);
	
	if(isInsideFunctionDef)
		return ret;
	
	if(VarDecl* VD = ret.getAs<VarDecl>()){
		PragmaList matched;
		std::list<PragmaPtr>::reverse_iterator  I = pimpl->pending_pragma.rbegin(),
												E = pimpl->pending_pragma.rend();
	
		while(I != E && isAfterRange(VD->getSourceRange(),(*I)->getStartLocation(), SourceMgr))
			++I;
	
		while(I != E){
			(*I)->setDecl(VD);
			matched.push_back(*I);
			++I;
		}
		EraseMatchedPragmas(pimpl->pending_pragma, matched);
	}
	// VarDecl* = Sema::ActOnDeclarator
	return ret;
}

void InsiemeSema::addPragma(PragmaPtr P){
	pimpl->pragma_list.push_back(P);
	pimpl->pending_pragma.push_back(P);
}

void InsiemeSema::dump(){
//	DEBUG_RUN(
//		DEBUG("{InsiemeSema}:\nRegistered Pragmas: " << pimpl->pragma_list.size());
//		for(PragmaList::iterator I = pimpl->pragma_list.begin(),
//								 E = pimpl->pragma_list.end(); I != E; ++I)
//			(*I)->dump();
//	);
}

void insieme::RegisterPragmaHandlers(Preprocessor& pp){
    pp.AddPragmaHandler("insieme", PragmaHandlerFactory::CreatePragmaHandler<insieme::InlinePragma>(
		"insieme", pp.getIdentifierInfo("inline"),
		!(l_paren_p >>											// 	'!('
		t<tok::numeric_constant>("LEVEL") >> 					//	LEVEL
		r_paren_p) >> eom_p					 					//	')' eom
	));
}

insieme::InlinePragma::InlinePragma(SourceLocation const& startLoc, 
                        			SourceLocation const& endLoc, 
                        			std::string const& 	  name,
                        			MatchMap const& 	  mmap): Pragma(startLoc,endLoc,name,mmap){
    MatchMap::const_iterator it = mmap.find("LEVEL");
    if(it != mmap.end()){
        llvm::APInt level =  dyn_cast<IntegerLiteral>(it->second.back()->take<Stmt*>())->getValue();
        mLevel = (unsigned) *level.getRawData();
     }else
        mLevel = 0;
}
			

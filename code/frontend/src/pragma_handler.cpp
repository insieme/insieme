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

#include "pragma_handler.h"

#include <clang/Lex/Preprocessor.h>
#include "clang/Parse/Parser.h"

using namespace clang;

MatcherResult node::match(Preprocessor& PP){
	MatchMap mmap;
	
	bool ret = match(PP, mmap);
	return std::make_pair(ret, mmap);
}

concat node::operator>>(node const& n){ return concat(*this, n); }

star node::operator*(){ return star(*this); }

choice node::operator|(node const& n){ return choice(*this, n); }

option node::operator!(){ return option(*this); }

bool concat::match(Preprocessor& PP, MatchMap& mmap) const {
	PP.EnableBacktrackAtThisPos();
	if(first->match(PP, mmap) && second->match(PP, mmap)){
		PP.CommitBacktrackedTokens();
		return true;
	}
	PP.Backtrack();
	return false;
}

bool star::match(Preprocessor& PP, MatchMap& mmap) const {
	while(n->match(PP, mmap)) ;
	return true;
}

bool choice::match(Preprocessor& PP, MatchMap& mmap) const {
	PP.EnableBacktrackAtThisPos();
	if(first->match(PP, mmap)){
		PP.CommitBacktrackedTokens();
		return true;
	}
	PP.Backtrack();
	PP.EnableBacktrackAtThisPos();
	if(second->match(PP, mmap)){
		PP.CommitBacktrackedTokens();
		return true;
	}
		
	PP.Backtrack();
	return false;
}

bool option::match(Preprocessor& PP, MatchMap& mmap) const {
	PP.EnableBacktrackAtThisPos();
	if(n->match(PP,mmap)){
		PP.CommitBacktrackedTokens();
		return true;
	}
	PP.Backtrack();
	return true;
}

bool expr::match(Preprocessor& PP, MatchMap& mmap) const {
	// ClangContext::get().getParser()->Tok.setKind(*firstTok);
	PP.EnableBacktrackAtThisPos();
	Expr* result = ParserProxy::get().ParseExpression(PP);
	
	if(result) {
		PP.CommitBacktrackedTokens();
		ParserProxy::get().EnterTokenStream(PP);
		PP.LookAhead(1); // THIS IS CRAZY BUT IT WORKS
		if(map_str.size())
			mmap[map_str].push_back( ValueUnionPtr( new ValueUnion(result) ) );
		return true;
	}
	PP.Backtrack();
	return false;
}

bool kwd::match(Preprocessor& PP, MatchMap& mmap) const {
	if(t<tok::identifier>::match(PP,mmap) && 
		ParserProxy::get().CurrentToken().getIdentifierInfo()->getName() == kw)
	{
		mmap[kw];
		return true;
	}
	return false;
}

void AddToMap(tok::TokenKind 		tok, 
			  Token const& 			token, 
			  std::string const& 	map_str, 
			  MatchMap& 			mmap)
{
	if(!map_str.size())
		return;
	Action& A = ParserProxy::get().getParser()->getActions();
	switch(tok){
		case tok::numeric_constant:
			mmap[map_str].push_back( ValueUnionPtr(
					new ValueUnion(
							A.ActOnNumericConstant(token).takeAs<IntegerLiteral>()
					)
			) );
			break;
		case tok::identifier:
		{
			UnqualifiedId Name;
			CXXScopeSpec ScopeSpec;
			Name.setIdentifier(token.getIdentifierInfo(), token.getLocation());
			
			mmap[map_str].push_back( ValueUnionPtr(
					new ValueUnion(
							A.ActOnIdExpression(
							ParserProxy::get().CurrentScope(),
							ScopeSpec,
							Name,
							false,
							false).takeAs<Stmt>()
					)
			) );
			break;
		}
		default:
		{
			if( token.isLiteral() ){
				mmap[map_str].push_back( ValueUnionPtr(
						new ValueUnion(
								std::string(token.getLiteralData(),
								token.getLiteralData() + token.getLength())
						)
				) );
			}else{
				mmap[map_str].push_back( ValueUnionPtr(
						new ValueUnion(
								std::string(tok::getTokenSimpleSpelling(tok))
						)
				));
			}
			break;
		}
	}
}

std::string Pragma::toString() const {
	std::ostringstream ss;
//	ss << "(" << LOC2S(getStartLocation()) << ", " <<
//				 LOC2S(getEndLocation()) <<
//		  "),\n\t";
//	if(isStatement())
//		ss << "Stmt -> ";
//	else
//		ss << "Decl -> ";
//	ss << "(";
//	if(isStatement() && getStatement())
//		ss << LOC2S(getStatement()->getLocStart()) << ", " <<
//				 LOC2S(getStatement()->getLocEnd());
//	else if(isDecl() && getDecl())
//		ss << LOC2S(getDecl()->getLocStart()) << ", " <<
//				 LOC2S(getDecl()->getLocEnd());
//	ss << ")";
		// << "\n" << PrintClangStmt(getStatement())
	;
	return ss.str();
}

void Pragma::dump() const {
//	DEBUG_RUN(
//		llvm::outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" <<
//						"|~> Pragma: " << getType() << " -> " << toString() << "\n";
//	);
}

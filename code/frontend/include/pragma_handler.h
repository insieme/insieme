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

#include <llvm/ADT/PointerUnion.h>

#include <clang/Basic/SourceLocation.h>
#include <clang/Lex/Pragma.h>
#include <clang/Lex/Token.h>

#include <memory>
#include <sstream>
#include <map>

namespace clang {
class Stmt;
class Decl;
class Expr;
class Preprocessor;
class Scope;
}

class ValueUnion: public llvm::PointerUnion<clang::Stmt*,std::string*> {
	bool ptrOwner;
	
public:
	ValueUnion(clang::Stmt* stmt):
		llvm::PointerUnion<clang::Stmt*,std::string*>(stmt), ptrOwner(true) { }
		
	ValueUnion(std::string const& str): 
		llvm::PointerUnion<clang::Stmt*,std::string*>(new std::string(str)),
		ptrOwner(true) { }
	
	template <class T>
	T take() {
		T ret = get<T>(); 
		if(ret)	ptrOwner = false;
		return ret;
	}
	
	~ValueUnion() {
//		if(ptrOwner && is<clang::Stmt*>())
//			ClangContext::get().getASTContext().Deallocate(get<Stmt*>());
//		if(ptrOwner && is<std::string*>())
//			delete get<std::string*>();
	}
};

typedef std::shared_ptr<ValueUnion> 	ValueUnionPtr;
typedef std::vector< ValueUnionPtr > 	ValueList;

class MatchMap: public std::map<std::string, ValueList> { };
typedef std::pair<bool, MatchMap> 		MatcherResult;

//===------------------------- Pragma ---------------------------===//
// Defines a generic pragma which is defined by its location (start,end) 
// and the associated statement 
class Pragma {
	clang::SourceLocation startLoc;
	clang::SourceLocation endLoc;
	std::string name;
	llvm::PointerUnion<clang::Stmt*,clang::Decl*> ref;
	
public:	
	Pragma(clang::SourceLocation const& startLoc, clang::SourceLocation const& endLoc, std::string const& name):
		startLoc(startLoc), endLoc(endLoc), name(name) { }
			
	Pragma(clang::SourceLocation const& startLoc,
		   clang::SourceLocation const& endLoc,
		   std::string 			 const&	name,
		   MatchMap 			 const& mmap):
		startLoc(startLoc), endLoc(endLoc), name(name) { }
	
	clang::SourceLocation const& getStartLocation() const { return startLoc; }
	clang::SourceLocation const& getEndLocation() const { return endLoc; }
	
	void setStatement(clang::Stmt* stmt) { ref = stmt; }
	void setDecl(clang::Decl* decl) { ref = decl; }
	
	clang::Stmt const* getStatement() const {
		assert(!ref.isNull() && isStatement());
		return ref.get<clang::Stmt*>();
	}
	
	clang::Decl const* getDecl() const {
		assert(!ref.isNull() && isDecl());
		return ref.get<clang::Decl*>();
	}
	
	bool isStatement() const { return ref.is<clang::Stmt*>(); }
	bool isDecl() const { return ref.is<clang::Decl*>(); }
	
	std::string const& getType() const { return name; }
	
	virtual void dump() const;
	std::string toString() const;
	
	virtual ~Pragma() { }
};

class concat;
class star;
class choice;
class option;

template <clang::tok::TokenKind T>
struct t;

struct node{
	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const = 0;
	virtual node* copy() const = 0;
	
	concat 	operator>>(node const& n);
	star 	operator*();
	choice 	operator|(node const& n);
	option  operator!();
	
	MatcherResult match(clang::Preprocessor& PP);
	
	virtual ~node() { }
};

template <class T>
struct val_pair: public node, public std::pair<node*,node*> {
	val_pair(node* n1, node* n2): std::pair<node*, node*>(n1,n2) { }
	
	node* copy() const { return new T(*first, *second); }
	~val_pair() { delete first; delete second; }
};

struct concat: public val_pair<concat> {
	concat(node const& n1, node const& n2): val_pair<concat>::val_pair(n1.copy(), n2.copy()) { }
	
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

struct choice: public val_pair<choice> {
	choice(node const& n1, node const& n2): val_pair<choice>::val_pair(n1.copy(), n2.copy()) { }
	
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

struct option: public node {
	node* n;
	option(node const& n): n( n.copy() ) { }
	
	node* copy() const { return new option(*n); }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
	~option(){ delete n; }
};

struct star: public node {
	node* n;
	
	template <class T>
	star(T const& n): n( n.copy() ) { }
	
	node* copy() const { return new star(*n); }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
	~star(){ delete n; }
};

struct expr: public node {
	std::string map_str;
	
	expr() { }
	expr(std::string const& map_str): map_str(map_str) { }
	node* copy() const { return new expr(map_str); }
	
	expr operator()(std::string const& str) { return expr(str); }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

void AddToMap(clang::tok::TokenKind tok,
			  clang::Token const& 	token,
			  std::string const& 	map_str, 
			  MatchMap& 			mmap);

#include "insieme.h"

template <clang::tok::TokenKind T>
struct t: public node {
	std::string map_str;
	
	t(): map_str() {}
	t(std::string const& map_str): map_str(map_str) { }
		
	node* copy() const { return new t<T>(map_str); }
	
	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const {
		clang::Token& token = ParserProxy::get().ConsumeToken();
		if( token.is(T) ) {
			AddToMap(T, token, map_str, mmap);
			return true;
		}
		return false;
	}
};

struct kwd: public t<clang::tok::identifier> {
	std::string kw;
	
	kwd(std::string const& kw): t<clang::tok::identifier>::t(), kw(kw) { }
	
	node* copy() const { return new kwd(kw); }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};


#define PUNCTUATOR(name, _) \
	static t<clang::tok::name>  name##_p = t<clang::tok::name>();
#define TOK(name) \
	static t<clang::tok::name>  name##_p = t<clang::tok::name>();
#include <clang/Basic/TokenKinds.def>
#undef PUNCTUATOR
#undef TOK
static expr 					expr_p		= expr();

template <class T>
class BasicPragmaHandler: public clang::PragmaHandler {
	std::string base_name;
	node* reg_exp;
	
public:
	BasicPragmaHandler(std::string const& base_name, clang::IdentifierInfo* name, node const& reg_exp):
		PragmaHandler(name), base_name(base_name), reg_exp( reg_exp.copy() ) { }
		
	void HandlePragma(clang::Preprocessor& PP, clang::Token &FirstToken) {
		// DEBUG("PRAGMA HANDLER: " << 
		// 		std::string(getName()->getNameStart(), 
		//					getName()->getNameStart()+getName()->getLength()) );
		// ParserProxy::CurrentToken().getName().setKind(FirstToken);
		
		// '#' symbol is 1 position before
		clang::SourceLocation startLoc =
			ParserProxy::get().CurrentToken().getLocation().getFileLocWithOffset(-1);
		
		MatcherResult MR = reg_exp->match(PP);
		if(MR.first){
			// act on pragma
			// DEBUG(ParserProxy::CurrentToken().getName());
			std::ostringstream pragma_name;
			pragma_name << base_name;
			if(getName())
				pragma_name << "::" << 
					std::string(getName()->getNameStart(), getName()->getNameStart()+getName()->getLength());
			
			clang::SourceLocation endLoc = ParserProxy::get().CurrentToken().getLocation();
//			((InsiemeSema&) ClangContext::get().getParser()->getActions()).
//					ActOnPragma<T>(pragma_name.str(), MR.second, startLoc, endLoc);
		}else{
//			DEBUG("MATCHING FAILED!");
//			PP.DiscardUntilEndOfDirective();
		}
	}
	
	~BasicPragmaHandler() { delete reg_exp; } 
};

struct PragmaHandlerFactory {
	
	template <class T>
	static clang::PragmaHandler* CreatePragmaHandler(const char* base_name, clang::IdentifierInfo* name, node const& re) {
		return new BasicPragmaHandler<T>(base_name, name, re);
	}
};


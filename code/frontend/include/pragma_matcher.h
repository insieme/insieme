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

#include <memory>
#include <string>
#include <vector>
#include <map>

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <clang/Lex/Token.h>

#include <llvm/ADT/PointerUnion.h>

// forward declarations
namespace clang {
class Preprocessor;
class Stmt;
class ASTContext;
}

using clang::tok::TokenKind;

namespace insieme {
namespace frontend {

// forward declarations
class concat;
class star;
class choice;
class option;

class ValueUnion: public llvm::PointerUnion<clang::Stmt*, std::string*> {
	bool ptrOwner;
	clang::ASTContext* clangCtx;

public:
	ValueUnion(clang::Stmt* stmt, clang::ASTContext* ctx) :
		llvm::PointerUnion<clang::Stmt*, std::string*>(stmt), ptrOwner(true), clangCtx(ctx) { }

	ValueUnion(std::string const& str) :
		llvm::PointerUnion<clang::Stmt*, std::string*>(new std::string(str)), ptrOwner(true), clangCtx(NULL) { }

	template<class T>
	T take() {
		T ret = get<T> ();
		if (ret) ptrOwner = false;
		return ret;
	}

	void dump() const;
	~ValueUnion();
};

typedef std::shared_ptr<ValueUnion> ValueUnionPtr;
typedef std::vector<ValueUnionPtr> ValueList;

class MatchMap: public std::map<std::string, ValueList> {
public:
	typedef std::map<std::string, ValueList>::value_type value_type;

	typedef std::map<std::string, ValueList>::key_type key_type;
};
typedef std::pair<bool, MatchMap> MatcherResult;

template<clang::tok::TokenKind T>
struct t;

struct node {
	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const = 0;
	virtual node* copy() const = 0;

	concat operator>>(node const& n) const;
	star operator*() const;
	choice operator|(node const& n) const;
	option operator!() const;

	virtual node& operator()(const std::string& map_name) = 0;
	MatcherResult match(clang::Preprocessor& PP);

	virtual ~node() { }
};

template<class T>
struct val_pair: public node, public std::pair<node*, node*> {
	val_pair(node* n1, node* n2) : std::pair<node*, node*>(n1, n2) { }

	node* copy() const { return new T(*first, *second);	}

	node& operator()(const std::string& map_name) {
		(*first)(map_name);
		(*second)(map_name);
		return *this;
	}

	~val_pair() {
		delete first;
		delete second;
	}
};

struct concat: public val_pair<concat> {
	concat(node const& n1, node const& n2) : val_pair<concat>::val_pair(n1.copy(), n2.copy()) {	}

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

struct choice: public val_pair<choice> {
	choice(node const& n1, node const& n2) : val_pair<choice>::val_pair(n1.copy(), n2.copy()) {	}

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

struct option: public node {
	node* n;
	option(node const& n) : n(n.copy()) { }

	node* copy() const { return new option(*n); }

	node& operator()(const std::string& map_name) {
		(*n)(map_name);
		return *this;
	}

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
	~option() { delete n; }
};

struct star: public node {
	node* n;

	template<class T>
	star(T const& n) :	n(n.copy()) { }

	node* copy() const { return new star(*n); }

	node& operator()(const std::string& map_name) {
		(*n)(map_name);
		return *this;
	}

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
	~star() { delete n;	}
};

struct expr_p: public node {
	std::string map_str;

	expr_p() { }
	expr_p(std::string const& map_str) : 	map_str(map_str) { }

	node* copy() const { return new expr_p(map_str); }
	node& operator()(const std::string& str) {
		map_str = str;
		return *this;
	}

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

void AddToMap(TokenKind tok, clang::Token const& token, std::string const& map_str, MatchMap& mmap);

template<TokenKind T>
struct t: public node {
	std::string map_str;
	bool addToMap;

	t(bool addToMap = true) : map_str(), addToMap(addToMap) { }
	t(std::string const& map_str, bool addToMap = true) : map_str(map_str), addToMap(addToMap) { }

	node& operator()(std::string const& str) { map_str = str; return *this; }

	node* copy() const { return new t<T> (map_str, addToMap); }
	t<T> operator~() const { return t<T>(map_str, false); }

	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const {
		clang::Token& token = ParserProxy::get().ConsumeToken();
		if (token.is(T)) {
			if(addToMap) AddToMap(T, token, map_str, mmap);
			return true;
		}
		return false;
	}
};

struct kwd: public t<clang::tok::identifier> {
	std::string kw;
	std::string map_str;

	kwd(std::string const& kw) : t<clang::tok::identifier>::t(), kw(kw), map_str(kw) { }
	kwd(std::string const& kw, std::string const& map_str) : t<clang::tok::identifier>::t(), kw(kw), map_str(map_str) { }

	node* copy() const { return new kwd(kw, map_str); }

	node& operator()(std::string const& str) { map_str = str; return *this; }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

// import token definitions from clang
namespace tok {
#define PUNCTUATOR(name, _) \
	static t<clang::tok::name>  name = t<clang::tok::name>();
#define TOK(name) \
	static t<clang::tok::name>  name = t<clang::tok::name>();
#include <clang/Basic/TokenKinds.def>
#undef PUNCTUATOR
#undef TOK
static expr_p expr = expr_p();

} // End tok namespace
} // End frontend namespace
} // End insieme namespace

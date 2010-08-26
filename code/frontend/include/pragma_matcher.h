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

namespace insieme {
namespace frontend {

// forward declarations
class concat;
class star;
class choice;
class option;

/**
 * This class is used to keep the
 */
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

	std::string toStr() const;
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
struct tol;

struct node {
	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const = 0;
	virtual node* copy() const = 0;

	concat operator>>(node const& n) const;
	star operator*() const;
	choice operator|(node const& n) const;
	option operator!() const;

	virtual node& operator[](const std::string& map_name) = 0;
	MatcherResult match(clang::Preprocessor& PP);

	virtual ~node() { }
};

template<class T>
class val_single: public node {
	node* n;
public:
	val_single(node* n) : n(n) { }

	node* copy() const { return new T(*n); }

	node& operator[](const std::string& map_name) {
		(*n)[map_name];
		return *this;
	}

	node* getNode() const { return n; }

	~val_single() { delete n; }
};

template<class T>
struct val_pair: public node, public std::pair<node*, node*> {
	val_pair(node* n1, node* n2) : std::pair<node*, node*>(n1, n2) { }

	node* copy() const { return new T(*first, *second);	}

	node& operator[](const std::string& map_name) {
		(*first)[map_name];
		(*second)[map_name];
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

struct option: public val_single<option> {
	option(node const& n): val_single<option>(n.copy()) { }

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

struct star: public val_single<star> {
	star(node const& n) : val_single<star>(n.copy()) { }

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

template <class T>
class MappableNode: public node {
	std::string mapName;
	bool addToMap;

public:
	MappableNode(std::string const& str=std::string(), bool addToMap=true) : mapName(str), addToMap(addToMap) { }

	node& operator[](const std::string& str) {
		mapName = str;
		return *this;
	}

	node* copy() const { return new T( getMapName(), addToMap ); }
	T operator~() const { return T( getMapName(), false); }

	const std::string& getMapName() const { return mapName; }
	bool isAddToMap() const { return addToMap; }
};

struct expr_p: public MappableNode<expr_p> {
	expr_p() { }
	expr_p(std::string const& map_str, bool addToMap=true) : MappableNode<expr_p>(map_str, addToMap) { }

	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

void AddToMap(clang::tok::TokenKind tok, clang::Token const& token, std::string const& map_str, MatchMap& mmap);

template<clang::tok::TokenKind T>
struct Tok: public MappableNode<Tok<T>> {
	Tok() { }
	Tok(std::string const& str, bool addToMap = true) : MappableNode<Tok<T>>(str, addToMap) { }

	virtual bool match(clang::Preprocessor& PP, MatchMap& mmap) const {
		clang::Token& token = ParserProxy::get().ConsumeToken();
		if (token.is(T)) {
			if(MappableNode<Tok<T>>::isAddToMap()) AddToMap(T, token, MappableNode<Tok<T>>::getMapName(), mmap);
			return true;
		}
		return false;
	}
};

struct kwd: public Tok<clang::tok::identifier> {
	std::string kw;

	kwd(std::string const& kw) : Tok<clang::tok::identifier>(), kw(kw) { }
	kwd(std::string const& kw, std::string const& map_str, bool addToMap=true) : Tok<clang::tok::identifier>(map_str, addToMap), kw(kw) { }

	node* copy() const { return new kwd(kw, getMapName(), isAddToMap()); }
	kwd operator~() const { return kwd(kw, getMapName(), false); }
	bool match(clang::Preprocessor& PP, MatchMap& mmap) const;
};

// import token definitions from clang
namespace tok {
#define PUNCTUATOR(name, _) \
	static Tok<clang::tok::name>  name = Tok<clang::tok::name>();
#define TOK(name) \
	static Tok<clang::tok::name>  name = Tok<clang::tok::name>();
#include <clang/Basic/TokenKinds.def>
#undef PUNCTUATOR
#undef TOK
static expr_p expr = expr_p();

} // End tok namespace
} // End frontend namespace
} // End insieme namespace

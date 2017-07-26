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
 */

#pragma once

#include "insieme/frontend/clang.h"
#include "insieme/frontend/compiler.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/printable.h"

#include <memory>
#include <algorithm>
#include <string>
#include <vector>
#include <map>

#include <llvm/ADT/PointerUnion.h>

namespace insieme {
namespace frontend {
namespace pragma {

	// ------------------------------------ ParserStack ---------------------------
	/**
	 * Data structure used for error reporting due to unmatched pragmas.
	 * Choice points generated new records which are allocated in a stack-like data
	 * structure.
	 */
	class ParserStack {
	  public:
		/**
		 * Contains a parsing error encountered during the evaluation of pragmas.
		 * It has a string representing the expecting tokens and the location at
		 * which the error was encountered.
		 */
		struct Error {
			std::string expected;
			clang::SourceLocation loc;

			Error(const std::string& exp, const clang::SourceLocation& loc) : expected(exp), loc(loc) {}
		};

		typedef std::vector<Error> LocErrorList;

		ParserStack() : mRecordId(0) {}

		size_t openRecord();

		void addExpected(size_t recordId, const Error& pe);

		void discardRecord(size_t recordId);

		size_t getFirstValidRecord();

		void discardPrevRecords(size_t recordId);

		const LocErrorList& getRecord(size_t recordId) const;

		size_t stackSize() const {
			return mRecords.size();
		}

	  private:
		size_t mRecordId;
		std::vector<LocErrorList> mRecords;
	};

	/**
	 * Function which create an error report error using the clang Diagnostics utilities.
	 * It reports to std error the location at which the parser found the error and the list
	 * of keywords he was expecting.
	 */
	void errorReport(clang::Preprocessor& pp, clang::SourceLocation& pragmaLoc, ParserStack& errStack);

	// forward declarations
	struct concat;
	struct star;
	struct choice;
	struct option;

	// ------------------------------------ ValueUnion ---------------------------

	/**
	 * This class is used to keep the tokens extracted during the parsing of pragmas.
	 * Two kind of tokens can be stored, strings (which usually results from parsing
	 * of keywords) and clang AST nodes (stmt) which are instead extracted when
	 * identifiers, expressions are parsed.
	 */
	class ValueUnion : public llvm::PointerUnion<clang::Stmt*, std::string*>, public insieme::utils::Printable {
		bool ptrOwner;
		bool isExp;
		clang::ASTContext* clangCtx;

	  public:
		ValueUnion(clang::Stmt* stmt, clang::ASTContext* ctx, bool isExpr = false)
		    : llvm::PointerUnion<clang::Stmt*, std::string*>(stmt), ptrOwner(true), isExp(isExpr), clangCtx(ctx) {}

		ValueUnion(std::string const& str, bool isExpr = false)
		    : llvm::PointerUnion<clang::Stmt*, std::string*>(new std::string(str)), ptrOwner(true), isExp(isExpr), clangCtx(NULL) {}

		ValueUnion(ValueUnion& other, bool transferOwnership = false, bool isExpr = false)
		    : llvm::PointerUnion<clang::Stmt*, std::string*>(other), ptrOwner(true), isExp(isExpr), clangCtx(other.clangCtx) {
			if(transferOwnership) {
				other.ptrOwner = false;
			} else {
				ptrOwner = false;
			}
		}

		/**
		 * A ValueUnion instance always owns the internal value. This method transfer the ownership to the owner.
		 */
		template <class T>
		T take() {
			T ret = get<T>();
			if(ret) { ptrOwner = false; }
			return ret;
		}

		std::ostream& printTo(std::ostream& out) const;

		std::string toStr() const;
		bool isExpr() const;
		bool isString() const {
			return is<std::string*>();
		}
		~ValueUnion();
	};

	typedef std::shared_ptr<ValueUnion> ValueUnionPtr;
	typedef std::vector<ValueUnionPtr> ValueList;

	// forward declarations
	class MatchMap : public std::map<std::string, ValueList>, public insieme::utils::Printable {
	  public:
		typedef std::map<std::string, ValueList>::value_type value_type;
		typedef std::map<std::string, ValueList>::key_type key_type;

		MatchMap() {}
		MatchMap(const MatchMap& other);

		std::ostream& printTo(std::ostream& out) const;
	};

	class MatchObject {
	  private:
		bool initialized;
		typedef std::vector<core::VariablePtr> VarList;
		typedef std::vector<core::ExpressionPtr> ExprList;
		typedef std::vector<std::string> StringList;
		std::map<std::string, VarList> varList;
		std::map<std::string, ExprList> exprList;
		std::map<std::string, StringList> stringList;
		core::VariablePtr getVar(const ValueUnionPtr& p, conversion::Converter& fact);
		core::ExpressionPtr getExpr(const ValueUnionPtr& p, conversion::Converter& fact);
		conversion::Converter* converter;

	  public:
		MatchObject() : initialized(false) {};

		conversion::Converter& getConverter() const { return *converter; }

		const VarList getVars(const std::string& s) const {
			if(varList.find(s) == varList.end()) { return VarList(); }
			return varList.at(s);
		}
		const ExprList getExprs(const std::string& s) const {
			if(exprList.find(s) == exprList.end()) { return ExprList(); }
			return exprList.at(s);
		}

		const core::ExpressionPtr getSingleExpr(const std::string& key) const {
			const auto fitV = getVars(key);
			const auto fitE = getExprs(key);

			if(fitE.empty() && fitV.empty()) { return core::ExpressionPtr(); }

			// we have an expression
			if(fitV.empty()) {
				assert_eq(fitE.size(), 1);
				return fitE.at(0);
			}
			// we have a variable
			if(fitE.empty()) {
				assert_eq(fitV.size(), 1);
				return fitV.at(0);
			}
			assert_fail() << "single (e.g. if, num_threads, ...) pragma element must contain either a variable or an expression.";
			return core::ExpressionPtr();
		}

		const StringList getStrings(const std::string& k) const {
			if(stringList.find(k) == stringList.end()) { return StringList(); }
			return stringList.at(k);
		}
		const std::string getString(const std::string& k) const {
			if(stringList.find(k) == stringList.end()) { return std::string(); }
			assert_eq(getStrings(k).size(), 1) << "Expected StringList to have only one entry, but contents are: " << getStrings(k);
			return getStrings(k).front();
		}

		bool stringValueExists(const std::string& k) const {
			return (stringList.find(k) != stringList.end());
		}

		bool empty() const {
			return (varList.empty() && exprList.empty() && stringList.empty());
		}

		void cloneFromMatchMap(const MatchMap& mmap, conversion::Converter& fact);

		friend std::ostream& operator<<(std::ostream& out, const MatchObject& mo);
	};

	typedef std::pair<bool, MatchMap> MatcherResult;
	template <clang::tok::TokenKind T, bool Resolve>
	struct Tok;

	// ------------------------------------ pragma matcher ---------------------------
	/**
	 * A node is a abstract class representing a generic node of the matching tree
	 * composed to parse a determined pragma.
	 */
	struct node : public insieme::utils::Printable {
		/**
		 * This method consumes token from the input stream (using the clang lexer and tokenizer) and
		 * try to match with the current node. If the read token matches the node definition, true is
		 * returned otherwise false. The MatchMap object contains the map of value derived during the
		 * parsing.
		 */
		virtual bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const = 0;

		virtual node* copy() const = 0;

		/**
		 * The semantics of the >> operator is redefined to implement "followed-by". n1 >> n2 means that
		 * node n1 is followed by node n2.
		 */
		concat operator>>(node const& n) const;
		/**
		 * The semantics of the unary operator * is repetitions. *(n1) means that node n1 can be
		 * repeated from 0 to an infinite amount of times.
		 */
		star operator*() const;
		/**
		 * Operator | implements the choice semantics. n1 | n2 matches only if n1 or n2 is found.
		 */
		choice operator|(node const& n) const;
		/**
		 * The semantics of operator ! is that the following node is optional. !n1 matches either if n1
		 * is found or no tokens are available from the input stream.
		 */
		option operator!() const;

		/**
		 * Each node can be decorated with a name which states to which name the parsed value should be
		 * associated in the outgoing map.
		 */
		virtual node& operator[](const std::string& map_name) = 0;

		bool MatchPragma(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack) {
			return match(PP, mmap, errStack, errStack.openRecord());
		}

		virtual ~node() {}

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * Abstract class representing an unary operator (i.e. !, *).
	 */
	template <class T>
	class val_single : public node {
		// This is the node to which the operator is applied to.
		node* n;

	  public:
		val_single(node* n) : n(n) {}

		node* copy() const {
			return new T(*n);
		}

		node& operator[](const std::string& map_name) {
			(*n)[map_name];
			return *this;
		}

		node* getNode() const {
			return n;
		}

		virtual ~val_single() {
			delete n;
		}
	};

	/**
	 * Abstract class representing a binary operator (i.e. >>, |).
	 */
	template <class T>
	struct val_pair : public node, public std::pair<node*, node*> {
		val_pair(node* n1, node* n2) : std::pair<node*, node*>(n1, n2) {}

		node* copy() const {
			return new T(*first, *second);
		}

		node& operator[](const std::string& map_name) {
			(*first)[map_name];
			(*second)[map_name];
			return *this;
		}

		virtual ~val_pair() {
			delete first;
			delete second;
		}
	};

	/**
	 * Implements the followed-by ('>>') semantics
	 */
	struct concat : public val_pair<concat> {
		concat(node const& n1, node const& n2) : val_pair<concat>::val_pair(n1.copy(), n2.copy()) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * Implements the choice ('|') semantics
	 */
	struct choice : public val_pair<choice> {
		choice(node const& n1, node const& n2) : val_pair<choice>::val_pair(n1.copy(), n2.copy()) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;
	};

	/**
	 * Implements the optional ('!') semantics
	 */
	struct option : public val_single<option> {
		option(node const& n) : val_single<option>(n.copy()) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * Implements the repetition ('*') semantics
	 */
	struct star : public val_single<star> {
		star(node const& n) : val_single<star>(n.copy()) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A MappableNode is a node which, once matched, will be stored in the matcher map. The class owns
	 * the key (mapName) value and a special flag which is used in such cases where a token has to be
	 * excluded from the map (e.g. this is useful for punctuation tokens).
	 */
	template <class T>
	class MappableNode : public node {
		std::string mapName;
		bool toAddToMap;

	  public:
		MappableNode(std::string const& str = std::string(), bool toAddToMap = true) : mapName(str), toAddToMap(toAddToMap) {}

		node& operator[](const std::string& str) {
			mapName = str;
			return *this;
		}

		node* copy() const {
			return new T(getMapName(), toAddToMap);
		}
		/**
		 * The operator '~' is used to say the current node (even if a map key has been assigned) will
		 * never stored in the matcher map.
		 */
		T operator~() const {
			return T(getMapName(), false);
		}

		const std::string& getMapName() const {
			return mapName;
		}
		bool isToBeAddedToMap() const {
			return toAddToMap;
		}
	};

	/**
	 * This node matches an expression, due to the complexity of defining a regular expression which can
	 * map C/C++ expressions, the clang Parser is used directly. NOTICE: a comma separated value list
	 * will be consumed by this node as it is a regular C expression.
	 */
	struct expr_p : public MappableNode<expr_p> {
		expr_p() {}
		expr_p(std::string const& map_str, bool toAddToMap = true) : MappableNode<expr_p>(map_str, toAddToMap) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};
	
	/**
	 * Matches a string literal according to CPP11 standard, even in C files
	 */
	struct cpp_string_lit_p : public MappableNode<cpp_string_lit_p> {
		cpp_string_lit_p() {}
		cpp_string_lit_p(std::string const& map_str, bool toAddToMap = true) : MappableNode<cpp_string_lit_p>(map_str, toAddToMap) {}

		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * Utility function for adding a token with a specific key to the matcher map. Triggering sematic actions while doing so.
	 */
	void resolveAndAddToMap(clang::tok::TokenKind tok, clang::Token const& token, std::string const& map_str, MatchMap& mmap);
	/**
	 * Utility function for adding a token with a specific key to the matcher map. Avoiding semantic actions while doing so.
	 */
	void avoidAndAddToMap(clang::tok::TokenKind tok, clang::Token const& token, std::string const& map_str, MatchMap& mmap);

	std::string TokenToStr(clang::tok::TokenKind tok);
	std::string TokenToStr(const clang::Token& token);

	/**
	 * This class represents a wrapper for clang basic tokens.
	 */
	template <clang::tok::TokenKind T, bool Resolve = false>
	struct Tok : public MappableNode<Tok<T,Resolve>> {
		std::string tok;

		Tok() : tok("undefined") {}
		Tok(std::string const& str, bool toAddToMap = true) : MappableNode<Tok<T,Resolve>>(str, toAddToMap), tok(str) { }

		node* copy() const {
			return new Tok<T,Resolve>(MappableNode<Tok<T,Resolve>>::getMapName(), MappableNode<Tok<T,Resolve>>::isToBeAddedToMap());
		}

		virtual bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const {
			clang::Token& token = ParserProxy::get().ConsumeToken();
			if(token.is(T)) {
				if(MappableNode<Tok<T,Resolve>>::isToBeAddedToMap()) { 
                    if (Resolve) resolveAndAddToMap(T, token, MappableNode<Tok<T,Resolve>>::getMapName(), mmap); 
                    else         avoidAndAddToMap(T, token, MappableNode<Tok<T,Resolve>>::getMapName(), mmap); 
                }
				return true;
			}
			errStack.addExpected(recID, ParserStack::Error("\'" + TokenToStr(T) + "\'", token.getLocation()));
			return false;
		}

	    std::ostream& printTo(std::ostream& out) const {
            if(tok.empty()) {
                return out << clang::tok::getTokenName(T);
            } else {
                return out << "Tok(" << clang::tok::getTokenName(T) << ": " << tok << ")";
            }
        }
	};

	/**
	 * A keyword is a string which is expected to appear in the input stream.
	 */
	struct kwd : public Tok<clang::tok::identifier,false> {
		std::string kw;

		kwd(std::string const& kw) : Tok<clang::tok::identifier,false>(), kw(kw) {}
		kwd(std::string const& kw, std::string const& map_str, bool toAddToMap = true) 
        : Tok<clang::tok::identifier,false>(map_str, toAddToMap), kw(kw) {}

		node* copy() const {
			return new kwd(kw, getMapName(), isToBeAddedToMap());
		}
		kwd operator~() const {
			return kwd(kw, getMapName(), false);
		}
		bool match(clang::Preprocessor& PP, MatchMap& mmap, ParserStack& errStack, size_t recID) const;
		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A var is an identifier which we have to resolve to get the actual variable identifier
	 * This is a hack which has been done to solve the problem with OpenMP regions which receive an
	 * identifier as name and this could be arbitrary
	 */
	struct var_p : public Tok<clang::tok::identifier,true> {
		var_p() : Tok<clang::tok::identifier,true>("", true) {}
        //FIXME: here, true resolve node
		var_p(std::string const& str) : Tok<clang::tok::identifier,true>(str, true) {}

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	// import token definitions from clang
	namespace tok {
	#define PUNCTUATOR(name, _) static Tok<clang::tok::name, false> name = Tok<clang::tok::name, false>();
	#define TOK(name) static Tok<clang::tok::name, false> name = Tok<clang::tok::name, false>();
	#include <clang/Basic/TokenKinds.def>
	#undef PUNCTUATOR
	#undef TOK

		template<class T>
		struct MatchNodeGenerator {
			inline T operator[](const std::string name) {
				return T(name);
			}
			inline operator T() {
				return T();
			}
		};

		__insieme_unused static MatchNodeGenerator<expr_p> expr;
		__insieme_unused static MatchNodeGenerator<var_p> var;
		__insieme_unused static MatchNodeGenerator<cpp_string_lit_p> cpp_string_lit;

	} // End tok namespace
} // End pragma namespace
} // End frontend namespace
} // End insieme namespace

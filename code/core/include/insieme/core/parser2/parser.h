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

#include <algorithm>
#include <exception>
#include <functional>
#include <limits>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/range.h"

#include "insieme/core/parser2/lexer.h"

namespace insieme {
namespace core {
namespace parser {


	class Term;
	typedef std::shared_ptr<Term> TermPtr;

	class Rule;
	typedef std::shared_ptr<Rule> RulePtr;

	class Grammar;

	typedef std::vector<Token> Tokens;
	typedef typename Tokens::const_iterator TokenIter;
	typedef typename utils::range<TokenIter> TokenRange;

	namespace detail {
		class TokenSet;
	}


	class ScopeManager {

		struct Scope {

			// is this scope a fresh scope or a nested one
			bool nested;

			// the currently registered symbols on some level
			vector<std::pair<TokenRange, NodePtr>> symbols;

			Scope(bool nested = false) : nested(nested) {};

			void add(const TokenRange& range, const NodePtr& node) {
				symbols.push_back(std::make_pair(range, node));
			}

			NodePtr lookup(const TokenRange& range) const {
				// search list ...
				for(auto it = symbols.rbegin(); it!=symbols.rend(); ++it) {
					if (it->first == range) return it->second;
				}
				return NodePtr(); // .. nothing found
			}
		};

		// the stack of scopes managed by this manager
		vector<Scope> scopeStack;

	public:

		class Backup {
			unsigned stackSize;
			unsigned topElementSize;
		public:
			Backup(const ScopeManager& manager)
				: stackSize(manager.scopeStack.size()),
				  topElementSize(manager.scopeStack.back().symbols.size()) {}

			void restore(ScopeManager& manager) const {
				// restore size of stack size
				assert(manager.scopeStack.size() >= stackSize);
				while(manager.scopeStack.size() > stackSize) {
					manager.scopeStack.pop_back();
				}

				// restore size of top-element
				auto& symbols = manager.scopeStack.back().symbols;
				assert(symbols.size() >= topElementSize);
				while(symbols.size() > topElementSize) {
					symbols.pop_back();
				}
			}
		};

		ScopeManager() : scopeStack(toVector(Scope())) {}

		void add(const TokenRange& range, const NodePtr& node) {
			scopeStack.back().add(range, node);
		}

		NodePtr lookup(const TokenRange& range) const {
			// search scopes stack top-down
			for(auto it = scopeStack.rbegin(); it!= scopeStack.rend(); ++it) {
				if (NodePtr res = it->lookup(range)) return res;
				if (!it->nested) return NodePtr(); // end of scope nesting
			}
			return NodePtr(); // .. nothing found
		}

		Backup backup() const {
			return Backup(*this);
		}

		void restore(const Backup& backup) {
			backup.restore(*this);
		}

		void pushScope(bool nested = true) {
			scopeStack.push_back(Scope(nested));
		}

		void popScope() {
			assert(!scopeStack.empty());
			scopeStack.pop_back();
		}
	};



	class Context : public IRBuilder {

		bool speculative;

		// the evaluation result of non-terminals
		vector<NodePtr> subTerms;

		// captured sub-ranges
		vector<TokenRange> subRanges;

		// a scope manager for variable names
		std::shared_ptr<ScopeManager> variableScope;

		// a scope manager for type names
		std::shared_ptr<ScopeManager> typeScope;

		// the nesting level of the recursive decent
		unsigned nestingLevel;

		/**
		 * The memorization cache for the non-terminal parser.
		 */
		std::map<pair<string,TokenRange>, NodePtr> mem;

	public:

		NodeManager& manager;
		const Grammar& grammar;
		const TokenIter begin;
		const TokenIter end;

		class Backup {

			bool speculative;

			unsigned termListLength;

			unsigned rangeListLength;

			unsigned nestingLevel;

			ScopeManager::Backup varScopeBackup;

			ScopeManager::Backup typeScopeBackup;

		public:

			Backup(const Context& context)
				: speculative(context.speculative),
				  termListLength(context.subTerms.size()),
				  rangeListLength(context.subRanges.size()),
				  nestingLevel(context.nestingLevel),
				  varScopeBackup(context.variableScope->backup()),
				  typeScopeBackup(context.typeScope->backup()) {}

			void restore(Context& context) const {
				context.speculative = speculative;
				context.nestingLevel = nestingLevel;

				// remove sub-terms
				assert(context.subTerms.size() >= termListLength);
				while(context.subTerms.size() > termListLength) {
					context.subTerms.pop_back();
				}

				// remove sub-ranges
				assert(context.subRanges.size() >= rangeListLength);
				while(context.subRanges.size() > rangeListLength) {
					context.subRanges.pop_back();
				}

				// restore variable scope
				varScopeBackup.restore(*context.variableScope);

				// restore type scope
				typeScopeBackup.restore(*context.typeScope);
			}
		};


		Context(const Grammar& grammar, NodeManager& manager, const TokenIter& begin, const TokenIter& end, bool speculative = true)
			: IRBuilder(manager),
			  speculative(speculative),
			  variableScope(std::make_shared<ScopeManager>()),
			  typeScope(std::make_shared<ScopeManager>()),
			  nestingLevel(0),
			  manager(manager),
			  grammar(grammar),
			  begin(begin),
			  end(end) {}

		Context(const Context& context, const TokenIter& begin, const TokenIter& end)
			: IRBuilder(context.getNodeManager()),
			  speculative(context.speculative),
			  variableScope(context.variableScope),
			  typeScope(context.typeScope),
			  nestingLevel(context.nestingLevel),
			  manager(context.manager),
			  grammar(context.grammar),
			  begin(begin),
			  end(end) {}

		Backup backup() const {
			return Backup(*this);
		}

		void restore(const Backup& backup) {
			backup.restore(*this);
		}

		void push(const NodePtr& node) {
			subTerms.push_back(node);
		}

		void swap(const NodePtr& node) {
			subTerms.back() = node;
		}

		void push(const TokenRange& range) {
			subRanges.push_back(range);
		}

		void popRange() {
			subRanges.pop_back();
		}

		const vector<NodePtr>& getTerms() const {
			return subTerms;
		}

		const NodePtr& getTerm(unsigned index) const {
			assert(index < subTerms.size());
			return subTerms[index];
		}

		const vector<TokenRange>& getSubRanges() const {
			return subRanges;
		}

		const TokenRange& getSubRange(unsigned index) const {
			assert(index < subRanges.size());
			return subRanges[index];
		}

		ScopeManager& getVarScopeManager() {
			return *variableScope;
		}

		ScopeManager& getSymbolManager() {
			return *typeScope;
		}

		void setSpeculative(bool value = true) {
			speculative = value;
		}

		bool isSpeculative() const {
			return speculative;
		}

		void incLevel() { nestingLevel++; }
		void decLevel() { nestingLevel--; }

		unsigned getLevel() const { return nestingLevel; }

		NodePtr lookup(const string& nonTerminal, const TokenRange& range) const {
			auto pos = mem.find(std::make_pair(nonTerminal, range));
			if (pos != mem.end()) {
				return pos->second;
			}
			return NodePtr();
		}

		void store(const string& nonTerminal, const TokenRange& range, const NodePtr& node) {
			mem[std::make_pair(nonTerminal, range)] = node;
		}
	};

	class Result {
		bool success;
		NodePtr res;

	public:

		Result(bool success = false) : success(success) {}

		template<typename NodeType>
		Result(const Pointer<NodeType>& res) : success(res), res(res) {}

		operator bool() const { return success; }
		operator NodePtr() const { return res; }
	};


	class ParseException : public std::exception {

		string msg;

	public:

		ParseException(const string& msg) : msg(msg) {}
		ParseException(const TokenIter& begin, const TokenIter& end);
		virtual ~ParseException() throw() {};

		virtual const char* what() const throw() {
			return msg.c_str();
		}
	};

	// -- Term Constructs --------------------------------------------------------------------------------

	class Limit {

		unsigned min;
		unsigned max;

	public:

		Limit(unsigned min = 0, unsigned max = std::numeric_limits<unsigned>::max())
			: min(min), max(max) {
			assert(min <= max);
		}

		Limit(const Limit& other)
			: min(other.min), max(other.max) {
			assert(min <= max);
		}

		unsigned getMin() const {
			return min;
		}

		unsigned getMax() const {
			return max;
		}

		bool isConstLength() const {
			return min == max;
		}

		bool covers(std::size_t length) const {
			return min <= length && length <= max;
		}

		bool covers(const TokenIter& begin, const TokenIter& end) const {
			return covers(std::distance(begin, end));
		}

		Limit& operator&=(const Limit& other) {
			min = std::max(min, other.min);
			max = std::min(max, other.max);
			assert(min <= max);
			return *this;
		}

		Limit& operator|=(const Limit& other) {
			min = std::min(min, other.min);
			max = std::max(max, other.max);
			assert(min <= max);
			return *this;
		}

		Limit& operator+=(const Limit& other) {
			static auto add = [](unsigned a, unsigned b) {
				unsigned res = a + b;
				return (res >= a) ? res : std::numeric_limits<unsigned>::max();
			};

			min = add(min, other.min);
			max = add(max, other.max);
			assert(min <= max);
			return *this;
		}

		Limit operator&(const Limit& other) const {
			return Limit(*this)&=other;
		}

		Limit operator|(const Limit& other) const {
			return Limit(*this)|=other;
		}

	};

	namespace detail {

		class TokenSet : public utils::Printable {

			/**
			 * Individual tokens covered by this set.
			 */
			vector<Token> tokens;

			/**
			 * A bit-vector representing the mask of token types covered
			 * by this set.
			 */
			unsigned tokenTypeMask;

		public:
			struct all {};

			TokenSet() : tokenTypeMask(0) {};

			template<typename ... Elements>
			TokenSet(const Elements& ... elements)
				: tokens(toVector(elements...)),
				  tokenTypeMask(0) {};

			TokenSet(const Token::Type& type)
				: tokenTypeMask(1<<type) {}

			TokenSet(const all&)
				: tokenTypeMask((1<<16) -1) {}

			bool contains(const Token& token) const {
				return coversType(token) || coversToken(token);
			}

			bool add(const Token& token);
			bool add(const Token::Type& type);
			bool add(const TokenSet& other);

			bool isSubSet(const TokenSet& other) const;

			TokenSet& operator+=(const Token& token);
			TokenSet& operator+=(const Token::Type& type);
			TokenSet& operator+=(const TokenSet& other);

			TokenSet operator+(const TokenSet& other) const {
				return TokenSet(*this) += other;
			}

		protected:

			virtual std::ostream& printTo(std::ostream& out) const;

		private:

			bool coversType(const Token::Type& type) const {
				return (tokenTypeMask & (1<<type));
			}

			bool coversType(const Token& token) const {
				return coversType(token.getType());
			}

			bool coversToken(const Token& token) const {
				return any(tokens, [&](const Token& cur) { return cur==token; });
			}

		};

	}


	/**
	 * A base class for all grammar terms. A term could be a terminal, a non-terminal,
	 * a repetition or an alternative .
	 */
	class Term : public utils::Printable {

		/**
		 * The range limits of this term.
		 */
		Limit range_limit;

	public:

		Term(const Limit& limit = Limit())
			: range_limit(limit) {}

		Term(int min, int max)
			: range_limit(min, max) {}

		virtual ~Term() {}

		Result match(Context& context, const TokenIter& begin, const TokenIter& end) const;

		const Limit& getLimit() const { return range_limit; }
		unsigned getMinRange() const { return range_limit.getMin(); }
		unsigned getMaxRange() const { return range_limit.getMax(); }

		bool isTerminal() const { return range_limit.isConstLength(); }

		/**
		 * Requests this term to update its begin/end token set based on the context defined within
		 * the given grammar.
		 *
		 * @return true if new items have been added, false otherwise
		 */
		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const =0;

		/**
		 * A function to be used for collecting all sub-terms of this term.
		 * The default implementation does not add any terms.
		 */
		virtual void addSubTerms(std::set<TermPtr>& terms) const { };

	protected:

		void setLimit(const Limit& limit) {
			range_limit = limit;
		}

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const =0;
	};


	class Empty : public Term {
	public:

		Empty() : Term(0,0) {}

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const {
			return false; // do not change anything - needs to be managed by context
		}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin == end;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "_e";
		}
	};


	class Terminal : public Term {

		Token terminal;

	public:

		Terminal(const Token& terminal) : Term(1,1), terminal(terminal) {}

		const Token& getTerminal() const { return terminal; }

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end && *begin == terminal;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "'" << terminal.getLexeme() << "'";
		}
	};

	class Any : public Term {

		Token::Type type;

	public:

		Any(Token::Type type = (Token::Type)0) : Term(1,1), type(type) {}

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end && (!type || begin->getType() == type);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!type) return out << "_";
			return out << "_:" << type;
		}
	};


	class NonTerminal : public Term {

		string nonTerminal;

	public:

		NonTerminal(const string& nonTerminal = "E")
			: Term(Limit(1)), nonTerminal(nonTerminal) {}		// TODO: limit those as well - non-terminals are usually recursive => potentially infinite in size but never empty ...

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "<" << nonTerminal << ">";
		}
	};

	namespace detail {
		struct actions {
			void enter(Context& context, const TokenIter& begin, const TokenIter& end) const { }
			void accept(Context& context, const TokenIter& begin, const TokenIter& end) const { }
			void reject(Context& context, const TokenIter& begin, const TokenIter& end) const { }
			void leave(Context& context, const TokenIter& begin, const TokenIter& end) const { }
		};
	}

	template<typename actions>
	class Action : public Term {
		actions action_handler;
		TermPtr subTerm;
	public:
		Action(const TermPtr& term)
			: Term(term->getLimit()), subTerm(term) {}

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const {
			return subTerm->updateTokenSets(g,begin,end);
		}

		virtual void addSubTerms(std::set<TermPtr>& terms) const {
			terms.insert(subTerm);
			subTerm->addSubTerms(terms);
		};

	protected:
		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			// trigger initial action
			action_handler.enter(context, begin, end);
			// match sub-term
			auto res = subTerm->match(context, begin, end);
			// trigger accept / fail actions
			if (res) {
				action_handler.accept(context, begin, end);
			} else {
				action_handler.reject(context, begin, end);
			}
			// trigger leave action
			action_handler.leave(context, begin, end);
			return res;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *subTerm;
		}
	};

	class Sequence : public Term {

		struct SubSequence {
			bool terminal;
			Limit limit;
			vector<TermPtr> terms;

			SubSequence(bool terminal = false)
				: terminal(terminal), limit(0,0) {}

			bool updateStartSet(const Grammar& g, detail::TokenSet& start) const;
			bool updateEndSet(const Grammar& g, detail::TokenSet& end) const;
		};

		vector<SubSequence> sequence;

		bool leftAssociative;

	public:

		Sequence(const vector<TermPtr>& sequence, bool leftAssociative = true)
			: sequence(prepare(sequence)),
			  leftAssociative(leftAssociative) {
			updateLimit();
		}

		template<typename ... Terms>
		Sequence(const Terms& ... terms)
			: sequence(prepare(toVector<TermPtr>(terms...))),
			  leftAssociative(true) {
			updateLimit();
		}

		template<typename ... Terms>
		Sequence(bool leftAssociative, const Terms& ... terms)
			: sequence(prepare(toVector<TermPtr>(terms...))),
			  leftAssociative(leftAssociative) {
			updateLimit();
		}

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

		virtual void addSubTerms(std::set<TermPtr>& terms) const;

		void addTokenPairs(std::map<Token,Token>& map) const;

		bool supportsPair(const pair<Token,Token>& pair) const;

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << join(" ", sequence, [](std::ostream& out, const SubSequence& cur) {
				out << join(" ", cur.terms, print<deref<TermPtr>>());
			});
		}

	private:

		static vector<SubSequence> prepare(const vector<TermPtr>& terms);

		void updateLimit();
	};


	class Alternative : public Term {
		// TODO: improve
		vector<TermPtr> alternatives;

	public:

		Alternative(const vector<TermPtr>& alternatives)
			: alternatives(alternatives) {
			assert(alternatives.size() > 1);
			updateLimit();
		}

		template<typename ... Terms>
		Alternative(const TermPtr& a, const TermPtr& b, const Terms& ... terms)
			: alternatives(toVector(a, b, terms...)) {
			updateLimit();
		}


		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

		virtual void addSubTerms(std::set<TermPtr>& terms) const {
			for(const TermPtr& cur : alternatives) {
				terms.insert(cur); cur->addSubTerms(terms);
			}
		}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "( " << join(" | ", alternatives, print<deref<TermPtr>>()) << " )";
		}

	private:

		void updateLimit() {
			Limit limit = alternatives[0]->getLimit();
			for(const TermPtr& cur : alternatives) limit |= cur->getLimit();
			setLimit(limit);
		}
	};

	class Loop : public Term {

		TermPtr body;

		// the set of tokens forming definite terminal characters
		detail::TokenSet terminator;

	public:

		Loop(const TermPtr& body) : body(body) {}

		template<typename ... Terminators>
		Loop(const TermPtr& body, const Terminators& ... terminators) : body(body), terminator(terminators...) {}

		virtual bool updateTokenSets(const Grammar& g, detail::TokenSet& begin, detail::TokenSet& end) const;

		virtual void addSubTerms(std::set<TermPtr>& terms) const {
			terms.insert(body); body->addSubTerms(terms);
		}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "( " << *body << " )*";
		}
	};

	class Rule : public utils::Printable {
	public:

		typedef std::function<NodePtr(Context& context)> Action;

	private:

		TermPtr pattern;
		Action action;
		int priority;

	public:

		Rule(const TermPtr& pattern, const Action& action, int priority=0)
			: pattern(pattern), action(action), priority(priority) {}

		NodePtr match(Context& context, const TokenIter& begin, const TokenIter& end) const {
			if (pattern->match(context, begin, end)) {
				return action(context);
			}
			return NodePtr();	// no result
		}

		unsigned getPriority() const {
			return priority;
		}

		const TermPtr& getPattern() const {
			return pattern;
		}

		bool operator<(const Rule& other) const {
			// higher priority should be considered first
			if (priority != other.priority) return priority > other.priority;
			return pattern < other.pattern;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *pattern << " => ...";
		}
	};



	class Grammar : public utils::Printable {

	public:

		typedef std::multiset<RulePtr, compare_target<RulePtr>> RuleSet;
		typedef std::map<string, RuleSet> Productions;

		typedef pair<detail::TokenSet, detail::TokenSet> StartEndSets;

		struct TermInfo : public utils::Printable {
			std::map<string, StartEndSets> nonTerminalInfos;
			std::map<TermPtr, StartEndSets> termInfos;

		private:

			// identified pairs of terminals
			std::set<Token> leftParenthese;
			std::set<Token> rightParenthese;
			std::map<Token,Token> parenthesePairs;

		public:

			void addParenthese(const Token& open, const Token& close) {
				leftParenthese.insert(open);
				rightParenthese.insert(close);
				parenthesePairs[open] = close;
			}

			bool isLeftParenthese(const Token& token) const {
				return leftParenthese.find(token) != leftParenthese.end();
			}

			bool isRightParenthese(const Token& token) const {
				return rightParenthese.find(token) != rightParenthese.end();
			}

			const Token& getClosingParenthese(const Token& open) const {
				auto pos = parenthesePairs.find(open);
				assert(pos != parenthesePairs.end());
				return pos->second;
			}

			bool hasParenthesePairs() const {
				return !parenthesePairs.empty();
			}

		protected:
			virtual std::ostream& printTo(std::ostream& out) const;
		};

	private:

		Productions productions;
		string start;

		mutable TermInfo info;
		mutable bool infoValid;

	public:

		Grammar(const string& start = "E") : start(start), infoValid(false) {}

		Grammar(const RulePtr& rule, const string& start = "E")
			: productions(toProductions(start, toVector(rule))), start("E"), infoValid(false) {}

		Grammar(const vector<RulePtr>& rules, const string& start = "E")
			: productions(toProductions(start, rules)), start("E"), infoValid(false) {}

		template<typename ... Rules>
		Grammar(const RulePtr& first, const Rules& ... rest)
			: productions(toProductions("E", toVector(first, rest...))), start("E"), infoValid(false) {}

		Grammar(const string& start, const Productions& productions)
			: productions(productions), start(start), infoValid(false) {}

		void setStartSymbol(const string& start) { this->start = start; }
		const string& getStartSymbol() const { return start; }

		void addRule(const string& symbol, const RulePtr& rule) {
			productions[symbol].insert(rule);
			infoValid = false;
		}

		NodePtr match(NodeManager& manager, const string& code, bool throwOnFail = false) const;

		NodePtr match(Context& context, const TokenIter& begin, const TokenIter& end, const string& nonTerminal) const;

		const TermInfo& getTermInfo() const {
			if (!infoValid) updateTermInfo();
			assert(infoValid);
			return info;
		}

		const StartEndSets& getStartEndSets(const TermPtr& subTerm) const {
			const auto& info = getTermInfo().termInfos;
			assert(info.find(subTerm) != info.end());
			return info.find(subTerm)->second;
		}

		const detail::TokenSet& getStartSet(const TermPtr& subTerm) const {
			return getStartEndSets(subTerm).first;
		}

		const detail::TokenSet& getEndSet(const TermPtr& subTerm) const {
			return getStartEndSets(subTerm).second;
		}

		const StartEndSets& getStartEndSets(const string& nonTerminal) const {
			const auto& info = getTermInfo().nonTerminalInfos;
			assert(info.find(nonTerminal) != info.end());
			return info.find(nonTerminal)->second;
		}

		const detail::TokenSet& getStartSet(const string& nonTerminal) const {
			return getStartEndSets(nonTerminal).first;
		}

		const detail::TokenSet& getEndSet(const string& nonTerminal) const {
			return getStartEndSets(nonTerminal).second;
		}

	protected:

		NodePtr matchInternal(Context& context, const TokenIter& begin, const TokenIter& end, const string& nonTerminal) const;

		virtual std::ostream& printTo(std::ostream& out) const;

	private:

		static Productions toProductions(const string& symbol, const vector<RulePtr>& rules);

		void updateTermInfo() const;

		bool checkParenthese(const TokenIter& begin, const TokenIter& end) const;
	};



	// -- factory functions -----------------------------------------------------------------------------

	extern const TermPtr empty;

	extern const TermPtr identifier;

	inline TermPtr operator|(const TermPtr& a, const TermPtr& b) {
		return std::make_shared<Alternative>(a, b);
	}

	inline TermPtr lit(const Token& token) {
		return std::make_shared<Terminal>(token);
	}

	inline TermPtr lit(const string& str) {
		auto list = lex(str);
		if (list.empty()) return empty;
		if (list.size() == 1u) return lit(list[0]);
		return std::make_shared<Sequence>(::transform(list, (TermPtr(*)(const Token&))&lit));
	}

	inline TermPtr any(Token::Type type = (Token::Type)0) {
		return std::make_shared<Any>(type);
	}

	TermPtr cap(const TermPtr& term);

	TermPtr varScop(const TermPtr& term);

	TermPtr newScop(const TermPtr& term);

	inline TermPtr rec(const string& nonTerminal = "E") {
		return std::make_shared<NonTerminal>(nonTerminal);
	}

	namespace {

		void term_list_helper(vector<TermPtr>& res) { }

		// forward declaration of mutual recursive helpers
		template<typename ... Rest> void term_list_helper(vector<TermPtr>& res, const TermPtr& term, const Rest& ... rest);
		template<typename ... Rest> void term_list_helper(vector<TermPtr>& res, const char* term, const Rest& ... rest);
		template<typename ... Rest> void term_list_helper(vector<TermPtr>& res, const string& term, const Rest& ... rest);

		// implementation
		template<typename ... Rest>
		void term_list_helper(vector<TermPtr>& res, const TermPtr& term, const Rest& ... rest) {
			res.push_back(term); term_list_helper(res, rest...);
		}

		template<typename ... Rest>
		void term_list_helper(vector<TermPtr>& res, const string& term, const Rest& ... rest) {
			res.push_back(lit(term)); term_list_helper(res, rest...);
		}

		template<typename ... Rest>
		void term_list_helper(vector<TermPtr>& res, const char* term, const Rest& ... rest) {
			res.push_back(lit(term)); term_list_helper(res, rest...);
		}

		template<typename ... List>
		vector<TermPtr> toTermList(const List& ... list) {
			vector<TermPtr> res;
			term_list_helper(res, list...);
			return res;
		}
	}

	template<typename ... Terms>
	inline std::shared_ptr<Sequence> seq(const Terms& ... terms) {
		return std::make_shared<Sequence>(toTermList(terms...));
	}

	template<typename ... Terms>
	inline TermPtr seq_r(const Terms& ... terms) {
		return std::make_shared<Sequence>(toTermList(terms...), false);
	}

	inline TermPtr operator<<(const TermPtr& a, const TermPtr& b) {
		return seq(a,b);
	}

	inline TermPtr opt(const TermPtr& term) {
		return std::make_shared<Alternative>(term, empty);
	}

	inline TermPtr opt(const string& term) {
		return opt(lit(term));
	}

	inline TermPtr loop(const TermPtr& body) {
		return std::make_shared<Loop>(body);
	}

	template<typename ... Terminators>
	inline TermPtr loop(const TermPtr& body, const Terminators& ... terminators) {
		return std::make_shared<Loop>(body, terminators...);
	}

	inline TermPtr list(const TermPtr& element, const TermPtr& seperator) {
		return empty | (element << loop(seperator << element));
	}

	inline TermPtr list(const TermPtr& element, const string& seperator) {
		return list(element, lit(seperator));
	}

	inline RulePtr rule(const TermPtr& pattern, const typename Rule::Action& action, int priority = 0) {
		return std::make_shared<Rule>(pattern, action, priority);
	}

	inline RulePtr rule(const string& str, const typename Rule::Action& action, unsigned priority = 10) {
		return rule(lit(str), action, priority);
	}
} // end namespace parser
} // end namespace core
} // end namespace insieme

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
#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace core {
namespace parser {


	class Term;
	typedef std::shared_ptr<Term> TermPtr;

	class Rule;
	typedef std::shared_ptr<Rule> RulePtr;

	class Grammar;

	typedef std::vector<string> Tokens;
	typedef typename Tokens::const_iterator TokenIter;


	class Context {

		bool speculative;

		vector<NodePtr> subTerms;

		// the nesting level of the recursive decent
		unsigned nestingLevel;

		/**
		 * The memorization cache for the non-terminal parser.
		 */
		std::map<pair<TokenIter, TokenIter>, NodePtr> mem;

	public:

		NodeManager& manager;
		const Grammar& grammar;
		const TokenIter begin;
		const TokenIter end;

		class Backup {

			bool speculative;

			// TODO: try just saving length of list
			unsigned termListLength;

			unsigned nestingLevel;

		public:

			Backup(const Context& context)
				: speculative(context.speculative),
				  termListLength(context.subTerms.size()),
				  nestingLevel(context.nestingLevel) {}

			void restore(Context& context) const {
				context.speculative = speculative;
				context.nestingLevel = nestingLevel;

				// remove sub-terms
				assert(context.subTerms.size() >= termListLength);
				while(context.subTerms.size() > termListLength) {
					context.subTerms.pop_back();
				}
			}
		};


		Context(const Grammar& grammar, NodeManager& manager, const TokenIter& begin, const TokenIter& end, bool speculative = true)
			: speculative(speculative), nestingLevel(0), manager(manager), grammar(grammar), begin(begin), end(end) {}

		Context(const Context& context, const TokenIter& begin, const TokenIter& end)
			: speculative(context.speculative), nestingLevel(context.nestingLevel), manager(context.manager), grammar(context.grammar), begin(begin), end(end) {}

		Backup backup() const {
			return Backup(*this);
		}

		void restore(const Backup& backup) {
			backup.restore(*this);
		}

		void push(const NodePtr& node) {
			subTerms.push_back(node);
		}

		const vector<NodePtr>& getTerms() const {
			return subTerms;
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

		NodePtr lookup(const pair<TokenIter, TokenIter>& range) const {
			auto pos = mem.find(range);
			if (pos != mem.end()) {
				return pos->second;
			}
			return NodePtr();
		}

		void store(const pair<TokenIter, TokenIter>& range, const NodePtr& node) {
			mem[range] = node;
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

	protected:

		void setLimit(const Limit& limit) {
			range_limit = limit;
		}

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const =0;
	};


	class Empty : public Term {
	public:

		Empty() : Term(0,0) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin == end;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "_e";
		}
	};


	class Terminal : public Term {

		string terminal;

	public:

		Terminal(const string& terminal) : Term(1,1), terminal(terminal) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end && *begin == terminal;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "'" << terminal << "'";
		}
	};


	class Any : public Term {
	public:

		Any() : Term(1,1) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end;	// accepts just every thing of length 1
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "_";
		}
	};


	class NonTerminal : public Term {
	public:

		NonTerminal() : Term(Limit(1)) {}		// TODO: limit those as well? - non-terminals are usually recursive => potentially infinite in size but never empty ...

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "<E>";
		}
	};


	class Sequence : public Term {

		struct SubSequence {
			bool terminal;
			Limit limit;
			vector<TermPtr> terms;

			SubSequence(bool terminal = false)
				: terminal(terminal), limit(0,0) {}
		};

		vector<SubSequence> sequence;

		bool leftAssociative;

	public:

		Sequence(const vector<TermPtr>& sequence, bool leftAssociative = true)
			: sequence(prepair(sequence)),
			  leftAssociative(leftAssociative) {
			updateLimit();
		}

		template<typename ... Terms>
		Sequence(const Terms& ... terms)
			: sequence(prepair(toVector<TermPtr>(terms...))),
			  leftAssociative(true) {
			updateLimit();
		}

		template<typename ... Terms>
		Sequence(bool leftAssociative, const Terms& ... terms)
			: sequence(prepair(toVector<TermPtr>(terms...))),
			  leftAssociative(leftAssociative) {
			updateLimit();
		}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << join(" ", sequence, [](std::ostream& out, const SubSequence& cur) {
				out << join(" ", cur.terms, print<deref<TermPtr>>());
			});
		}

	private:

		static vector<SubSequence> prepair(const vector<TermPtr>& terms);

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

	public:

		Loop(const TermPtr& body) : body(body) {}

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
		unsigned priority;

	public:

		Rule(const TermPtr& pattern, const Action& action, unsigned priority=10)
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

		bool operator<(const Rule& other) const {
			if (priority != other.priority) return priority < other.priority;
			return pattern < other.pattern;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *pattern << " => ...";
		}
	};


	class Grammar : public utils::Printable {

		std::set<RulePtr> rules;
//		std::map<string, std::set<RulePtr>> head_rules;
//		std::map<string, std::set<RulePtr>> tail_rules;

	public:

		Grammar(const RulePtr& rule)
			: rules(utils::set::toSet<std::set<RulePtr>>(rule)) {}

		Grammar(const vector<RulePtr>& rules)
			: rules(rules.begin(), rules.end()) {}

		template<typename ... Rules>
		Grammar(const Rules& ... rules)
			: rules(utils::set::toSet<std::set<RulePtr>>(rules...)) {}

		NodePtr match(NodeManager& manager, const string& code, bool throwOnFail = false) const;

		NodePtr match(Context& context, const TokenIter& begin, const TokenIter& end) const;

	protected:

		NodePtr matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "E = " << join(" | ", rules, print<deref<RulePtr>>());
		}
	};



	// -- factory functions -----------------------------------------------------------------------------

	extern const TermPtr empty;

	extern const TermPtr any;

	extern const TermPtr rec;

	inline TermPtr operator|(const TermPtr& a, const TermPtr& b) {
		return std::make_shared<Alternative>(a, b);
	}

	inline TermPtr lit(const string& term) {
		return std::make_shared<Terminal>(term);
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
	inline TermPtr seq(const Terms& ... terms) {
		return std::make_shared<Sequence>(toTermList(terms...));
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

	inline TermPtr list(const TermPtr& element, const TermPtr& seperator) {
		return empty | (element << loop(seperator << element));
	}

	inline TermPtr list(const TermPtr& element, const string& seperator) {
		return list(element, lit(seperator));
	}

	inline RulePtr rule(const TermPtr& pattern, const typename Rule::Action& action, unsigned priority = 10) {
		return std::make_shared<Rule>(pattern, action, priority);
	}


} // end namespace parser
} // end namespace core
} // end namespace insieme

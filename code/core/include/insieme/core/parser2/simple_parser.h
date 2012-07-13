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

#include <exception>
#include <string>
#include <memory>
#include <vector>
#include <functional>

#include "insieme/core/ir.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace core {
namespace parser {


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

	public:

		NodeManager& manager;
		const Grammar& grammar;
		const TokenIter begin;
		const TokenIter end;

		class Backup {

			bool speculative;

			// TODO: try just saving length of list
			std::vector<NodePtr> subTerms;

			unsigned nestingLevel;

		public:

			Backup(const Context& context)
				: speculative(context.speculative),
				  subTerms(context.subTerms),
				  nestingLevel(context.nestingLevel) {}

			void restore(Context& context) const {
				context.speculative = speculative;
				context.subTerms = subTerms;
				context.nestingLevel = nestingLevel;
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
	};

	class Result {
		bool success;
		NodePtr res;

	public:

		Result(bool success) : success(success) {}

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

	// -- Rule Constructs --------------------------------------------------------------------------------

	/**
	 * A base class for all rule elements. A rule element could
	 * be a terminal, a recursively nested sub-term, a repetition,
	 * an alternative or a production goal.
	 */
	class Rule : public utils::Printable {

		// the minimum and maximum number of tokens consumed by this rule, -1 if not known
		int min;
		int max;

	public:

		Rule(int min = -1, int max = -1)
			: min(min), max(max) {}

		virtual ~Rule() {}

		Result match(Context& context, const TokenIter& begin, const TokenIter& end) const;

		int getMinRange() const { return min; }
		int getMaxRange() const { return max; }

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const =0;
	};


	class Empty : public Rule {
	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin == end;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "_e";
		}
	};


	class Terminal : public Rule {

		string terminal;

	public:

		Terminal(const string& terminal) : Rule(1,1), terminal(terminal) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end && *begin == terminal;
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "'" << terminal << "'";
		}
	};


	class Any : public Rule {
	public:

		Any() : Rule(1,1) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			return begin + 1 == end;	// accepts just every thing of length 1
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "_";
		}
	};


	class NonTerminal : public Rule {
	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "<E>";
		}
	};


	class Sequence : public Rule {

		vector<RulePtr> sequence;

	public:

		Sequence(const vector<RulePtr>& sequence)
			: sequence(merge(sequence)) {}

		template<typename ... Rules>
		Sequence(const Rules& ... rules)
			: sequence(merge(toVector<RulePtr>(rules...))) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << join(" ", sequence, print<deref<RulePtr>>());
		}

	private:

		static vector<RulePtr> merge(const vector<RulePtr>& terms);
	};


	class Alternative : public Rule {
		// TODO: improve
		vector<RulePtr> alternatives;

	public:

		template<typename ... Rules>
		Alternative(const Rules& ... rules)
			: alternatives(toVector<RulePtr>(rules...)) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "( " << join(" | ", alternatives, print<deref<RulePtr>>()) << " )";
		}
	};

	class Loop : public Rule {

		RulePtr body;

	public:

		Loop(const RulePtr& body) : body(body) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "( " << *body << " )*";
		}
	};

	class Accept : public Rule {
	public:

		typedef std::function<Result(Context& context)> Action;

	private:

		Action action;

	public:

		Accept(const Action& action) : action(action) {}

	protected:

		virtual Result matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
			if (begin!=end) return false;
			return action(context);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "=> ...";
		}
	};


	class Grammar : public utils::Printable {


		RulePtr consumer;

//		RulePtr leftRecursions;

	public:

		Grammar(const RulePtr& rule = std::make_shared<Any>()) : consumer(rule) {}

		NodePtr match(NodeManager& manager, const string& code) const;

		NodePtr match(Context& context, const TokenIter& begin, const TokenIter& end) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "E = " << *consumer;
		}
	};



	// -- factory functions -----------------------------------------------------------------------------

	extern const RulePtr empty;

	extern const RulePtr any;

	extern const RulePtr rec;

	inline RulePtr operator|(const RulePtr& a, const RulePtr& b) {
		return std::make_shared<Alternative>(a, b);
	}

	inline RulePtr lit(const string& term) {
		return std::make_shared<Terminal>(term);
	}

	namespace {

		void term_list_helper(vector<RulePtr>& res) { }

		// forward declaration of mutual recursive helpers
		template<typename ... Rest> void term_list_helper(vector<RulePtr>& res, const RulePtr& term, const Rest& ... rest);
		template<typename ... Rest> void term_list_helper(vector<RulePtr>& res, const char* term, const Rest& ... rest);
		template<typename ... Rest> void term_list_helper(vector<RulePtr>& res, const string& term, const Rest& ... rest);

		// implementation
		template<typename ... Rest>
		void term_list_helper(vector<RulePtr>& res, const RulePtr& term, const Rest& ... rest) {
			res.push_back(term); term_list_helper(res, rest...);
		}

		template<typename ... Rest>
		void term_list_helper(vector<RulePtr>& res, const string& term, const Rest& ... rest) {
			res.push_back(lit(term)); term_list_helper(res, rest...);
		}

		template<typename ... Rest>
		void term_list_helper(vector<RulePtr>& res, const char* term, const Rest& ... rest) {
			res.push_back(lit(term)); term_list_helper(res, rest...);
		}

		template<typename ... List>
		vector<RulePtr> toTermList(const List& ... list) {
			vector<RulePtr> res;
			term_list_helper(res, list...);
			return res;
		}
	}

	template<typename ... Terms>
	inline RulePtr seq(const Terms& ... terms) {
		return std::make_shared<Sequence>(toTermList(terms...));
	}

	inline RulePtr operator<<(const RulePtr& a, const RulePtr& b) {
		return seq(a,b);
	}

	inline RulePtr opt(const RulePtr& term) {
		return std::make_shared<Alternative>(term, empty);
	}

	inline RulePtr opt(const string& term) {
		return opt(lit(term));
	}

	inline RulePtr loop(const RulePtr& body) {
		return std::make_shared<Loop>(body);
	}

	inline RulePtr rule(const RulePtr rule, const typename Accept::Action& action) {
		return rule << std::make_shared<Accept>(action);
	}


} // end namespace parser
} // end namespace core
} // end namespace insieme

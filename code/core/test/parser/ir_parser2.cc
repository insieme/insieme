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

#include <gtest/gtest.h>

#include <string>
#include <vector>
#include <algorithm>

#include <boost/tokenizer.hpp>
#include <boost/optional.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/timer.h"

namespace insieme {
namespace core {

	using std::string;

	typedef string Token;

	struct RuleElement : public string {
		enum Kind {
			Symbol, NonTerminal
		};

		Kind kind;

		RuleElement(const string& name, Kind kind = RuleElement::Symbol)
			: string(name), kind(kind) {}
	};

	RuleElement sym(const string& name) { return RuleElement(name, RuleElement::Symbol); }
	RuleElement var(const string& name) { return RuleElement(name, RuleElement::NonTerminal); }

	struct Rule : public utils::Printable {

		enum Associativity {
			Left, Right
		};


		int priority;
		Associativity associativity;
		vector<RuleElement> pattern;

		Rule() {}

		template<typename ... T>
		Rule(int priority, Associativity assoc, const T& ... elements)
			: priority(priority), associativity(assoc), pattern(toVector(RuleElement(elements)...)) {}


		std::ostream& printTo(std::ostream& out) const {
			return out << join("", pattern);
		}

		bool operator<(const Rule& rule) const {
			return priority < rule.priority;
		}
	};


	struct Term : public utils::Printable {

		int num;

		Rule op;
		vector<Term> args;

		Term(unsigned num)
			: num(num) {}

		Term(const Rule& op, const vector<Term>& args)
			: num(-1), op(op), args(args) {}

		std::ostream& printTo(std::ostream& out) const {
			if (num >= 0) { return out << num; }
			return out << "<" << op << ">(" << join(",", args) << ")";
		}
	};

	// --- Variant A ---

	typedef boost::optional<Term> TermOpt;

	typedef boost::tokenizer< boost::char_separator<char> > Tokenizer;
	typedef Tokenizer::iterator token_iterator;

	typedef vector<RuleElement>::const_iterator rule_iter;

	TermOpt parseRecursive(token_iterator cur, const token_iterator& end, const vector<Rule>& rules);

	bool match(vector<Term>& res, rule_iter cur, const rule_iter& end, token_iterator in_cur, const token_iterator& in_end, const vector<Rule>& rules) {

		// check terminal state
		if (cur == end) {
			return in_cur == in_end; // full string consumed?
		}

		if (in_cur == in_end) {
			return false; 			// no more to consume
		}

		token_iterator next_in = in_cur;
		next_in++;

		// check symbol
		if (cur->kind == RuleElement::Symbol) {
			// there has to be a symbol at this point => check it
			return *cur == *in_cur && match(res, cur+1, end, next_in, in_end, rules);
		}

		// check terminal variable
		assert(cur->kind == RuleElement::NonTerminal);

		// if this is the last element within the rule and a non-terminal ..
		if (cur+1 == end) {
			TermOpt a = parseRecursive(in_cur, in_end, rules);
			if (a) res.push_back(*a);
			return a;
		}

		// search range of current variable
		token_iterator split = next_in;
		while (split != in_end) {
			TermOpt a = parseRecursive(in_cur, split, rules);
			if (a && match(res, cur+1, end, split, in_end, rules)) {
				res.push_back(*a);
				return true;
			}
			split++;
		}

		// this rule can not be matched
		return false;
	}

	// compute returns arguments
	vector<Term> match(const Rule& rule, token_iterator cur, const token_iterator& end, const vector<Rule>& rules) {

		vector<Term> res;
		bool success = match(res, rule.pattern.begin(), rule.pattern.end(), cur, end, rules);
		if (success) {
			std::reverse(res.begin(), res.end());
			return res;
		}
		return vector<Term>(); // empty = no match
	}


	TermOpt parseRecursive(token_iterator cur, const token_iterator& end, const vector<Rule>& rules) {

		token_iterator next = cur; next++;
		if (next == end) {
			try {
				// this is the final token => identity
				Term arg = Term(utils::numeric_cast<unsigned>(*cur));
				return arg;
			} catch (...) {
				return TermOpt();
			}
		}

		// go through rule book and search for a match
		for(auto it = rules.begin(); it != rules.end(); it++) {
			const Rule& rule = *it;
			auto args = match(rule, cur, end, rules);
			if (!args.empty()) {
				return Term(rule, args);
			}
		}

		return TermOpt();

	}


	TermOpt parseA(const string& input, const vector<Rule>& rules) {

		// step 1) start by creating a tokenizer
		boost::char_separator<char> sep(" \t\n");
		Tokenizer tokenizer(input, sep);

		// step 2) parse recursively
		return parseRecursive(tokenizer.begin(), tokenizer.end(), rules);
	}


	// --- Variant B ---

	template<typename I, typename R>
	struct Range : public utils::Printable {
		I begin, end;
		R rbegin, rend;

		template<typename C>
		Range(const C& c)
			: begin(c.begin()), end(c.end()),
			  rbegin(c.rbegin()), rend(c.rend()) {}

		Range(const I& begin, const I& end, const R& rbegin, const R& rend)
			: begin(begin), end(end), rbegin(rbegin), rend(rend) {}

		Range<R,I> reverse() const {
			return Range<R,I>(rbegin, rend, begin, end);
		}

		bool empty() const {
			return begin==end;
		}

		bool single() const {
			return begin+1 == end;
		}

		Range<I,R> subrange(int i) const {
			return subrange(i, size());
		}

		Range<I,R> subrange(int i, int j) const {
			// check for emptiness
			if (i >= j) return Range<I,R>(end,end,rend,rend);
			int k = size() - j;
			return Range<I,R>(begin + i, end - k, rbegin + k, rend - i);
		}

		Range<I,R> operator+(int i) const {
			return Range<I,R>(begin+i, end, rbegin, rend-i);
		}

		std::size_t size() const { return std::distance(begin, end); }

		std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",", begin, end) << "]";
		}
	};

	template<typename C>
	Range<typename C::const_iterator, typename C::const_reverse_iterator> range(const C& c) {
		return Range<typename C::const_iterator, typename C::const_reverse_iterator>(c);
	}

	template<typename FI, typename RI>
	TermOpt parseRecursiveB(const Range<FI,RI>& token, const vector<Rule>& rules);

//	template<typename FIP, typename BIP, typename FIT, typename BIT>
//	bool matchB(vector<Term>& res, const Range<FIP,BIP>& pattern, const Range<FIT,BIT>& token, const vector<Rule>& rules) {
//
//		// check terminal state
//		if (pattern.empty()) {
//			return token.empty(); // full string consumed?
//		}
//
//		if (token.empty()) {
//			return false; 			// no more to consume
//		}
//
//		// check symbol
//		if (pattern.begin->kind == RuleElement::Symbol) {
//			// there has to be a symbol at this point => check it
//			return *pattern.begin == *token.begin && matchB(res, pattern+1, token+1, rules);
//		}
//
//		// check terminal variable
//		assert(pattern.begin->kind == RuleElement::NonTerminal);
//
//		// if this is the last element within the rule and a non-terminal ..
//		if (pattern.single()) {
//			TermOpt a = parseRecursiveB(token, rules);
//			if (a) res.push_back(*a);
//			return a;
//		}
//
//		// search range of current variable
//		unsigned size = token.size();
//		for(unsigned i=1; i<size; i++) {
//
//			Range<FIT,BIT> partA = (token.reverse() + (size - i)).reverse();
//			Range<FIT,BIT> partB = token + i;
//
//			TermOpt a = parseRecursiveB(partA, rules);
//			if (a && matchB(res, pattern+1, partB, rules)) {
//				res.push_back(*a);
//				return true;
//			}
//		}
//
//		// this rule can not be matched
//		return false;
//
//	}


	// --- improved matcher ---

	template<typename FIP, typename BIP, typename FIT, typename BIT>
	bool matchVarOnly(vector<Term>& res, const Range<FIP,BIP>& pattern, const Range<FIT,BIT>& token, const vector<Rule>& rules) {

		// check terminal state
		if (pattern.empty()) {
			return token.empty(); // full string consumed?
		}

		if (token.empty()) {
			return false; 			// no more to consume
		}

		// check terminal variable
		assert(pattern.begin->kind == RuleElement::NonTerminal);

		// if this is the last element within the rule and a non-terminal ..
		if (pattern.single()) {
			TermOpt a = parseRecursiveB(token, rules);
			if (a) res.push_back(*a);
			return a;
		}

		// search range of current variable
		unsigned size = token.size();
		for(unsigned i=1; i<size; i++) {

			Range<FIT,BIT> partA = (token.reverse() + (size - i)).reverse();
			Range<FIT,BIT> partB = token + i;

			TermOpt a = parseRecursiveB(partA, rules);
			if (a && matchVarOnly(res, pattern+1, partB, rules)) {
				res.push_back(*a);
				return true;
			}
		}

		// this rule can not be matched
		return false;
	}

	template<typename FIP, typename BIP, typename FIT, typename BIT>
	bool matchHelperB(vector<Term>& res, const Range<FIP,BIP>& pattern, const Range<FIT,BIT>& token, const vector<Rule>& rules) {

		auto begin = pattern.begin;
		auto end = pattern.end;

		// find first symbol in pattern
		int pos = 0;
		while(begin != end && begin->kind != RuleElement::Symbol) { begin++; pos++; }

		if (begin == end) {
			// no more symbols => use variable only matcher
			return matchVarOnly(res, pattern, token, rules);
		}

		// partition pattern
		auto symbol = *begin;
		auto before = pattern.subrange(0,pos);
		auto after = pattern.subrange(pos+1);

		// now search for symbol within tokens
		pos = 0;
		auto t_begin = token.begin;
		auto t_end = token.end;

		while(true) {
			// find next corresponding symbol
			while(t_begin != t_end && *t_begin != symbol) { t_begin++; pos++; }

			// check whether the requested symbol has been found
			if (t_begin == t_end) { return false; }

			// check whether left side is matching
			vector<Term> copy = res;
			if (matchVarOnly(copy, before, token.subrange(0, pos), rules)						// left hand side is a match
					&& matchHelperB(copy, after, token.subrange(pos+1), rules)) {		// right hand side is a match
				res = copy;
				return true;
			}

			// try next element
			t_begin++; pos++;
		}

	}

	template<typename FIP, typename BIP, typename FIT, typename BIT>
	bool matchB(vector<Term>& res, const Range<FIP,BIP>& pattern, const Range<FIT,BIT>& token, const vector<Rule>& rules) {
		return matchHelperB(res, pattern, token, rules);
	}

	template<typename FI, typename RI>
	vector<Term> matchB(const Rule& rule, const Range<FI,RI>& token, const vector<Rule>& rules) {

		auto pattern = range(rule.pattern);

		vector<Term> res;
		bool success = matchB(res, pattern, token, rules);
		if (success) {
			return res;
		}
		return vector<Term>(); // empty = no match
	}

	template<typename FI, typename RI>
	TermOpt parseRecursiveB(const Range<FI,RI>& token, const vector<Rule>& rules) {

		if (token.single()) {
			try {
				// this is the final token => identity
				Term arg = Term(utils::numeric_cast<unsigned>(*token.begin));
				return arg;
			} catch (...) {
				return TermOpt();
			}
		}

		// go through rule book and search for a match
		for(auto it = rules.begin(); it != rules.end(); it++) {
			const Rule& rule = *it;
			auto args = matchB(rule, token, rules);
			if (!args.empty()) {
				return Term(rule, args);
			}
		}

		return TermOpt();

	}

	TermOpt parseB(const string& input, const vector<Rule>& rules) {

		// step 1) start by creating a tokenizer
		boost::char_separator<char> sep(" \t\n");
		Tokenizer tokenizer(input, sep);
		vector<string> tokens(tokenizer.begin(), tokenizer.end());

		// step 2) parse recursively
		return parseRecursiveB(range(tokens), rules);
	}

	TEST(IrParser, Range) {

		auto list = toVector(1,2,3,4,5,6);

		auto r = range(list);
		EXPECT_EQ("[1,2,3,4,5,6]", toString(r));

		EXPECT_EQ("[2]", toString(r.subrange(1,2)));
		EXPECT_EQ("[2,3]", toString(r.subrange(1,3)));
		EXPECT_EQ("[]", toString(r.subrange(7,3)));

	}


	TEST(IrParser, Tryout) {

		// goal: parse 1 + 2 * 3 * 4 correctly

		Rule deref(9, Rule::Left, "*", var("a"));
		Rule add(6, Rule::Left, var("a"), "+", var("b"));
		Rule mul(8, Rule::Left, var("a"), "*", var("b"));

		// multiple symbols
		Rule access(10, Rule::Left, var("a"), "[", var("b"), "]");

		EXPECT_EQ("*a", toString(deref));
		EXPECT_EQ("a+b", toString(add));
		EXPECT_EQ("a*b", toString(mul));
		EXPECT_EQ("a[b]", toString(access));

		auto rules = toVector(deref, add, mul, access);
		std::sort(rules.begin(), rules.end());			// sort according to priority - weakest first


		// ---- test variant A ---

		// start with something simple
		EXPECT_EQ("1", toString(parseA("1", rules)));

		// slightly more complex
		EXPECT_EQ("<a+b>(1,2)", toString(*parseA("1 + 2", rules)));

		// even more complex
		EXPECT_EQ("<a+b>(1,<a*b>(2,3))", toString(*parseA("1 + 2 * 3", rules)));
		EXPECT_EQ("<a+b>(<a*b>(1,2),3)", toString(*parseA("1 * 2 + 3", rules)));

		// finally: the big one!
		EXPECT_EQ("<a+b>(1,<a*b>(2,<a*b>(3,4)))", toString(*parseA("1 + 2 * 3 * 4", rules)));

		// an operator including multiple symbols
		EXPECT_EQ("<a+b>(1,<a*b>(<a[b]>(2,<a+b>(<a*b>(3,4),1)),2))", toString(*parseA("1 + 2 [ 3 * 4 + 1 ] * 2", rules)));

		// ---- test variant B ---

		// start with something simple
		EXPECT_EQ("1", toString(parseB("1", rules)));

		// slightly more complex
		EXPECT_EQ("<a+b>(1,2)", toString(*parseB("1 + 2", rules)));

		// even more complex
		EXPECT_EQ("<a+b>(1,<a*b>(2,3))", toString(*parseB("1 + 2 * 3", rules)));
		EXPECT_EQ("<a+b>(<a*b>(1,2),3)", toString(*parseB("1 * 2 + 3", rules)));

		// finally: the big one!
		EXPECT_EQ("<a+b>(1,<a*b>(2,<a*b>(3,4)))", toString(*parseB("1 + 2 * 3 * 4", rules)));

		// an operator including multiple symbols
		EXPECT_EQ("<a+b>(1,<a*b>(<a[b]>(2,<a+b>(<a*b>(3,4),1)),2))", toString(*parseB("1 + 2 [ 3 * 4 + 1 ] * 2", rules)));


		// ---- speed comparision ---

		double time;
		TermOpt a, b;

		auto code = "1 + 2 [ 3 * 4 + 1 ] * 2 + 1 + 2 [ 3 * 4 + 1 ] * 2 * 1 + 2 [ 3 * 4 + 1 ] * 2";
//		auto code = "1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1";

		// something complex to eval execution time
		time = TIME(a = parseA(code, rules));
		std::cout << "Variant A - Parsing took: " << time << "sec\n";

		// something complex to eval execution time
		time = TIME(b = parseB(code, rules));
		std::cout << "Variant B - Parsing took: " << time << "sec\n";

		EXPECT_EQ(toString(*a), toString(*b));

		std::cout << toString(*a);
	}

} // end namespace core
} // end namespace insieme

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

#include "insieme/core/parser2/parser.h"

#include <sstream>
#include <iterator>
#include <boost/tokenizer.hpp>

namespace insieme {
namespace core {
namespace parser {


	namespace {

		Result fail(Context& context, const TokenIter& begin, const TokenIter& end) throw(ParseException) {
			if (!context.isSpeculative())
				throw ParseException(begin, end);
			return false;
		}

		string buildErrorMessage(const TokenIter& begin, const TokenIter& end) {
			std::stringstream out;
			out << "Unable to parse token sequence \""
					<< join(" ", begin, end) << "\"";
			return out.str();
		}

	}


	ParseException::ParseException(const TokenIter& begin, const TokenIter& end)
		: msg(buildErrorMessage(begin, end)){}


	// -- Term Implementations -------------


	Result Term::match(Context& context, const TokenIter& begin, const TokenIter& end) const {

		if (true) {

			// -- non-debug version --
			if (!range_limit.covers(begin,end)) {
				return fail(context, begin, end);
			}
			return matchInternal(context, begin, end);

		} else {

			// -- debug version --

			// print debug infos
			auto offset = times("  ", context.getLevel());
			std::cout << offset << "Try matching " << *this << " against " << join(" ", begin, end) << "\n";

			// check constraints on number of tokens
			if (!range_limit.covers(begin,end)) {
				std::cout << offset << "Match skipped => range limit\n";
				return fail(context, begin, end);
			}

			assert(begin <= end);

			context.incLevel();
			auto res = matchInternal(context, begin, end);
			context.decLevel();

			// debug infos ...
			std::cout << offset << "Match result: " << ((res)?"OK":"Failed") << " - context: " << context.getTerms() << "\n";

			return res;

		}

	}


	Result NonTerminal::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		NodePtr res = context.grammar.match(context, begin, end);
		if (res) {
			context.push(res);
			return res;
		}
		return fail(context,begin,end);
	}

	namespace {

		template<typename Iter>
		struct Range { // : public utils::Printable {
			Iter begin;
			Iter end;

			typedef typename std::iterator_traits<Iter>::value_type value_type;

			Range(const Iter& begin, const Iter& end)
				: begin(begin), end(end) {
				assert(begin <= end);
			}

			bool empty() const {
				return begin==end;
			}

			bool single() const {
				return begin+1 == end;
			}

			Range<Iter> subrange(int i) const {
				return subrange(i, size());
			}

			Range<Iter> subrange(int i, int j) const {
				// check for emptiness
				if (i >= j) return Range<Iter>(end,end);
				int k = size() - j;
				return Range<Iter>(begin + i, end - k);
			}

			const value_type& operator[](unsigned i) const {
				assert(i < size());
				return *(begin + i);
			}

			const value_type& first() const {
				return *begin;
			}

			const value_type& last() const {
				return *(end-1);
			}

			Range<Iter>& operator+=(unsigned i) {
				begin += i; return *this;
			}

			Range<Iter>& operator-=(unsigned i) {
				end -= i; return *this;
			}

			Range<Iter> operator+(unsigned i) const {
				return Range<Iter>(*this) += i;
			}

			Range<Iter> operator-(unsigned i) const {
				return Range<Iter>(*this) -= i;
			}

			std::size_t size() const {
				return std::distance(begin, end);
			}

//			std::ostream& printTo(std::ostream& out) const {
//				return out << "[" << join(",", begin, end) << "]";
//			}
		};

		template<typename C>
		Range<typename C::const_iterator> range(const C& c) {
			return Range<typename C::const_iterator>(c.begin(), c.end());
		}

		template<typename Iter>
		Range<Iter> range(const Iter& begin, const Iter& end) {
			return Range<Iter>(begin, end);
		}

		typedef typename vector<TermPtr>::const_iterator TermIter;
		typedef typename vector<Sequence::SubSequence>::const_iterator SubSeqIter;

		struct SubRange {
			Range<TermIter> pattern;
			Range<TokenIter> tokens;

			SubRange(const Range<TermIter>& pattern, const Range<TokenIter>& tokens)
				: pattern(pattern), tokens(tokens) {}
		};

		Result matchVarOnly(Context& context, const Range<TermIter>& pattern, const Range<TokenIter>& tokens) {

			// check terminal state
			if (pattern.empty()) {
				return tokens.empty(); // full string consumed?
			}

			// check that head is a non-terminal entry and not an epsilon
			assert(!(*pattern.begin)->isTerminal());
			assert(!dynamic_pointer_cast<Empty>(*pattern.begin));
			auto curVar = *pattern.begin;

			// if the pattern is only a single variable (or the last within the recursive processing) ...
			if (pattern.single()) {
				return curVar->match(context, tokens.begin, tokens.end);
			}

			// compute boundaries for length of token stream to by covered by the first variable
			unsigned size = tokens.size();
			int min = curVar->getMinRange();
			int max = curVar->getMaxRange();
			min = (min < 0)?0:min;
			max = (max < 0)?size:max;

			// guess end of current variable and process recursively
			auto backup = context.backup();
			for(int i=min; i<=max; i++) {
				backup.restore(context);

				Range<TokenIter> partA(tokens.begin, tokens.begin + i);
				Range<TokenIter> partB(tokens.begin + i, tokens.end);

				// start by trying to match first variable (head)
				Result a = curVar->match(context, partA.begin, partA.end);
				if (!a) { continue;	} // try next split point

				// continue recursively with the rest of the variables
				Result b = matchVarOnly(context, pattern+1, partB);
				if (b) { return b; }
			}

			// this term can not be matched
			return fail(context, tokens.begin, tokens.end);
		}

		Result matchSubRanges(Context& context, const vector<SubRange>& ranges) {

			// process sub-ranges
			Result res;
			auto backup = context.backup();
			for(const SubRange& cur : ranges) {
				res = matchVarOnly(context, cur.pattern, cur.tokens);
				if (!res) {
					backup.restore(context);
					return fail(context, cur.tokens.begin, cur.tokens.end);
				}
			}

			// return last result
			return res;
		}

		Result matchInfixSequence(Context& context, const Range<SubSeqIter>& sequence, const Range<TokenIter>& tokens, bool leftAssociative, vector<SubRange>& ranges) {

			// some pre-condition (checking that sequence is an infix sequence)
			assert(!sequence.empty());
			assert(sequence.size() % 2 == 1); 			// uneven number of entries for infix
			assert(!(sequence.begin->terminal));
			assert(!((sequence.end-1)->terminal));

			// -- recursively build up sub-range list --

			// terminal case (last non-terminal sequence)
			if (sequence.single()) {
				// add final sub-range (has to match all the rest)
				ranges.push_back(SubRange(range(sequence.begin->terms), tokens));

				// try solving individual sub-ranges
				auto res = matchSubRanges(context, ranges);

				// remove final su-range entry
				ranges.pop_back();
				return res;
			}


			// find match for next terminal
			auto& terminal = sequence[1];
			assert(terminal.terminal);

			unsigned terminalSize = terminal.limit.getMin();
			assert(terminalSize == terminal.limit.getMax());

			// check for sufficient token stream size
			if (tokens.size() < terminalSize) {
				return fail(context, tokens.begin, tokens.end);
			}

			// limit range of search space
			unsigned min = sequence[0].limit.getMin();
			unsigned max = std::min(sequence[0].limit.getMax(), (unsigned)(tokens.size()-terminalSize))+1;
			unsigned inc = 1;

			// walk in the other direction if left associative
			if (leftAssociative) {
				max--; min--;
				unsigned h = min; min = max; max = h;
				min--;			// extra shift since upper boundary not really to be tested!
				inc = -1;
			}

			// search within potential scope
			for(unsigned i = min; i!= max; i+=inc) {

				// check terminal at corresponding position
				TokenIter pos = tokens.begin + i;
				bool fit = true;
				for(const TermPtr& cur : terminal.terms) {
					fit = fit && cur->match(context, pos, pos+1);
					pos++;
				}

				// no match at this position
				if (!fit) continue;

				// recursively match remaining terminals
				ranges.push_back(SubRange(range(sequence.begin->terms), tokens.subrange(0,i)));
				Result res = matchInfixSequence(context, sequence+2, tokens.subrange(i+terminalSize), leftAssociative, ranges);

				// check result => if OK, done
				if (res) { return res; }

				// continue search
				ranges.pop_back();
			}

			// no matching assignment found
			return fail(context, tokens.begin, tokens.end);

		}


	}

	Result Sequence::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {

		auto pattern = range(sequence);
		auto token = range(begin, end);

		// -- special cases --

		// check empty sequences
		if (pattern.empty()) {
			return token.empty();
		}

		// check whether token stream is empty (min range will be > 0 if there are any terminals)
		if (getMinRange() > 0 && token.empty()) {
			return fail(context, begin, end);
		}

		// Initial terminals:
		if (pattern.begin->terminal) {
			const SubSequence& cur = pattern[0];

			// match initial terminals
			for(const TermPtr& rule : cur.terms) {
				if(token.empty() || !rule->match(context, token.begin, token.begin+1)) {
					return fail(context, token.begin, token.begin+1);
				}
				token+=1;
			}

			// prune pattern
			pattern+=1;
		}
//std::cout << "Remaining Pattern: " << join(" ", pattern.begin, pattern.end, [](std::ostream& out, const SubSequence& cur){
//	out << join(" ", cur.terms, print<deref<TermPtr>>());
//}) << "\n";
		// if it is only a sequence of terminals => done!
		if (pattern.empty()) {
			return token.empty();
		}

		// Tailing terminals:
		if ((pattern.end-1)->terminal) {
			const SubSequence& cur = *(pattern.end-1);

			// match tailing terminals
			for(auto it = cur.terms.rbegin(); it != cur.terms.rend(); ++it) {
				const TermPtr& rule = *it;
				if(token.empty() || !rule->match(context, token.end-1, token.end)) {
					return fail(context, token.end-1, token.end);
				}
				token-=1;
			}

			// prune pattern
			pattern-=1;
		}

		// Now there should only be an infix pattern (head and tail is non-terminal)
		assert(!pattern.empty());
		assert(pattern.size() % 2 == 1);
		assert(!pattern.begin->terminal);
		assert(!(pattern.end-1)->terminal);

		// use recursive matching algorithm
		vector<SubRange> ranges;
		return matchInfixSequence(context, pattern, token, leftAssociative, ranges);
	}

	vector<Sequence::SubSequence> Sequence::prepair(const vector<TermPtr>& terms) {

		// merge all elements into a single list (inline nested sequences and skip epsilons)
		vector<TermPtr> flat;
		for(const TermPtr& cur : terms) {
			if (auto seq = dynamic_pointer_cast<Sequence>(cur)) {
				// add all sub-sequences
				for(const SubSequence& subList : seq->sequence) {
					flat.insert(flat.end(), subList.terms.begin(), subList.terms.end());
				}
			} else if (!dynamic_pointer_cast<Empty>(cur)){
				flat.push_back(cur);
			}
		}

		// create list of sub-sequences of pure terminal / non-terminal elements
		vector<SubSequence> res;
		if (flat.empty()) {
			return res;
		}

		// partition into sub-sequences
		SubSequence* curSeq = 0;
		for(const TermPtr& cur : flat) {
			bool terminal = cur->isTerminal();

			// check whether new sub-sequence needs to be started
			if (!curSeq || curSeq->terminal != terminal) {
				// start new sub-sequence
				res.push_back(SubSequence(terminal));
				curSeq = &res.back();
			}

			// add term to sub-sequence
			curSeq->limit += cur->getLimit();
			curSeq->terms.push_back(cur);
		}

		return res;
	}

	void Sequence::updateLimit() {
		Limit limit(0,0);
		for(const SubSequence& sub : sequence) {
			for(const TermPtr& rule : sub.terms) {
				limit += rule->getLimit();
			}
		}
		setLimit(limit);
	}

	Result Alternative::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		// stupid implementation => improve with priorities and index!
		auto backup = context.backup();
		for(auto it = alternatives.begin(); it != alternatives.end(); ++it) {
			Result res = (*it)->match(context, begin, end);
			if (res) { return res; }	// FIXME: Here alternatives are lost!
			backup.restore(context);
		}
		return false;
	}


	Result Loop::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		// stupid approach - pumping sequences

//		// terminal case => only one iteration
//		vector<TermPtr> list;
//		auto backup = context.backup();
//		unsigned dist = std::distance(begin,end);
//		while(list.size() <= dist) {
//			Result res = Sequence(list).match(context, begin, end);
//			if (res) { return res; }
//			backup.restore(context);
//
//			// try one more repedition
//			list.push_back(body);
//		}
//
//		// no match
//		return false;

		/**
		 * Idea:
		 * 		1) search smallest block (from start) matching the loop body => first match
		 * 		2) repeat recursively until range has been consumed
		 * 		3) if recursive resolution worked => done, otherwise search next match in 1)
		 */

		// terminal case - empty case => will be accepted
		if (begin == end) return true;

		// search largest portion starting from head being accepted by the body
		unsigned size = std::distance(begin, end);
		TokenIter curEnd = begin + std::min(body->getMinRange(), size);
		TokenIter upperLimit = begin + std::min(body->getMaxRange(), size) + 1;

		auto backup = context.backup();

		// gradually reduce the range to be matched
		while (curEnd != upperLimit) {

			// try current sub-range
			Result res = body->match(context, begin, curEnd);
			if (res) {
				// try matching the rest!
				auto res = match(context, curEnd, end);

				// if matching => done
				if (res) { return res; }

				// otherwise try next ..
				backup.restore(context);
			}

			// try one step smaller
			curEnd++;
		}

		// no first match found => no match at all
		return fail(context, begin, end);
	}


	NodePtr Grammar::match(NodeManager& manager, const string& code, bool throwOnFail) const {
		// step 1) start by obtaining list of tokens
		auto tokens = lex(code);

//std::cout << "Token List: " << tokens << "\n";

		// step 2) parse recursively
		Context context(*this, manager, tokens.begin(), tokens.end());
		return match(context, tokens.begin(), tokens.end());


		// TODO: error handling with exceptions!
//		try {
//			Context context(*this, manager, tokens.begin(), tokens.end(), true);
//			auto res = match(context, tokens.begin(), tokens.end());
//			if (!res && throwOnFail) throw fail(context, tokens.begin(), tokens.end());
//			return res;
//		} catch (const ParseException& pe) {
//			// handle exception depending on flag
//			if (throwOnFail) throw pe;
//		}
//
//		return NodePtr();
	}

	NodePtr Grammar::match(Context& context, const TokenIter& begin, const TokenIter& end) const {
		return matchInternal(context, begin, end);

//		static int hitCounter = 0;
//		static int missCounter = 0;
//
//		// check the cache
//		pair<TokenIter, TokenIter> range = std::make_pair(begin, end);
//
//		// check cache
//		NodePtr res = context.lookup(range);
//		if (res) {
//			std::cout << "Hit " << ++hitCounter << "\n";
//			return res;	// use cached value
//		}
//		std::cout << "Miss " << ++missCounter << "\n";
//		// parse element +
//		res = matchInternal(context, begin, end);
//		context.store(range, res);
//		return res;
	}

	NodePtr Grammar::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		// create new temporary context
		Context localContext(context, begin, end);
		auto backup = localContext.backup();
		for(const RulePtr& rule : rules) {
			NodePtr res = rule->match(localContext, begin, end);
			if (res) return res;
			backup.restore(localContext);
		}
		return NodePtr();
	}

	const TermPtr empty = std::make_shared<Empty>();

	const TermPtr identifier = any(Token::Identifier);

	const TermPtr rec = std::make_shared<NonTerminal>();

} // end namespace parser
} // end namespace core
} // end namespace insieme

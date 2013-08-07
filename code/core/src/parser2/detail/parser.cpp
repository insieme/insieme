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

#include "insieme/core/parser2/detail/parser.h"

#include <sstream>
#include <iterator>
#include <boost/tokenizer.hpp>

namespace insieme {
namespace core {
namespace parser {
namespace detail {

	namespace {

		Result fail(Context& context, const TokenIter& begin, const TokenIter& end) throw(ParseException) {
			if (!context.isSpeculative())
				throw ParseException(begin, end);
			return false;
		}

		string buildErrorMessage(const TokenIter& begin, const TokenIter& end) {
			std::stringstream out;
			out << "Unable to parse token sequence \""
					<< join(" ", begin, end, [](std::ostream& out, const Token& cur) { out << cur.getLexeme(); }) << "\"";
			return out.str();
		}

	}


	ParseException::ParseException(const TokenIter& begin, const TokenIter& end)
		: msg(buildErrorMessage(begin, end)){}


	// -- Term Implementations -------------


	Result Term::match(Context& context, const TokenIter& begin, const TokenIter& end) const {

		// setting this flag will print a lot of debug messages
		// TODO: find a better way to use this flag
		const bool DEBUG = false;

		if (!DEBUG) {

			// -- non-debug version --
			if (!range_limit.covers(begin,end)) {
				return fail(context, begin, end);
			}
			return matchInternal(context, begin, end);

		} else {

			// -- debug version --

			// print debug infos
			auto offset = times("  ", context.getLevel());
			std::cout << offset << "Try matching " << *this << " against " << join(" ", begin, end) << " - speculation: " << ((context.isSpeculative())?"On":"Off") << "\n";

			try {
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

			} catch (const ParseException& pe) {
				std::cout << offset << "Failed with error: " << pe.what() << "\n";
				throw pe;
			}
			return false;	// just to get rid of warnings
		}

	}


	Result NonTerminal::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		NodePtr res = context.grammar.match(context, begin, end, nonTerminal);
		if (!res) return fail(context,begin,end);
		context.push(res);
		return res;
	}

	namespace {

		template<typename C>
		utils::range<typename C::const_iterator> range(const C& c) {
			return utils::range<typename C::const_iterator>(c.begin(), c.end());
		}

		template<typename Iter>
		utils::range<Iter> range(const Iter& begin, const Iter& end) {
			return utils::range<Iter>(begin, end);
		}

		typedef typename vector<TermPtr>::const_iterator TermIter;
		typedef typename vector<Sequence::SubSequence>::const_iterator SubSeqIter;

		struct SubRange {
			utils::range<TermIter> pattern;
			utils::range<TokenIter> tokens;

			SubRange(const utils::range<TermIter>& pattern, const utils::range<TokenIter>& tokens)
				: pattern(pattern), tokens(tokens) {}
		};

		vector<TokenIter> findSplitCandidates(const Grammar::TermInfo& info,
				const TokenIter& begin, const TokenIter& end, const TokenIter& lowerLimit,
				const TokenSet& endSet, const TokenSet& followSet,
				const TokenSet& terminator = TokenSet()
		) {

			// initialize candidate list
			vector<TokenIter> candidates;

			// handle begin differently
			if (begin != end && begin >= lowerLimit && followSet.contains(*begin)) {
				candidates.push_back(begin);
			}

			// compute list of candidate-split-points
			vector<Token> parentheseStack;
			for(auto it = begin; it != end; ++it) {
				const Token& cur = *it;

				// if current token is an opener => put closer on the stack
				if (info.isLeftParenthese(cur)) {
					// check whether this is a right-parentheses as well (e.g. $ .. $ )
					if (!info.isRightParenthese(cur) || parentheseStack.empty() || parentheseStack.back() != cur) {
						// it is not a right-parentheses or it is not the expected one
						parentheseStack.push_back(info.getClosingParenthese(cur));
						continue;
					}
				}

				// check whether it is a closer
				if (info.isRightParenthese(cur)) {
					if (!parentheseStack.empty() && parentheseStack.back() == cur) {
						parentheseStack.pop_back();	// all fine
					} else {
						// nothing more will be matched - parentheses not balanced
						break;	// no need to continue search for candidates
					}
				}

				// check whether iterator is inside a nested expression
				if (!parentheseStack.empty()) {
					continue;	// jup, still inside => no candidate
				}

				// so, there is nothing on the stack ..
				assert(parentheseStack.empty());

				// we are getting closer - check whether we are within the search range
				if (it+1 < lowerLimit) {
					continue;
				}

				// now check whether current token is a terminator for the body
				if (!endSet.contains(cur)) {
					continue;
				}

				// check whether following element is within the follow set
				if (it+1 != end && !followSet.contains(*(it+1))) {
					continue;
				}

				// nice - we have a candidate (the end is the next token)
				candidates.push_back(it+1);

				// check whether this entry is a terminating one
				if (terminator.contains(cur)) {
					return candidates;		// we are done her!
				}
			}

			// that's it - we have a list of candidates
			return candidates;
		}

		Result matchVarOnly(Context& context, const utils::range<TermIter>& pattern, const utils::range<TokenIter>& tokens) {

			// check terminal state
			if (pattern.empty()) {
				return tokens.empty(); // full string consumed?
			}

			// check that head is a non-terminal entry and not an epsilon
			assert(!pattern.front()->isTerminal());
			assert(!dynamic_pointer_cast<Empty>(pattern.front()));
			auto curVar = *pattern.begin();

			// if the pattern is only a single variable (or the last within the recursive processing) ...
			if (pattern.single()) {
				return curVar->match(context, tokens.begin(), tokens.end());
			}

			// special treatment for empty token list
			if (tokens.empty()) {
				if (!curVar->match(context, tokens.begin(), tokens.end())) {
					return fail(context, tokens.begin(), tokens.end());
				}
				return matchVarOnly(context, pattern+1, tokens);
			}

			// --- search candidates for split-points ---

			// search largest portion starting from head being accepted by the body
			unsigned size = tokens.size();
			TokenIter lowerLimit = tokens.begin() + std::min(curVar->getMinRange(), size);
			TokenIter upperLimit = tokens.begin() + std::min(curVar->getMaxRange(), size);

			// the set of tokens ending the body
			const auto& endSet = context.grammar.getEndSet(pattern[0]);
			const auto& followSet = context.grammar.getFollowSet(pattern[0]);

			// search set of candidate-split-points
			vector<TokenIter> candidates = findSplitCandidates(
					context.grammar.getTermInfo(), tokens.begin(), upperLimit, lowerLimit,
					endSet, followSet
			);

			// if there are no candidates this will not match
			if (candidates.empty()) {
				return fail(context, tokens.begin(), tokens.end());
			}

			// update speculation flag
			context.setSpeculative(context.isSpeculative() || candidates.size() > 1);

			// guess end of current variable and process recursively
			auto backup = context.backup();
			for(const TokenIter& cur : candidates) {
				backup.restore(context);

				utils::range<TokenIter> partA(tokens.begin(), cur);
				utils::range<TokenIter> partB(cur, tokens.end());

				// start by trying to match first variable (head)
				Result a = curVar->match(context, partA.begin(), partA.end());
				if (!a) { continue;	} // try next split point

				// continue recursively with the rest of the variables
				Result b = matchVarOnly(context, pattern+1, partB);
				if (b) { return b; }
			}

			// this term can not be matched
			return fail(context, tokens.begin(), tokens.end());
		}

		Result matchSubRanges(Context& context, const vector<SubRange>& ranges) {

			// process sub-ranges
			Result res;
			auto backup = context.backup();
			bool speculationBackup = context.isSpeculative();
			for(const SubRange& cur : ranges) {
				res = matchVarOnly(context, cur.pattern, cur.tokens);
				if (!res) {
					backup.restore(context);
					return fail(context, cur.tokens.begin(), cur.tokens.end());
				}
				context.setSpeculative(speculationBackup);
			}

			// return last result
			return res;
		}

		Result matchInfixSequence(Context& context, const utils::range<SubSeqIter>& sequence, const utils::range<TokenIter>& tokens, bool leftAssociative, vector<SubRange>& ranges) {
			// some pre-condition (checking that sequence is an infix sequence)
			assert(!sequence.empty());
			assert(sequence.size() % 2 == 1); 			// uneven number of entries for infix
			assert(!(sequence.begin()->terminal));
			assert(!((sequence.end()-1)->terminal));

			// -- recursively build up sub-range list --

			// terminal case (last non-terminal sequence)
			if (sequence.single()) {
				// add final sub-range (has to match all the rest)
				ranges.push_back(SubRange(range(sequence.begin()->terms), tokens));

				// try solving individual sub-ranges
				auto res = matchSubRanges(context, ranges);

				// remove final su-range entry
				ranges.pop_back();
				return res;
			}


			// find match for next terminal
			auto& terminal = sequence[1];
			assert(terminal.terminal);

			// derive filters for before/after
			const TokenSet& before = context.grammar.getSequenceEndSet(sequence[0]);
			const TokenSet& after  = context.grammar.getSequenceStartSet(sequence[2]);

			bool beforeMayBeEmpty = sequence[0].limit.getMin() == 0;
			bool afterMayBeEmpty  = sequence[2].limit.getMin() == 0;

			unsigned terminalSize = terminal.limit.getMin();
			assert(terminalSize == terminal.limit.getMax());

			// check for sufficient token stream size
			if (tokens.size() < terminalSize) {
				return fail(context, tokens.begin(), tokens.end());
			}

			// limit range of search space
			unsigned min = sequence[0].limit.getMin();
			unsigned max = std::min(sequence[0].limit.getMax(), (unsigned)(tokens.size()-terminalSize));

			// get the first terminal to be looking for
			TermPtr headTerminal = terminal.terms[0];

			// while searching candidates we are speculating ...
			bool speculationBackup = context.isSpeculative();
			context.setSpeculative(true);

			// search all candidates first
			const Grammar::TermInfo& info = context.grammar.getTermInfo();
			vector<Token> parentheseStack;
			vector<unsigned> candidates;
			for(unsigned i = 0; i<=max; ++i) {
				const Token& cur = tokens[i];

				// if current token is an opener => put closer on the stack
				if (info.isLeftParenthese(cur)) {
					parentheseStack.push_back(info.getClosingParenthese(cur));
				}

				// check whether it is a closer
				if (info.isRightParenthese(cur)) {
					if (!parentheseStack.empty() && parentheseStack.back() == cur) {
						parentheseStack.pop_back();	// all fine
					} else {
						if (headTerminal->match(context, tokens.begin() + i, tokens.begin() + i +1)) {
							// will be the last to be matched (it is the closing parentheses pair)
							max = i;
						} else {
							// nothing more will be matched - parentheses not balanced
							break;	// no need to continue search for candidates
						}
					}
				}

				// check whether iterator is inside a nested expression
				//		- if searched head node is start of parentheses pair we still have to continue
				bool isOpenToken = info.isLeftParenthese(cur) && headTerminal->match(context, tokens.begin() + i, tokens.begin() + i +1);
				if (!(parentheseStack.empty() || (isOpenToken && parentheseStack.size() == 1u))) {
					continue;	// jup, still inside => no candidate
				}

				// skip elements outside search range
				if (i < min) continue;

				// check token before and after token sequence
				if (i > 0 && !beforeMayBeEmpty && !before.contains(tokens[i-1])) {
					continue;
				}
				if (i+terminalSize < tokens.size() && !afterMayBeEmpty && !after.contains(tokens[i+terminalSize])) {
					continue;
				}

				// check terminals at corresponding position
				TokenIter pos = tokens.begin() + i;
				bool fit = true;
				for(const TermPtr& cur : terminal.terms) {
					fit = fit && cur->match(context, pos, pos+1);
					pos++;
				}
				if (!fit) continue;

				// found a candidate
				candidates.push_back(i);

			}

			// check whether candidates are empty
			if (candidates.empty()) {
				return fail(context, tokens.begin(), tokens.end());
			}

			// update speculation flag
			context.setSpeculative(speculationBackup || candidates.size() > 1u);

			// reverse search order in left-associative case
			if (leftAssociative) {
				std::reverse(candidates.begin(), candidates.end());
			}

			// search within potential scope
			for(unsigned i : candidates) {

				// recursively match remaining terminals
				ranges.push_back(SubRange(range(sequence.front().terms), tokens.subrange(0,i)));
				Result res = matchInfixSequence(context, sequence+2, tokens.subrange(i+terminalSize), leftAssociative, ranges);

				// check result => if OK, done
				if (res) { return res; }

				// continue search
				ranges.pop_back();
			}

			// no matching assignment found
			return fail(context, tokens.begin(), tokens.end());

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
		if (pattern.begin()->terminal) {
			const SubSequence& cur = pattern[0];

			// match initial terminals
			for(const TermPtr& rule : cur.terms) {
				if(token.empty() || !rule->match(context, token.begin(), token.begin()+1)) {
					return fail(context, token.begin(), token.begin()+1);
				}
				token+=1;
			}

			// prune pattern
			pattern+=1;
		}

		// if it is only a sequence of terminals => done!
		if (pattern.empty()) {
			return token.empty();
		}

		// Tailing terminals:
		if ((pattern.end()-1)->terminal) {
			const SubSequence& cur = *(pattern.end()-1);

			// match tailing terminals
			for(auto it = cur.terms.rbegin(); it != cur.terms.rend(); ++it) {
				const TermPtr& rule = *it;
				if(token.empty() || !rule->match(context, token.end()-1, token.end())) {
					return fail(context, token.end()-1, token.end());
				}
				token-=1;
			}

			// prune pattern
			pattern-=1;
		}

		// Now there should only be an infix pattern (head and tail is non-terminal)
		assert(!pattern.empty());
		assert(pattern.size() % 2 == 1);
		assert(!pattern.begin()->terminal);
		assert(!(pattern.end()-1)->terminal);

		// use recursive matching algorithm
		vector<SubRange> ranges;
		return matchInfixSequence(context, pattern, token, leftAssociative, ranges);
	}

	vector<Sequence::SubSequence> Sequence::prepare(const vector<TermPtr>& terms) {

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

		// get list of candidates
		vector<TermPtr> candidates;
		if (begin == end) {
			// take all rules with potential length 0
			for(const TermPtr& cur : alternatives) {
				if (cur->getMinRange() == 0u) {
					candidates.push_back(cur);
				}
			}
		} else {
			// take all rules with matching start and end tokens
			for(const TermPtr& cur : alternatives) {
				const auto& sets = context.grammar.getSets(cur);
				if (sets.startSet.contains(*begin) && sets.endSet.contains(*(end-1))) {
					candidates.push_back(cur);
				}
			}
		}

		// check number of candidates
		if (candidates.empty()) {
			return fail(context, begin, end);		// will not match
		}


		// stupid implementation => improve with priorities and index!
		auto backup = context.backup();
		context.setSpeculative(context.isSpeculative() || candidates.size() > 1u);
		for(const TermPtr& cur : candidates) {
			Result res = cur->match(context, begin, end);
			if (res) { return res; }
			backup.restore(context);
		}
		return fail(context, begin, end);
	}


	Result Loop::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {

		/**
		 * Idea:
		 * 		1) search smallest block (from start) matching the loop body => first match
		 * 		2) repeat recursively until range has been consumed
		 * 		3) if recursive resolution worked => done, otherwise search next match in 1)
		 */

		// terminal case - empty case => will be accepted
		if (begin == end) return true;

		// the set of tokens ending the body
		const auto& sets = context.grammar.getSets(body);
		const auto& startSet = sets.startSet;
		const auto& endSet = sets.endSet;

		// check head and tail element
		if (!startSet.contains(*begin) || !endSet.contains(*(end-1))) {
			return fail(context, begin, end);	// will never match
		}

		// search largest portion starting from head being accepted by the body
		unsigned size = std::distance(begin, end);
		TokenIter lowerLimit = begin + std::min(body->getMinRange(), size);
		TokenIter upperLimit = begin + std::min(body->getMaxRange(), size);

		// find candidates of potential splitting points
		vector<TokenIter> candidates = findSplitCandidates(
				context.grammar.getTermInfo(), begin, upperLimit, lowerLimit,
				endSet, startSet, terminator
		);

		// check whether there are candidates
		if (candidates.empty()) {
			return fail(context, begin, end);
		}
		// update speculation flag
		auto backup = context.backup();
		context.setSpeculative(context.isSpeculative() || candidates.size() > 1u);

		// gradually reduce the range to be matched
		for(const auto& curEnd : candidates) {

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
		}

		// no first match found => no match at all
		return fail(context, begin, end);
	}


	bool Terminal::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		return begin.add(terminal) || end.add(terminal);
	}

	bool Any::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		TokenSet set = (!type)?(TokenSet(TokenSet::all())):(TokenSet(type));
		return begin.add(set) || end.add(set);
	}

	bool NonTerminal::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		return begin.add(g.getStartSet(nonTerminal)) || end.add(g.getEndSet(nonTerminal));
	}

	bool Alternative::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		bool res = false;
		for(const TermPtr& opt : alternatives) {
			res = begin.add(g.getStartSet(opt)) || res;
			res = end.add(g.getEndSet(opt)) || res;
		}
		return res;
	}

	bool Loop::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		bool res = false;
		res = begin.add(g.getStartSet(body)) || res;
		res = end.add(g.getEndSet(body)) || res;
		return res;
	}


	bool Sequence::SubSequence::updateStartSet(const Grammar& g, TokenSet& start) const {
		if (terms.empty()) return false;
		bool res = false;
		auto it = terms.begin();
		do {
			res = start.add(g.getStartSet(*it)) || res;
		} while ((*it)->getMinRange() == 0u && (++it) != terms.end());
		return res;
	}

	bool Sequence::SubSequence::updateEndSet(const Grammar& g, TokenSet& end) const {
		bool res = false;
		auto it = terms.rbegin();
		do {
			res = end.add(g.getEndSet(*it)) || res;
		} while ((*it)->getMinRange() == 0u && (++it) != terms.rend());
		return res;
	}

	vector<TermPtr> Sequence::getTerms() const {
		vector<TermPtr> res;
		for (auto& cur : sequence) {
			res.insert(res.end(), cur.terms.begin(), cur.terms.end());
		}
		return res;
	}

	bool Sequence::updateTokenSets(const Grammar& g, TokenSet& begin, TokenSet& end) const {
		bool res = false;

		// deal with potentially empty sub-sequences in the front
		{
			auto it = sequence.begin();
			do {
				res = it->updateStartSet(g, begin) || res;
			} while(it->limit.getMin() == 0 && (++it) != sequence.end());
		}

		// deal with potentially empty sub-sequences in the back
		{

			auto it = sequence.rbegin();
			do {
				res = it->updateEndSet(g, end) || res;
			} while(it->limit.getMin() == 0 && (++it) != sequence.rend());
		}

		return res;
	}

	void Sequence::addSubTerms(std::set<TermPtr>& terms) const {
		// collect all sub-terms
		for(const SubSequence& cur : sequence) {
			for(const TermPtr& term : cur.terms) {
				terms.insert(term);
				term->addSubTerms(terms);
			}
		}
	}

	void Sequence::addTokenPairs(std::map<Token,Token>& map) const {
		// not the best but a fool-proof implementation (hopefully)
		vector<TermPtr> terms;
		for(const SubSequence& cur : sequence) {
			for(const TermPtr& term : cur.terms) {
				terms.push_back(term);
			}
		}

		// now search token pairs
		for(auto a = terms.begin(); a != terms.end(); ++a) {
			auto terminal = dynamic_pointer_cast<Terminal>(*a);
			if (!terminal) continue;

			// found a start symbol
			const Token& start = terminal->getTerminal();
			if (start.getType() != Token::Symbol) { continue; }

			// search end symbol
			bool spansNonTerminal = false;
			for(auto b=a+1; b != terms.end(); ++b) {
				auto terminal = dynamic_pointer_cast<Terminal>(*b);
				if (!terminal) {
					spansNonTerminal = true;
					continue;
				}

				// if there was no variable part in between ..
				if (!spansNonTerminal) {
					break;		// .. it can be skipped
				}

				const Token& end = terminal->getTerminal();
				if (end.getType() != Token::Symbol) {
					spansNonTerminal = true;
					continue;
				}

				// an end has been found!
				map[start] = end;
				break;	// no further search necessary
			}

		}
	}

	namespace {

		template<typename TokenIter>
		bool checkParenthese(const Token& start, const Token& end, const TokenIter& tbegin, const TokenIter& tend) {
			// search through range for start token
			for(auto a = tbegin; a != tend; ++a) {
				if (*a != start) continue;

				// if start is present, end has to be as well!
				bool found = false;
				for(auto b=a+1; !found && b != tend; ++b) {
					if (*b == end) {
						found = true;
						a = b;	// continue search from here
					} else if (*b == start) {
						return false; // another start before the end => pair not supported
					}
				}

				// if there is no corresponding end-symbol the pair is not supported!
				if (!found) return false;
			}

			// no violation found => everything is fine
			return true;
		}

	}


	bool Sequence::supportsPair(const pair<Token,Token>& pair) const {

		// create list of tokens included within this sequence
		vector<Token> tokenSeq;
		for(const SubSequence& cur : sequence) {
			for(const TermPtr& term : cur.terms) {
				if (auto cur = dynamic_pointer_cast<Terminal>(term)) {
					const Token& token = cur->getTerminal();
					if (token.getType() == Token::Symbol) {
						tokenSeq.push_back(token);
					}
				}
			}
		}

		// extract start and end tokens
		const Token& start = pair.first;
		const Token& end = pair.second;

		// search forward and backward direction
		return	checkParenthese(start, end, tokenSeq.begin(), tokenSeq.end()) &&
				checkParenthese(end, start, tokenSeq.rbegin(), tokenSeq.rend());

	}

	// -- begin token set

		bool TokenSet::add(const Token& token) {
			if (contains(token)) return false;
			*this += token;
			return true;
		}
		bool TokenSet::add(const Token::Type& type) {
			if (coversType(type)) return false;
			*this += type;
			return true;
		}

		bool TokenSet::add(const TokenSet& other) {
			if (isSubSet(other)) return false;
			*this += other;
			return true;
		}

		bool TokenSet::isSubSet(const TokenSet& other) const {
			return ((tokenTypeMask | other.tokenTypeMask) == tokenTypeMask) &&
					(::all(other.tokens, [&](const Token& cur) { return contains(cur); }));
		}

		TokenSet& TokenSet::operator+=(const Token& token) {
			if (!coversType(token) && !coversToken(token)) {
				tokens.push_back(token);
			}
			return *this;
		}

		TokenSet& TokenSet::operator+=(const Token::Type& type) {
			if (tokenTypeMask & (1<<type)) {
				return *this; // nothing to add
			}

			// add to mask
			tokenTypeMask = tokenTypeMask | (1<<type);

			// kick out all tokens matched by type mask
			auto newEnd = std::remove_if(tokens.begin(), tokens.end(),
					[&](const Token& cur) { return coversType(cur); });
			tokens.erase(newEnd, tokens.end());

			// return updated
			return *this;
		}

		TokenSet& TokenSet::operator+=(const TokenSet& other) {
			// merge type mask
			tokenTypeMask = tokenTypeMask | other.tokenTypeMask;

			// filter out local set
			auto newEnd = std::remove_if(tokens.begin(), tokens.end(),
					[&](const Token& cur) { return coversType(cur); });
			tokens.erase(newEnd, tokens.end());

			// merge in other set
			for(const Token& cur : other.tokens) {
				if (!coversType(cur) && !coversToken(cur)) {
					tokens.push_back(cur);
				}
			}
			return *this;
		}

		std::ostream& TokenSet::printTo(std::ostream& out) const {
			assert(Token::Symbol == 1 && Token::String_Literal == 9 && "If this changes, check this code!");

			out << "{";

			for(unsigned i = Token::Symbol; i != Token::String_Literal; i++) {
				Token::Type cur = (Token::Type)i;
				if (coversType(cur)) {
					out << "(" << cur << ":*),";
				}
			}
			return out << join(",", tokens) << "}";
		}

	// -- end token set

	std::ostream& Grammar::TermInfo::printTo(std::ostream& out) const {
		return out << "TermInfo: {\n\t" <<
				join("\n\t", termInfos,
					[](std::ostream& out, const pair<TermPtr,Sets>& cur) {
						out << *cur.first << ": \t" << cur.second.startSet << " ... " << cur.second.endSet << " : " << cur.second.followSet;
				}) << "\n\t" <<
				join("\n\t", nonTerminalInfos,
					[](std::ostream& out, const pair<string,Sets>& cur) {
						out << cur.first << ": \t" << cur.second.startSet << " ... " << cur.second.endSet << " : " << cur.second.followSet;
				}) << "\n TerminalPairs: \n\t" <<
				join("\n\t", parenthesePairs,
					[](std::ostream& out, const pair<Token,Token>& cur) {
						out << cur.first.getLexeme() << " " << cur.second.getLexeme();
				}) << "\n}";
	}


	NodePtr Grammar::match(NodeManager& manager, const string& code, bool throwOnFail, const std::map<string, NodePtr>& symbols) const {
		// step 1) start by obtaining list of tokens
		auto tokens = lex(code);

		// step 2) check parenthesis - if not properly nested, it is wrong!
		if (!checkParenthese(tokens.begin(), tokens.end())) {
			if (throwOnFail) throw ParseException("Unbalanced parentheses encountered!");
			return NodePtr(); // parenthesis not properly nested!
		}

		// step 2) parse recursively
		NodePtr res;
		try {
			// create a context for the translation
			Context context(*this, manager, tokens.begin(), tokens.end(), false);

			// register pre-defined symbols
			auto& symManager = context.getSymbolManager();
			vector<vector<Token>> symbolTokens;
			for(const pair<string, NodePtr>& cur : symbols) {
				symbolTokens.push_back(toVector(Token::createIdentifier(cur.first)));
				symManager.add(range(symbolTokens.back()), cur.second);
			}

			// run recursive match
			res = match(context, tokens.begin(), tokens.end(), start);
			if (!res && throwOnFail) throw ParseException("Unknown parser error!");
		} catch (const ParseException& pe) {
			// handle exception depending on flag
			if (throwOnFail) throw pe;
		}
		return res;
	}

	NodePtr Grammar::match(Context& context, const TokenIter& begin, const TokenIter& end, const string& nonTerminal) const {
		return matchInternal(context, begin, end, nonTerminal);

//		static int hitCounter = 0;
//		static int missCounter = 0;
//
//		// check the cache
//		TokenRange range(begin,end);
//
//		// check cache
//		NodePtr res = context.lookup(nonTerminal, range);
//		if (res) {
//			std::cout << "Hit " << ++hitCounter << " vs " << missCounter << "\n";
//			return res;	// use cached value
//		}
//		++missCounter;
////		std::cout << "Miss " << ++missCounter << "\n";
//		// parse element +
//		res = matchInternal(context, begin, end, nonTerminal);
//		context.store(nonTerminal, range, res);
//		return res;
	}

	NodePtr Grammar::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end, const string& nonTerminal) const {

		// search for rule set for given non-terminal
		auto pos = productions.find(nonTerminal);
		if(pos == productions.end()) {
			return NodePtr();		// a non-terminal without productions will not be matched
		}

		// compute set of potential rules
		vector<RulePtr> candidates;
		if (begin == end) {
			// take all rules with potential length 0
			for(const RulePtr& rule : pos->second) {
				if (rule->getPattern()->getMinRange() == 0u) {
					candidates.push_back(rule);
				}
			}

		} else {
			// take all rules with matching start and end tokens
			for(const RulePtr& rule : pos->second) {
				const auto& sets = getSets(rule->getPattern());
				if (sets.startSet.contains(*begin) && sets.endSet.contains(*(end-1))) {
					candidates.push_back(rule);
				}
			}
		}

		// see whether there are candidates
		if (candidates.empty()) {
			return NodePtr();
		}

		// create new temporary context
		Context localContext(context, begin, end);
		localContext.setSpeculative(context.isSpeculative() || candidates.size() > 1u);
		auto backup = localContext.backup();
		for(const RulePtr& rule : candidates) {
			NodePtr res = rule->match(localContext, begin, end);
			if (res) return res;
			backup.restore(localContext);
		}
		return NodePtr();
	}

	Grammar::Productions Grammar::toProductions(const string& symbol, const vector<RulePtr>& rules) {
		Grammar::Productions res;
		res[symbol].insert(rules.begin(), rules.end());
		return res;
	}

	std::ostream& Grammar::printTo(std::ostream& out) const {
		return out << "(" << start << ",{\n\t" << join("\n\t", productions, [](std::ostream& out, const pair<string, RuleSet>& cur) {
			out << cur.first << " =\t" << join(" |\n\t\t", cur.second, [](std::ostream& out, const RulePtr& rule) {
				out << *rule->getPattern();
			}) << "\n";
		}) << "})";
	}

	void Grammar::updateTermInfo() const {
		if (infoValid) return;

		// mark as up-to-date (already during processing to avoid infinite recursion)
		infoValid = true;

		// collect set of terms
		std::set<TermPtr> terms;
		for(const auto& product : productions) {
			for(const RulePtr& rule : product.second) {
				terms.insert(rule->getPattern());
				rule->getPattern()->addSubTerms(terms);
			}
		}

		// re-initialize term info
		info = TermInfo();

		// ---- compute start / end sets -----

		// initialize information
		for(const TermPtr& cur : terms) {
			info.termInfos[cur];	// uses default initialization
		}
		for(const auto& production : productions) {
			info.nonTerminalInfos[production.first];
		}

		// iteratively solve equation system for start/end sets
		bool changed = true;
		while (changed) {
			changed = false;

			// update sub-sequences
			for(const TermPtr& cur : terms) {
				if (auto seq = dynamic_pointer_cast<const Sequence>(cur)) {
					for(const Sequence::SubSequence& cur : seq->getSubSequences()) {
						auto& sets = info.subSequenceInfo[&cur];
						changed = cur.updateStartSet(*this,sets.startSet) || changed;
						changed = cur.updateEndSet(*this, sets.endSet) || changed;
					}
				}
			}

			// update terms
			for(const TermPtr& cur : terms) {
				auto& sets = info.termInfos[cur];
				changed = cur->updateTokenSets(*this, sets.startSet, sets.endSet) || changed;
			}

			// updated non-terminals
			for(const auto& production : productions) {
				// update productions for current non-terminal
				auto& sets = info.nonTerminalInfos[production.first];
				for(const auto& rule : production.second) {
					changed = sets.startSet.add(getStartSet(rule->getPattern())) || changed;
					changed = sets.endSet.add(getEndSet(rule->getPattern())) || changed;
				}
			}
		}

		// update follow sets
		for(const TermPtr& cur : terms) {
			if (auto seq = dynamic_pointer_cast<const Sequence>(cur)) {

				// get sequence of terms
				vector<TermPtr> terms = seq->getTerms();

				// set up follow sets
				for(auto a = terms.begin(); a != terms.end(); ++a) {
					TokenSet& followSet = info.termInfos[*a].followSet;
					for(auto b= a+1; b != terms.end(); ++b) {
						followSet.add(info.termInfos[*b].startSet);
						if ((*b)->getLimit().getMin() > 0) break;
					}
				}
			}
		}

		// ---- compute pairs -----

		// start by collecting top-level production rule sequences
		typedef std::shared_ptr<Sequence> SequencePtr;
		std::vector<SequencePtr> sequences;
		for(const auto& production : productions) {
			for(const auto& rule : production.second) {
				const TermPtr& cur = rule->getPattern();
				if (auto sequence = dynamic_pointer_cast<Sequence>(cur)) {
					sequences.push_back(sequence);
				} else {
					// turn into a sequence and add it to the list
					sequences.push_back(seq(cur));
				}
			}
		}

		// create list of token-pair candidates
		std::map<Token,Token> tokenPairCandidates;
		for(const SequencePtr& cur : sequences) {
			cur->addTokenPairs(tokenPairCandidates);
		}

		// check candidates (whenever the left-hand-side is present, the right has to be as well)
		for(const pair<Token,Token>& cur : tokenPairCandidates) {

			// if all sequences support the current pair ...
			auto support = [&](const SequencePtr& seq) { return seq->supportsPair(cur); };
			if (all(sequences, support)) {
				info.addParenthese(cur.first, cur.second); // .. we can add it to the result
			}
		}
	}

	bool Grammar::checkParenthese(const TokenIter& begin, const TokenIter& end) const {
		const TermInfo& info = getTermInfo();
		if (!info.hasParenthesePairs()) {
			return true;	// nothing to check
		}

		// check proper nesting
		vector<Token> parentheseStack;
		for(const Token& cur : range(begin, end)) {
			if (info.isLeftParenthese(cur)) {
				parentheseStack.push_back(info.getClosingParenthese(cur));
			}
			if (info.isRightParenthese(cur)) {
				if (parentheseStack.empty() || parentheseStack.back() != cur) {
					return false;		// not matching parentheses
				}
				parentheseStack.pop_back();
			}
		}

		// now all parentheses should be closed
		return parentheseStack.empty();
	}

	TermPtr cap(const TermPtr& term) {
		// define action event handler
		struct capture : public detail::actions {
			void accept(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.push(TokenRange(begin, end));
			}
		};
		return std::make_shared<Action<capture>>(term);
	}

	TermPtr varScop(const TermPtr& term) {
		// define action event handler
		struct var_scope_handler : public detail::actions {
			void enter(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getVarScopeManager().pushScope(true);
			}
			void leave(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getVarScopeManager().popScope();
			}
		};
		return std::make_shared<Action<var_scope_handler>>(term);
	}

	TermPtr newScop(const TermPtr& term) {
		// define action event handler
		struct new_scope_handler : public detail::actions {
			void enter(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getVarScopeManager().pushScope(false);
			}
			void leave(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getVarScopeManager().popScope();
			}
		};
		return std::make_shared<Action<new_scope_handler>>(term);
	}

	TermPtr symScop(const TermPtr& term) {
		// define action event handler
		struct new_scope_handler : public detail::actions {
			void enter(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getSymbolManager().pushScope(true);
			}
			void leave(Context& context, const TokenIter& begin, const TokenIter& end) const {
				context.getSymbolManager().popScope();
			}
		};
		return std::make_shared<Action<new_scope_handler>>(term);
	}

} // end namespace detail
} // end namespace parser
} // end namespace core
} // end namespace insieme

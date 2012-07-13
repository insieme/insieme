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

#include "insieme/core/parser2/simple_parser.h"

#include <sstream>
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


	// -- Rule Implementations -------------


	Result Rule::match(Context& context, const TokenIter& begin, const TokenIter& end) const {

		// check constraints on number of tokens
		if (!range_limit.covers(begin,end)) {
			return fail(context, begin, end);
		}

		assert(begin <= end);

		// print debug infos
		auto offset = times("  ", context.getLevel());
		std::cout << offset << "Try matching " << *this << " against " << join(" ", begin, end) << "\n";
		context.incLevel();
		auto res = matchInternal(context, begin, end);
		std::cout << offset << "Match result: " << ((res)?"OK":"Failed") << "\n";
		context.decLevel();
		return res;

		// short version:
//		return matchInternal(context, begin, end);
	}


	Result NonTerminal::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		NodePtr res = context.grammar.match(context, begin, end);
		if (res) return res;
		return fail(context,begin,end);
	}

	namespace {

		template<typename Iter>
		struct Range : public utils::Printable {
			Iter begin;
			Iter end;

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

			Range<Iter> operator+(int i) const {
				return Range<Iter>(begin+i, end);
			}

			std::size_t size() const {
				return std::distance(begin, end);
			}

			std::ostream& printTo(std::ostream& out) const {
				return out << "[" << join(",", begin, end) << "]";
			}
		};

		template<typename C>
		Range<typename C::const_iterator> range(const C& c) {
			return Range<typename C::const_iterator>(c.begin(), c.end());
		}

		template<typename Iter>
		Range<Iter> range(const Iter& begin, const Iter& end) {
			return Range<Iter>(begin, end);
		}

		typedef typename vector<RulePtr>::const_iterator RuleIter;


		bool isTerminal(const RulePtr& term) {
			return dynamic_pointer_cast<Terminal>(term) || dynamic_pointer_cast<Any>(term);
		}


		template<typename PIter, typename TIter>
		Result matchVarOnly(Context& context, const Range<PIter>& pattern, const Range<TIter>& tokens) {

			// check terminal state
			if (pattern.empty()) {
				return tokens.empty(); // full string consumed?
			}

			// check that head is a non-terminal entry and not an epsilon
			assert(!isTerminal(*pattern.begin));
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
			for(int i=min; i<=max; i++) {
				Range<TIter> partA(tokens.begin, tokens.begin + i);
				Range<TIter> partB(tokens.begin + i, tokens.end);

				// start by trying to match first variable (head)
				Result a = curVar->match(context, partA.begin, partA.end);
				if (!a) {
					continue;	// try next split point
				}

				// continue recursively with the rest of the variables
				Result b = matchVarOnly(context, pattern+1, partB);
				if (b) {
					return b;
				}
			}

			// this rule can not be matched
			return fail(context, tokens.begin, tokens.end);
		}

		template<typename PIter, typename TIter>
		Result matchSubSequence(Context& context, const Range<PIter>& pattern, const Range<TIter>& tokens) {


			// --- Search first terminal within rule ---

			auto begin = pattern.begin;
			auto end = pattern.end;

			// find first terminal symbol in pattern
			int pos = 0;
			while(begin != end && !isTerminal(*begin)) { begin++; pos++; }


			// --- handle special cases ---

			// there is no symbol in the pattern (left) => handle var-only code
			if (begin == end) {
				// no more symbols => use variable only matcher
				return matchVarOnly(context, pattern, tokens);
			}

			// get identified non-terminal symbol
			RulePtr symbol = *begin;
			assert(isTerminal(symbol));

			// it is the first element in the sequence => check right side only
			if (begin == pattern.begin) {
				if (!tokens.empty() && symbol->match(context, tokens.begin, tokens.begin+1)) {
					// match rest
					return matchSubSequence(context, pattern+1, tokens+1);
				}
				return fail(context, tokens.begin, tokens.end);
			}


			// --- handle general case - first terminal is somewhere in the middle ---

			// partition pattern
			auto before = pattern.subrange(0,pos);
			auto after = pattern.subrange(pos+1);

			// now search for symbol within tokens
			pos = 0;
			auto t_begin = tokens.begin;
			auto t_end = tokens.end;

			// create a backup of the current context state
			// TODO: replace by pushing and poping off sub-terms!
			auto backup = context.backup();
			while(true) {
				// find next corresponding symbol
				while(t_begin != t_end && !symbol->match(context, t_begin, t_begin+1)) { t_begin++; pos++; }

				// check whether the requested symbol has not been found
				if (t_begin == t_end) { return false; }		// => no match

				// check whether right side is matching (by first checking other terminals before descending)
				Result a = matchSubSequence(context, after, tokens.subrange(pos+1));
				if (a) {
					// check non-terminal
					Result b = matchVarOnly(context, before, tokens.subrange(0,pos));
					if (b) {
						return a;		// return result of last match (which has been evaluated first)
					}
				}


				// roll-back and try next
				backup.restore(context);

				// try next element
				t_begin++; pos++;
			}

		}


	}


	Result Sequence::matchInternal(Context& context, const TokenIter& begin, const TokenIter& end) const {
		return matchSubSequence(context, range(sequence), range(begin, end));
	}

	vector<RulePtr> Sequence::merge(const vector<RulePtr>& terms) {
		vector<RulePtr> res;
		for(const RulePtr& cur : terms) {
			if (auto seq = dynamic_pointer_cast<Sequence>(cur)) {
				res.insert(res.end(), seq->sequence.begin(), seq->sequence.end());
			} else if (!dynamic_pointer_cast<Empty>(cur)){
				res.push_back(cur);
			}
		}
		return res;
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

		// terminal case => only one iteration
		vector<RulePtr> list;
		auto backup = context.backup();
		unsigned dist = std::distance(begin,end);
		while(list.size() <= dist) {
			Result res = Sequence(list).match(context, begin, end);
			if (res) { return res; }
			backup.restore(context);

			// try one more repedition
			list.push_back(body);
		}

		// no match
		return false;

//		/**
//		 * Idea:
//		 * 		search largest block (from start) matching the loop body => first match
//		 * 		repeat recursively until range has been consumed
//		 *
//		 * Still bad performance since end is guessed => could be obtained by supporting undefined end-of-range points
//		 */
//
//		// terminal case - empty case => will be accepted
//		if (begin == end) return true;
//
//		// search largest portion starting from head being accepted by the body
//		TokenIter curEnd = end;
//
//		// gradually reduce the range to be matched
//		while (begin != curEnd) {
//
//			// try current sub-range
//			Result res = body->match(context, begin, curEnd);
//			if (res.successfull) {
//				// try matching the rest!
//				return match(context, curEnd, end);
//			}
//
//			// try one step smaller
//			curEnd--;
//		}
//
//		// no first match found => no match at all
//		return false;	// TODO: send error report
	}



//	class Alternative : public Rule {
//		// TODO: improve
//		vector<RulePtr> alternatives;
//	public:
//		virtual Result match(Context& context, TokenIter& begin, const TokenIter& end) const;
//	};



	namespace {

		/**
		 * The tokenizer is splitting up the stream to be parsed by parts of
		 * the IR parser into tokens.
		 */
		struct IR_Tokenizer {

			template<typename InputIterator>
			bool isTerminal(InputIterator next, InputIterator end) const {
				// the list of terminals
				static const string terminals = "+-*/%=()<>{}[]&|,:;?!~^°'´\\";

				// check whether end has been reached
				return next != end && contains(terminals, *next);
			}

			/**
			 * Realizes the actual identification of the next token by searching
			 * its boundaries within the interval [next, end) and writing the result
			 * into the passed token.
			 */
			template <typename InputIterator, typename Token>
			bool operator()(InputIterator& next, InputIterator end, Token& tok) const {

				// a manipulation utility for the token
				typedef boost::tokenizer_detail::assign_or_plus_equal<
					typename boost::tokenizer_detail::get_iterator_category<InputIterator>::iterator_category
				> assigner;

				// clear the token
				assigner::clear(tok);

				// skip over white spaces and comments
				bool isComment = false;
				while(next != end && (isspace(*next) || *next=='#' || isComment)) {
					if (isComment && (*next=='\n' || *next=='#')) isComment = false;
					if (!isComment && *next=='#') isComment = true;
					++next;
				}

				// check end-position
				if (next == end) {
					return false;
				}

				InputIterator start(next);

				// check whether next token is a symbol
				if (isTerminal(start, end)) {
					assigner::plus_equal(tok,*next);
					assigner::assign(start, ++next, tok);
					return true;
				}

				// not a symbol => read token
				while (next != end && !isspace(*next) && !isTerminal(next, end)) {
					assigner::plus_equal(tok,*next);
					++next;
				}
				assigner::assign(start, next, tok);
				return true;
			}

			void reset() const {
				// no internal state
			}

		};


		typedef boost::tokenizer<IR_Tokenizer> Tokenizer;


	}


	NodePtr Grammar::match(NodeManager& manager, const string& code) const {
		// step 1) start by obtaining list of tokens
		Tokenizer tokenizer(code);
		vector<string> tokens(tokenizer.begin(), tokenizer.end());

std::cout << "Token List: " << tokens << "\n";

		// step 2) parse recursively
		Context context(*this, manager, tokens.begin(), tokens.end());
		return match(context, tokens.begin(), tokens.end());
	}

	NodePtr Grammar::match(Context& context, const TokenIter& begin, const TokenIter& end) const {
		// create new temporary context
		Context localContext(context, begin, end);
		return consumer->match(localContext, begin, end);
	}

	const RulePtr empty = std::make_shared<Empty>();

	const RulePtr any = std::make_shared<Any>();

	const RulePtr rec = std::make_shared<NonTerminal>();

} // end namespace parser
} // end namespace core
} // end namespace insieme

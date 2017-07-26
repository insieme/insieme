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

#include "insieme/core/printer/lexer.h"

#include <regex>
#include <boost/tokenizer.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace printer {
	namespace detail {


		namespace {

			// - the tokenizer implementation conducting the lexing -

			using std::regex;

			/**
			 * This struct is realizing the tokenizer function identifying
			 * the boundaries of language tokens.
			 */
			struct IR_Tokenizer {
				/**
				 * Identifies symbols. Symbols are characters from within a pre-defined set
				 * of characters.
				 */
				template <typename InputIterator>
				bool isSymbol(InputIterator next) const {
					// the list of terminals
					static const string terminals = "+-*/%=()<>{}[]&|.,:;?!~^°'´\\#$";

					// check whether end has been reached
					return contains(terminals, *next);
				}

				/**
				 * Checks whether the following prefix within the input iterator is a literal.
				 * If so, the handed in result-token tok will be updated and true will be returned.
				 * Otherwise the result will be false.
				 */
				template <typename InputIterator>
				bool resolveLiterals(InputIterator& next, const InputIterator& end, Token& tok) const {
					// the type used for storing a regex-literal type pair
					struct LiteralType {
						Token::Type type;
						regex rx;
					};

					static const regex::flag_type flags = regex::optimize |  // use optimized engine
					                                      regex::ECMAScript; // use ~JavaScript syntax

					// statically compiled regex patterns for the various literal types
					static const auto literalTypes = toVector( // the order is important!
					    (LiteralType){Token::Bool_Literal, regex(R"(true|false)", flags)},
					    (LiteralType){Token::Float_Literal, regex(R"(((([1-9][0-9]*)|0)\.[0-9]+[fF]))", flags)},
					    (LiteralType){Token::Double_Literal, regex(R"(((([1-9][0-9]*)|0)\.[0-9]+))", flags)},
					    (LiteralType){Token::Int_Literal, regex(R"((([1-9][0-9]*)|(0[xX][0-9A-Fa-f]+)|(0[0-7]*))u?l?)", flags)},
					    (LiteralType){Token::Char_Literal, regex(R"('\\?.')", flags)},
					    (LiteralType){Token::String_Literal, regex(R"("(\\.|[^\\"])*")", flags)});

					// check one after another whether regexes are matching the current prefix
					for(const LiteralType& cur : literalTypes) {
						// search for the current regex at the beginning of the rest of the code
						std::match_results<InputIterator> m;
						bool found = std::regex_search(next, end, m, cur.rx, std::regex_constants::match_continuous);

						if(!found) { continue; }

						// update token
						tok = Token::createLiteral(cur.type, m[0]);

						// check following character
						auto newEnd = next + m.length();

						// test whether it is followed by white space, comment or symbol
						if(newEnd == end || isSymbol(newEnd) || isspace(*newEnd)) {
							// update next
							next = newEnd;

							// this was a success
							return true;
						}
					}

					// this is not a literal
					return false;
				}

				/**
				 * Consumes the heading white-spaces of the given range.
				 */
				template <typename InputIterator>
				bool consumeWhiteSpaces(InputIterator& next, const InputIterator& end, Token& tok) const {
					// consume spaces
					InputIterator start = next;
					while(next != end && isspace(*next)) {
						++next;
					}

					// see whether there is a white-space
					if(start == next) { return false; }

					// create white-space token
					tok = Token::createWhitespace(string(start, next));
					return true;
				}

				/**
				 * Consumes commends at the head of the given range.
				 */
				template <typename InputIterator>
				bool consumeComment(InputIterator& next, const InputIterator& end, Token& tok) const {
					// consume white-spaces
					if(consumeWhiteSpaces(next, end, tok)) { return true; }
					if(next == end) { return false; }

					// check for start commend symbol ( // or /* )
					InputIterator a = next;
					InputIterator b = next + 1;
					if(a == end || b == end) { return false; }

					// search for // commend
					if(*a == '/' && *b == '/') {
						// => lasts until end of line
						next = b;
						while(next != end && *next != '\n') {
							++next;
						}
					}

					// search for /* commend
					if(*a == '/' && *b == '*') {
						// search for */ ending the comment
						while(b != end && (*a != '*' || *b != '/')) {
							++a;
							++b;
						}
						next = (b == end) ? end : b + 1;
					}

					// check whether a comment has been found
					if(a == next) { return false; }

					// create comment token
					tok = Token::createComment(string(a, next));
					return true;
				}

				/**
				 * Realizes the actual identification of the next token by searching
				 * its boundaries within the interval [next, end) and writing the result
				 * into the passed token.
				 */
				template <typename InputIterator>
				bool operator()(InputIterator& next, InputIterator end, Token& tok) const {
					// skip over white spaces and comments
					if(consumeComment(next, end, tok)) { return true; }

					// check end-position
					if(next == end) { return false; }

					// support literals
					if(resolveLiterals(next, end, tok)) { return true; }

					// check whether next token is a symbol
					if(isSymbol(next)) {
						// convert and consume symbol
						tok = Token::createSymbol(*(next++));
						return true;
					}

					// not a symbol => read token
					InputIterator start(next);
					while(next != end && !isspace(*next) && !isSymbol(next)) {
						++next;
					}

					// get current lexeme
					string lexeme = string(start, next);

					// define set of keywords to be considered
					static const vector<string> KEYWORD = {
					    "if", "else", "while", "for",
						"let", "in", "auto", "decl", "def", "var",
						"function", "lambda",
						"return", "break", "continue",
						"struct", "union",
					    //						"array", "vector", "ref", "channel",
					    "spawn", "syncAll", "using",
					};

					// check whether it is a keyword
					if(contains(KEYWORD, lexeme)) {
						// it is => create keyword token
						tok = Token::createKeyword(lexeme);
					} else {
						// everything else is an identifier
						tok = Token::createIdentifier(lexeme);
					}

					return true;
				}

				void reset() const {
					// no internal state
				}
			};


			typedef boost::tokenizer<IR_Tokenizer, std::string::const_iterator, Token> Tokenizer;
		}


		vector<Token> lex(const std::string& code, bool filterCommentAndWhiteSpace) {
			// just create and run tokenizer
			Tokenizer tokenizer(code);

			// create list of tokens, filtering comments and whitespaces if requested
			vector<Token> res;
			for(auto it = tokenizer.begin(); it != tokenizer.end(); it++) {
				const Token& cur = *it;
				if(!filterCommentAndWhiteSpace || (cur.getType() != Token::Type::Comment && cur.getType() != Token::Type::WhiteSpace)) { res.push_back(cur); }
			}
			return res;
		}


		std::ostream& Token::printTo(std::ostream& out) const {
			return out << "(" << type << ":" << lexeme << ")";
		}

		std::ostream& operator<<(std::ostream& out, const Token::Type& type) {
			switch(type) {
			case Token::Symbol: out << "Symbol"; break;
			case Token::Identifier: out << "Ident"; break;
			case Token::Keyword: out << "Keyword"; break;
			case Token::Bool_Literal: out << "BoolLit"; break;
			case Token::Int_Literal: out << "IntLit"; break;
			case Token::Float_Literal: out << "FloatLit"; break;
			case Token::Double_Literal: out << "DoubleLit"; break;
			case Token::Char_Literal: out << "CharLit"; break;
			case Token::String_Literal: out << "StrLit"; break;
			case Token::Comment: out << "Comment"; break;
			case Token::WhiteSpace: out << "WhiteSpace"; break;
			}
			return out;
		}

	} // end namespace detail
} // end namespace printer
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/parser2/lexer.h"

#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace parser {


	namespace {

		// - the tokenizer implementation conducting the lexing -

		using boost::regex;

		/**
		 * This struct is realizing the tokenizer function identifying
		 * the boundaries of language tokens.
		 */
		struct IR_Tokenizer {

			template<typename InputIterator>
			bool isSymbol(InputIterator next) const {
				// the list of terminals
				static const string terminals = "+-*/%=()<>{}[]&|.,:;?!~^°'´\\#$";

				// check whether end has been reached
				return contains(terminals, *next);
			}

			template<typename InputIterator>
			bool resolveLiterals(InputIterator& next, const InputIterator& end, Token& tok) const {

				// the type used for storing a regex-literal type pair
				struct LiteralType {
					Token::Type type;
					regex rx;
				};

				static const regex::flag_type flags =
						regex::optimize |			// use optimized engine
						regex::ECMAScript;			// use ~JavaScript syntax

				static const auto literalTypes = toVector(	// the order is important!
					(LiteralType){Token::Bool_Literal, 		regex(R"(true|false)",flags)},
					(LiteralType){Token::Float_Literal, 	regex(R"(((([1-9][0-9]*)|0)\.[0-9]+[fF]))", flags)},
					(LiteralType){Token::Double_Literal, 	regex(R"(((([1-9][0-9]*)|0)\.[0-9]+))", flags)},
					(LiteralType){Token::Int_Literal, 		regex(R"((([1-9][0-9]*)|(0[xX][0-9A-Fa-f]+)|(0[0-7]*))u?l?)", flags)},
					(LiteralType){Token::Char_Literal, 		regex(R"('\\?.')", flags)},
					(LiteralType){Token::String_Literal, 	regex(R"("(\\.|[^\\"])*")", flags)}
				);

				for(const LiteralType& cur : literalTypes) {

					// search for the current regex at the beginning of the rest of the code
					boost::match_results<InputIterator> m;
					bool found = boost::regex_search(next, end, m, cur.rx,
							boost::match_flag_type::match_continuous);

					if (!found) continue;

					// update token
					tok = Token::createLiteral(cur.type, m[0]);

					// update next
					next += m.length();

					// this was a success
					return true;
				}

				// this is not a literal
				return false;
			}

			template<typename InputIterator>
			void consumeWhiteSpaces(InputIterator& next, const InputIterator& end) const {
				while(next != end && isspace(*next)) ++next;
			}

			template<typename InputIterator>
			void consumeComment(InputIterator& next, const InputIterator& end) const {

				// consume white-spaces
				consumeWhiteSpaces(next,end);
				if (next == end) { return; }

				// check for start commend symbol ( // or /* )
				InputIterator a = next;
				InputIterator b = next+1;
				if (a == end || b == end) { return; }

				// search for // commend
				if (*a=='/' && *b=='/') {
					// => lasts until end of line
					next = b;
					while (next != end && *next != '\n') ++next;

					// consume potential successive comment
					consumeComment(next, end);
					return;
				}

				// search for /* commend
				if (*a=='/' && *b=='*') {

					// search for */ ending the comment
					while(b!=end && (*a!='*' || *b!='/')) { ++a; ++b; }
					next = (b==end)?end:b+1;

					// consume potential successive comments
					consumeComment(next,end);
				}
			}

			/**
			 * Realizes the actual identification of the next token by searching
			 * its boundaries within the interval [next, end) and writing the result
			 * into the passed token.
			 */
			template <typename InputIterator>
			bool operator()(InputIterator& next, InputIterator end, Token& tok) const {

				// skip over white spaces and comments
				consumeComment(next, end);

				// check end-position
				if (next == end) {
					return false;
				}

				// support literals
				if (resolveLiterals(next, end, tok)) {
					return true;
				}

				// check whether next token is a symbol
				if (isSymbol(next)) {
					// convert and consume symbol
					tok = Token::createSymbol(*(next++));
					return true;
				}

				// not a symbol => read token
				InputIterator start(next);
				while (next != end && !isspace(*next) && !isSymbol(next)) {
					++next;
				}

				// get identified sub-string
				tok = Token::createIdentifier(string(start,next));
				return true;
			}

			void reset() const {
				// no internal state
			}

		};


		typedef boost::tokenizer<
				IR_Tokenizer,
				std::string::const_iterator,
				Token
		> Tokenizer;

	}


	vector<Token> lex(const std::string& code) {
		// just create and run tokenizer
		Tokenizer tokenizer(code);
		// TODO: think about lazy evaluation by returning tokenizer instance itself
		return vector<Token>(tokenizer.begin(), tokenizer.end());
	}


	std::ostream& Token::printTo(std::ostream& out) const {
		return out << "(" << type << ":" << lexeme << ")";
	}

	std::ostream& operator<<(std::ostream& out, const Token::Type& type) {
		switch(type) {
			case Token::Symbol :    		out << "Symbol"; break;
			case Token::Identifier: 		out << "Ident"; break;
			case Token::Bool_Literal:		out << "BoolLit"; break;
			case Token::Int_Literal: 		out << "IntLit"; break;
			case Token::Float_Literal: 		out << "FloatLit"; break;
			case Token::Double_Literal: 	out << "DoubleLit"; break;
			case Token::Char_Literal: 		out << "CharLit"; break;
			case Token::String_Literal: 	out << "StrLit"; break;
		}
		return out;
	}

} // end namespace parser
} // end namespace core
} // end namespace insieme

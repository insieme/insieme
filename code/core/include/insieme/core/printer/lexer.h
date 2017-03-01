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
 *
 */
#pragma once

#include <cassert>
#include <string>
#include <memory>
#include <vector>

#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"


namespace insieme {
namespace core {
namespace printer {
	namespace detail {

		using std::string;
		using std::vector;

		class Token;


		/**
		 * The main function provided by this header file. Given a string
		 * containing a program code (fragment), it will be converted into
		 * a list of tokens.
		 *
		 * @param code the code to be tokenized
		 * @param filterCommentAndWhiteSpace if set, no comment and whitespace tokens will be returned
		 * @return the list of identified tokens.
		 */
		vector<Token> lex(const std::string& code, bool filterCommentAndWhiteSpace = true);


		// -- Token Types ----------------------------------------

		/**
		 * The class used to represent a token within the parser.
		 */
		class Token : public utils::Printable {
		  public:
			/**
			 * The type of tokens distinguished during the parsing.
			 */
			enum Type {
				// type 0 is used for "Any" when filtering those
				Symbol = 1,     // < tokens describing symbols / operators like +,-/($:...
				Identifier,     // < everything else - variable names, type names, ...
				Keyword,        // < known keywords like if, for, let, return, array, ...
				Bool_Literal,   // < boolean literals true / false
				Int_Literal,    // < integer literals, including oct and hex-values
				Float_Literal,  // < floating point literals, including e-notation
				Double_Literal, // < double literals
				Char_Literal,   // < character literals like 'x' and '\n', including the ''
				String_Literal, // < string literals, including the ""
				Comment,        // < a token representing a comment
				WhiteSpace      // < a token representing a white space
			};

		  private:
			/**
			 * The type of this token, according to the enum above.
			 */
			Type type;

			/**
			 * The string represented by this token.
			 */
			string lexeme;

			/**
			 * Creates a new token based on the given type and lexeme. This constructor
			 * is private. New Tokens have to be created using the public static factory
			 * functions to ensure the constrains regarding types and lexeme.
			 *
			 * @param type the type of the new token
			 * @param the associated lexeme
			 */
			Token(Type type, const string& lexeme) : type(type), lexeme(lexeme) {}

		  public:
			/**
			 * A default constructor.
			 */
			Token() : type(Symbol), lexeme("?") {}

			// Getter:

			/**
			 * Obtains the type of this token.
			 */
			Type getType() const {
				return type;
			}

			/**
			 * Obtains the string represented by this token.
			 */
			const string& getLexeme() const {
				return lexeme;
			}

			// Factory Functions:

			/**
			 * Creates a new token covering the given symbol. Symbols must only consist of
			 * a single character (to simplify the handling of potential symbol combination
			 * ambiguities - e.g. << for 2 opening brackets or a bitwise shift).
			 *
			 * @param lexeme the symbol to be converted into a token
			 * @return a token representing the given symbol
			 */
			static Token createSymbol(char lexeme) {
				return Token(Symbol, string(&lexeme, 1));
			}

			/**
			 * Creates an identifier token based on the given non-empty lexeme.
			 *
			 * @param lexeme the text of the identifier to be covered (must not be empty)
			 * @return an identifier token for the given text
			 */
			static Token createIdentifier(const string& lexeme) {
				assert_false(lexeme.empty());
				return Token(Identifier, lexeme);
			}

			/**
			 * Creates a keyword token based on the given non-empty lexeme.
			 *
			 * @param lexeme the text of the keyword to be covered (must not be empty)
			 * @return a keyword token for the given text
			 */
			static Token createKeyword(const string& lexeme) {
				assert_false(lexeme.empty());
				return Token(Keyword, lexeme);
			}

			/**
			 * Creates an arbitrary literal-token based on the given type and
			 * lexeme.
			 *
			 * @param type the type of the token to be created (has to be a literal type)
			 * @param lexeme the text to be represented
			 * @return the requested token instance
			 */
			static Token createLiteral(Type type, const string& lexeme) {
				assert_false(lexeme.empty());
				assert_true(Bool_Literal <= type && type <= String_Literal);
				return Token(type, lexeme);
			}

			/**
			 * Creates a comment token based on the given lexeme.
			 *
			 * @param lexeme the text of the comment to be covered
			 * @return a comment token for the given text
			 */
			static Token createComment(const string& lexeme) {
				return Token(Comment, lexeme);
			}

			/**
			 * Creates a comment token based on the given lexeme.
			 *
			 * @param lexeme the text of the whitespace to be covered
			 * @return a whitespace token for the given text
			 */
			static Token createWhitespace(const string& lexeme) {
				return Token(WhiteSpace, lexeme);
			}

			// Operator:

			bool operator==(const Token& other) const {
				return type == other.type && lexeme == other.lexeme;
			}

			bool operator==(const string& str) const {
				return lexeme == str;
			}

			bool operator==(const char* str) const {
				return lexeme == str;
			}

			bool operator==(char symbol) const {
				return type == Symbol && lexeme[0] == symbol;
			}

			template <typename T>
			bool operator!=(const T& other) const {
				return !(*this == other);
			}

			bool operator<(const Token& other) const {
				return type < other.type || (type == other.type && lexeme < other.lexeme);
			}

			/**
			 * An implicit to-string converter of tokens.
			 */
			operator const string&() const {
				return lexeme;
			}

			/**
			 * Allows tokens to be printed to the streams in a readable format.
			 */
			std::ostream& printTo(std::ostream& out) const;
		};

		/**
		 * Allows token types to be printed to a stream in a readable format.
		 */
		std::ostream& operator<<(std::ostream& out, const Token::Type& type);


	} // end namespace detail
} // end namespace printer
} // end namespace core
} // end namespace insieme

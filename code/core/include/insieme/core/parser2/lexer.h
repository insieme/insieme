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

#include <cassert>
#include <string>
#include <memory>
#include <vector>

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace parser {

	using std::string;
	using std::vector;

	class Token;

	/**
	 * The main function provided by this header file. Given a string
	 * containing a program code (fragment), it will be converted into
	 * a list of tokens.
	 */
	vector<Token> lex(const std::string& code);


	// -- Token Types ----------------------------------------



	class Token : public utils::Printable {

	public:

		enum Type {
			Symbol = 1,
			Identifier,
			Bool_Literal,
			Int_Literal,
			Float_Literal,
			Double_Literal,
			Char_Literal,
			String_Literal
		};

	private:

		Type type;

		string lexeme;

		Token(Type type, const string& lexeme)
			: type(type), lexeme(lexeme) {}

	public:

		Token() : type(Symbol), lexeme("?") {}

		// Getter:

		Type getType() const {
			return type;
		}

		const string& getLexeme() const {
			return lexeme;
		}

		// Factory Functions:

		static Token createSymbol(char lexeme) {
			return Token(Symbol, string(&lexeme, 1));
		}

		static Token createIdentifier(const string& lexeme) {
			assert(!lexeme.empty());
			return Token(Identifier, lexeme);
		}

		static Token createLiteral(Type type, const string& lexeme) {
			assert(!lexeme.empty());
			assert(Bool_Literal <= type && type <= String_Literal);
			return Token(type, lexeme);
		}

		// Operator:

		bool operator==(const Token& other) const {
			return type == other.type && lexeme == other.lexeme;
		}

		bool operator==(char symbol) const {
			return type == Symbol && lexeme[0] == symbol;
		}

		bool operator!=(const Token& other) const {
			return !(*this == other);
		}

		bool operator!=(char symbol) const {
			return !(*this == symbol);
		}

		bool operator<(const Token& other) const {
			return type < other.type || (type == other.type && lexeme < other.lexeme);
		}

		operator const string&() const {
			return lexeme;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const;

	};

	std::ostream& operator<<(std::ostream& out, const Token::Type& type);



} // end namespace parser
} // end namespace core
} // end namespace insieme

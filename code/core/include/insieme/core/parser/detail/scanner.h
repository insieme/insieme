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

#pragma once

#include <vector>
#include <sstream>

// flex lexer base class
#undef yyFlexLexer
#define yyFlexLexer Trick_Lexer
#include <FlexLexer.h>

// these file is generated and the path is provided
#include "location.hh"
#include "inspire_parser.hpp"

namespace insieme {
namespace core {
namespace parser {
	namespace detail {

		class InspireDriver;

		/**
		 *  the scanner wrapper is an interface to implement differen inputs for the scanner
		 */
		class InspireScanner : public Trick_Lexer {
			location loc;

			InspireParser::symbol_type typeToken;
			InspireParser::symbol_type expressionToken;
			InspireParser::symbol_type statementToken;
			InspireParser::symbol_type programToken;

			InspireParser::symbol_type* startToken;

		  public:
			InspireScanner(std::istream* stream)
			    : Trick_Lexer(stream), typeToken(InspireParser::make_TYPE_ONLY(loc)), expressionToken(InspireParser::make_EXPR_ONLY(loc)),
			      statementToken(InspireParser::make_STMT_ONLY(loc)), programToken(InspireParser::make_FULL_PROG(loc)) {
				loc.initialize();
			}

			// workaround since generated code is difficult to adjust
			#ifdef __clang__
			#pragma clang diagnostic push
			#pragma clang diagnostic ignored "-Woverloaded-virtual"
			#endif

			InspireParser::symbol_type yylex(InspireDriver& driver);

			#ifdef __clang__
			#pragma clang diagnostic pop
			#endif

			void setStartProgram() {
				startToken = &programToken;
			}
			void setStartStatement() {
				startToken = &statementToken;
			}
			void setStartExpression() {
				startToken = &expressionToken;
			}
			void setStartType() {
				startToken = &typeToken;
			}

			int yywrap() {
				return 1;
			}

			virtual ~InspireScanner() {}
		};

		/**
		 * Bison-Flex interaction in c++ mode is not very neat:
		 * Bison will call a function yylex, and to keep an unique instance of the scanner, we pass it by argument
		 * this function bridges the bison flex interaction.
		 */
		InspireParser::symbol_type yylex(InspireDriver& driver, InspireScanner& scanner);

	} // namespace detail
} // namespace parser
} // namespace core
} // namespace insieme

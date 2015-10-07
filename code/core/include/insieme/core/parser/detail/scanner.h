/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

			#undef YY_DECL
			#define YY_DECL InspireParser::symbol_type InspireScanner::yylex(InspireDriver& driver)
			InspireParser::symbol_type yylex(InspireDriver& driver);

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

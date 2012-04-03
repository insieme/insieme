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

#include <boost/tokenizer.hpp>

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace parser2 {

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
	typedef Tokenizer::iterator TokenIter;


} // end namespace parser2
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/parser2/old/types.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"


namespace insieme {
namespace core {
namespace parser2 {


	TypePtr parseType(NodeManager& manager, const string& type) {

		// create tokenizer and use base implementation
		Tokenizer tok(type);

		TokenIter begin = tok.begin();
		TokenIter end = tok.end();
		TypePtr res = parseType(manager, begin, end);

		if (res && begin == end) {
			return res;
		}
		return TypePtr();
	}

	namespace {

		TypePtr parseTupleOrFunction(NodeManager& manager, TokenIter& cur, const TokenIter& end) {
			assert(*cur == "(" && "Not a valid start for a tuple or function type!");
			IRBuilder builder(manager);

			// start collecting element types
			TypeList list;
			cur++;
			while(cur != end && *cur != ")") {
				list.push_back(parseType(manager, cur, end));
				if (*cur == ",") cur++;
			}
			assert(*cur == ")");	// TODO: add exceptions!
			cur++;

			// determine whether it is a tuple or a function
			if (cur == end || (*cur != "-" && *cur != "=")) {
				// it is a tuple
				return builder.tupleType(list);
			}

			assert(*cur == "-" || *cur == "=");
			bool plain = *cur == "-";

			cur++;
			assert(*cur == ">");

			// in this case it is a function type
			return builder.functionType(list, parseType(manager, ++cur, end), plain);
		}

		GenericTypePtr parseGenericType(NodeManager& manager, TokenIter& cur, const TokenIter& end) {
			assert(cur != end);
			IRBuilder builder(manager);

			// parameters to be collected
			string name;
			TypeList typeParams;
			IntParamList intParams;

			// get name
			name = *cur;

			// collect parameters
			cur++;
			if (cur != end && *cur == "<") {
				// collect type parameters
				cur++;
				while(cur != end && *cur != ">") {
					typeParams.push_back(parseType(manager, cur, end));
					if (*cur == ",") cur++;
				}
				cur++;
			}

			// build result
			return builder.genericType(name, typeParams, intParams);
		}

		TypeVariablePtr parseTypeVariable(NodeManager& manager, TokenIter& cur, const TokenIter& end) {
			assert(cur != end && *cur == "'");
			cur++;
			assert(cur != end);		// TODO: add exception
			return IRBuilder(manager).typeVariable(*(cur++));
		}

	}


	TypePtr parseType(NodeManager& manager, TokenIter& cur, const TokenIter& end) {

		// check whether full stream has been consumed
		if (cur == end) {
			return TypePtr();
		}

		// determine procedure based on first token
		if (*cur == "(") {								// might be a tuple or function type
			return parseTupleOrFunction(manager, cur, end);
		}
		if (*cur == "'") {
			return parseTypeVariable(manager, cur, end);
		}

		// else it is a generic type
		return parseGenericType(manager, cur, end);
	}


} // end namespace parser
} // end namespace core
} // end namespace insieme

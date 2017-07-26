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

#include <map>
#include <vector>
#include "insieme/core/forward_decls.h"


/**
 * This is the facade header file for the IR Parser II, which should be used as a replacing
 * of the old, spirit based parser.
 *
 * This header file should be included by anyone who is using the parser as a simple end-user.
 * More detailed access to internal aspects of the parser can be obtained via the
 *
 * 									detail/grammar.h
 *
 * header - which is supposed to be used whenever the IR grammar needs to be further cutomized.
 */

namespace insieme {
namespace core {
namespace parser {

	/**
	 * A type for an entity producing a node.
	 */
	typedef std::function<NodePtr()> NodeFactory;

	/**
	 * A type defining a mapping form symbol names to factories producing the corresponding IR node.
	 */
	typedef std::map<std::string, NodeFactory> DefinitionMap;

	/**
	 * A type defining a mapping between generic types and type aliases.
	 */
	typedef std::map<GenericTypePtr, TypePtr> TypeAliasMap;

	/**
	 * Parses an IR fragment and creates the corresponding IR DAG using the given node manager. The given
	 * string is interpreted as an arbitrary piece of IR code (could be a statement, expression, type, ...). The
	 * first successful match consuming the full string will be returned. If the given string can not be
	 * successfully parsed to an IR construct, the flag onFailThrow is deciding whether a IRParserException
	 * is thrown or a null-pointer will be returned.
	 *
	 * The parser supports the definition of pre-defined constructs (definitions). Identifiers can be mapped
	 * to some pre-defined constructs. Whenever during the parsing one of those identifiers is encountered, the
	 * corresponding construct is used as a substitute. This, however, is not disabling the default back-tracking
	 * mechanism which will still try to parse the full string even without considering those identifiers.
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment to be parsed
	 * @param onFailThrow a flag determining whether a parsing error should result in a null-pointer (false) or a Parsing Exception (true)
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return the parsed construct or null if the parsing was not successful and onFailThrow was set to false
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	NodePtr parseAny(NodeManager& manager, const string& code, bool onFailThrow = false,
	                 const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function focusing specifically on types.
	 * Using the specialized variant
	 * can help avoiding ambiguities at the top-level of the Grammar.
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment describing the type to be parsed
	 * @param onFailThrow a flag determining whether a parsing error should result in a null-pointer (false) or a Parsing Exception (true)
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return the parsed construct or null if the parsing was not successful and onFailThrow was set to false
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	TypePtr parseType(NodeManager& manager, const string& code, bool onFailThrow = false,
	                  const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function focusing specifically on expressions. Using the specialized variant
	 * can help avoiding ambiguities at the top-level of the Grammar.
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment describing the expression to be parsed
	 * @param onFailThrow a flag determining whether a parsing error should result in a null-pointer (false) or a Parsing Exception (true)
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return the parsed construct or null if the parsing was not successful and onFailThrow was set to false
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	ExpressionPtr parseExpr(NodeManager& manager, const string& code, bool onFailThrow = false,
	                        const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function which is focusing specifically on statements. Using the specialized variant
	 * can help avoiding ambiguities at the top-level of the Grammar.
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment describing the statement to be parsed
	 * @param onFailThrow a flag determining whether a parsing error should result in a null-pointer (false) or a Parsing Exception (true)
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return the parsed construct or null if the parsing was not successful and onFailThrow was set to false
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	StatementPtr parseStmt(NodeManager& manager, const string& code, bool onFailThrow = false,
	                       const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function which is focusing specifically on full applications. Using the specialized variant
	 * can help avoiding ambiguities at the top-level of the Grammar.
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment encoding the full program to be parsed
	 * @param onFailThrow a flag determining whether a parsing error should result in a null-pointer (false) or a Parsing Exception (true)
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return the parsed construct or null if the parsing was not successful and onFailThrow was set to false
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	ProgramPtr parseProgram(NodeManager& manager, const string& code, bool onFailThrow = false,
	                        const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	* A specialized version of the general parse function which is considering labels within the IR marking constructs
	* which's addresses should be returned by the parsing process. Expressions within the given type can be marked using $ .. $ symbols. For Instance,
	* parsing 1 + $2 * $3$$ will return a list of addresses referencing the 2*3 part and the 3. The addresses in the
	* resulting vector will be order according to their natural order (lexicographically).
	*
	* @param manager the manager to be used for creating the resulting IR DAG
	* @param code the code fragment to be parsed, including marked locations.
	* @param onFailThrow a flag determining whether a parsing error should result in an empty list or a Parsing Exception
	* @param definitions a map of pre-defined symbols
	* @param aliases a map of pre-defined type aliases
	* @return a list of all addresses referencing marked sub-constructs within the parsed IR or an empty list of the parsing failed and onFailThrow was not set
	* @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	*/
	std::vector<NodeAddress> parseAddressesType(NodeManager& manager, const string& code, bool onFailThrow = false,
		const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function which is considering labels within the IR marking constructs
	 * which's addresses should be returned by the parsing process. Expressions can be marked using $ .. $ symbols. For Instance,
	 * parsing 1 + $2 * $3$$ will return a list of addresses referencing the 2*3 part and the 3. The addresses in the
	 * resulting vector will be order according to their natural order (lexicographically).
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment to be parsed, including marked locations.
	 * @param onFailThrow a flag determining whether a parsing error should result in an empty list or a Parsing Exception
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return a list of all addresses referencing marked sub-constructs within the parsed IR or an empty list of the parsing failed and onFailThrow was not set
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	std::vector<NodeAddress> parseAddressesExpression(NodeManager& manager, const string& code, bool onFailThrow = false,
	                                                 const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * A specialized version of the general parse function which is considering labels within the IR marking constructs
	 * which's addresses should be returned by the parsing process. Expressions can be marked using $ .. $ symbols. For Instance,
	 * parsing 1 + $2 * $3$$ will return a list of addresses referencing the 2*3 part and the 3. The addresses in the
	 * resulting vector will be order according to their natural order (lexicographically).
	 *
	 * @param manager the manager to be used for creating the resulting IR DAG
	 * @param code the code fragment to be parsed, including marked locations.
	 * @param onFailThrow a flag determining whether a parsing error should result in an empty list or a Parsing Exception
	 * @param definitions a map of pre-defined symbols
	 * @param aliases a map of pre-defined type aliases
	 * @return a list of all addresses referencing marked sub-constructs within the parsed IR or an empty list of the parsing failed and onFailThrow was not set
	 * @throw an IRParserException if the parsing failed and the onFailThrow flag was set; the Exception tries to explain the reason for the parsing error.
	 */
	std::vector<NodeAddress> parseAddressesStatement(NodeManager& manager, const string& code, bool onFailThrow = false,
	                                                 const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * equivalent to parse_addresses_statement but parsing a program, this avoids ambiguity in the grammar, allows us to use a simpler parser
	 */
	std::vector<NodeAddress> parseAddressesProgram(NodeManager& manager, const string& code, bool onFailThrow = false,
	                                               const DefinitionMap& definitions = DefinitionMap(), const TypeAliasMap& aliases = TypeAliasMap());

	/**
	 * The type of exception to be thrown by the parser in case the parsing was not successful.
	 */
	class IRParserException : public std::exception {
		/**
		 * A message trying to describe the potential reason for the failure of
		 * the parsing. It might help resolving the problem (do not expect too much).
		 */
		string msg;

	  public:
		/**
		 * A simple constructor being called with a message describing the
		 * potential reason for the failed parsing process.
		 */
		IRParserException(const string& msg) : msg(msg) {}

		/**
		 * A virtual destructor - actually inherited from std::exception - promising to not
		 * throw an exception (as required for all exceptions).
		 */
		virtual ~IRParserException() throw(){};

		/**
		 * Obtains a reference to the message describing the problem.
		 */
		const string& getMessage() const {
			return msg;
		}

		/**
		 * Obtains a reference to the explanation stored within this exception.
		 */
		virtual const char* what() const throw() {
			return msg.c_str();
		}
	};

} // end namespace parser
} // end namespace core
} // end namespace insieme

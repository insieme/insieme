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

#include "insieme/core/ir_parse.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

// ----------------------- SPIRIT QI/PHOENIX survival hints
// What to do if
// - error: invalid initialization ... --> check whether ph::ref is used to pass references
//									   --> check if you're using the right placeholders (char_ counts as auto attrib, literals and plain characters do NOT)
// - error: template substitution failure --> check potential ambiguities, try supplying default parameters explicitly
// - error: some operator can not be applied (eg =) --> attribute type may be different from expected
// - error: invalid use of void expression --> trying to do a ph::ref of a reference? Placeholders are already references!
// - other: use phoenix::bind and phoenix::construct instead of plain calls
// ----------------------- - Peter

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

namespace {
	Identifier makeId(char start, const vector<char>& tail) {
		return Identifier(string(&start, &start+1) + string(tail.begin(), tail.end()));
	}
	Identifier makePrefixId(char start, const Identifier& ident) {
		return Identifier(string(&start, &start+1) + ident.getName());
	}
}

IRParser::IRParser(NodeManager& nodeMan) {

	auto nManRef = ph::ref(nodeMan);

	// RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

	identifier = 
		( ascii::alpha >> *qi::char_("a-zA-Z_0-9") )				[ qi::_val = ph::bind(&makeId, qi::_1, qi::_2) ];
	
	typeVarLabel =
		( qi::char_('\'') >> identifier )							[ qi::_val = ph::bind(&TypeVariable::getFromId, nManRef, qi::_2) ];

	intTypeParamLabel = 
		( qi::char_('#') >> qi::char_ )								[ qi::_val = ph::bind(&IntTypeParam::getVariableIntParam, qi::_2) ];

	typeVariable =
		typeRule													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ];

	intTypeParam =
		qi::uint_													[ qi::_val = ph::bind(&IntTypeParam::getConcreteIntParam, qi::_1) ]
		| qi::lit("#inf")											[ qi::_val = ph::bind(&IntTypeParam::getInfiniteIntParam) ]
		| intTypeParamLabel;

	arrayType =
		( qi::lit("array<") >> typeRule								[ qi::_a = qi::_1 ]
		>> ( ',' >> intTypeParam									[ qi::_b = qi::_1 ] 
		) >> '>' )													[ qi::_val = ph::bind(&ArrayType::get, nManRef, qi::_a, qi::_b) ];

	vectorType =
		( qi::lit("vector<") >> typeRule 
		>> ',' >> intTypeParam >> '>' )								[ qi::_val = ph::bind(&VectorType::get, nManRef, qi::_1, qi::_2) ];

	refType =
		( qi::lit("ref<") >> typeRule >> '>' )						[ qi::_val = ph::bind(&RefType::get, nManRef, qi::_1) ];

	channelType =
		( qi::lit("channel<") >> typeRule 
		>> ',' >> intTypeParam >> '>' )								[ qi::_val = ph::bind(&ChannelType::get, nManRef, qi::_1, qi::_2) ];

	genericType =
		( identifier												[ qi::_a = qi::_1 ]
		>> -( '<' >> -( typeVariable								[ ph::push_back(qi::_b, qi::_1) ]
		% ',' )
		>> -qi::char_(',') >> -( intTypeParam						[ ph::push_back(qi::_c, qi::_1) ]
		% ',' )
		>> '>') )													[ qi::_val = ph::bind(&GenericType::get, nManRef, qi::_a, qi::_b, qi::_c, TypePtr()) ];

	tupleType =
		( qi::char_('(') >> -( typeRule								[ ph::push_back(qi::_a, qi::_1) ]
		% ',' ) >> ')' )											[ qi::_val = ph::bind(&TupleType::get, nManRef, qi::_a) ];

	functionType =
		( tupleType >> qi::lit("->") >> typeRule ) 					[ qi::_val = ph::bind(&FunctionType::get, nManRef, qi::_1, qi::_2) ];

	structType =
		( qi::lit("struct<") >> (( identifier >> ':' >> typeRule )	[ ph::push_back(qi::_a, ph::construct<std::pair<Identifier,TypePtr>>(qi::_1, qi::_2)) ]
		% ',' ) >> '>' )											[ qi::_val = ph::bind(&StructType::get, nManRef, qi::_a) ];

	unionType =
		( qi::lit("union<") >> (( identifier >> ':' >> typeRule )	[ ph::push_back(qi::_a, ph::construct<std::pair<Identifier,TypePtr>>(qi::_1, qi::_2)) ]
		% ',' ) >> '>' )											[ qi::_val = ph::bind(&UnionType::get, nManRef, qi::_a) ];

	typeRule = 
		functionType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| (qi::lit("(|") >> typeRule >> qi::lit("|)"))				[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| arrayType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| vectorType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| refType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| channelType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| tupleType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| structType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| unionType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| genericType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ];

	//BOOST_SPIRIT_DEBUG_NODE(typeRule);
	//BOOST_SPIRIT_DEBUG_NODE(typeDefinition);
	//BOOST_SPIRIT_DEBUG_NODE(functionType);
	//BOOST_SPIRIT_DEBUG_NODE(genericType);
	//BOOST_SPIRIT_DEBUG_NODE(intTypeParam);
	//BOOST_SPIRIT_DEBUG_NODE(typeVariable);
	//BOOST_SPIRIT_DEBUG_NODE(typeLabel);
	//BOOST_SPIRIT_DEBUG_NODE(identifier);
}


TypePtr IRParser::parseType(const std::string& input) {
	TypePtr result;
	auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = qi::phrase_parse(startIt, endIt, typeRule, qi::space, result);
	parse_result = parse_result && (startIt == endIt);
	if(!parse_result) throw ParseException();
	return result;
}

TypePtr parseType(NodeManager& nodeMan, const string& input) {
	IRParser parser(nodeMan);
	return parser.parseType(input);
}

} // namespace parse 
} // namespace core
} // namespace insieme

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

#include <sstream>
#include <stdlib.h>


#include <gtest/gtest.h>

#define BOOST_SPIRIT_DEBUG

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/include/at_c.hpp>

// ----------------------- SPIRIT QI/PHOENIX survival hints
// What to do if
// - error: invalid initialization ... --> check whether ph::ref is used to pass references
//									   --> check if you're using the right placeholders (char_ counts as auto attrib, literals and plain characters do NOT)
// - error: template substitution failure --> check potential ambiguities, try supplying default parameters explicitly
// - error: some operator can not be applied (eg =) --> attribute type may be different from expected
// - error: invalid use of void expression --> trying to do a ph::ref of a reference? Placeholders are already references!
// - other: use phoenix::bind and phoenix::construct instead of plain calls
// ----------------------- - Peter


#include "identifier.h"
#include "types.h"
#include "ir_parse.h"
#include "ast_builder.h"

#include "string_utils.h"

using namespace insieme::core;
using namespace insieme::core::parse;


Identifier makeId(char start, const vector<char>& tail) {
	return Identifier(string(&start, &start+1) + string(tail.begin(), tail.end()));
}
Identifier makePrefixId(char start, const Identifier& ident) {
	return Identifier(string(&start, &start+1) + ident.getName());
}


//IntTypeParam makeIntTypeParam(unsigned int num) {
//	IntTypeParam::getConcreteIntParam(num);
//}
//IntTypeParam makeIntTypeParam(char id) {
//	IntTypeParam::getVariableIntParam(id);
//}
//IntTypeParam makeIntTypeParamInf() {
//	IntTypeParam::getInfiniteIntParam();
//}

//struct TypeWrapper {
//	std::shared_ptr<TypePtr> typ;
//
//public:
//	TypeWrapper() {
//	}
//};
//std::ostream& operator<<(std::ostream& os, const TypeWrapper& tw) {
//	os << "TypeWrapper(" << &tw << ")";
//	return os;
//}

//class TypeDefinition {
//	Identifier label;
//	TypeWrapper type;
//
//public:
//	TypeDefinition(const Identifier& label, const TypeWrapper& type) : label(label), type(type) {
//
//	}
//};

//class TypeVariableWrapper {
//	std::shared_ptr<TypeVariable> v;
//
//public:
//	TypeVariableWrapper() {
//	}
//
//	TypeVariableWrapper(const TypeWrapper& tw) {
//	}
//
//	TypeVariableWrapper(const char& c) {
//	}
//
//	TypeVariableWrapper(const Identifier& id) {
//	}
//};


//class IntTypeParamWrapper {
//	std::shared_ptr<IntTypeParam> v;
//
//public:
//	IntTypeParamWrapper() {
//		v = std::make_shared<IntTypeParam>(IntTypeParam::getInfiniteIntParam());
//	}
//
//	IntTypeParamWrapper(const char& c) {
//		v = std::make_shared<IntTypeParam>(IntTypeParam::getVariableIntParam(c));
//	}
//
//	IntTypeParamWrapper(const unsigned val) {
//		v = std::make_shared<IntTypeParam>(IntTypeParam::getConcreteIntParam(val));
//	}
//};

namespace std {

	//ostream& operator<<(ostream& os, const TypeDefinition& tw) {
	//	os << "TypeDefinition(" << &tw << ")";
	//	return os;
	//}

	//ostream& operator<<(ostream& os, const TypeVariableWrapper& tw) {
	//	os << "TypeVariableWrapper(" << &tw << ")";
	//	return os;
	//}

	//ostream& operator<<(ostream& os, const IntTypeParamWrapper& tw) {
	//	os << "IntTypeParamWrapper(" << &tw << ")";
	//	return os;
	//}

	ostream& operator<<(ostream& os, const Identifier& tw) {
		os << "Identifier(" << &tw << ")";
		return os;
	}
}

//namespace {
//	IntTypeParam buildVarITP(const char symbol) { return IntTypeParam::getVariableIntParam(symbol); }
//	IntTypeParam buildConcreteITP(const unsigned num) { return IntTypeParam::getConcreteIntParam(num); }
//	IntTypeParam buildInfITP() { return IntTypeParam::getInfiniteIntParam(); }
//}

TEST(IRParser, RuleTest) {
	//IRParser parser;

//	string identifierTestPass("  adasdjk_232");
//	string identifierTestFail("=45");
	
	//qi::phrase_parse()
	//Identifier parsed;
//	bool parse_result = qi::phrase_parse(identifierTestPass.begin(), identifierTestPass.end(), 
//		(ascii::alpha >> +ascii::graph)[ qi::_val = Identifier(string(qi::_2.begin(), qi::_2.end())); ], qi::skip, &parsed);
//		
	

	NodeManager nMan;
	auto nManRef = ph::ref(nMan);

	typedef string::iterator IT;

	// terminals, no skip parsing
	qi::rule<IT, Identifier()> identifier;
	//qi::rule<IT, Identifier()> typeLabel;
	qi::rule<IT, TypePtr()> typeVarLabel;
	qi::rule<IT, IntTypeParam()> intTypeParamLabel;

	// nonterminals with skip parsers
	qi::rule<IT, TypePtr(), qi::space_type> typeRule;
	qi::rule<IT, TypePtr(), qi::space_type> typeVariable;
	qi::rule<IT, IntTypeParam(), qi::space_type> intTypeParam;
	qi::rule<IT, ArrayTypePtr(), qi::locals<TypePtr, IntTypeParam>, qi::space_type> arrayType;
	qi::rule<IT, VectorTypePtr(), qi::space_type> vectorType;
	qi::rule<IT, RefTypePtr(), qi::space_type> refType;
	qi::rule<IT, ChannelTypePtr(), qi::space_type> channelType;
	qi::rule<IT, TupleTypePtr(), qi::locals<vector<TypePtr>>, qi::space_type> tupleType;
	qi::rule<IT, TypePtr(), qi::locals<Identifier, vector<TypePtr>, vector<IntTypeParam>>, qi::space_type> genericType;
	qi::rule<IT, TypePtr(), qi::space_type> functionType;
	qi::rule<IT, StructTypePtr(), qi::locals<StructType::Entries>, qi::space_type> structType;
	qi::rule<IT, UnionTypePtr(), qi::locals<UnionType::Entries>, qi::space_type> unionType;

	identifier = 
		( ascii::alpha >> *qi::char_("a-zA-Z_0-9") )				[ qi::_val = ph::bind(&makeId, qi::_1, qi::_2) ];
	
	//typeLabel =
	//	( qi::char_('$') >> identifier )							[ qi::_val = ph::bind(&makePrefixId, qi::_1, qi::_2) ];

	typeVarLabel =
		( qi::char_('\'') >> identifier )							[ qi::_val = ph::bind(&TypeVariable::getFromId, nManRef, qi::_2) ];

	intTypeParamLabel = 
		( qi::char_('#') >> qi::char_ )								[ qi::_val = ph::bind(&IntTypeParam::getVariableIntParam, qi::_2) ];

	typeVariable =
	//	typeLabel													[ qi::_val = ph::bind(&TypeVariable::getFromId, nManRef, qi::_1) /* lookup type */ ]
		typeRule													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ];

	intTypeParam =
		qi::uint_													[ qi::_val = ph::bind(&IntTypeParam::getConcreteIntParam, qi::_1) ]
	  | qi::lit("#inf")												[ qi::_val = ph::bind(&IntTypeParam::getInfiniteIntParam) ]
	  | intTypeParamLabel;
	
	arrayType =
		( qi::lit("array<") >> typeRule								[ qi::_a = qi::_1 ]
	  >> ( ',' >> intTypeParam										[ qi::_b = qi::_1 ] )
	  >> '>' )														[ qi::_val = ph::bind(&ArrayType::get, nManRef, qi::_a, qi::_b) ];

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
	  >> -( '<' >> -( typeVariable									[ ph::push_back(qi::_b, qi::_1) ]
	  % ',' )
	  >> -qi::char_(',') >> -( intTypeParam							[ ph::push_back(qi::_c, qi::_1) ]
	  % ',' )
	  >> '>') )														[ qi::_val = ph::bind(&GenericType::get, nManRef, qi::_a, qi::_b, qi::_c, TypePtr()) ];

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
	  functionType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | (qi::char_('(') >> typeRule >> qi::char_(')'))				[ qi::_val = ph::construct<TypePtr>(qi::_2) ]
	  | arrayType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | vectorType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | refType														[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | channelType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | tupleType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | structType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | unionType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
	  | genericType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ];
	//		| typeLabel;

	//qi::rule<IT, TypeDefinition()> typeDefinition = 
	//	(qi::lit("define") >> typeLabel >> ':' >> typeRule)			[ qi::_val = ph::construct<TypeDefinition>(qi::_1, qi::_2) ];


	//BOOST_SPIRIT_DEBUG_NODE(typeRule);
	//BOOST_SPIRIT_DEBUG_NODE(typeDefinition);
	//BOOST_SPIRIT_DEBUG_NODE(functionType);
	//BOOST_SPIRIT_DEBUG_NODE(genericType);
	//BOOST_SPIRIT_DEBUG_NODE(intTypeParam);
	//BOOST_SPIRIT_DEBUG_NODE(typeVariable);
	//BOOST_SPIRIT_DEBUG_NODE(typeLabel);
	//BOOST_SPIRIT_DEBUG_NODE(identifier);

	string testStr("testGenType");
	if(getenv("IR_PARSE_STR")) testStr = string(getenv("IR_PARSE_STR"));
	TypePtr result;
	auto startIt = testStr.begin();
	bool parse_result = qi::phrase_parse(startIt, testStr.end(), typeRule, qi::space, result);
	parse_result = parse_result && (startIt == testStr.end());

	std::cout << "--------------------------------------\n" <<(parse_result?"true":"false") << std::endl
		<< "result: \n" << result << std::endl << "--------------------------------------\n";
}
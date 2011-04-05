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

#include "insieme/core/parser/type_parse.h"

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

template<typename T>
IdentifierPtr TypeGrammar<T>::identifierHelp(char start, const vector<char>& tail) {
    return Identifier::get(nodeMan, string(&start, &start+1) + string(tail.begin(), tail.end()));
}
//	IdentifierPtr makePrefixId(NodeManager& manager, char start, const IdentifierPtr& ident) {
//		return Identifier::get(manager, string(&start, &start+1) + ident->getName());
//	}

template<typename T>
T TypeGrammar<T>::typeVarLabelHelp(const IdentifierPtr& id) {
    return TypeVariable::getFromId(nodeMan, id);
}

template<typename T>
IntTypeParamPtr TypeGrammar<T>::intTypeParamLabelHelp(const char symbol) {
    return VariableIntTypeParam::get(nodeMan, symbol);
}

template<typename T>
void TypeGrammar<T>::typeVariableHelp(const T& type) {
    boost::phoenix::actor<boost::phoenix::composite<boost::phoenix::detail::construct_eval<insieme::core::Pointer<const insieme::core::Type> >, boost::fusion::vector<boost::phoenix::value<insieme::core::Pointer<const insieme::core::Type> >, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_> > > x = ph::construct<T>(type);
}

template<typename T>
IntTypeParamPtr TypeGrammar<T>::concreteTypeParamHelp(const size_t value) {
    return ConcreteIntTypeParam::get(nodeMan, value);
}

template<typename T>
IntTypeParamPtr TypeGrammar<T>::infiniteTypeParamHelp() {
    return InfiniteIntTypeParam::get(nodeMan);
}

template<typename T>
T TypeGrammar<T>::arrayTypeHelp(const T& type, const IntTypeParamPtr& nDims) {
    return ArrayType::get(nodeMan, type, nDims);
}

template<typename T>
T TypeGrammar<T>::vectorTypeHelp(const T& type, const IntTypeParamPtr& nElems) {
    return VectorType::get(nodeMan, type, nElems);
}

template<typename T>
T TypeGrammar<T>::refTypeHelp(const T& type) {
    return RefType::get(nodeMan, type);
}

template<typename T>
T TypeGrammar<T>::channelTypeHelp(const T& type, const IntTypeParamPtr& size) {
    return ChannelType::get(nodeMan, type, size);
}

template<typename T>
T TypeGrammar<T>::genericTypeHelp(const IdentifierPtr& name, const vector<T>& typeParams, const vector<IntTypeParamPtr>& intTypeParams, const T& baseType) {
    return GenericType::getFromID(nodeMan, name, typeParams, intTypeParams, baseType);
}

template<typename T>
T TypeGrammar<T>::tupleTypeHelp(const vector<T>& types) {
    return TupleType::get(nodeMan, types);
}

template<typename T>
T TypeGrammar<T>::functionTypeHelp(const vector<T>& argTypes, const T& retType) {
    return FunctionType::get(nodeMan, argTypes, retType);
}

template<typename T>
T TypeGrammar<T>::structTypeHelp(const vector<std::pair<IdentifierPtr,T> >& entries) {
    return StructType::get(nodeMan, entries);
}

template<typename T>
T TypeGrammar<T>::unionTypeHelp(const vector<std::pair<IdentifierPtr,T> >& entries) {
    return UnionType::get(nodeMan, entries);
}


template<typename T>
TypeGrammar<T>::TypeGrammar(NodeManager& nMan) : TypeGrammar::base_type(typeRule), nodeMan(nMan) {

	auto nManRef = ph::ref(nodeMan);

	// RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

	// terminals, no skip parser

	identifier = 
		( ascii::alpha >> *qi::char_("a-zA-Z_0-9") )				[ qi::_val = ph::bind(&TypeGrammar<T>::identifierHelp, this, qi::_1, qi::_2) ];
	
	typeVarLabel =
		( qi::char_('\'') >> identifier )							[ qi::_val = ph::bind(&TypeGrammar<T>::typeVarLabelHelp, this, qi::_2) ];

	intTypeParamLabel = 
		( qi::char_('#') >> qi::char_ )								[ qi::_val = ph::bind(&TypeGrammar<T>::intTypeParamLabelHelp, this, qi::_2) ];

	// nonterminals, skip parser

	typeVariable =
		typeRule													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ];

	intTypeParam =
		qi::uint_													[ qi::_val = ph::bind(&TypeGrammar<T>::concreteTypeParamHelp, this, qi::_1) ]
		| qi::lit("#inf")											[ qi::_val = ph::bind(&TypeGrammar<T>::infiniteTypeParamHelp, this) ]
		| intTypeParamLabel											[ qi::_val = qi::_1 ];

	arrayType =
		( qi::lit("array<") >> typeRule								[ qi::_a = qi::_1 ]
		>> ',' >> intTypeParam										[ qi::_b = qi::_1 ] 
		>> '>' )													[ qi::_val = ph::bind(&TypeGrammar<T>::arrayTypeHelp, this, qi::_a, qi::_b) ];

	vectorType =
		( qi::lit("vector<") >> typeRule 
		>> ',' >> intTypeParam >> '>' )								[ qi::_val = ph::bind(&TypeGrammar<T>::vectorTypeHelp, this, qi::_1, qi::_2) ];

	refType =
		( qi::lit("ref<") >> typeRule >> '>' )						[ qi::_val = ph::bind(&TypeGrammar<T>::refTypeHelp, this, qi::_1) ];

	channelType =
		( qi::lit("channel<") >> typeRule 
		>> ',' >> intTypeParam >> '>' )								[ qi::_val = ph::bind(&TypeGrammar<T>::channelTypeHelp, this, qi::_1, qi::_2) ];

	genericType =
		( identifier												[ qi::_a = qi::_1 ]
		>> -( '<' >> -( typeVariable								[ ph::push_back(qi::_b, qi::_1) ]
		% ',' )
		>> -qi::char_(',') >> -( intTypeParam						[ ph::push_back(qi::_c, qi::_1) ]
		% ',' )
		>> '>') )													[ qi::_val = ph::bind(&TypeGrammar<T>::genericTypeHelp, this, qi::_a, qi::_b, qi::_c, TypePtr()) ];

	tupleType =
		( qi::char_('(') >> -( typeRule								[ ph::push_back(qi::_a, qi::_1) ]
		% ',' ) >> ')' )											[ qi::_val = ph::bind(&TypeGrammar<T>::tupleTypeHelp, this, qi::_a) ];

	functionType =
		( qi::lit("(") >> -( typeRule                               [ ph::push_back(qi::_a, qi::_1) ]
		% ',' ) >> ')' >> 
		qi::lit("->") >> typeRule									[ qi::_b = qi::_1 ]
		)															[ qi::_val = ph::bind(&TypeGrammar<T>::functionTypeHelp, this, qi::_a, qi::_b) ];


	structType =
		( qi::lit("struct<") >> (( identifier >> ':' >> typeRule )	[ ph::push_back(qi::_a, ph::construct<std::pair<IdentifierPtr,TypePtr>>(qi::_1, qi::_2)) ]
		% ',' ) >> '>' )											[ qi::_val = ph::bind(&TypeGrammar<T>::structTypeHelp, this, qi::_a) ];

	unionType =
		( qi::lit("union<") >> (( identifier >> ':' >> typeRule )	[ ph::push_back(qi::_a, ph::construct<std::pair<IdentifierPtr,TypePtr>>(qi::_1, qi::_2)) ]
		% ',' ) >> '>' )											[ qi::_val = ph::bind(&TypeGrammar<T>::unionTypeHelp, this, qi::_a) ];

	typeRule = 
		functionType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| arrayType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| vectorType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| refType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| channelType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| tupleType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| typeVarLabel												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| structType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| unionType													[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| genericType												[ qi::_val = ph::construct<TypePtr>(qi::_1) ]
		| (qi::lit("(|") >> typeRule >> qi::lit("|)"))				[ qi::_val = ph::construct<TypePtr>(qi::_1) ];

	// debugging
	//BOOST_SPIRIT_DEBUG_NODE(identifier);
	//BOOST_SPIRIT_DEBUG_NODE(typeVarLabel);
	//BOOST_SPIRIT_DEBUG_NODE(intTypeParamLabel);
	//BOOST_SPIRIT_DEBUG_NODE(functionType);
	//BOOST_SPIRIT_DEBUG_NODE(typeVariable);
	//BOOST_SPIRIT_DEBUG_NODE(intTypeParam);
	//BOOST_SPIRIT_DEBUG_NODE(refType);
	//BOOST_SPIRIT_DEBUG_NODE(channelType);
	//BOOST_SPIRIT_DEBUG_NODE(vectorType);
	//BOOST_SPIRIT_DEBUG_NODE(arrayType);
	//BOOST_SPIRIT_DEBUG_NODE(tupleType);
	//BOOST_SPIRIT_DEBUG_NODE(structType);
	//BOOST_SPIRIT_DEBUG_NODE(unionType);
	//BOOST_SPIRIT_DEBUG_NODE(genericType);
	//BOOST_SPIRIT_DEBUG_NODE(typeRule);
}

// explicit template instantiation
template struct TypeGrammar<TypePtr>;

} // namespace parse 
} // namespace core
} // namespace insieme


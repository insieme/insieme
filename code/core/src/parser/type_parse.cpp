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

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
IdentifierPtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::identifierHelp(char start, const vector<char>& tail) {
    return Identifier::get(nodeMan, string(&start, &start+1) + string(tail.begin(), tail.end()));
}
//	IdentifierPtr makePrefixId(NodeManager& manager, char start, const IdentifierPtr& ident) {
//		return Identifier::get(manager, string(&start, &start+1) + ident->getName());
//	}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::typeVarLabelHelp(const IdentifierPtr& id) {
    return TypeVariable::getFromId(nodeMan, id);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
IntTypeParamPtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::intTypeParamLabelHelp(const char symbol) {
    return VariableIntTypeParam::get(nodeMan, symbol);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::typeVariableHelp(const TypePtr& type) {
//    boost::phoenix::actor<boost::phoenix::composite<boost::phoenix::detail::construct_eval<insieme::core::Pointer<const insieme::core::Type> >, boost::fusion::vector<boost::phoenix::value<insieme::core::Pointer<const insieme::core::Type> >, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_, boost::fusion::void_> > > x = ph::construct<TypePtr>(type);
    return TypePtr(type);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
IntTypeParamPtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::concreteTypeParamHelp(const size_t value) {
    return ConcreteIntTypeParam::get(nodeMan, value);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
IntTypeParamPtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::infiniteTypeParamHelp() {
    return InfiniteIntTypeParam::get(nodeMan);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::arrayTypeHelp(const TypePtr& type, const IntTypeParamPtr& nDims) {
    return ArrayType::get(nodeMan, type, nDims);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::vectorTypeHelp(const TypePtr& type, const IntTypeParamPtr& nElems) {
    return VectorType::get(nodeMan, type, nElems);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::refTypeHelp(const TypePtr& type) {
    return RefType::get(nodeMan, type);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::channelTypeHelp(const TypePtr& type, const IntTypeParamPtr& size) {
    return ChannelType::get(nodeMan, type, size);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::genericTypeHelp(const IdentifierPtr& name, const vector<TypePtr>& typeParams, const vector<IntTypeParamPtr>& intTypeParams, const TypePtr& baseType) {
    return GenericType::getFromID(nodeMan, name, typeParams, intTypeParams, baseType);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::tupleTypeHelp(const vector<TypePtr>& types) {
    return TupleType::get(nodeMan, types);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::functionTypeHelp(const vector<TypePtr>& argTypes, const TypePtr& retType) {
    return FunctionType::get(nodeMan, argTypes, retType);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::structTypeHelp(const vector<std::pair<IdentifierPtr,TypePtr> >& entries) {
    return StructType::get(nodeMan, entries);
}

template<class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
TypePtr TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>::unionTypeHelp(const vector<std::pair<IdentifierPtr,TypePtr> >& entries) {
    return UnionType::get(nodeMan, entries);
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, V()>  TypeGrammar<T, U, V>::getIdentifier() {
    return ( ascii::alpha >> *qi::char_("a-zA-Z_0-9") )             [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::identifierHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T()> TypeGrammar<T, U, V>::getTypeVarLabel() {
    return ( qi::char_('\'') >> identifier )                        [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::typeVarLabelHelp, this, qi::_2) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, U()> TypeGrammar<T, U, V>::getIntTypeParamLabel() {
    return ( qi::char_('#') >> qi::char_ )                          [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::intTypeParamLabelHelp, this, qi::_2) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, U(), qi::space_type> TypeGrammar<T, U, V>::getIntTypeParam() {
    return qi::uint_                                                [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::concreteTypeParamHelp, this, qi::_1) ]
        | qi::lit("#inf")                                           [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::infiniteTypeParamHelp, this) ]
        | intTypeParamLabel                                         [ qi::_val = qi::_1 ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<vector<T>, T>, qi::space_type> TypeGrammar<T, U, V>::getFunctionType() {
    return ( qi::lit("(") >> -( typeRule                            [ ph::push_back(qi::_a, qi::_1) ]
        % ',' ) >> ')' >>
        qi::lit("->") >> typeRule                                   [ qi::_b = qi::_1 ]
        )                                                           [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::functionTypeHelp, this, qi::_a, qi::_b) ];
}

template<typename T, typename U, typename V>
Rule TypeGrammar<T, U, V>::getTypeVariable() {
    return typeRule                                                 [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::typeVariableHelp, this, qi::_1) ]
        | typeVarLabel                                              [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::typeVariableHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V>
Rule TypeGrammar<T, U, V>::getRefType() {
    return ( qi::lit("ref<") >> typeRule >> '>' )                   [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::refTypeHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V>
Rule TypeGrammar<T, U, V>::getChannelType() {
    return ( qi::lit("channel<") >> typeRule
        >> ',' >> intTypeParam >> '>' )                             [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::channelTypeHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V>
Rule TypeGrammar<T, U, V>::getVectorType() {
    return ( qi::lit("vector<") >> typeRule
        >> ',' >> intTypeParam >> '>' )                             [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::vectorTypeHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<T, U>, qi::space_type> TypeGrammar<T, U, V>::getArrayType() {
    return ( qi::lit("array<") >> typeRule                          [ qi::_a = qi::_1 ]
        >> ',' >> intTypeParam                                      [ qi::_b = qi::_1 ]
        >> '>' )                                                    [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::arrayTypeHelp, this, qi::_a, qi::_b) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<vector<T>>, qi::space_type> TypeGrammar<T, U, V>::getTupleType() {
    return ( qi::char_('(') >> -( typeRule                          [ ph::push_back(qi::_a, qi::_1) ]
        % ',' ) >> ')' )                                            [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::tupleTypeHelp, this, qi::_a) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> TypeGrammar<T, U, V>::getStructType() {
    return ( qi::lit("struct<") >>
        (( identifier >> ':' >> typeRule )                          [ ph::push_back(qi::_a, ph::construct<std::pair<IdentifierPtr,TypePtr>>(qi::_1, qi::_2)) ]
        % ',' ) >> '>' )                                            [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::structTypeHelp, this, qi::_a) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> TypeGrammar<T, U, V>::getUnionType() {
    return ( qi::lit("union<") >> (( identifier >> ':' >> typeRule )[ ph::push_back(qi::_a, ph::construct<std::pair<IdentifierPtr,TypePtr>>(qi::_1, qi::_2)) ]
        % ',' ) >> '>' )                                            [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::unionTypeHelp, this, qi::_a) ];
}

template<typename T, typename U, typename V>
qi::rule<ParseIt, T(), qi::locals<IdentifierPtr, vector<T>, vector<U>>, qi::space_type> TypeGrammar<T, U, V>::getGenericType() {
    return ( identifier                                             [ qi::_a = qi::_1 ]
        >> -( '<' >> -( typeVariable                                [ ph::push_back(qi::_b, qi::_1) ]
        % ',' )
        >> -qi::char_(',') >> -( intTypeParam                       [ ph::push_back(qi::_c, qi::_1) ]
        % ',' )
        >> '>') )                                                   [ qi::_val = ph::bind(&TypeGrammar<T, U, V>::genericTypeHelp, this, qi::_a, qi::_b, qi::_c, TypePtr()) ];

}

template<typename T, typename U, typename V>
Rule TypeGrammar<T, U, V>::getTypeRule() {
    return functionType                                             [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | arrayType                                                 [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | vectorType                                                [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | refType                                                   [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | channelType                                               [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | tupleType                                                 [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | typeVarLabel                                              [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | structType                                                [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | unionType                                                 [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | genericType                                               [ qi::_val = ph::construct<TypePtr>(qi::_1) ]
        | (qi::lit("(|") >> typeRule >> qi::lit("|)"))              [ qi::_val = ph::construct<TypePtr>(qi::_1) ];
}


template<typename T, typename U, typename V>
TypeGrammar<T, U, V>::TypeGrammar(NodeManager& nMan) : TypeGrammar::base_type(typeRule), nodeMan(nMan) {

	auto nManRef = ph::ref(nodeMan);

	// RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

	// terminals, no skip parser

	identifier = getIdentifier();
	
	typeVarLabel = getTypeVarLabel();

	intTypeParamLabel = getIntTypeParamLabel();

	// nonterminals, skip parser

	intTypeParam = getIntTypeParam();

    functionType = getFunctionType();

    typeVariable = getTypeVariable();

	refType = getRefType();

	channelType = getChannelType();

	vectorType = getVectorType();

	arrayType = getArrayType();

	tupleType = getTupleType();

	structType = getStructType();

	unionType = getUnionType();

	genericType = getGenericType();

	typeRule = getTypeRule();

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
template struct TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>;

} // namespace parse 
} // namespace core
} // namespace insieme


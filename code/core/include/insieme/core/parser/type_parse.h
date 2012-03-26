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

#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace core {
namespace parse {

#define Rule qi::rule<ParseIt, T(), qi::space_type>

// parser usage:
// T = TypePtr
// U = IntTypeParamPtr
// V = IdentifierPtr
template <typename T, typename U, typename V>
struct TypeGrammar : public qi::grammar<ParseIt, T(), qi::space_type> {
	
	TypeGrammar(NodeManager& nMan);
	virtual ~TypeGrammar() { }

	NodeManager& nodeMan;

	// terminal rules, no skip parsing
	qi::rule<ParseIt, V()> identifier;
	qi::rule<ParseIt, T()> typeVarLabel;
	qi::rule<ParseIt, U()> intTypeParamLabel;
	qi::rule<ParseIt, U(), qi::space_type> intTypeParam;

	// nonterminal rules with skip parsing
	qi::rule<ParseIt, T(), qi::locals<vector<T>, T, bool>, qi::space_type> functionType;
	qi::rule<ParseIt, T(), qi::space_type> typeVariable;
	qi::rule<ParseIt, T(), qi::space_type> refType;
	qi::rule<ParseIt, T(), qi::space_type> channelType;
	qi::rule<ParseIt, T(), qi::space_type> vectorType;
	qi::rule<ParseIt, T(), qi::locals<T, U>, qi::space_type> arrayType;
	qi::rule<ParseIt, T(), qi::locals<vector<T>>, qi::space_type> tupleType;
	qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> structType;
	qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> unionType;
	qi::rule<ParseIt, T(), qi::locals<V, vector<T>, vector<U>>, qi::space_type> genericType;
	qi::rule<ParseIt, T(), qi::space_type> typeRule;

    // member functions applying the rules
    #define get(op) virtual Rule get##op ();
	qi::rule<ParseIt, V()>  getStringValue();
	qi::rule<ParseIt, T()> getTypeVarLabel();
	qi::rule<ParseIt, U()> getIntTypeParamLabel();
	qi::rule<ParseIt, U(), qi::space_type> getIntTypeParam();
    qi::rule<ParseIt, T(), qi::locals<vector<T>, T, bool>, qi::space_type> getFunctionType();
    get(TypeVariable)
    get(RefType)
    get(ChannelType)
    get(VectorType)
    qi::rule<ParseIt, T(), qi::locals<T, U>, qi::space_type> getArrayType();
    qi::rule<ParseIt, T(), qi::locals<vector<T>>, qi::space_type> getTupleType();
    qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> getStructType();
    qi::rule<ParseIt, T(), qi::locals<vector<std::pair<V, T> > >, qi::space_type> getUnionType();
    qi::rule<ParseIt, T(), qi::locals<V, vector<T>, vector<U>>, qi::space_type> getGenericType();
    get(TypeRule)
    #undef get

protected:
    // member functions providing the rules
	virtual V identifierHelp(char start, const vector<char>& tail);
	virtual T typeVarLabelHelp(const V& id);
	virtual U intTypeParamLabelHelp(const char symbol);
	virtual T typeVariableHelp(const T& type);
	virtual U concreteTypeParamHelp(const size_t value);
	virtual U infiniteTypeParamHelp();
	virtual T arrayTypeHelp(const T& type, const U& nDims);
	virtual T vectorTypeHelp(const T& type, const U& nElems);
	virtual T refTypeHelp(const T& type);
	virtual T channelTypeHelp(const T& type, const U& size);
	virtual T genericTypeHelp(const V& name, const vector<T>& typeParams, const vector<U>& intTypeParams, const T& baseType);
    virtual T tupleTypeHelp(const vector<T>& type);
    virtual T functionTypeHelp(const vector<T>& argTypes, const T& retType, bool plain);
    virtual T structTypeHelp(const vector<std::pair<V,T> >& entries);
    virtual T unionTypeHelp(const vector<std::pair<V,T> >& entries);
};

}
}
}

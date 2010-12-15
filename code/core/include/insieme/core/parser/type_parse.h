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

struct TypeGrammar : public qi::grammar<ParseIt, TypePtr(), qi::space_type> {
	
	TypeGrammar(NodeManager& nodeMan);

	// terminal rules, no skip parsing
	qi::rule<ParseIt, Identifier()> identifier;
	qi::rule<ParseIt, TypePtr()> typeVarLabel;
	qi::rule<ParseIt, IntTypeParam()> intTypeParamLabel;

	// nonterminal rules with skip parsing
	qi::rule<ParseIt, TypePtr(), qi::locals<vector<TypePtr>, vector<TypePtr>, TypePtr>, qi::space_type> functionType;
	qi::rule<ParseIt, TypePtr(), qi::space_type> typeVariable;
	qi::rule<ParseIt, IntTypeParam(), qi::space_type> intTypeParam;
	qi::rule<ParseIt, RefTypePtr(), qi::space_type> refType;
	qi::rule<ParseIt, ChannelTypePtr(), qi::space_type> channelType;
	qi::rule<ParseIt, VectorTypePtr(), qi::space_type> vectorType;
	qi::rule<ParseIt, ArrayTypePtr(), qi::locals<TypePtr, IntTypeParam>, qi::space_type> arrayType;
	qi::rule<ParseIt, TupleTypePtr(), qi::locals<vector<TypePtr>>, qi::space_type> tupleType;
	qi::rule<ParseIt, StructTypePtr(), qi::locals<StructType::Entries>, qi::space_type> structType;
	qi::rule<ParseIt, UnionTypePtr(), qi::locals<UnionType::Entries>, qi::space_type> unionType;
	qi::rule<ParseIt, TypePtr(), qi::locals<Identifier, vector<TypePtr>, vector<IntTypeParam>>, qi::space_type> genericType;
	qi::rule<ParseIt, TypePtr(), qi::space_type> typeRule;
};

}
}
}

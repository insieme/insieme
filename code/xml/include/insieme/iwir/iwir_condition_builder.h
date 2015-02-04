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
#include <string>

#define BOOST_SPIRIT_USE_PHOENIX_V3
#define BOOST_RESULT_OF_USE_DECLTYPE
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/optional.hpp>

#include "insieme/utils/logging.h"
#include "insieme/iwir/iwir_ast.h"

namespace insieme {
namespace iwir {
namespace condition_ast {

using namespace insieme::utils::log;
using namespace std;

namespace qi    = boost::spirit::qi;
namespace phx   = boost::phoenix;

/* 
* CONDITION := CONJUNCTION {or CONJUNCTION}
* CONJUNCTION := EQUATION {and EQUATION}
* EQUATION := RELATION {EQUOP RELATION}
* RELATION := FACTOR {COMPOP FACTOR}
* FACTOR := not FACTOR | (CONDITION) | STRING | PORT | integer | double | bool
* COMPOP := &gt; | &gt;= | &lt; | &lt;=
* EQUOP := != | =
* STRING := "string"
* PORT := string
* 
* The preference is
* not >> &gt; | &gt;= | &lt; | &lt;= >> != | = >> and >> or
* We use blank and parenthesis as delimiter
* For example 3=3  is invalid. (3 < 4)> 3 is valid.
*/
typedef map<pair<std::string, std::string>, iwir::ast::Port*> PortMap;

/*
 * Parse condition-expr-string used in IfTask/WhileTask and reprsent it as ConditionExpr (ast)
 */
template <typename It, typename Skipper = qi::space_type>
struct parser : qi::grammar<It, ConditionExpr(), Skipper> {
private:
	qi::rule<It, int() , Skipper> intVal;
	qi::rule<It, double() , Skipper> doubleVal;
	qi::rule<It, bool() , Skipper> boolVal;
	qi::rule<It, std::string() , Skipper> str;
	qi::rule<It, std::string() , Skipper> portString;
	qi::rule<It, ConditionExpr(), Skipper> condition_, conjunction_, equation_, relation_, factor_, port_, ConditionExpr_;

	const std::string& parentTaskStr;
	
public:

	parser(const std::string& parentTaskStr) : parser::base_type(ConditionExpr_), parentTaskStr(parentTaskStr)
	{
		qi::real_parser<double, qi::strict_ureal_policies<double>> strict_udouble;	//without sign, requires dot
		qi::real_parser<double, qi::strict_real_policies<double>> strict_double;	//with sign, requires dot
		using namespace qi;

		ConditionExpr_ = condition_.alias();

		//CONDITION := CONJUNCTION {or CONJUNCTION}
		condition_ = ( 
				( conjunction_ >> "or" >> condition_ ) [ _val = phx::construct<binop<op_or>>(_1, _2)] |
				conjunction_ [ _val = _1 ]
				); 

		//CONJUNCTION := EQUATION {and EQUATION}
		conjunction_ = (
				( equation_ >> "and" >> conjunction_ ) [ _val = phx::construct<binop<op_and>>(_1, _2)] |
				equation_ [ _val = _1 ]
				);

		//EQUATION := RELATION {EQUOP RELATION}
		//EQUOP := != | =
		equation_ = ( 
				( relation_ >> "!=" >> equation_ ) [ _val = phx::construct<binop<op_neq>>(_1, _2)] |
				( relation_ >> "=" >> equation_ ) [ _val = phx::construct<binop<op_eq>>(_1, _2)] |
				relation_ [ _val = _1 ]
				);
		
		//RELATION := FACTOR {COMPOP FACTOR}
		//COMPOP := &gt; | &gt;= | &lt; | &lt;=
		relation_ = ( 
				//( factor_ >> compop_ >> relation_ ) [ _val = phx::construct<binop<_2>>(_1, _3) ] |
				( factor_ >> "&gt;" >> relation_ ) [ _val = phx::construct<binop<op_gt>>(_1, _2) ] |
				( factor_ >> "&gt;=" >> relation_ ) [ _val = phx::construct<binop<op_gte>>(_1, _2) ] |
				( factor_ >> "&lt;" >> relation_ ) [ _val = phx::construct<binop<op_lt>>(_1, _2) ] |
				( factor_ >> "&lt;=" >> relation_ ) [ _val = phx::construct<binop<op_lte>>(_1, _2) ] |
				factor_ [ _val = _1 ]
				);
		
		//FACTOR := not FACTOR | (CONDITION) | STRING | PORT | integer | double | bool
		factor_ = ( 
				('!' >> factor_) [ _val = phx::construct<unop<op_not>>(_1) ] |
				('(' >> condition_ >> ')') [ _val = _1 ] |
				(boolVal)	[ _val = _1 ] |
				(doubleVal) [ _val = _1 ] |
				(intVal)	[ _val = _1 ] |
				(str)	[ _val = _1 ] |
				(port_)	[ _val = _1 ]
				);

		intVal = (qi::int_);
		
		//accept only doubles with a . regardless of sign
		doubleVal = (strict_double | strict_udouble);

		//accept case insensitive true/false as boolean value
		boolVal = (no_case[qi::bool_]);

		//STRING := "string"
		str = ('"' >> (qi::lexeme[ +(alnum) ]) >> '"');

		//PORT := string
		//port_ = portString [ _val = phx::construct<port>(_1) ];
		//port_ = portString [ _val = phx::construct<iwir::ast::Port*>(nullptr) /*lookup(_1)*/ ];
		port_ = portString [ _val = phx::construct<port>(_1 /*, lookup(_1)*/) ];

		portString = qi::lexeme[+alnum];

		BOOST_SPIRIT_DEBUG_NODE(ConditionExpr_);
		BOOST_SPIRIT_DEBUG_NODE(condition_);
		BOOST_SPIRIT_DEBUG_NODE(conjunction_);
		BOOST_SPIRIT_DEBUG_NODE(equation_);
		BOOST_SPIRIT_DEBUG_NODE(relation_);
		BOOST_SPIRIT_DEBUG_NODE(factor_);
		BOOST_SPIRIT_DEBUG_NODE(intVal);
		BOOST_SPIRIT_DEBUG_NODE(doubleVal);
		BOOST_SPIRIT_DEBUG_NODE(boolVal);
		BOOST_SPIRIT_DEBUG_NODE(str);
		BOOST_SPIRIT_DEBUG_NODE(port_);
	}
};

boost::optional<ConditionExpr> parseConditionString(const std::string& conditionString, const std::string& parentTaskStr, const PortMap& portMap);

} //condition_ast
} //iwir
} //insieme

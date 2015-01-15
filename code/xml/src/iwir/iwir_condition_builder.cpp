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

#include "insieme/iwir/iwir_condition_builder.h"
namespace iwir {
namespace condition_ast {

typedef map<pair<std::string, std::string>, iwir::ast::Port*> PortMap;

/*
 * take a ConditionExpr (a condition_ast) and replaces the PortNames with the Port* used in the iwir_ast
 */
struct set_port_pointer: boost::static_visitor<void> {
	const std::string& parentTaskStr;
	const PortMap& portMap;
	set_port_pointer(const std::string& parentTaskStr, const PortMap& portMap): parentTaskStr(parentTaskStr), portMap(portMap) {}

	iwir::ast::Port* lookup(std::string parentTaskStr, std::string portName) const {
		VLOG(2) << parentTaskStr << " " << portName;
		auto p = portMap.find({parentTaskStr, portName});
		if(p != portMap.end()) {
			return p->second;
		} else {
			return nullptr;	
		}
	};
	//
	void operator()(int& v) const { }
	void operator()(double& v) const { }
	void operator()(bool& v) const { }
	void operator()(std::string& v) const {  }
	void operator()(port& v) const { 
		auto p = lookup(parentTaskStr, v.name);
		v.p = p;
	}

	void operator()(binop<op_and>& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_or >& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_eq >& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_neq>& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_gt >& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_gte>& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_lt >& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()(binop<op_lte>& b) const { boost::apply_visitor(*this, b.oper1); boost::apply_visitor(*this, b.oper2); }
	void operator()( unop<op_not>& u) const { boost::apply_visitor(*this, u.oper1); }
};

boost::optional<ConditionExpr> parseConditionString(const std::string& conditionString, const std::string& parentTaskStr, const PortMap& portMap) {
	static const boost::optional<ConditionExpr> fail;

	using namespace condition_ast;
	auto f(std::begin(conditionString)), l(std::end(conditionString));
	parser<decltype(f)> p(parentTaskStr);

	ConditionExpr result;
	try
	{
		bool ok = qi::phrase_parse(f,l,p,qi::space,result);

		if (!ok) {
			std::cerr << "invalid input\n";
			return fail;
		} else {
			if (f!=l) {
				std::cerr << "unparsed: '" << std::string(f,l) << "'\n";
				return fail;
			} else {
				std::cout << "Success -- " << result << "\n";
				// when succesfully parsed the condition-expr-string fill in the Port*
				boost::apply_visitor(set_port_pointer(parentTaskStr,portMap), result);
				return result;
			}
		}
	} catch (const qi::expectation_failure<decltype(f)>& e) {
		std::cerr << "expectation_failure at '" << std::string(e.first, e.last) << "'\n";
		return fail;
	}
	return fail;
}

} //condition_ast
} //iwir

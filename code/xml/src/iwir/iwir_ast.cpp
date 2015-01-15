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

#include "insieme/iwir/iwir_ast.h"

namespace iwir {
namespace condition_ast {

	//simple printer
	struct printer : boost::static_visitor<void> {
		printer(std::ostream& os) : _os(os) {}
		std::ostream& _os;

		//
		void operator()(const int& v) const { _os << v; }
		void operator()(const double& v) const { _os << v; }
		void operator()(const bool& v) const { _os << v; }
		void operator()(const std::string& v) const { _os << v; }
		//void operator()(const port& v) const { _os << "port[" << v.name << "]"; }
		void operator()(const port& v) const { _os << "port[" << v.name << "]"; }
		//void operator()(const iwir::ast::Port* v) const { _os << "port[" << *v << "]"; }

		void operator()(const binop<op_and>& b) const { print(" and ",	b.oper1, b.oper2); }
		void operator()(const binop<op_or >& b) const { print(" or ",	b.oper1, b.oper2); }
		void operator()(const binop<op_eq >& b) const { print(" eq ",	b.oper1, b.oper2); }
		void operator()(const binop<op_neq>& b) const { print(" neq ",	b.oper1, b.oper2); }
		void operator()(const binop<op_gt >& b) const { print(" gt ",	b.oper1, b.oper2); }
		void operator()(const binop<op_gte>& b) const { print(" gte ",	b.oper1, b.oper2); }
		void operator()(const binop<op_lt >& b) const { print(" lt ",	b.oper1, b.oper2); }
		void operator()(const binop<op_lte>& b) const { print(" lte ",	b.oper1, b.oper2); }

		void print(const std::string& op, const ConditionExpr& l, const ConditionExpr& r) const
		{
			_os << "(";
				boost::apply_visitor(*this, l);
				_os << op;
				boost::apply_visitor(*this, r);
			_os << ")";
		}

		void operator()(const unop<op_not>& u) const
		{
			_os << "(";
				_os << "!";
				boost::apply_visitor(*this, u.oper1);
			_os << ")";
		}
	};
	std::ostream& operator<<(std::ostream& os, const ConditionExpr& e) {
		boost::apply_visitor(printer(os), e); 
		return os; 
	}
} //condition_ast end
} //iwir end

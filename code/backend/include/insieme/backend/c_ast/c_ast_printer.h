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
 *
 */
#pragma once

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/utils/printable.h"


namespace insieme {
namespace backend {
namespace c_ast {

	/**
	 * A class capable of printing a C AST to some output stream.
	 *
	 * Usage:  out << CPrint(fragment);
	 */
	class CPrint : public utils::Printable {
		/**
		 * The C code fragment to be printed.
		 */
		const NodePtr fragment;

	  public:
		/**
		 * A simple constructor allowing to specify the fragment to be printed.
		 */
		CPrint(const NodePtr fragment) : fragment(fragment) {}

		/**
		 * Prints the fragment set up within the constructor to the given output stream.
		 */
		std::ostream& printTo(std::ostream& out) const;
	};

	string toC(const NodePtr& node);

	string toC(const c_ast::CodeFragmentPtr& fragment);

	struct ParameterPrinter : public utils::Printable {
		const vector<c_ast::VariablePtr> params;

	  public:
		ParameterPrinter(const TypePtr& type, const IdentifierPtr& name) : params(toVector(type->getManager()->create<c_ast::Variable>(type, name))) {}
		ParameterPrinter(const VariablePtr& param) : params(toVector(param)) {}
		ParameterPrinter(const vector<VariablePtr>& params) : params(params) {}

		std::ostream& printTo(std::ostream& out) const;
	};


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

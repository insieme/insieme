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
#include "insieme/core/lang/complex.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/types/match.h"

namespace insieme {
namespace core {
namespace lang {

	bool isComplexType(const NodePtr& node) {
		// some simple checks
		if (!node) return false;
		if (auto expr = node.isa<ExpressionPtr>()) return isComplexType(node);

		auto type = node.isa<TupleTypePtr>();
		if(!type) return false;

		// simple approach: use unification
		NodeManager& mgr = node.getNodeManager();
		const ComplexExtension& ext = mgr.getLangExtension<ComplexExtension>();

		// unify given type with template type
		auto ref = ext.getGenComplex();
		auto sub = types::match(mgr, type, ref);
		if(!sub) return false;

		// check whether the value instantiation is a numeric type
		const BasicGenerator& base = mgr.getLangBasic();
		return base.isSelect((*sub).applyTo(TypeVariable::get(mgr, "'a")));
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme

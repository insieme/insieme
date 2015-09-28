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

		auto type = node.isa<TagTypePtr>();
		if(!type || !type.isStruct()) return false;

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

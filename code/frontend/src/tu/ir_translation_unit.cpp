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

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/core/ir.h"
#include "insieme/core/types/subtyping.h"

namespace insieme {
namespace frontend {
namespace tu {

	void IRTranslationUnit::addGlobal(const Global& global) {
		assert(core::types::isSubTypeOf(global.second->getType(), global.first->getType()));
		assert(!any(globals, [&](const Global& cur)->bool { return *global.first == *cur.first; }));
		globals.push_back(global);
	}

	std::ostream& IRTranslationUnit::printTo(std::ostream& out) const {
		return out << "TU(\n\t"
				<< join("\n\t", types, [](std::ostream& out, const std::pair<core::GenericTypePtr, core::TypePtr>& cur) { out << *cur.first << " => " << *cur.second; })
				<< ",\n "
				<< join("\n\t", functions, [](std::ostream& out, const std::pair<core::LiteralPtr, core::ExpressionPtr>& cur) { out << *cur.first << " => " << *cur.second; })
				<< ",\n " << globals << ")";
	}

	IRTranslationUnit merge(const IRTranslationUnit& a, const IRTranslationUnit& b) {
		IRTranslationUnit res = a;

		// copy types
		for(auto cur : b.getTypes()) {
			res.addType(cur.first, cur.second);
		}

		// copy functions
		for(auto cur : b.getFunctions()) {
			res.addFunction(cur.first, cur.second);
		}

		// copy globals
		for(auto cur : b.getGlobals()) {
			res.addGlobal(cur);
		}

		// done
		return res;
	}

	IRTranslationUnit merge(const vector<IRTranslationUnit>& units) {
		assert(!units.empty());
		IRTranslationUnit res;
		for(const auto& cur : units) {
			res = merge(res, cur);
		}
		return res;
	}

	core::ProgramPtr toProgram(const IRTranslationUnit& a) {
		assert(false && "Not Implemented!!");
		return 0;
	}

} // end namespace tu
} // end namespace frontend
} // end namespace insieme

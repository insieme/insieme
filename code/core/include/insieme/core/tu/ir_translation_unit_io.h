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

#include "insieme/core/tu/ir_translation_unit.h"

namespace insieme {
namespace core {
namespace tu {

	/**
	 * dumps the TU into a sigle IR node, useful tu use visitors on the whole thing
	 * and do not loose a bit
	 */
	core::ExpressionPtr toIR(core::NodeManager& manager, const IRTranslationUnit& unit);

	/**
	 * 	builds a translation unit from a previously dumped TU, or spetialy crafted IR node
	 */
	IRTranslationUnit fromIR(const core::ExpressionPtr& node);

	/**
	 * Dumps the given translation unit to the given output stream.
	 *
	 * @param out the target stream
	 * @param unit the translation unit to be dumped
	 */
	void dump(std::ostream& out, const IRTranslationUnit& unit);

	/**
	 * Load a translation unit from the given input stream.
	 *
	 * @param in the stream to read from
	 * @param manager the node manager to be utilized for creating resulting nodes
	 * @return the restored translation unit
	 */
	IRTranslationUnit load(std::istream& in, core::NodeManager& manager);

} // end namespace tu
} // end namespace core
} // end namespace insieme

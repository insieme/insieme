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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/logging.h"
//#include "insieme/frontend/omp/omp_annotation.h"

namespace insieme {
namespace frontend {

namespace tu { class IRTranslationUnit; }

namespace omp {

/** Applies OMP semantics to given code fragment.
 ** */
const core::ProgramPtr applySema(const core::ProgramPtr& prog, core::NodeManager& resultStorage);

/**
 * Applies OMP semantics to all the definitions within the given translation unit.
 *
 * @param unit the translation unit to be processed
 * @param mgr the node manager to be utilized for creating IR nodes
 * @return the modified translation unit
 */
tu::IRTranslationUnit applySema(const tu::IRTranslationUnit& unit, core::NodeManager& mgr);


} // namespace omp
} // namespace frontend
} // namespace insieme

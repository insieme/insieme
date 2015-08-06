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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/extension.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace backend {
namespace runtime {

/**
 * This class offers a list of IR extensions required to model concepts within the
 * Insieme Runtime. The extensions include literals and types to model work items,
 * data items and additional runtime functionality.
 */
class Extensions : public core::lang::Extension {
public:

	// Adds the definition of constant, public members to this definition using a macro file
	
#define TYPE(name, type) 			const core::TypePtr name;
#define LITERAL(name, value, type) 	const core::LiteralPtr name;
#include "ir_extensions.def"
#undef TYPE
#undef LITERAL
	
private:

	int dummy;
	
	friend class core::NodeManager;
	
	/**
	 * Creates a new instance of this extension set. The given manager is used to construct
	 * the contained literals and types.
	 *
	 * @param manager the manager to be used to construct the required types and literals
	 */
	Extensions(core::NodeManager& manager);
	
};

} // end namespace encoder
} // end namespace core
} // end namespace insieme

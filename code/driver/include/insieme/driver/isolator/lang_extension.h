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

#include "insieme/core/lang/extension.h"
#include "insieme/core/ir_expressions.h"


namespace insieme {
namespace driver {
namespace isolator {


	struct Extension : public core::lang::Extension {

		/**
		 * Creates a new instance based on the given node manager.
		 */
		Extension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}


		// -- recording literals --

		// literals managing the capturing module
		LANG_EXT_LITERAL(Init,   "capture.init", "()->unit");
		LANG_EXT_LITERAL(Finish, "capture.finish", "()->unit");

		// literals marking the begin and end of regions
		LANG_EXT_LITERAL(Start, "capture.start", "(intTypeParam<#n>)->unit");
		LANG_EXT_LITERAL(Stop,  "capture.stop",  "(intTypeParam<#n>)->unit");

		// literals recording accesses to data values
		LANG_EXT_LITERAL(Read,     "capture.read",      "(ref<'a>)->'a");
		LANG_EXT_LITERAL(Write,    "capture.write",     "(ref<'a>,'a)->unit");
		LANG_EXT_LITERAL(ReadPtr,  "capture.read_ptr",  "(ref<ref<'a>>)->ref<'a>");
		LANG_EXT_LITERAL(WritePtr, "capture.write_ptr", "(ref<ref<'a>>,ref<'a>)->unit");

		// literals used for registering blocks
		LANG_EXT_LITERAL(RegisterBlock, "capture.reg.block", "(ref<'a>, uint<8>)->ref<'a>");
		LANG_EXT_LITERAL(TagBlock,      "capture.tag.block", "(ref<'a>, intTypeParam<#n>)->unit");


		// -- restoring literals --

		LANG_EXT_LITERAL(Load,         "capture.load",     "(ref<'a>, intTypeParam<#n>, intTypeParam<#m>)->unit");
		LANG_EXT_LITERAL(Finalize,     "capture.finalize", "()->unit");
		LANG_EXT_LITERAL(CheckLifeOut, "capture.check",    "(intTypeParam<#n>)->unit");

	};

} // end namespace isolator
} // end namespace driver
} // end namespace insieme

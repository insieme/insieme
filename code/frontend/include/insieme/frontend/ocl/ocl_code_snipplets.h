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

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace frontend {
namespace ocl {

//device
core::ExpressionPtr getConvert(unsigned length, core::IRBuilder builder);

//host

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:

public:
	Ocl2Inspire() { }

	bool extractSizeFromSizeof(const core::ExpressionPtr& arg,
			core::ExpressionPtr& size, core::TypePtr& type, bool foundMul = false);

	core::ExpressionPtr getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet, core::IRBuilder builder);
	core::ExpressionPtr getClCopyBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClCopyBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClWriteBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClWriteBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClReadBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClReadBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClGetIDs(core::IRBuilder builder);
};


}
}
}

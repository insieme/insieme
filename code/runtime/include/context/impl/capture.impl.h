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
#ifndef __GUARD_CONTEXT_IMPL_CAPTURE_IMPL_H
#define __GUARD_CONTEXT_IMPL_CAPTURE_IMPL_H

#include "context/capture.h"

// this impl-header is just dispatching between the potential implementations.

// to enable / disable debugging
#define DEBUG(X)
//#define DEBUG(X) X

/**
 * The alignment considered when restoring data blocks within
 * isolated kernels.
 *
 * It is set to 4KB.
 */
#define IRT_CONTEXT_CAPTURE_ALIGNMENT (1<<12)

/**
 * The magic number used within profile files to provide some
 * simple verification of the file type.
 */
// in memorial to a great event for mankind ...
#define MAGIC_NUMBER 2063


#ifdef RECORD
#include "context/impl/record.impl.h"
#endif


#ifdef RESTORE
#include "context/impl/restore.impl.h"
#endif


// clear definitions
#undef DEBUG
#undef MAGIC_NUMBER
#undef IRT_CONTEXT_CAPTURE_ALIGNMENT


#endif // ifndef __GUARD_CONTEXT_IMPL_CAPTURE_IMPL_H

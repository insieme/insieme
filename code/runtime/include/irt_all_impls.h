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
#ifndef __GUARD_IRT_ALL_IMPLS_H
#define __GUARD_IRT_ALL_IMPLS_H

#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/data_item.impl.h"
#include "impl/work_group.impl.h"
#include "impl/irt_events.impl.h"
#include "impl/irt_lock.impl.h"
#include "impl/ir_interface.impl.h"
#include "impl/irt_loop_sched.impl.h"
#include "impl/irt_logging.impl.h"
#include "impl/papi_helper.impl.h"
#include "irt_types.h"
#include "meta_information/meta_infos.h"
#include "wi_implementation.h"
#include "abstraction/impl/rdtsc.impl.h"
#include "utils/impl/timing.impl.h"
#include "utils/impl/frequency.impl.h"

#ifndef IRT_MIN_MODE
#include "impl/irt_mqueue.impl.h"
#include "utils/impl/affinity.impl.h"
#endif

#ifdef _WIN32
#include "include_win32/memalign.h"
#endif

#include "runtime.h"
//#include "context/impl/capture.impl.h"

#ifdef USE_OPENCL
#include "impl/irt_ocl.impl.h"
#endif

#include "impl/irt_debug.impl.h"


#endif // ifndef __GUARD_IRT_ALL_IMPLS_H

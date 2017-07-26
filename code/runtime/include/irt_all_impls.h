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

#ifdef IRT_ENABLE_OPENCL
#include "impl/irt_opencl.impl.h"
#endif

#include "impl/irt_debug.impl.h"


#endif // ifndef __GUARD_IRT_ALL_IMPLS_H

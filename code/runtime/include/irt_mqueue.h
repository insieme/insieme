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
 *
 */
#pragma once
#ifndef __GUARD_IRT_MQUEUE_H
#define __GUARD_IRT_MQUEUE_H

#include "declarations.h"

// include implementation only in non-min mode

//#ifndef IRT_MIN_MODE
#ifdef _WIN32
#include "include_win32\mqueue.h"
#else
#include <mqueue.h>
#endif
//#endif

extern mqd_t irt_g_message_queue;

/* ------------------------------ data structures ----- */

typedef enum _irt_mqueue_msg_type { IRT_MQ_NEW_APP } irt_mqueue_msg_type;

typedef struct _irt_mqueue_msg {
	irt_mqueue_msg_type type;
	size_t size;
} irt_mqueue_msg;

typedef struct _irt_mqueue_msg_new_app {
	irt_mqueue_msg_type type;
	size_t size;
	char app_name[128];
} irt_mqueue_msg_new_app;


/* ------------------------------ operations ----- */

void irt_mqueue_init();
void irt_mqueue_cleanup();

void irt_mqueue_send(const irt_mqueue_msg* msg);
void irt_mqueue_send_new_app(const char* appname);

/** Retrieves a message from the IRT message queue,
 ** NULL is returned if the message queue is empty.
 ** Note: must call free on returned object
 **/
irt_mqueue_msg* irt_mqueue_receive();


#endif // ifndef __GUARD_IRT_MQUEUE_H

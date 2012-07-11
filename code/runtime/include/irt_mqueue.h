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

#include "declarations.h"

#ifdef _MSC_VER
	#include "include_win32\mqueue.h"
#else
	#include <mqueue.h>
#endif

extern mqd_t irt_g_message_queue;

/* ------------------------------ data structures ----- */

typedef enum _irt_mqueue_msg_type {
	IRT_MQ_NEW_APP
} irt_mqueue_msg_type;

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

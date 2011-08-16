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

#include "irt_events.h"

static inline irt_wi_event_register* _irt_get_wi_event_register() {
	irt_worker* self = irt_worker_get_current();
	irt_wi_event_register* reg = self->wi_ev_register_list;
	if(reg) {
		self->wi_ev_register_list = reg->lookup_table_next;
		memset(reg, 0, sizeof(irt_wi_event_register));
		return reg;
	} else {
		irt_wi_event_register* ret = (irt_wi_event_register*)calloc(1, sizeof(irt_wi_event_register));
		pthread_spin_init(&ret->lock, PTHREAD_PROCESS_PRIVATE); // TODO check destroy
		return ret;
	}
}


void _irt_wi_event_register_only(irt_wi_event_register *reg) {
	 irt_wi_event_register_table_insert(reg);
}

uint32 irt_wi_event_check_and_register(irt_work_item_id wi_id, irt_wi_event_code event_code, irt_event_lambda *handler) {
	irt_wi_event_register *newreg = _irt_get_wi_event_register();
	newreg->id.value.full = wi_id.value.full;
	newreg->id.cached = newreg;
	irt_wi_event_register *reg = irt_wi_event_register_table_lookup_or_insert(newreg);
	pthread_spin_lock(&reg->lock);
	// check if event already occurred
	if(reg->occurence_count[event_code]>0) {
		// if so, return occurrence count
		pthread_spin_unlock(&reg->lock);
		return reg->occurence_count[event_code];
	}
	// else insert additional handler
	handler->next = reg->handler[event_code];
	reg->handler[event_code] = handler;
	pthread_spin_unlock(&reg->lock);
	return 0;
}

void irt_wi_event_trigger(irt_work_item_id wi_id, irt_wi_event_code event_code) {
	irt_wi_event_register *newreg = _irt_get_wi_event_register();
	newreg->id.value.full = wi_id.value.full;
	newreg->id.cached = newreg;
	irt_wi_event_register *reg = irt_wi_event_register_table_lookup_or_insert(newreg);
	pthread_spin_lock(&reg->lock);
	// increase event count
	++reg->occurence_count[event_code];
	// go through all event handlers
	irt_event_lambda *cur = reg->handler[event_code];
	irt_event_lambda *prev = NULL;
	while(cur != NULL) {
		if(!cur->func(reg, cur->data)) { // if event handled, remove
			if(prev == NULL) reg->handler[event_code] = cur->next;
			else prev->next = cur;
		} else { // else keep the handler in the list
			prev = cur;
		}
		cur = cur->next;
	}
	pthread_spin_unlock(&reg->lock);
}

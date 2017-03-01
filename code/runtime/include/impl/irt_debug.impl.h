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
#include "declarations.h"

#include "abstraction/atomic.h"
#include "meta_information/meta_infos.h"

// IRT context information printing
void irt_dbg_dump_context(FILE* fd, irt_context* c) {
	fprintf(fd, "--------\nIRT context dump:\n");
	fprintf(fd, "Client app library: %p\n", c->client_app ? (void*)c->client_app->library : NULL);
	fprintf(fd, "Number of instrumentation regions: %u\n", c->num_regions);
	fprintf(fd, "----\nType table (%u entries):\n", c->type_table_size);
	for(uint32 i = 0; i < c->type_table_size; ++i) {
		irt_type* t = &c->type_table[i];
		fprintf(fd, "  Type %3u -- kind: %16s, bytes: %6u, #components: %3u\n", i, irt_type_kind_get_name(t->kind), t->bytes, t->num_components);
	}
	fprintf(fd, "----\nWork Item table (%u entries):\n", c->impl_table_size);
	for(uint32 i = 0; i < c->impl_table_size; ++i) {
		irt_wi_implementation* impl = &c->impl_table[i];
		fprintf(fd, "  WI %3u -- #variants: %3u\n", i, impl->num_variants);
		for(uint32 j = 0; j < impl->num_variants; ++j) {
			irt_wi_implementation_variant* v = &impl->variants[j];
			fprintf(fd, "    Variant %2u -- requirements (DIs/channels): %3u/%3u, meta info index: %3ld\n", j, v->num_required_data_items,
			        v->num_required_channels, v->meta_info ? (long signed)(v->meta_info - c->info_table) : 0l);
		}
	}
	fprintf(fd, "----\nMeta Info table (%u entries):\n", c->info_table_size);
	for(uint32 i = 0; i < c->info_table_size; ++i) {
		irt_meta_info_table_entry* m = &c->info_table[i];
		fprintf(fd, "  Info %3u:\n", i);
		#define INFO_STRUCT_BEGIN(__name)                                                                                                                      \
			fprintf(fd, "    ");                                                                                                                               \
			irt_meta_info_print_##__name(fd, m);                                                                                                               \
			fprintf(fd, "\n");
		#include "insieme/common/meta_infos.def"
	}
	fprintf(fd, "--------\n");
}

void irt_dbg_print_context(irt_context* c) {
	irt_dbg_dump_context(stdout, c);
}


// active WI printing

#ifdef IRT_ENABLE_INSTRUMENTATION
void irt_dbg_print_worker_events(int32 wid, int32 num) {
	int32 s = irt_g_workers[wid]->instrumentation_event_data->number_of_elements - 1;
	for(int32 i = s; i >= 0 && i > s - num; --i) {
		irt_inst_event_data_output_single(irt_g_workers[wid]->instrumentation_event_data->data[i], stdout, true);
	}
	if(s < 0) { printf("\n"); }
}
#else
void irt_dbg_print_worker_events(int32 wid, int32 num) {
	printf("\n");
}
#endif

const char* irt_dbg_get_worker_state_string(irt_worker_state state) {
	switch(state) {
	case IRT_WORKER_STATE_CREATED: return "IRT_WORKER_STATE_CREATED";
	case IRT_WORKER_STATE_READY: return "IRT_WORKER_STATE_READY";
	case IRT_WORKER_STATE_START: return "IRT_WORKER_STATE_START";
	case IRT_WORKER_STATE_RUNNING: return "IRT_WORKER_STATE_RUNNING";
	case IRT_WORKER_STATE_SLEEPING: return "IRT_WORKER_STATE_SLEEPING";
	case IRT_WORKER_STATE_DISABLED: return "IRT_WORKER_STATE_DISABLED";
	case IRT_WORKER_STATE_WAITING: return "IRT_WORKER_STATE_WAITING";
	case IRT_WORKER_STATE_STOP: return "IRT_WORKER_STATE_STOP";
	default: return "IRT_WORKER_STATE_UNKNOWN";
	}
}

void irt_dbg_print_worker_state(int32 wid) {
#if IRT_SCHED_POLICY != IRT_SCHED_POLICY_STEALING_CIRCULAR
	printf("Worker #%03d: %32s - q:%4d || ", wid, irt_dbg_get_worker_state_string(irt_atomic_load(&irt_g_workers[wid]->state)),
	#if IRT_SCHED_POLICY == IRT_SCHED_POLICY_UBER
	       irt_cwb_size(&irt_g_workers[wid]->sched_data.queue)
	#else
	       irt_g_workers[wid]->sched_data.queue.size
	#endif
	           );
	#else
	printf("Worker #%03d: %32s - q:%4d || ", wid, irt_dbg_get_worker_state_string(irt_atomic_load(&irt_g_workers[wid]->state)),
	       irt_cwb_size(&irt_g_workers[wid]->sched_data.queue));
	#endif
	irt_dbg_print_worker_events(wid, 1);
}

void irt_dbg_print_worker_overview() {
	for(uint32 w = 0; w < irt_g_worker_count; ++w) {
		irt_dbg_print_worker_state(w);
	}
}

#define _IRT_DBG_MAX_EVENTS (1024 * 32)
typedef struct __irt_dbg_wi_list {
	uint16 thread;
	uint32 wi_index;
	irt_instrumentation_event_data* events[_IRT_DBG_MAX_EVENTS];
	uint32 num_events;
	struct __irt_dbg_wi_list* next;
} _irt_dbg_wi_list;
void _irt_dbg_wi_list_add(_irt_dbg_wi_list** l, uint16 thread, uint32 index) {
	_irt_dbg_wi_list* elem = (_irt_dbg_wi_list*)malloc(sizeof(_irt_dbg_wi_list));
	elem->thread = thread;
	elem->wi_index = index;
	elem->next = *l;
	*l = elem;
}
void _irt_dbg_wi_list_remove(_irt_dbg_wi_list** l, uint16 thread, uint32 index) {
	// first
	if(*l && (*l)->thread == thread && (*l)->wi_index == index) {
		_irt_dbg_wi_list* temp = (*l)->next;
		free(*l);
		*l = temp;
		return;
	}
	// others
	_irt_dbg_wi_list* li = *l;
	while(li) {
		if(li->next && li->next->thread == thread && li->next->wi_index == index) {
			_irt_dbg_wi_list* temp = li->next->next;
			free(li->next);
			li->next = temp;
			return;
		}
		li = li->next;
	}
	printf("DBG analysis error: irt_dbg_wi_list tried to remove non-existent element [%d %d]\n", thread, index);
}
_irt_dbg_wi_list* _irt_dbg_wi_list_get(_irt_dbg_wi_list* l, uint16 thread, uint32 index) {
	while(l) {
		if(l->thread == thread && l->wi_index == index) { return l; }
		l = l->next;
	}
	return NULL;
}
void _irt_dbg_wi_list_clear(_irt_dbg_wi_list** l) {
	while(*l) {
		_irt_dbg_wi_list* temp = (*l)->next;
		free(*l);
		*l = temp;
	}
}
void _irt_dbg_wi_list_print(_irt_dbg_wi_list* l) {
	printf("(");
	while(l) {
		printf("[%d %d],", l->thread, l->wi_index);
		l = l->next;
	}
	printf(")\n");
}

#ifdef IRT_ENABLE_INSTRUMENTATION
void irt_dbg_print_active_wis() {
	// store current index on each worker's event list
	uint64* worker_ev_indices = (uint64*)calloc(irt_g_worker_count, sizeof(uint64));
	irt_instrumentation_event_data* next_ev = NULL;
	_irt_dbg_wi_list* active_wis = NULL;
	do {
		// find oldest event
		uint64 min_timestamp = UINT64_MAX;
		uint32 min_worker = 0;
		next_ev = NULL;
		for(uint32 w = 0; w < irt_g_worker_count; ++w) {
			uint32 s = irt_g_workers[w]->instrumentation_event_data->number_of_elements;
			if(worker_ev_indices[w] < s) {
				irt_instrumentation_event_data* ev = &irt_g_workers[w]->instrumentation_event_data->data[worker_ev_indices[w]];
				if(ev->timestamp < min_timestamp) {
					min_worker = w;
					min_timestamp = ev->timestamp;
					next_ev = ev;
				}
			}
		}
		// process event
		if(next_ev) {
			worker_ev_indices[min_worker]++;
			// add/remove to/from active list
			switch(next_ev->event_id) {
			case IRT_INST_WORK_ITEM_CREATED: {
				// printf("created [%d %d]!\n", next_ev->thread, next_ev->index);
				_irt_dbg_wi_list_add(&active_wis, next_ev->thread, next_ev->index);
			} break;
			case IRT_INST_WORK_ITEM_FINALIZED: {
				// printf("finalized [%d %d]!\n", next_ev->thread, next_ev->index);
				_irt_dbg_wi_list_remove(&active_wis, next_ev->thread, next_ev->index);
			} break;
			}
			// store events for active
			_irt_dbg_wi_list* entry = _irt_dbg_wi_list_get(active_wis, next_ev->thread, next_ev->index);
			if(entry && entry->num_events < _IRT_DBG_MAX_EVENTS) { entry->events[entry->num_events++] = next_ev; }
		}
	} while(next_ev != NULL);
	// print active wi indices
	_irt_dbg_wi_list* l = active_wis;
	while(l) {
		printf("--------\nActive wi [%d %d]\n", l->thread, l->wi_index);
		for(uint32 i = 0; i < l->num_events; ++i) {
			irt_inst_event_data_output_single(*l->events[i], stdout, true);
		}
		l = l->next;
	}
	// cleanup
	free(worker_ev_indices);
	_irt_dbg_wi_list_clear(&active_wis);
}
#else  // IRT_ENABLE_INSTRUMENTATION
void irt_dbg_print_active_wis() {
	printf("Active WI tracking requires instrumentation.\n");
}
#endif // IRT_ENABLE_INSTRUMENTATION

/*************************************************************************
  This program is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/ompscr/
	e-mail: ompscr@etsii.ull.es

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  (LICENSE file) along with this program; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  Boston, MA  02111-1307  USA

FILE:		c_testPath.c
VERSION:	1.0
DATE:
AUTHOR:		Arturo González-Escribano
COMMENTS TO:	arturo@infor.uva.es
DESCRIPTION:
		Test if there exist a path between two given nodes in
		a directed graph.

COMMENTS:       Method: Parallel search synchronized by a workers-farm
			paradigm: Accessing to a shared task-pool in
			a critical region

REFERENCES:
BASIC PRAGMAS:	parallel, critical
USAGE: 		./c_testPath.par <source_node_num> <target_node_num> <graph_file>
INPUT:		A graph stored in a file
OUTPUT:		True or False
		Compile with -DDEBUG to see more information about the
		push/pop operations of the threads.
FILE FORMATS:
		Graph file should use .gph format
		This format is intended to store task graphs information.
		Each node may store a list of values which indicate
		resources consumption associated with the task (e.g. CPU time).

		-------------------------------------------------------------
		FORMAT
		-------------------------------------------------------------

		# <optional_comments>
		# <optional_comments>

		T: <number_of_nodes>
		R: <number_of_resources_per_task>
		t<i> <list_of_values> s<num_succesors>: <list_of_succ>
		t<i> <list_of_values> s<num_succesors>: <list_of_succ>
		t<i> <list_of_values> s<num_succesors>: <list_of_succ>
		...

		-------------------------------------------------------------
		EXAMPLES:
		-------------------------------------------------------------

		# Small graph example
		T: 4
		R: 0
		t0: s2: 1 2
		t1: s1: 3
		t2: s1: 3
		t3: s0:

		                         0
					/ \
				       1   2
				        \ /
					 3

		-------------------------------------------------------------

		# Another small example with two sources and two sinks
		# It uses 1 resource to store CPU time of each task
		T: 5
		R: 1
		t0: 0.4500 s1: 2
		t1: 1.2340 s1: 2
		t2: 0.8000 s2: 3 4
		t3: 1.1000 s0:
		t4: 0.9086 s0:

		                        0   1
					 \ /
                                          2
					 / \
					3   4

		-------------------------------------------------------------

RESTRICTIONS:	Only directed graphs
REVISION HISTORY:
**************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include "OmpSCR.h"
#include "AStack.h"
#include "tg.h"


/* PROTOYPES */
void testPath(int, int, int, tg);


/* MAIN: PROCESS PARAMETERS */
int main(int argc, char *argv[]) {
int nthreads, source, target;
char *graphFileName;
FILE *graphFile;
tg graph;

char *argNames[3] = { "source_node_num", "target_node_num", "graph_file" };
char *defaultValues[3] = { "1", "29", "exampleGraph_01.gph"  };
char *timerNames[1] = { "EXE_TIME" };

nthreads = omp_get_max_threads();
OSCR_init( nthreads,
	"Check if there is a path on a directed graph.",
	NULL,
	3,
	argNames,
	defaultValues,
	1,
	1,
	timerNames,
	argc,
	argv );

/* 1. GET PARAMETERS */
source = OSCR_getarg_int(1);
target = OSCR_getarg_int(2);
graphFileName = OSCR_getarg_string(3);

/* 2. READ GRAPH */
graphFile = fopen(graphFileName,"r");
if (graphFile==NULL) {
	fprintf(stderr,"Imposible to open graph file: %s\n",graphFileName);
	exit(-1);
	}
graph = tg_read(graphFile);
fclose(graphFile);

/* 3. CALL COMPUTATION */
testPath(nthreads, source, target, graph);

/* 4. REPORT */
OSCR_report();

return 0;
}

/*
*
* Graph search. Test if exists a path from a source node to a target node
*
* Parallelization method: Shared-Memory workers-farm
*
*/
void testPath(int nthreads, int source, int target, tg graph) {
/* SHARED STRUCTURES */
Bool	*searched=NULL;
Astack	pool;
Bool	found = FALSE;
int	ind;

/* ENDING CONTROL */
int		num_waiting=0;

/* 1. ALLOCATE MEMORY FOR ANCILLARY STRUCTURES */
pool = Ast_init();
searched = OSCR_calloc(tg_nodes(graph), sizeof(Bool));

for (ind=0; ind<tg_nodes(graph); ind++) { searched[ind]=FALSE; }

/* 2. INIT "nodes to explore" POOL WITH THE source ID */
Ast_push(pool, source);


/* 3. START TIMER */
OSCR_timer_start(0);

/* 4. SPAWN WORKERS */
#pragma omp parallel default(none) shared(nthreads,num_waiting,graph,searched,pool,target,found)
{
Bool		waiting = FALSE;
tg_task		next=TG_NULLID;
task_list	succs;
int		num_succs;
int		ind;
#ifdef DEBUG
int		numPops=0;
int		numNoPops=0;
int		thread = omp_get_thread_num();
#endif

/* WORKER WORKS UNTIL:
 * 	ALL WORKERS ARE WAITING (TARGET NOT FOUND)
 * 	OR SOMEONE FINDS THE TARGET
 */
while ( num_waiting != nthreads && !found ) {

	/* 1. GET NEXT ELEMENT TO PROCESS (OR WAIT UNTIL MORE ELEMENTS) */
	while( next == TG_NULLID && num_waiting != nthreads && !found) {

		/* ALL POOL OPERATIONS ARE MONITORIZED */
		#pragma omp critical
			{
			/* 1.1. CHECK THE POOL */
			if ( Ast_more(pool) ) {
				/* 1.1.1. ELEMENTS IN THE POOL: GET NEXT */
				next = Ast_pop(pool);
#ifdef DEBUG
numPops++;
#endif

				/* 1.1.2. IF WAITING, CHANGE STATE */
				if ( waiting ) {
					waiting = FALSE;
					num_waiting--;
					}
				}
			else {
				/* 1.1.3. EMPTY POOL: IF NOT WAITING, CHANGE STATE */
#ifdef DEBUG
numNoPops++;
#endif
				if ( !waiting ) {
					waiting = TRUE;
					num_waiting++;
					}
				}
			/* OMP END CRITICAL: MONITORIZED OPERATION */
			}

		} /* END GET next ELEMENT FROM THE POOL */


	/* 2. PROCESS next ELEMENT */
	if ( next != TG_NULLID ) {

		/* 2.1. TARGET FOUND: END ALL */
		if (next == target) { found = TRUE; }

		/* 2.2. NO SUCCESORS: END */
		else if ( tg_succ_num(graph, next) == 0 ) { next = TG_NULLID; }

		/* 2.3. GET SUCCESORS LIST AND PUSH IT TO THE POOL */
		else {
			/* 2.3.1. GET SUCCS LIST */
			num_succs = tg_succ_num(graph, next);
			succs = tg_succ(graph, next);

			/* 2.3.2. PUSH SUCCS TO POOL: MONITORIZED OPERATION */
			#pragma omp critical
			if ( num_succs > 0 ) {
				for(ind=0; ind<num_succs; ind++) {
					tg_task	vp = succs[ind];

					/* PUSH ONLY NON-EXPLORED NODES */
					if ( ! searched[ vp ] ) {
						searched[ vp ] = TRUE;
						Ast_push(pool, vp);
						}
					}
				/* END OMP CRITICAL: MONITORIZED OPERATION */
				}
			}

		/* 2.4. END PROCESSING ELEMENT */
		next = TG_NULLID;
		}
	} /* END PROCESSING */

#ifdef DEBUG
printf("#DEBUG Thread %d ENDING ----> Pops: %d, NoPops: %d\n",thread,numPops,numNoPops);
#endif

/* WORKERS END: PARALLEL REGION */
}

/* 5. STOP TIMER */
OSCR_timer_stop(0);

/* 6. WRITE RESULT */
printf("\nPath(%d,%d) = %d\n\n", source, target, found);

/* 7. END */
}


/*---------------------------------------------------------------------------
 * Tg v 3.0
 *
 * tgP.h	-- 	Performance Evaluator
 * 			Task graph ADT definition
 *			Private definitions
 * v 1.2
 *
 * Copyright (C) 1999, Arturo González-Escribano
 *
 * Adapted by Arturo González-Escribano for OmpSCR, Jun 2004
 *
 *---------------------------------------------------------------------------
 */
#ifndef TGP_H
#define TGP_H

#include"ATypes.h"

/*
 * 1. TYPES
*/

/* 1.2. MAIN STRUCTURE TYPE FOR A TASK, POINTER TO A TASK, AND LIST */
typedef int	tg_task;
typedef int 	* task_list;

typedef	struct tg_tsk {
	int		tid;		/* Task identifier */
	double		*resour;	/* Resources time list */
	int		prenum;		/* Predecesors number */
	task_list 	pred;		/* Predecesors list */
	int		succnum;	/* Succesors number */
	task_list 	succ;		/* Succesors lists */
} tg_tsk;

typedef tg_tsk 	* tg_ptask;
typedef tg_tsk 	** ptask_list;

/* 1.3. STRUCTURE TYPE FOR A TASK GRAPH AND POINTER TO A GRAPH */
typedef	struct Tgadt_tg {
	int   		resnum;		/* Resources number */
	int     	tasknum;	/* Tasks number */
	int		maxid;		/* Maximum number for an identifier */
	ptask_list	idarray;	/* Array of task pointers, id indexed */
	tg_task		root;		/* Root of the graph */
} tg_graph;

typedef tg_graph * tg;


/*
 * 2. INTERNAL FUNCTION PROTOTYPES
 */


/*
 * 3. INTERNAL CONSTANTS
 */


/*
 * 4. INTERNAL MACROS
 */
/* Returns the pointer of a task identifier */
#define tgp(graph, task)	(graph->idarray[task])


#endif /* TGP_H */



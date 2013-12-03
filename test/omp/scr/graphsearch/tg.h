/*---------------------------------------------------------------------------
 * Tg v 3.0
 *
 * tg.h	-- 	Performance Evaluator
 * 			Task graph ADT definition
 *			Public definitions
 * v 2.2
 *
 * Copyright (C) 1999, Arturo González-Escribano
 *
 *---------------------------------------------------------------------------
 */
#ifndef TG_H
#define TG_H

#include <stdio.h>
#include "tgP.h"


/*
 * 0. CONSTANTS
 */
/* NULL identification for a task */
#define	TG_NULLID	-1

/*
 * 2. INTERFACE FUNCTION PROTOTYPES
 */
/* 2.1. BASIC FUNCTIONS */
tg tg_init(int);	/* Initialize without nodes graph with n resources */
void tg_delete(tg);	/* Erase the graph information and free the memory space */
tg tg_dup(tg);		/* Duplicate the graph */
void tg_dup_resources(tg, tg);	/* Copy the resources values from a graph to another, to tasks with the same id */

Bool tg_add_node(tg, tg_task);	/* Add a task node (if identifier does not exist) */
void tg_del_node(tg, tg_task);	/* Delete a node !!!! and all its arcs */
void tg_add_arc(tg, tg_task, tg_task);	/* Create arc conecting 2 nodes */
void tg_del_arc(tg, tg_task, tg_task);	/* Delete arc/s conecting 2 nodes */

/* 2.2. TASK IDENTIFICATION FUNCTIONS */
Bool tg_change_id(tg, tg_task, tg_task);	/* Changes the identifier number of a task (if the target one does not exist) */
tg_task tg_free_id(tg);	/* Returns the minimum free identifier not used by a task */

/* 2.3. TEXT I/O INTERFACE FUNCTIONS */
tg tg_read(FILE *);		/* Read a graph from a text file */
void tg_write(FILE *, tg);	/* Write a graph to a text file */

/* 2.2. INFORMATION FUNCTIONS */
Bool tg_arc(tg, tg_task, tg_task);	/* True if the arc exists */

/* 2.3. HIGH LEVEL OPERATION FUNCTIONS */
void tg_del_arcs_A(tg graph, tg_task node); /* Delete all arriving arcs of a node */
void tg_del_arcs_L(tg graph, tg_task node); /* Delete all leaving arcs of a node */
void tg_move_succ(tg, tg_task, tg_task); /* Move the arcs to direct succesors to another node */
void tg_move_pred(tg, tg_task, tg_task); /* Move the arcs from direct predecesors to another node */

/* 2.4. HIGH LEVEL INFORMATION RECOVERY FUNCTIONS */
/* Returns the list of nodes in the graph that have a int number of predecessors */
int tg_pred_grade(tg, int, task_list *);
/* Returns the list of nodes in the graph that have a int number of succesors */
int tg_pred_grade(tg, int, task_list *);


/*
 * 3. INTERFACE MACROS FOR TASK AND GRAPH INFORMATION 
 */

/* Number of nodes on the graph */
#define tg_nodes(graph)	(graph->tasknum)

/* Returns the number of resources for this graph nodes */
#define tg_resnum(graph)		(graph->resnum)

/* Sets the root task */
#define tg_set_root(graph, task)	(graph->root = task)

/* Obtains the id of the root task */
#define tg_root(graph)	(graph->root)

/* Returns the maximum number used as identifier */
#define tg_maxid(graph)		(graph->maxid)

/* Checking if a identifier exists */
#define tg_id(graph, task)	((graph->maxid < task) ? FALSE:(graph->idarray[task] == NULL) ? FALSE : TRUE)

/* Sets the value for a task resource */
#define tg_set_resource(graph, task, number, value)	(tgp(graph,task)->resour[number] = value)

/* Gets the value of a task resource */
#define tg_get_resource(graph, task, number)	(tgp(graph,task)->resour[number])

/* Returns the number of direct succesors of a task */
#define tg_succ_num(graph, task)	(tgp(graph,task)->succnum)

/* Returns the number of direct predecesors of a task */
#define tg_pred_num(graph, task)	(tgp(graph,task)->prenum)

/* Returns the list of direct succesors of a task */
#define tg_succ(graph, task)	(tgp(graph,task)->succ)

/* Returns the list of direct predecesors of a task */
#define tg_pred(graph, task)	(tgp(graph,task)->pred)

#endif /* TG_H */


/************************************************************************
  This program is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/OmpSCR/
	e-mail: ompscr@zion.deioc.ull.es

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

**************************************************************************/
/*---------------------------------------------------------------------------
 * Tg v 3.0
 *
 * tg.c	--  	Performance Evaluator
 * 		Task graph ADT definition
 * v 2.2
 *
 *		NOTE: This file has chage completly from the version 1.0
 *			'cause all the interface and ADT is different
 *
 * Copyright (C) 1999, Arturo González-Escribano
 *
 * v 2.3
 *	Copyright (C) 2004, Arturo González-Escribano
 *	Adapted by Arturo González-Escribano for OmpSCR, Jun 2004
 *	(Portable allocation statements)
 *
 *---------------------------------------------------------------------------
 */
#define TG_C

#include <stdio.h>
#include "tg.h"
#include "AError.h"
#include <stdlib.h>
#include <string.h>


/*---------------------------------------------------------------------------
 * 1. PUBLIC TASK INTERFACE FUNCTIONS:
 *---------------------------------------------------------------------------
 */



/* --------------------------------------------------------------------
 * INITIALIZE GRAPH
 */
tg	tg_init(int resources)
{
	tg	graph = NULL;

	/* 1. CHECKING INPUT */
	if (resources < 0)
		Asystem_error("tg_init","Improper number of resources input","");

	/* 2. CREATING SPACE FOR THE GRAPH STRUCTURE */
	graph = (tg) malloc( sizeof(tg_graph) );
	if ( graph == NULL ) {
		Asystem_error("tg_init", "Not enough memory", "graph");
		}

	/* 3. FILLING FIELDS */
	graph->tasknum = 0;
	graph->root = TG_NULLID;
	graph->resnum = resources;
	graph->maxid = TG_NULLID;
	graph->idarray = NULL;

	/* 4. END */
	return graph;
}




/* --------------------------------------------------------------------
 * DELETE GRAPH
 */
void tg_delete(tg graph)
{
	int	count;

	/* 0. IF NULL POINTER, END */
	if (graph==NULL) return;

	/* 1. DELETING ALL TASK */
	for (count = 0;
		count <= tg_maxid(graph);
		count ++)
		tg_del_node(graph, count);

	/* 2. FREEING INTERNAL SPACE */
	free(graph->idarray); graph->idarray=NULL;
	free(graph); graph=NULL;

	/* 3. END */
}



/* --------------------------------------------------------------------
 * DUPLICATE THE GRAPH
 */
tg tg_dup(tg graph)
{
	tg		g2;
	int		node;
	task_list	arcs=NULL;
	int		elements, count;
	int		resource;

	/* 1. INITIALIZE THE NEW GRAPH */
	g2 = tg_init(tg_resnum(graph));

	/* 2. FOR ALL NODES ON THE OLD ONE */
	for (node = 0;
		node <= tg_maxid(graph);
		node ++ )
		{
		/* 2.1. ... THAT EXISTS ... */
		if (!tg_id(graph, node))
			continue;

		/* 2.2. CREATE NODE */
		tg_add_node(g2, node);

		/* 2.3. COPY INFORMATION */
		for (resource = 0;
			resource < tg_resnum(graph);
			resource ++ )
			{
			tg_set_resource(g2, node, resource, tg_get_resource(graph, node, resource));
			}

		/* 2.4. CREATE ARCS FOR SUCCESORS */
		elements = tg_succ_num(graph, node);
		arcs = (task_list) malloc( sizeof(tg_task) * elements);
		if ( arcs == NULL ) {
			Asystem_error("tg_dup", "Not enough memory", "arcs");
			}
		memcpy(arcs,tg_succ(graph,node), sizeof(tg_task)*elements );

		for (count = 0;
			count < elements;
			count ++ )
			{
			/* 2.4.1. IF SUCCESOR DOES NOT EXIST YET, CREATE */
			if (!tg_id(g2, arcs[count]))
				tg_add_node(g2, arcs[count]);

			/* 2.4.2. ADD ARC */
			tg_add_arc(g2, node, arcs[count]);
			}
		free(arcs); arcs=NULL;
		}

	/* 3. SET ROOT */
	tg_set_root(g2, tg_root(graph));

	/* 4. RETURN THE NEW GRAPH */
	return g2;
}


/* --------------------------------------------------------------------
 * COPY RESOURCES VALUES FROM A GRAPH TO ANOTHER
 */
void tg_dup_resources(tg g1, tg g2)
{
	int	count, resource;

	/* 0. CHECK THE RESOURCE NUMBERS ON THE GRAPHS */
	if (tg_resnum(g1) != tg_resnum(g2))
		Asystem_error("tg_dup_resources","Different resnum","");

	/* 1. FOR ALL TASKS ON THE ORIGIN GRAPH */
	for (count = 0;
		count <= tg_maxid(g1);
		count ++ )
		{
		/* 1.1. ... THAT EXISTS ON THE TWO GRAPHS ... */
		if (!tg_id(g1, count) || !tg_id(g2,count))
			continue;

		/* 1.2. COPY RESOURCE VALUES */
		for (resource = 0;
			resource < tg_resnum(g1);
			resource ++ )
			{
			tg_set_resource(g2, count, resource, tg_get_resource(g1, count, resource));
			}
		}

	/* 2. END */
}



/* --------------------------------------------------------------------
 * ADD NODE
 */
Bool tg_add_node(tg graph, tg_task id)
{
	tg_ptask	ptask=NULL;
	int		count;

	/* 0. CHECKING THE IDENTIFIER */
	if (tg_id(graph, id))
		return FALSE;

	/* 1. CREATING SPACE FOR THE TASK */
	ptask = (tg_ptask) malloc( sizeof(tg_tsk) );
	if ( ptask == NULL ) {
		Asystem_error("tg_add_node", "Not enough memory", "ptask");
		}

	/* 2.1. ADDING ONE TO THE NUMBER OF TASKS */
	graph->tasknum ++;

	/* 2.2. IF id IS GREATER THAN maxid */
	if (id > graph->maxid)
		{
 		/* 2.1. ASK FOR MORE SPACE */
		graph->idarray = (ptask_list) realloc(graph->idarray, sizeof(tg_ptask)*(id+1));
		if ( graph->idarray == NULL ) {
			Asystem_error("tg_add_node","Not enough memory", "idarray" );
			}

		/* 2.2. INITIALIZE THE NEW POSITIONS */
		for (count = graph->maxid + 1;
			count < id;
			count ++ )
			{
			graph->idarray[count] = NULL;
			}

		/* 2.3. CHANGE TO THE NEW maxid */
		graph->maxid = id;
		}
	graph->idarray[id] = ptask;

	/* 3. SETTING THE TASK IDENTIFICATION */
	ptask->tid = id;

	/* 5. INITIALIZING TASK FIELDS */
	ptask->resour = NULL;
	ptask->resour = (double *) malloc( sizeof(double) * ( graph->resnum ));
	if ( ptask->resour == NULL ) {
		Asystem_error("tg_add_node", "Not enough memory", "resour");
		}
	for (count = 0;
		count < graph->resnum;
		count++ )
		{
		ptask->resour[ count ] = 0.0;
		}
	ptask->prenum = 0;
	ptask->pred = NULL;
	ptask->succnum = 0;
	ptask->succ = NULL;

	return TRUE;
}






/* --------------------------------------------------------------------
 * DELETE NODE
 */
void tg_del_node(tg graph, tg_task task)
{
	int		count;
	task_list	todelete=NULL;
	int		elements;

	/* 0. CHECKING IDENTIFICATION */
	if (!tg_id(graph, task))
		return;

	/* 1. DELETING ARCS */
	elements = tg_pred_num(graph,task);
	todelete = (task_list) malloc( sizeof(tg_task) * elements );
	if ( todelete == NULL ) {
		Asystem_error("tg_del_node", "Not enough memory", "todelete 1");
		}
	memcpy(todelete, tg_pred(graph,task), sizeof(tg_task)*elements);

	for (count=0;
		count < elements;
		count ++ )
		{
		tg_del_arc(graph, todelete[count], task);
		}
	free(todelete); todelete=NULL;

	elements = tg_succ_num(graph,task);
	todelete = (task_list) malloc( sizeof(tg_task) * elements );
	if ( todelete == NULL ) {
		Asystem_error("tg_del_node", "Not enough memory", "todelete 2");
		}
	memcpy(todelete, tg_succ(graph,task), sizeof(tg_task)*elements);

	for (count=0;
		count < elements;
		count ++ )
		{
		tg_del_arc(graph, task, todelete[count]);
		}
	free(todelete); todelete=NULL;

	/* 2. FREEING INTERNAL SPACE */
	free(tgp(graph,task)->resour); tgp(graph,task)->resour = NULL;

	/* 3. FREEING TASK SPACE */
	free(tgp(graph,task)); tgp(graph,task) = NULL;

	/* 4. CHANGING GRAPH STRUCTURE */
	graph->tasknum --;
	graph->idarray[task] = NULL;
	if (task == graph->maxid)
		graph->maxid --;	/* Innecesary reallocate */

	/* 5. END */
}


/* --------------------------------------------------------------------
 * ADD ARC
 */
void tg_add_arc(tg graph, tg_task task_from, tg_task task_to)
{
	tg_ptask	pfrom, pto;

	/* 1. CHECK TASKS IDENTIFIERS */
	if (!tg_id(graph, task_from) || !tg_id(graph,task_to))
		Asystem_error("tg_add_arc","Unknown task","Cheking task ids");

	/* 2. OBTAINING POINTERS */
	pfrom = tgp(graph, task_from);
	pto = tgp(graph, task_to);

	/* 3. IF ARC ALREADY EXISTS, RETURN */
	if (tg_arc(graph, task_from, task_to))
		return;

	/* 4. CREATING SPACE IN NODES */
	pfrom->succnum ++;
	pto->prenum ++;

	pfrom->succ = (task_list)realloc(pfrom->succ, sizeof(tg_task)*(pfrom->succnum));
	if ( pfrom->succ == NULL ) {
		Asystem_error("tg_add_arc", "Not enough memory", "pfrom->succ");
		}
	pto->pred = (task_list)realloc(pto->pred, sizeof(tg_task)*(pto->prenum));
	if ( pto->pred == NULL ) {
		Asystem_error("tg_add_arc", "Not enough memory", "pto->pred");
		}

	/* 5. INITIALIZATING POINTERS */
	pfrom->succ[ pfrom->succnum -1 ] = task_to;
	pto->pred[ pto->prenum -1 ] = task_from;
}



/* --------------------------------------------------------------------
 * DELETE ARC
 */
void tg_del_arc(tg graph, tg_task task_from, tg_task task_to)
{
	int	count;
	Bool	deleted;
	tg_ptask	pfrom, pto;

	/* 0. OBTAINING POINTERS */
	pfrom = tgp(graph, task_from);
	pto = tgp(graph, task_to);

	/* 1. IF ARC DOES NOT EXIST, RETURN */
	if (!tg_arc(graph, task_from, task_to))
		return;

	/* 2. CHANGING TO NODE STRUCTURE */
	/* 2.1. FOR ALL PRED, COMPARE UNTIL FIND THE ONE THAT MUST BE DELETED */
	for ( deleted = FALSE, count=0 ;
		count < pto->prenum;
		count ++ )
		{
		if ( ! deleted)
			{
			if ( task_from == pto->pred[count] )
				{
		/* 2.1.1. FINDED, CHANGE FLAG */
				deleted = TRUE;
				}
			}
		else
			{
		/* 2.1.2. MOVE ALL THE REST POINTERS BACK */
			pto->pred[count - 1] = pto->pred[count];
			}
		}

	/* 2.2. REALLOCATING IN FEW SPACE */
	pto->prenum --;
	pto->pred = (task_list)realloc(pto->pred, sizeof(tg_task)*(pto->prenum));
	if ( pto->pred == NULL ) {
		Asystem_error("tg_del_arc", "Not enough memory", "pto->pred");
		}

	/* 3. CHANGING FROM NODE STRUCTURE */
	/* 3.1. FOR ALL SUCC, COMPARE UNTIL FIND THE ONE THAT MUST BE DELETED */
	for ( deleted = FALSE, count=0 ;
		count < pfrom->succnum;
		count ++ )
		{
		if ( ! deleted)
			{
			if ( task_to == pfrom->succ[count] )
				{
		/* 3.1.1. FINDED, CHANGE FLAG */
				deleted = TRUE;
				}
			}
		else
			{
		/* 3.1.2. MOVE ALL THE REST POINTERS BACK */
			pfrom->succ[count - 1] = pfrom->succ[count];
			}
		}

	/* 3.2. REALLOCATING IN FEW SPACE */
	pfrom->succnum --;
	pfrom->succ = (task_list)realloc(pfrom->succ, sizeof(tg_task)*(pfrom->succnum));
	if ( pfrom->succ == NULL ) {
		Asystem_error("tg_del_arc", "Not enough memory", "pfrom->succ");
		}


	/* 4. END */
}



/* --------------------------------------------------------------------
 * CHANGES TASK IDENTIFICATION
 */
Bool tg_change_id(tg graph, tg_task task, tg_task id)
{
	tg_task		target;
	int 		count, count2;

	/* 1. IF id EXISTS OR task DOES NOT, RETURN FALSE */
	if (tg_id(graph,id) || !tg_id(graph,task))
		return FALSE;

	/* 2. IF id IS GREATER THAN maxid */
	if (id > graph->maxid)
		{
 		/* 2.1. ASK FOR MORE SPACE */
		graph->idarray = (ptask_list)realloc(graph->idarray, sizeof(tg_ptask)*(id+1));
		if ( graph->idarray == NULL ) {
			Asystem_error("tg_change_id","Not enough memory", "idarray");
			}

		/* 2.2. INITIALIZE THE NEW POSITIONS */
		for (count = graph->maxid + 1;
			count < id;
			count ++ )
			{
			graph->idarray[count] = NULL;
			}

		/* 2.3. CHANGE TO THE NEW maxid */
		graph->maxid = id;
		}

	/* 3. CHANGE POINTER */
	tgp(graph,task)->tid = id;
	graph->idarray[id] = graph->idarray[task];
	graph->idarray[task] = NULL;

	/* 4. CHANGE THE SUCCESORS AND PREDECESSORS POINTERS */
	for (count = 0;
		count < tg_pred_num(graph, id);
		count ++ )
		{
		target = tgp(graph, id)->pred[count];
		for (count2 = 0;
			count2 < tg_succ_num(graph, target);
			count2 ++ )
			{
			if (tgp(graph, target)->succ[count2] == task)
				{
				tgp(graph, target)->succ[count2] = id;
				}
			}
		}

	for (count = 0;
		count < tg_succ_num(graph, id);
		count ++ )
		{
		target = tgp(graph, id)->succ[count];
		for (count2 = 0;
			count2 < tg_pred_num(graph, target);
			count2 ++ )
			{
			if (tgp(graph, target)->pred[count2] == task)
				tgp(graph, target)->pred[count2] = id;
			}
		}


	/* 5. END */
	return TRUE;
}



/* --------------------------------------------------------------------
 * MINIMUM FREE IDENTIFIER
 */
int tg_free_id(tg graph)
{
	int	count;
	Bool	found;

	/* 1. CHECK IF THERE ARE IDENTIFIERS */
	if (graph->maxid < 0)
		return 0;

	/* 2. CHECK IF THERE ARE NOT HOLES IN THE INDEX ARRAY */
	if (tg_nodes(graph) == tg_maxid(graph)+1)
		return graph->maxid + 1;


	/* 3. LOCATE A NULL POINTER ON THE ARRAY */
	for (count = 0, found = FALSE;
		count <= graph->maxid && !found;
		count ++ )
		{
		if (graph->idarray[count] == NULL)
			found = TRUE;
		}

	/* 3. IF FOUND RETURN THE IDENTIFIER */
	if (found)
		return count - 1;

	/* 4. IF NOT, ERROR */
	else Asystem_error("tg_free_id","No NULL pointer in array","");

	/* 5. END */
	return 0;
}




/* --------------------------------------------------------------------
 * ARC EXISTS
 */
Bool tg_arc(tg graph, tg_task task_from, tg_task task_to)
{
	int	count;
	Bool	found;

	/* 1. TRY TO LOCATE TASK_TO AS SUCCESOR OF TASK_FROM */
	for ( found = FALSE, count = 0;
		count < tgp(graph,task_from)->succnum && !found ;
		count ++ )
		{
		if (tgp(graph,task_from)->succ[count] == task_to)
			found = TRUE;
		}

	/* 2. RETURN found */
	return found;
}






/*---------------------------------------------------------------------------
 * 2. HIGH LEVEL OPERATIONS AND INFORMATION ABOUT THE GRAPH STRUCTURE
 *---------------------------------------------------------------------------
 */

/* --------------------------------------------------------------------
 * DELETE ALL ARRIVING EDGES OF A NODE
 */
void tg_del_arcs_A(tg graph, tg_task node) {
	task_list	predlist=NULL;
	int		count;
	int		elements;

	/* 1. GET THE PREDECESSORS LIST OF THE NODE */
	elements = tg_pred_num(graph, node);
	predlist = (task_list) malloc( sizeof(tg_task)*elements);
	if ( predlist == NULL ) {
		Asystem_error("tg_del_arcs_A", "Not enough memory", "predlist");
		}
	memcpy(predlist, tg_pred(graph,node), sizeof(tg_task)*elements );

	/* 2. FOR ALL THE ARCS ON THE LIST */
	for (count = 0;
		count < elements;
		count ++ )
		{
		/* 2.1. DELETE ARC ORIGIN - SUCCESOR */
		tg_del_arc(graph, predlist[count], node);
		}

	/* 3. FREEING LIST SPACE */
	free(predlist); predlist = NULL;

	/* 4. END */
	}

/* --------------------------------------------------------------------
 * DELETE ALL LEAVING EDGES OF A NODE
 */
void tg_del_arcs_L(tg graph, tg_task node) {
	task_list	succlist=NULL;
	int		count;
	int		elements;

	/* 1. GET THE SUCCESORS LIST OF THE NODE */
	elements = tg_succ_num(graph, node);
	succlist = (task_list)malloc(sizeof(tg_task)*elements);
	if ( succlist == NULL ) {
		Asystem_error("tg_del_arcs_L", "Not enough memory", "succlist");
		}
	memcpy(succlist, tg_succ(graph,node), sizeof(tg_task)*elements);

	/* 2. FOR ALL THE ARCS ON THE LIST */
	for (count = 0;
		count < elements;
		count ++ )
		{
		/* 2.1. DELETE ARC ORIGIN - SUCCESOR */
		tg_del_arc(graph, node, succlist[count]);
		}

	/* 3. FREEING LIST SPACE */
	free(succlist); succlist = NULL;

	/* 4. END */
	}

/* --------------------------------------------------------------------
 * MOVE SUCCESORS FROM A NODE TO ANOTHER
 */
void tg_move_succ(tg graph, tg_task task_orig, tg_task task_dest)
{
	task_list	succlist=NULL;
	int		count;
	int		elements;

	/* 1. GET THE SUCCESORS LIST OF THE ORIGIN */
	elements = tg_succ_num(graph, task_orig);
	succlist = (task_list)malloc(sizeof(tg_task)*elements);
	if ( succlist == NULL ) {
		Asystem_error("tg_move_succ", "Not enough memory", "succlist");
		}
	memcpy(succlist, tg_succ(graph,task_orig), sizeof(tg_task)*elements);

	/* 2. FOR ALL THE ARCS ON THE LIST */
	for (count = 0;
		count < elements;
		count ++ )
		{
		/* 2.1. DELETE ARC ORIGIN - SUCCESOR */
		tg_del_arc(graph, task_orig, succlist[count]);

		/* 2.2. CREATE ARC DEST - SUCCESOR */
		tg_add_arc(graph, task_dest, succlist[count]);

		}

	/* 3. FREEING LIST SPACE */
	free(succlist); succlist = NULL;

	/* 4. END */
}


/* --------------------------------------------------------------------
 * MOVE PREDECESORS FROM A NODE TO ANOTHER
 */
void tg_move_pred(tg graph, tg_task task_orig, tg_task task_dest)
{
	task_list	predlist=NULL;
	int		count;
	int		elements;

	/* 1. GET THE PREDECESORS LIST OF THE ORIGIN */
	elements = tg_pred_num(graph, task_orig);
	predlist = (task_list) malloc( sizeof(tg_task)*elements);
	if ( predlist == NULL ) {
		Asystem_error("tg_move_pred", "Not enough memory", "predlist");
		}
	memcpy(predlist, tg_pred(graph,task_orig), sizeof(tg_task)*elements );

	/* 2. FOR ALL THE ARCS ON THE LIST */
	for (count = 0;
		count < elements;
		count ++ )
		{
		/* 2.1. DELETE ARC ORIGIN - SUCCESOR */
		tg_del_arc(graph, predlist[count], task_orig);

		/* 2.2. CREATE ARC DEST - SUCCESOR */
		tg_add_arc(graph, predlist[count], task_dest);

		}

	/* 3. FREEING NLIST SPACE */
	free(predlist); predlist = NULL;

	/* 4. END */
}



/* --------------------------------------------------------------------
 * LIST OF NODES OF PREDECESSORS GRADE n
 */
int tg_pred_grade(tg graph, int grade, task_list *resultlist)
{
	int 		count, already, nodes;
	task_list	list=NULL;

	/* 1. COUNT THE NUMBER OF NODES */
	for (count = 0, nodes = 0;
		count <= tg_maxid(graph);
		count ++ )
		{
		if (!tg_id(graph, count))
			continue;

		if (tg_pred_num(graph, count) == grade)
			nodes ++;
		}


	/* 2. GET SPACE FOR THE NULL LIST */
	list = (task_list) malloc( sizeof(tg_task) * (nodes + 1) );
	if ( list == NULL ) {
		Asystem_error("tg_pred_grade", "Not enough memory", "list");
		}

	/* 3. FILL THE LIST */
	 for (count = 0, already = 0;
		count <= tg_maxid(graph) && already < nodes;
		count ++ )
		{
		if (!tg_id(graph, count))
			continue;

		if (tg_pred_num(graph, count) == grade)
			{
			list[already] = count;
			already ++;
			}
		}

	/* 4. RETURN THE LIST */
	*resultlist = list;
	return nodes;
}


/* --------------------------------------------------------------------
 * LIST OF NODES OF SUCCESORS GRADE n
 */
int tg_succ_grade(tg graph, int grade, task_list *resultlist)
{
	int 		count, already, nodes;
	task_list	list=NULL;

	/* 1. COUNT THE NUMBER OF NODES */
	for (count = 0, nodes = 0;
		count <= tg_maxid(graph);
		count ++ )
		{
		if (!tg_id(graph, count))
			continue;

		if (tg_succ_num(graph, count) == grade)
			nodes ++;
		}


	/* 2. GET SPACE FOR THE NULL LIST */
	list = (task_list) malloc( sizeof(tg_task) * (nodes + 1) );
	if ( list == NULL ) {
		Asystem_error("tg_succ_grade", "Not enough memory", "list");
		}

	/* 3. FILL THE LIST */
	 for (count = 0, already = 0;
		count <= tg_maxid(graph) && already < nodes;
		count ++ )
		{
		if (!tg_id(graph, count))
			continue;

		if (tg_succ_num(graph, count) == grade)
			{
			list[already] = count;
			already ++;
			}
		}

	/* 4. RETURN THE LIST */
	*resultlist = list;
	return nodes;
}





/***************************************************************/
/*
 * From v1.0	(Adapted to the new internal structure)
 */

/* --------------------------------------------------------------------
 * READ THE GRAPH STRUCTURE FROM A TEXT FILE
 */
tg	tg_read(FILE *fp)
{
	tg	graph;
	tg_task	tid;
	int	tasknum, resnum, i;
	int	succnum, succ, t;
	double	rvalue;
	char	comments[256];
	task_list	list=NULL;
	int		elements;
	int	check;

	/* SKIP INITIAL COMMENTS BEGINING WITH # */
	check = fscanf(fp, "%[#]", comments);
	while(check==1) {
		fscanf(fp, "%[^\n]", comments);
		fscanf(fp, "\n");
		check = fscanf(fp, "%[#]", comments);
		}

	/* 1. READ NUMBER OF TASK AND RESOURCES */
	tasknum=-1;
	check = fscanf(fp, "T:%d\n", &tasknum);
	CheckError(check==1 && tasknum>0, "reading graph file: Number of tasks");
	check = fscanf(fp, "R:%d\n", &resnum);
	CheckError(check==1 && resnum>=0, "reading graph file: Number of resources");

	/* 2. INITIALIZE GRAPH WITH NUMBER OF RESOURCES */
	graph = tg_init(resnum);


	/* 3. READ TASKS */
	for (t = 0; t < tasknum; t++)
		{
		/* 3.1. READ NUMBER OF TASK */
		check = fscanf(fp,"t%d:",&tid);
		CheckError(check==1 && tid>=0, "reading graph file: Reading a task number");

		/* 3.2. CREATE TASK IF IT DOES NOT EXIST */
		if (!tg_id(graph, tid))
			tg_add_node(graph, tid);

		/* 3.3. RESOURCES */
		for ( i = 0; i < resnum; i++) {
			check = fscanf(fp,"%lf",&rvalue);
			CheckError(check==1 && rvalue>=0.0, "reading graph file: Reading task resource values");
			tg_set_resource(graph, tid, i, rvalue);
			}

		/* 3.4. NUMBER OF SUCCESORS */
		check = fscanf(fp,"%[ ]s%d:",comments,&succnum);
		CheckError(check==2 && succnum>=0.0, "reading graph file: Reading the number of successors of a task");

		/* 3.5. SUCCESORS */
		for ( i = 0; i < succnum; i++) {
			check = fscanf(fp,"%d",&succ);
			CheckError(check==1 && succ>=0, "reading graph file: Reading the successors of a task");
			if (!tg_id(graph, succ))
				{
				tg_add_node(graph, succ);
				}
			tg_add_arc(graph, tid, succ);
			}

		/* 3.6. END ON LINE */
		fscanf(fp,"%[ \n]", comments);
		}

	/* 4. DETECT ROOT */
	elements = tg_pred_grade(graph, 0, &list);
	if (elements == 1) tg_set_root(graph, list[0]);
	free(list); list = NULL;

	/* 5. RETURN THE NEW GRAPH */
	return graph;
}


/* --------------------------------------------------------------------
 * WRITE THE GRAPH STRUCTURE ON A TEXT FILE
 */
void   	tg_write(FILE *fp,tg graph)
{
        int     tid,count;
	task_list	list=NULL;
	int		elements;

	Assert(graph != NULL);

	/* 1. HEADING */
	fprintf(fp,"T: %d\n",tg_nodes(graph));
	fprintf(fp,"R: %d\n",tg_resnum(graph));

	/* 2. TASKS */
	for (tid = 0;
		tid <= tg_maxid(graph);
		tid ++ )
		{
		if (tg_id(graph, tid))
			{
			fprintf(fp,"t%d: ",tid);
			for (count = 0;
				count < tg_resnum(graph);
				count++)
				{
				fprintf(fp," %.4f",tg_get_resource(graph, tid, count));
				}
			fprintf(fp," s%d:",tg_succ_num(graph, tid));

			list = tg_succ(graph, tid);
			elements = tg_succ_num(graph, tid);

			for (count = 0;
				count < elements;
				count ++ )
				{
				fprintf(fp," %d",list[count]);
				}
			fprintf(fp,"\n");
			}
		}
}


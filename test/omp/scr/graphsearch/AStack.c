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
/*
* AStack.c:
*	Module for integer type stack
*
* Version:	1.0
* Date:		Mon Jun  7 1999
* Copyright (C) 1999, Arturo Gonzalez Escribano
*
* Adapted by Arturo Gonzalez-Escribano for OmpSCR, Jun 2004
*/

#include <stdio.h>
#include <stdlib.h>
#include "ATypes.h"
#include "AError.h"
#include "AStack.h"


/* --------------------------------------------------------------------
* 1. INITIALIZE FUNCTION
*/
Astack Ast_init (void)
{
	Astack	stack = NULL;

	/* 1. CREATING SPACE FOR THE NEW POINTER */
	stack = (Astack) malloc(sizeof(AstNode));
	if (stack == NULL)
		Asystem_error("Ast_init","Not enough memory", "-");

	/* 2. INITIALIZE THE POINTER TO NULL */
	*stack = NULL;

	/* 3. RETURNING THE NEW STACK */
	return stack;
}



/* --------------------------------------------------------------------
 * PUSH OPERATION
 */
void Ast_push (Astack stack, int datum)
{
	AstNode	node = NULL;

	/* 1. CREATING SPACE FOR THE NEW NODE */
	node = (AstNode) malloc(sizeof(Astnode));
	if (node == NULL)
		Asystem_error("Ast_push","Not enough memory", "-");


	/* 2. INITIALIZING FIELDS */
	node->datum = datum;
	node->next = *stack;

	/* 3. CHANGING THE STACK POINTER */
	*stack = node;
}



/* --------------------------------------------------------------------
 * POP OPERATION
 */
int Ast_pop (Astack stack)
{
	AstNode	node = NULL;
	int	datum;

	/* 0. CHECK EMPTY */
	if (Ast_more(stack)==FALSE)
		Asystem_error("Ast_pop","Empty stack","");

	/* 1. REMEMBER THE NEXT POINTER */
	node = (*stack)->next;

	/* 2. REMEMBER THE TASK POINTER */
	datum = (*stack)->datum;

	/* 3. FREEING THE SPACE OF THE NODE */
	free(*stack);

	/* 4. CHANGING THE STACK POINTER */
	*stack = node;

	/* 5. RETURNING THE TASK POINTER */
	return datum;
}


/* --------------------------------------------------------------------
 * CLEAN STACK
 */
void Ast_clean (Astack stack)
{
	AstNode	node = NULL;

	/* 1. FOR ALL NODES ON THE STACK, FREE SPACE */
	for (node = *stack;
		node != NULL;
		node = *stack )
		{
		*stack = node->next;
		free(node);
		}

	/* 2. END ( *stack IS JUST NULL) */
}




/* --------------------------------------------------------------------
 * DELETE A STACK
 */
void Ast_delete (Astack stack)
{
	/* 1. CLEAN THE STACK ELEMENTS */
	Ast_clean (stack);

	/* 2. FREE SPACE OF THE STACK POINTER */
	free(stack);
	stack = NULL;

	/* 3. END */
}



/* --------------------------------------------------------------------
 * ARE THERE MORE ELEMENTS ON THE STACK ?
 */
Bool Ast_more (Astack stack)
{
	/* 1. COMPARE THE POINTER OF THE STACK WITH NULL */
	if (*stack == NULL)
		return FALSE;
	else
		return TRUE;

	/* 2. END */
}





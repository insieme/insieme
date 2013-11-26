/*
* AStack.h:
*	Module for integer type stack
*
* Version:	1.0
* Date:		Mon Jun  7 1999
* Copyright (C) 1999, Arturo Gonzalez Escribano
*/

#ifndef ASTACK_H
#define ASTACK_H

#include"ATypes.h"

/*
* 1. STACK NODE STRUCTURE
*/
typedef struct Astnode {
	int		datum;
	struct Astnode	*next;
} Astnode;


/*
 * 2. RENAMING POINTER TO NODE AND CREATING STACK POINTER
 */
typedef Astnode	*AstNode;
typedef Astnode	**Astack;


	
/*
 * 3. PROTOTYPES OF INTERFACE FUNCTIONS
 */
Astack Ast_init(void);		/* Creates a new stack (NULL) */
void Ast_delete(Astack);	/* Deletes the stack and ALL the elements */
void Ast_clean(Astack);		/* Clean the stack. All elements are lost */	

void Ast_push(Astack, int);	/* Include element on the stack */	
int Ast_pop(Astack);		/* Extract first element of the stack */

Bool Ast_more(Astack);		/* True if there are alements on the stack */

/*
 * 4. MACROS
 */
/* Returns the first element on the stack without poping */	
#define Ast_top(st)	((*st)->datum)

#endif /* STACK_H */

/*
* AError.h:
*	Macro definitions for system error treatment
*
* Version:	1.0
* Date:		Thu May  6, 1999
* Copyright (C) 1999, Arturo Gonzalez Escribano
*/

#ifndef AERROR_H
#define AERROR_H

#define Asystem_error(origin,diagnostic,checkpoint)			\
		{							\
		fprintf(stderr,						\
			"Error in FILE %s[%d] %s: %s - %s\n",		\
			__FILE__, __LINE__,				\
			origin, diagnostic,checkpoint );		\
		exit(1);						\
		}

#ifdef DEBUG
#define Assert(E)	if (! (E))	\
			printf("Assert failed in file %s, line %d: '%s'\n",\
							__FILE__,__LINE__,#E);
#else
#define Assert(E)       {} /* no Assert */
#endif

#define CheckError(E,checkpoint)	if (! (E)) {	\
			fprintf(stderr,"Error %s\n", checkpoint); exit(1); }


#endif /* AERROR_H */


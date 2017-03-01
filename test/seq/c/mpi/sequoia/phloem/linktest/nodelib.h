/*==================================================*/
/* MPI Link-Test for Sequoia                        */
/* Node library / Header file                       */
/*==================================================*/

#ifndef NODELIB_H_
#define NODELIB_H_

#include "common.h"


/*==================================================*/
/* Constants */

#define MAXIDLEN 20

#define NODELIB_OK            0
#define NODELIB_ERR_MPI      -1
#define NODELIB_ERR_GETID    -2
#define NODELIB_ERR_DEFAULTS -3


/*==================================================*/
/* Types */

typedef struct nodeid_d
{
  char name[MAXIDLEN];
} nodeid_t;


/*==================================================*/
/* Prototypes */

int nodelib_init(MPI_Datatype *idtype);
int nodelib_getID(nodeid_t *id);
int nodelib_sameID(nodeid_t *id1, nodeid_t *id2);
int nodelib_getDefaultRoot(int *root,int size);
int nodelib_getDefaultNeighbors(int *num, int *nodelist, int root, int size);


/*==================================================*/
/* The End. */

#endif /* NODELIB_H_ */

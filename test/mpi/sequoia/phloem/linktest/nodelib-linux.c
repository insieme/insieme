/*==================================================*/
/* MPI Link-Test for Sequoia                        */
/* Node library / Implementation                    */
/*==================================================*/

#include <string.h>
#include "nodelib.h"


/*==================================================*/
/* Init the node lib 
 * Warning: can be a collective operation 
 *          must be called after MPI_Init */

int nodelib_init(MPI_Datatype *idtype)
{
  int err;

  err=MPI_Type_contiguous(MAXIDLEN,MPI_CHAR,idtype);
  if (err!=MPI_SUCCESS)
    return NODELIB_ERR_MPI;

  err=MPI_Type_commit(idtype);
  if (err!=MPI_SUCCESS)
    return NODELIB_ERR_MPI;

  return NODELIB_OK;
}


/*==================================================*/
/* Query the local node ID */

int nodelib_getID(nodeid_t *id)
{
  int err;

  err=gethostname(id->name,MAXIDLEN);
  if (err==0)
    {
      id->name[MAXIDLEN-1]=(char) 0;
      return NODELIB_OK;
    }
  else
    return NODELIB_ERR_GETID;
      
}


/*==================================================*/
/* Compare two node IDs */

int nodelib_sameID(nodeid_t *id1, nodeid_t *id2)
{
  if (strcmp(id1->name,id2->name)==0)
    return 1;
  else
    return 0;
}


/*==================================================*/
/* get a default root node if none is specified */

int nodelib_getDefaultRoot(int *root,int size)
{
  /* pick the middle node */
  *root=size/2;
  return NODELIB_OK;
}


/*==================================================*/
/* get a default set of neighbors */
  
int nodelib_getDefaultNeighbors(int *num, int *nodelist, int root, int size)
{
  int i,ptr;

  if (root<0)
    return NODELIB_ERR_DEFAULTS;

  if (size<2)
    return NODELIB_ERR_DEFAULTS;

  *num=size-1;
  ptr=0;

  for (i=0; i<size; i++)
    {
      if (i!=root)
	{
	  nodelist[ptr]=i;
	  ptr++;
	}
    }

  return NODELIB_OK;
}


/*==================================================*/
/* The End. */

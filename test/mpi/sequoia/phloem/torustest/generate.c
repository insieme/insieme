#include <stdio.h>
#include <stdlib.h>

#define NORM(x,d)    ((x+d)%d)
#define TORUS(x,y,z,t) ((NORM(x,dx)+NORM(y,dy)*dx+NORM(z,dz)*dx*dy)*nc+t)

int main(int argc, char** argv)
{
  int dx,dy,dz,num,nc;
  int i,j,k,t;
  int a,b,c;

  if (argc!=5)
    {
      printf("Usage: generate <NCORE> <dx> <dy> <dz>\n");
      printf("       with dx,dy,dz being number of nodes in each\n");
      printf("       direction of a virtual torus and NCORE being.\n");
      printf("       the number of cores per node\n");
      exit(1);
    }
  
  nc=atoi(argv[1]);
  dx=atoi(argv[2]);
  dy=atoi(argv[3]);
  dz=atoi(argv[4]);

  num=dx*dy*dz;

  printf("%i\n",num*nc);
  printf("%i\n",nc);
  printf("%i\n",26);
  for (k=0; k<dz; k++)
    for (j=0; j<dy; j++)
      for (i=0; i<dx; i++)
	{
	  for (t=0; t<nc; t++)
	    {
	      printf("%i",TORUS(i,j,k,t));
	      for (a=-1; a<2; a++)
		for (b=-1; b<2; b++)
		  for (c=-1; c<2; c++)
		    {
		      if ((a!=0) || (b!=0) || (c!=0))
			printf(" %i",TORUS(i+a,j+b,k+c,t));
		    }
	      printf("\n");
	    }
	}
  return 0;
}


#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "particles.h"
#include "parameters.h"

particle *particles_read(char *filename, int *N);
void write_psi_file(parameters *P, particle *list, int N, char *filename);

particle *read_particles(char *filename, int *N) {	
	particle *list = NULL;
	FILE *file;
	int i, k;
	
	file = fopen(filename,"r");
	if(!file) {
		printf("Can't open file %s for reading\n",filename);
		exit(0);
	}
	fscanf(file, "%d ", N);
	
	list = (particle*)malloc(*N*sizeof(particle));
	if(!list) {
		printf("Cannot allocate memory for particle list\n");
		exit(0);
	}
	
	for(i=0; i<*N; i++) {
		list[i].disabled = 0;
		fscanf(file, "%d ", &list[i].id);
		fscanf(file, "%lf %lf ", &list[i].pos[0], &list[i].pos[1]);
		fscanf(file, "%lf %lf ", &list[i].v[0], &list[i].v[1]);
		fscanf(file, "%lf ", &list[i].m);
		fscanf(file, "%lf ", &list[i].sigma);
		for (k=0;k<2;k++) {
			list[i].f[k] = 0;
			list[i].f_old[k] = 0;
		}
		fscanf(file, "%d ", &list[i].kind);
	}
	fclose(file);
	return list;
}


void write_psi_file(parameters *P, particle *list, int N, char *filename) {
	FILE *file;
	int i;
	
	file=fopen(filename,"w");
	if(file==NULL) {
		printf("Could not open file %s! \n",filename);
		exit(1);
	}
	
	fprintf(file,"# PSI Format 1.0\n");
	fprintf(file,"#column[0]=\"x\" \n");
	fprintf(file,"#column[1]=\"y\" \n");
	fprintf(file,"#column[2]=\"z\" \n");
	fprintf(file,"#column[3]=\"Mass\" \n");
	fprintf(file,"#symbol[3]=\"m\" \n");
	fprintf(file,"#type[3]=float \n");
	fprintf(file,"%d 0 0\n",N);
	fprintf(file,"1 0 0\n");
	fprintf(file,"0 1 0\n");
	fprintf(file,"0 0 1\n");
	
	for(i=0;i<N;i++)
		fprintf(file, "%f %f %f %f\n", list[i].pos[0], list[i].pos[1], list[i].pos[2], list[i].m);
	
	fclose(file);
	return;
}


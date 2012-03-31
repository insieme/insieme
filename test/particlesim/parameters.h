#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

typedef struct _parameters {
	int energy;				/* print energy (0/1)  */
	
	double length[2];		/* size of area */
	int boundary_cond[4];	/* boundary conditions (east/north/west/south) */
	int n;					/* number of particles */
	
	/* force-related parameters */
	double g[2];			/* external forces */
	double r_cut;			/* cut-off radius */
	double sigma;
	double epsilon;
	
	/* time-related parameters */
	double t;				/* current time */
	double delt;			/* time step */
	double t_end;			/* simulation end time */
	double t_vis;			/* output time */
	double delt_vis;		/* output time step */
	
	/* temperature-related parameters */
	double av_mass;
	int scaling, scaling_steps;
	double temperature;
	
	/* energies */
	double e_pot;			/* potential energy */
	double e_kin;			/* kinetic energy */
	
} parameters;

/* Clears the supplied "Paramters" structure, setting all
 * parameters to their default values. */
void parameters_clear(parameters *para);

/* Prints the parameters structure supplied in human-readable
 * form to the supplied file handle (you can use stdout). 
 * The output is also a valid parameters file structure which
 * can be parsed by parameters_read. */
void parameters_print(const parameters* const para, FILE *file);

/* Reads paramters from the file "filename" and writes them to
 * "para". Use parameters_print to get an overview of the file structure.
 * Params can be omitted, in that case the original value is retained.
 * This enables the use of multiple files specifying different parameters. 
 * Returns true on success, false on failure. */
bool parameters_read(parameters *para, const char* filename);

/* ------------------------------------------------------------------------- */


typedef struct _parameter_data {
	char name[16];
	char format[16];
	unsigned offset;
	unsigned count;
	unsigned datasize;
} parameter_data;

#define NUM_PARAMS 19
static parameter_data *g_parameter_data[NUM_PARAMS];
 
void parameter_data_set(parameter_data *pd, const char* const name, const char* const format, const unsigned offset) {
	strncpy(pd->name, name, sizeof(pd->name));
	strncpy(pd->format, format, sizeof(pd->name));
	pd->offset = offset;
	pd->count = 1;
	pd->datasize = 0;
}

void parameter_data_set_ex(parameter_data *pd, const char* const name, const char* const format, const unsigned offset, const unsigned num, const unsigned datasize) {
	parameter_data_set(pd, name, format, offset);
	pd->count = num;
	pd->datasize = datasize;
}

void parameter_data_init(parameter_data *pd[NUM_PARAMS]) {
	for(int i=0; i<NUM_PARAMS; ++i)
		if(pd[i] == NULL) pd[i] = (parameter_data*)malloc(sizeof(parameter_data));

	parameter_data_set(pd[0], "energy", "%d", 0); 
	parameter_data_set_ex(pd[1], "length", "%lf", offsetof(parameters,length), 2, sizeof(double));
	parameter_data_set_ex(pd[2], "boundary_cond", "%d", offsetof(parameters,boundary_cond), 4, sizeof(int));
	parameter_data_set(pd[3], "n", "%d", offsetof(parameters,n));
	parameter_data_set_ex(pd[4], "g", "%lf", offsetof(parameters,g), 2, sizeof(double));
	parameter_data_set(pd[5], "r_cut", "%lf", offsetof(parameters,r_cut));
	parameter_data_set(pd[6], "sigma", "%lf", offsetof(parameters,sigma));
	parameter_data_set(pd[7], "epsilon", "%lf", offsetof(parameters,epsilon));
	parameter_data_set(pd[8], "t", "%lf", offsetof(parameters,t));
	parameter_data_set(pd[9], "delt", "%lf", offsetof(parameters,delt));
	parameter_data_set(pd[10], "t_end", "%lf", offsetof(parameters,t_end));
	parameter_data_set(pd[11], "t_vis", "%lf", offsetof(parameters,t_vis));
	parameter_data_set(pd[12], "delt_vis", "%lf", offsetof(parameters,delt_vis));
	parameter_data_set(pd[13], "av_mass", "%lf", offsetof(parameters,av_mass));
	parameter_data_set(pd[14], "scaling", "%d", offsetof(parameters,scaling));
	parameter_data_set(pd[15], "scaling_steps", "%d", offsetof(parameters,scaling_steps));
	parameter_data_set(pd[16], "temperature", "%lf", offsetof(parameters,temperature));
	parameter_data_set(pd[17], "e_pot", "%lf", offsetof(parameters,e_pot));
	parameter_data_set(pd[18], "e_kin", "%lf", offsetof(parameters,e_kin));
}

void parameters_clear(parameters *para) {
	para->energy = 0;
	para->length[0] = 50; para->length[1] = 50;
	for(int i=0; i<4; ++i) para->boundary_cond[i] = 0;
	para->n = 0;
	para->g[0] = 0; para->g[1] = 0;
	para->r_cut = 0;
	para->sigma = 0;
	para->epsilon = 0;
	para->t = 0;
	para->delt= 0.1;
	para->t_end = 0;
	para->t_vis = 0;
	para->delt_vis = 0;
	para->av_mass = 0;
	para->scaling = 0;
	para->scaling_steps = 0;
	para->temperature = 0;
	para->e_pot = 0;
	para->e_kin = 0;
}
 
void parameters_print(const parameters* const para, FILE *file)
{
	fprintf(file, "# ------------------------------------------------\n");
	fprintf(file, "energy:\t%d\n", para->energy);
	fprintf(file, "length:\t%f, %f\n", para->length[0], para->length[1]);
	fprintf(file, "boundary_cond:\t%d, %d, %d, %d\n", para->boundary_cond[0], 
		para->boundary_cond[1], para->boundary_cond[2], para->boundary_cond[3]);
	fprintf(file, "n:\t%d\n", para->n);
	fprintf(file, "g:\t%f, %f\n", para->g[0], para->g[1]);
	fprintf(file, "r_cut:\t%f\n", para->r_cut);
	fprintf(file, "sigma:\t%f\n", para->sigma);
	fprintf(file, "epsilon:\t%f\n", para->epsilon);
	fprintf(file, "t:\t%f\n", para->t);
	fprintf(file, "delt:\t%f\n", para->delt);
	fprintf(file, "t_end:\t%f\n", para->t_end);
	fprintf(file, "t_vis:\t%f\n", para->t_vis);
	fprintf(file, "delt_vis:\t%f\n", para->delt_vis);
	fprintf(file, "av_mass:\t%f\n", para->av_mass);
	fprintf(file, "scaling:\t%d\n", para->scaling);
	fprintf(file, "scaling_steps:\t%d\n", para->scaling_steps);
	fprintf(file, "temperature:\t%f\n", para->temperature);
	fprintf(file, "e_pot:\t%f\n", para->e_pot);
	fprintf(file, "e_kin:\t%f\n", para->e_kin);
	fprintf(file, "# ------------------------------------------------\n\n");
}

bool parameters_read(parameters *para, const char* filename) {
	if(!g_parameter_data[0]) parameter_data_init(g_parameter_data);
	
	FILE* file = fopen(filename, "r");
	assert(file!=NULL);
	
	char line[256], scanstring[64], *commentchar;
	while(fgets(line, 255, file))
	{
		if((commentchar = strchr(line, '#'))) *commentchar = '\0';
		for(int i=0; i<NUM_PARAMS; ++i)
		{
			sprintf(scanstring, "%s:", g_parameter_data[i]->name);
			if(strstr(line, scanstring)==line)
			{
				int loc = (int)para+g_parameter_data[i]->offset;
				char* strloc = strchr(line, ':')+1;
				for(unsigned j=0; j<g_parameter_data[i]->count; 
					++j, loc+=g_parameter_data[i]->datasize, strloc = strchr(line, ',')+1)
						sscanf(strloc, g_parameter_data[i]->format, loc);
				break;
			}
		}
	}
	fclose(file);
	return true;
}

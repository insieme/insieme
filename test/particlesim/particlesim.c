#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "simulation.h"
#include "parameters.h"
#include "io.h"
#include "image_output.h"


int main(int argc, char *argv[]) {
	if(argc!=3) {
		printf("usage: parsim [parameter file] [particle file]\nexample: parsim inputs/example2b.par inputs/example2b.list\n");
		return 1;
	}
	
	parameters p;
	parameters_clear(&p);
	parameters_read(&p, argv[1]);
	//parameters_print(&p, stdout);

	particle *list = read_particles(argv[2], &p.n);
	
	int len = (int)ceil((p.t_end - p.t)/p.delt) + 5;
	double *e_pot_list = (double*)calloc(len, sizeof(double));
	double *e_kin_list = (double*)calloc(len, sizeof(double));
	
	int i=0;
	while(p.t < p.t_end) {
		printf("."); 
		fflush(stdout);

		/* calculate force */
		p.e_pot = 0;
		force(&p, list);
	
		/* move particles */
		p.e_kin = 0;
		update(&p, list);

		/* save e_pot and e_kin */
		e_pot_list[i] = p.e_pot;
		e_kin_list[i] = p.e_kin;
			
		/* write output image */
		if(p.t_vis > p.delt_vis) {
			p.t_vis = 0.0;
			write_particle_image("out", list, p.n, p.length, i, 10);
		}

		/* next time-step */
		p.t += p.delt;
		p.t_vis += p.delt;
		i++;
	}
	printf("\n");
	
	if(p.energy)
		for(int i=0; i<len; ++i)
			printf("%16.8lf %16.8lf\n", e_pot_list[i], e_kin_list[i]);
	
	free(e_pot_list);
	free(e_kin_list);
	free(list);
	
	return 0;
}

#pragma once

#include <math.h>

#include "particles.h"
#include "parameters.h"

void force(parameters* parms, particle* list);
void update(parameters* parms, particle* list);

#define norm(vec) sqrt(vec[0]*vec[0]+vec[1]*vec[1]) 

void force(parameters* parms, particle* list) {
	double e_pot = 0.0;
	#pragma omp parallel for reduction(+:e_pot)
	for(int i=0; i<parms->n; ++i) {
		if(list[i].disabled) continue;
		double* f_i = list[i].f;	
		list[i].f_old[0] = f_i[0];
		list[i].f_old[1] = f_i[1];
		f_i[0] = (parms->g)[0];
		f_i[1] = (parms->g)[1];
		
		for(int j=i+1; j<parms->n; ++j) {
			if(list[j].disabled) continue;
			/* force i <-> j */
			double r[2] = { 
				fabs(list[j].pos[0]-list[i].pos[0]),
				fabs(list[j].pos[1]-list[i].pos[1]) };
			double normr = norm(r);
			if(normr > parms->r_cut) continue;
			double q = pow(list[i].sigma/normr,6);
			double f_ij = -24.0 * parms->epsilon*1.0/pow(normr,2) * q*(1-2*q);
			
			f_i[0] += f_ij*r[0];
			f_i[1] += f_ij*r[1];
			double* f_j = list[j].f;
			f_j[0] -= f_ij*r[0];
			f_j[1] -= f_ij*r[1];

			/* e_pot */
			e_pot += 0.5 * list[i].m * (f_i[0] * f_i[0] + f_j[1] * f_j[1]);
		}
	}
	parms->e_pot += e_pot;
}

void update(parameters* parms, particle* list) {
	double e_kin = 0.0;
	#pragma omp parallel for reduction(+:e_kin)
	for(int i=0; i<parms->n; ++i)	{
		particle *p = &list[i];

		double dt = parms->delt;
		p->v[0] += 0.5*dt * (p->f[0]+p->f_old[0]) / p->m;
		p->v[1] += 0.5*dt * (p->f[1]+p->f_old[1]) / p->m;
		p->pos[0] += dt*(p->v[0]+0.5*dt*p->f[0]/p->m);
		p->pos[1] += dt*(p->v[1]+0.5*dt*p->f[1]/p->m);

		
		/* collision detection (border) */
		double x = p->pos[0], y = p->pos[1];
		if((x<0 || x>(parms->length)[0]) || (y<0 || y>(parms->length)[1])) {
			/* TODO handle boundary conditions */
			p->disabled = 1;
		}
		
		/* e_kin */
		e_kin += 0.5 * p->m * (p->v[0]*p->v[0] + p->v[1]*p->v[1]);
	}
	parms->e_kin += e_kin;
}

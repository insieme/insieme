#pragma once

#include <stdio.h>
#include <stdlib.h>

typedef struct _particle {
	double pos[2];		/* particle coordinates */
	double v[2];		/* particle velocity */
	double m;			/* mass */
	double f[2];		/* forces on particle */
	double f_old[2];	/* old forces */
	double sigma;		/* sigma */
	int id;				/* particle id */
	int kind;			/* type of particle */
	int disabled;		/* flag for disabled particles */
} particle;

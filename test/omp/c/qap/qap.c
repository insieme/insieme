
#include <stdio.h>

#ifndef N
	#define N 10
#endif

#define bool int
#define true 1
#define false 0

typedef double value;


// the weight matric (how much has to be transfered between nodes)
value w[N][N];

// the distance matric (distance between nodes)
value d[N][N];


typedef struct _solution {
	struct _solution* head;
	int pos;
} solution;


solution* empty() { return 0; }

void print(solution* solution) {
	if(!solution) return;
	print(solution->head);
	printf("-%d", solution->pos);
}

bool contains(solution* solution, int pos) {
	if (!solution) return false;
	return solution->pos == pos || contains(solution->head, pos);
}



double eval(solution* sol) {

	// get mapping from solution	
	int pos[N];
	solution* cur = sol;
	int* cur_pos = pos;
	while(cur) {
		*cur_pos = cur->pos;
		cur_pos++; cur = cur->head;
	}
	

	double res = 0;
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			res += w[i][j] * d[pos[i]][pos[j]];
		}
	}
	return res;
}


double solve_rec(solution* partial, int level) {
	// terminal case
	if (level == 0) {
//		print(partial); printf(" = %f\n", eval(partial));
		return eval(partial);
	}

	// fix current position
	double res[N];
	solution tmps[N];
	for(int i=0; i<N; i++) {
		#pragma omp task firstprivate(i)
		if (!contains(partial, i)) {
			tmps[i] = (solution){partial,i};
			res[i] = solve_rec(&tmps[i], level-1);
		} else {
			res[i] = 0;
		}
	}

	#pragma omp taskwait

	double max = 0;
	for(int i=0; i<N; i++) {
		if (max<res[i]) max = res[i];
	}
	return max;
}


double solve() {
	double res;
	solution* map = empty();
	#pragma omp parallel
	{
		#pragma omp single
		res = solve_rec(map, N);
	}
	return res;
}

int main() {
	
	// fill matrices
	printf("Init values ...\n");
	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			w[i][j] = i*j;
			d[i][j] = i + j;
		}
	}

	// run solver
	printf("Run solver ...\n");
	double best = solve();
	printf("Done!\n");
	printf("Best Result: %f\n", best);
	return 0;
}

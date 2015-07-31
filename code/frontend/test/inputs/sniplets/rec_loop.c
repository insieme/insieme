


#define bool int
#define true 1
#define false 0

#define N 10

typedef struct _solution {
	struct _solution* head;
	int pos;
} solution;


bool contains(solution* solution, int pos) {
	if (!solution) return false;
		return solution->pos == pos || contains(solution->head, pos);
}



///////////////////////////////////////////////////////////////////
//
double solve_rec(solution* partial, int level) {
	// terminal case
	if (level == 0) {
		return 0.0;
	}

	// fix current position
	double res[N];
	solution tmps[N];

	for(int i=0; i<N; i++) {
#pragma omp task firstprivate(i)
		if (!contains(partial, i)) {
	//		tmps[i] = (solution){partial,i};
			res[i] = solve_rec(&tmps[i], level-1);
		}
	}
	return 0.0;
}



int main(){
	solution* map;
	solve_rec(map, 10);
}

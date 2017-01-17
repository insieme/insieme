
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#define bool int
#define true 1
#define false 0

// A quadratic matrix, dynamically sized
typedef struct _qmatrix {
	int size;
	int data[];
} qmatrix;

qmatrix* qm_create(int size) {
	qmatrix* res = (qmatrix*)malloc(sizeof(qmatrix) + size * size * sizeof(int));
	res->size = size;
	return res;
}

void qm_del(qmatrix* matrix) {
	free(matrix);
}

#define get(M,I,J) M->data[I*M->size+J]


// a struct describing a QAP instance
typedef struct _problem {
	int size;		// the size of the problem
	qmatrix* A;		// the weight matrix (size x size)
	qmatrix* B;		// the distance matrix (size x size)
	int optimum;		// the value of the optimal solution
} problem;

problem* qap_load(char* file);

void qap_del(problem* problem) {
	qm_del(problem->A);
	qm_del(problem->B);
	free(problem);
}

#define getA(P,I,J) get(P->A,I,J)
#define getB(P,I,J) get(P->B,I,J)


// a struct representing a (partial) solution to the problem
typedef struct _solution {
	struct _solution* head;		// the solution is forming a linked list
	int pos;			// the location the current facility is assigned to
} solution;

solution* empty() { return 0; }


void print(solution* solution) {
	if(!solution) return;
	print(solution->head);
	printf("-%d", solution->pos);
}

int solve_rec(problem* problem, solution* partial, int plant, int used_mask, int cur_cost, volatile int* best_known) {
	// terminal case
	if (plant >= problem->size) {
		return cur_cost;
	}

	if (cur_cost >= *best_known) {
		return *best_known;
	}


	// fix current position
	for(int i=0; i<problem->size; i++) {
		// check whether current spot is a free spot
		if(!(1<<i & used_mask)) {
			// extend solution
			solution tmp = {partial, i};

			// compute additional cost of current assignment
			int new_cost = 0;
			
			int cur_plant = plant;
			solution* cur = &tmp;
			while(cur) {
				int other_pos = cur->pos;

				// add costs between current pair of plants
				new_cost += getA(problem, plant, cur_plant) * getB(problem, i, other_pos);
				new_cost += getA(problem, cur_plant, plant) * getB(problem, other_pos, i);

				// go to next plant
				cur = cur->head;
				cur_plant--;
			}

			// compute recursive rest
			int cur_best = solve_rec(problem, &tmp, plant+1, used_mask | (1<<i), cur_cost + new_cost, best_known);

			// update best known solution
			if (cur_best < *best_known) {
				int best;
				//   |--- read best ---|          |--- check ---|    |------------ update if cur_best is better ------------|
				do { best = *best_known; } while (cur_best < best && __sync_bool_compare_and_swap(best_known, best, cur_best));
			}
		}
	}

	return *best_known;
}


typedef struct {
	solution* partial;
	int plant;
	int used_mask;
	int cur_cost;
} sub_problem;


void solve_wide_filler(problem* problem, solution* partial, int plant, int used_mask, int cur_cost, int level, sub_problem* list, int* pos) {

	// terminal case
	assert(plant < problem->size);

	// if this is the cut-off level ...
	if (level == 0) { 
		// ... add 1 entry to sub_problem list
		list[*pos].partial = partial;
		list[*pos].plant = plant;
		list[*pos].used_mask = used_mask;
		list[*pos].cur_cost = cur_cost;
		(*pos)++;
		return;
	}

	// fix current position
	for(int i=0; i<problem->size; i++) {
		// check whether current spot is a free spot
		if(!(1<<i & used_mask)) {
			// extend solution (this time on the heap)
			solution* tmp = (solution*)malloc(sizeof(solution));
			tmp->head = partial;
			tmp->pos = i;

			// compute additional cost of current assignment
			int new_cost = 0;
			
			int cur_plant = plant;
			solution* cur = tmp;
			while(cur) {
				int other_pos = cur->pos;

				// add costs between current pair of plants
				new_cost += getA(problem, plant, cur_plant) * getB(problem, i, other_pos);
				new_cost += getA(problem, cur_plant, plant) * getB(problem, other_pos, i);

				// go to next plant
				cur = cur->head;
				cur_plant--;
			}

			// add sub_problems using a recursive call
			solve_wide_filler(problem, tmp, plant+1, used_mask | (1<<i), cur_cost + new_cost, level-1, list, pos);
		}
	}
}


int solve_wide(problem* problem) {
	const int cut_off_level = 3;

	// step 1 - create an array
	int max_problems = powl(problem->size, cut_off_level);
	sub_problem* sub_problems = (sub_problem*)malloc(max_problems * sizeof(sub_problem));

	// step 2 - fill it with sub-problem list
	int num_sub_problems = 0;
	solve_wide_filler(problem, 0, 0, 0, 0, cut_off_level, sub_problems, &num_sub_problems);

	printf("Sub-problem list filled %d/%d\n", num_sub_problems, max_problems);

	// step 3 - solve sub-problem list within a loop
	volatile int best = 1<<30;
	#pragma omp parallel for schedule(dynamic, 1)
	for(int i=0; i<num_sub_problems; i++) {
		solve_rec(problem, 
			sub_problems[i].partial,
			sub_problems[i].plant,
			sub_problems[i].used_mask,
			sub_problems[i].cur_cost,
			&best
		);
	}

	// step 4 - free temporary array - TODO: also free partial lists
	free(sub_problems);

	return best;
}

int solve(problem* problem) {
	int res;
	solution* map = empty();
	volatile int best = 1<<30;
/*
	#pragma omp parallel
	{
		#pragma omp single
		res = solve_rec(problem, map, 0, 0, 0, &best);
	}
	return res;
*/

	return solve_wide(problem);
}

int main(int argc, char** argv) {
	
	char* problem_file = "problems/chr10a.dat";
	if (argc >= 2) {
		problem_file = argv[1];
	}

	// load problem
	problem* p = qap_load(problem_file);

	// run solver
	printf("Run solver ...\n");
	int best = solve(p);
	printf("Done!\n");
	printf("Best Result: %d\n", best);

	// verify result
	bool success = (best == p->optimum);
	printf("Verification: %s\n", (success?"successful":"failed"));	

	// free problem
	qap_del(p);

	return success?0:1;
}


problem* qap_load(char* file) {

	FILE* fp = fopen(file, "r");
	printf("Loading Problem File %s ..\n", file);

	int dummy; // not the nice way ...

	// get problem size
	int problemSize;
	dummy = fscanf(fp, "%d", &problemSize);
	printf("  - problem size: %d\n", problemSize);
	
	// create problem instance
	problem* res = (problem*)malloc(sizeof(problem));
	res->size = problemSize;
	res->A = qm_create(problemSize);
	res->B = qm_create(problemSize);

	// load matrix A
	for(int i=0; i<problemSize; i++) {
		for(int j=0; j<problemSize; j++) {
			dummy = fscanf(fp, "%d", &getA(res,i,j));
		}
	}

	// load matrix B
	for(int i=0; i<problemSize; i++) {
		for(int j=0; j<problemSize; j++) {
			dummy = fscanf(fp, "%d", &getB(res,i,j));
		}
	}

	// load optimum
	dummy = fscanf(fp, "%d", &(res->optimum));
	printf("  - optimum: %d\n", res->optimum);

	fclose(fp);

	return res;
}



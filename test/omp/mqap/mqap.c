
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#define bool int
#define true 1
#define false 0

#define MAX(A,B) (((A)>(B))?(A):(B))
#define MIN(A,B) (((A)<(B))?(A):(B))

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

typedef union {
	struct { int x, y; };
	int d[2];
} int2;


// A quadratic matrix of vectors (static)
typedef struct _qvmatrix {
	int size;
	int2 data[];
} qvmatrix;

qvmatrix* qvm_create(int size) {
	qvmatrix* res = (qvmatrix*)malloc(sizeof(qmatrix) + size * size * sizeof(int2));
	res->size = size;
	return res;
}

void qvm_del(qvmatrix* matrix) {
	free(matrix);
}


// a struct describing a QAP instance
typedef struct _problem {
	int size;		// the size of the problem
	qmatrix* A;		// the distance matrix (size x size)
	qvmatrix* B;		// the weight matrix (size x size x #weights)
} problem;

problem* qap_load(char* file);

void qap_del(problem* problem) {
	qm_del(problem->A);
	qvm_del(problem->B);
	free(problem);
}

#define getA(P,I,J) (get(P->A,I,J))
#define getB(P,I,J) (get(P->B,I,J))


// a struct representing a (partial) solution to the problem
typedef struct _map {
	struct _map* head;		// the solution is forming a linked list
	int pos;			// the location the current facility is assigned to
} map;

map* empty() { return 0; }

void print(map* map) {
	if(!map) return;
	print(map->head);
	printf("-%d", map->pos);
}

// a struct used to represent a frontier
typedef struct {
	int size;
	int2 list[200];
} front;

// test whether one point dominates another (a dom b)
static inline bool p_dom(int2 a, int2 b) {
	return a.x < b.x && a.y < b.y;
}

// tests whether a front f is dominating a point p (f dom p)
static inline bool f_dom(front* f, int2 p) {
	// any of the points needs to dominate p
	for(int2* it=f->list; it<(f->list+f->size) && (it->x < p.x || (it->x==p.x && it->y < p.y)); ++it) {
		if (p_dom(*it,p)) return true;
	}
	return false;
}

// adds the given point p into the frontier f
static inline void f_insert(const front* f, int2 p, front* res) {
	int2* newList = res->list;
	int2* cur = newList;
	for(int i=0; i<f->size; i++) {
		if (!p_dom(p, f->list[i])) {
			*cur = f->list[i];
			cur++;
		}
	}

	// update size
	res->size = (cur - newList) + 1;	// p has not been added yet

	// insert new point in ordered place
	cur--;
	while(cur >= newList && (cur->x > p.x || (cur->x == p.x && cur->y > p.y))) {
		*(cur+1) = *cur;
		cur--;
	}
	cur++;
	*cur = p;
}

void f_print(front* f) {
	for(int i=0; i<f->size; i++) {
		printf("\t%3d. %6d %6d\n", i+1, f->list[i].x, f->list[i].y);
	}
}


void solve_rec(problem* problem, map* partial, int plant, int used_mask, int2 cur_cost, front** volatile cur_front, front* a, front* b) {
	// terminal case
	if (plant >= problem->size) {
		// check for dominance
		if(!f_dom(*cur_front, cur_cost)) {

			// better point found => update cur_front in a thread save way
			front* old;
			
			// take one of the two free frontiers
			front* new = a;
			if (a == *cur_front) new = b; // in case a is used currently

			// exchange the frontier
			bool better;
			do {
				old = *cur_front;				
				better = !f_dom(old, cur_cost);
				if (better) f_insert(old, cur_cost, new);
			} while (better && !__sync_bool_compare_and_swap(cur_front, old, new));
		}

		return;
	}

	// prune search space
	if (f_dom(*cur_front, cur_cost)) return;

	// fix current position
	for(int i=0; i<problem->size; i++) {
		// check whether current spot is a free spot
		if(!(1<<i & used_mask)) {
			// extend solution
			map tmp = {partial, i};

			// compute cost of current assignment
			int2 new_cost = cur_cost;
			
			int cur_plant = plant;
			map* cur = &tmp;
			while(cur) {
				int other_pos = cur->pos;

				// add costs between current pair of plants
				new_cost.x += getA(problem, plant, cur_plant) * getB(problem, i, other_pos).x;
				new_cost.y += getA(problem, plant, cur_plant) * getB(problem, i, other_pos).y;

				new_cost.x += getA(problem, cur_plant, plant) * getB(problem, other_pos, i).x;
				new_cost.y += getA(problem, cur_plant, plant) * getB(problem, other_pos, i).y;

				// go to next plant
				cur = cur->head;
				cur_plant--;
			}

			// compute recursive rest
			solve_rec(problem, &tmp, plant+1, used_mask | (1<<i), new_cost, cur_front, a, b);
		}
	}
}


typedef struct {
	map* partial;
	int plant;
	int used_mask;
	int2 cur_cost;
} sub_problem;


void solve_filler(problem* problem, map* partial, int plant, int used_mask, int2 cur_cost, int level, sub_problem* list, int* pos) {

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

			// extend solution
			map* tmp = (map*)malloc(sizeof(map));
			tmp->head = partial;
			tmp->pos = i;

			// compute cost of current assignment
			int2 new_cost = cur_cost;
			
			int cur_plant = plant;
			map* cur = tmp;
			while(cur) {
				int other_pos = cur->pos;

				// add costs between current pair of plants
				new_cost.x += getA(problem, plant, cur_plant) * getB(problem, i, other_pos).x;
				new_cost.y += getA(problem, plant, cur_plant) * getB(problem, i, other_pos).y;

				new_cost.x += getA(problem, cur_plant, plant) * getB(problem, other_pos, i).x;
				new_cost.y += getA(problem, cur_plant, plant) * getB(problem, other_pos, i).y;

				// go to next plant
				cur = cur->head;
				cur_plant--;
			}


			// add sub_problems using a recursive call
			solve_filler(problem, tmp, plant+1, used_mask | (1<<i), new_cost, level-1, list, pos);
		}
	}
}

front solve_wide(problem* problem) {
	const int cut_off_level = 3;

	// step 1 - create an array
	int max_problems = powl(problem->size, cut_off_level);
	sub_problem* sub_problems = (sub_problem*)malloc(max_problems * sizeof(sub_problem));

	// step 2 - fill it with sub-problem list
	int num_sub_problems = 0;
	solve_filler(problem, 0, 0, 0, (int2){0,0}, cut_off_level, sub_problems, &num_sub_problems);

	printf("Sub-problem list filled %d/%d\n", num_sub_problems, max_problems);

	// step 3 - solve sub-problem list within a loop
	front* f;
	front res;
	#pragma omp parallel
	{
		// prepare two frontiers for this local thread
		front a; a.size = 0;
		front b; b.size = 0;

		#pragma omp master
		f = &a;
		
		#pragma omp barrier
 
		#pragma omp for schedule(dynamic, 1)
		for(int i=0; i<num_sub_problems; i++) {
			solve_rec(problem, 
				sub_problems[i].partial,
				sub_problems[i].plant,
				sub_problems[i].used_mask,
				sub_problems[i].cur_cost,
				&f,
				&a, &b
			);
		}

		#pragma omp master
		res = *f;
	}

	// step 4 - free temporary array - TODO: also free partial lists
	free(sub_problems);

	return res;	
}

front solve(problem* problem) {
/*
	map* map = empty();
	front* f = (front*)malloc(sizeof(front));
	*f = (front){0,(int2){1<<30,1<<30}, NULL};
	solve_rec(problem, map, 0, 0, (int2){0,0}, &f);

	front res = *f;
	free(f);
	return res;
*/
	return solve_wide(problem);
}


int main(int argc, char** argv) {
	
	char* problem_file = "problems/KC10-2fl-2uni.dat";
	if (argc >= 2) {
		problem_file = argv[1];
	}

	// load problem
	problem* p = qap_load(problem_file);
	// run solver
	printf("Run solver ...\n");
	front best = solve(p);
	printf("Done!\n");


	printf("Frontier:\n");
	f_print(&best);

/*
	// verify result
	bool success = (best == p->optimum);
	printf("Verification: %s\n", (success?"successful":"failed"));	
*/
	bool success = true;

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
	res->B = qvm_create(problemSize);

	// load matrix A
	for(int i=0; i<problemSize; i++) {
		for(int j=0; j<problemSize; j++) {
			dummy = fscanf(fp, "%d", &getA(res,i,j));
		}
	}

	// load matrix B
	for(int k=0; k<2; k++) {
		for(int i=0; i<problemSize; i++) {
			for(int j=0; j<problemSize; j++) {
				dummy = fscanf(fp, "%d", &getB(res,i,j).d[k]);
			}
		}
	}

/*
	// load optimum
	dummy = fscanf(fp, "%d", &(res->optimum));
	printf("  - optimum: %d\n", res->optimum);
*/

	fclose(fp);

	return res;
}



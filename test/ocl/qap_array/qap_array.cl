
#define MAX_SIZE 25

typedef struct {
	int partial_index;
	int plant; 
	int used_mask;
	int cur_cost;
} call_record;

#define getA(I,J) A[I*size+J]
#define getB(I,J) B[I*size+J]


void solve_rec(int size, __global int* A, __global int* B, int* partial, int plant, int used_mask, int cur_cost, __global int* best_known) {

	// terminal case
	if (plant >= size) {
		// update best known solution
		if (cur_cost < *best_known) {
			int best;
			//   |--- read best ---|          |--- check ---|    |----- update if cur_best is better ----|
			do { best = *best_known; } while (cur_cost < best && atomic_cmpxchg(best_known, best, cur_cost));
		}
		return;
	}

	if (cur_cost >= *best_known) {
		return;
	}


	// fix current position
	for(int i=0; i<size; i++) {
		// check whether current spot is a free spot
		if(!(1<<i & used_mask)) {
			// extend solution
			int tmp[MAX_SIZE];
			for(int j=0; j<plant; j++) {
				tmp[j] = partial[j];
			}
			tmp[plant] = i;

			// compute additional cost of current assignment
			int new_cost = 0;
			
			for(int j=0; j<plant; j++) {
				int other_pos = tmp[j];

				// add costs between current pair of plants
				new_cost += getA(plant, j) * getB(i, other_pos);
				new_cost += getA(j, plant) * getB(other_pos, i);
			}

			// compute recursive rest
			solve_rec(size, A, B, tmp, plant+1, used_mask | (1<<i), cur_cost + new_cost, best_known);
			
		}
	}
}


__kernel void qap(int size, __global int* A, __global int* B, __global call_record* problems, __global int* block, __global int* best, int num_sub_problems) {
	int gid = get_global_id(0);

	// skip tails introduced to fill up multiple of local group size
	if (gid >= num_sub_problems) return;

	__global call_record* p = &problems[gid];

	// compute local part
	int partial[MAX_SIZE];
	for(int i=0; i<size; i++) {
		partial[i] = block[p->partial_index + i];
	}

	// run computation => will write result into best ptr
	solve_rec(size, A, B, partial, p->plant, p->used_mask, p->cur_cost, best);

}

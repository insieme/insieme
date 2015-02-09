
#include <stdio.h>
#include <omp.h>

#ifndef N
	#define N 3
#endif

int numPartitions(int n);

int main() {

	omp_set_nested(1);
	omp_set_max_active_levels(3);

	// compute partitions
	printf("Number of partitions: %d\n", numPartitions(N));

}

int numPartitionsInternal(int n, int cur, int sum);


int numPartitions(int n) {
	return numPartitionsInternal(n,1,0);
}

int numPartitionsInternal(int n, int cur, int sum) {

	// if to big => done
	if (n < sum) return 0;

	// if exact => we have one
	if (n == sum) return 1;

	// if cur is to big => done
	if (n < cur) return 0;

	// otherwise check next components
	int res = 0;

	#pragma omp parallel for reduction(+:res) schedule(dynamic)
	for(int i=0; i<=n; i++) {
		res += numPartitionsInternal(n, cur+1, sum+i*cur);
	}

	// return total number
	return res;
}



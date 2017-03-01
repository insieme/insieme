#include <stdio.h>
#include <omp.h>

#define LOOP_ITS 10
#define THREADS 10

omp_lock_t g_lock_a;
int g_a = 0, g_b = 0;

void incrementer(omp_lock_t* lock_b) {
	for(int i=0; i< LOOP_ITS; ++i) {
		omp_set_lock(&g_lock_a);
		g_a++;
		omp_unset_lock(&g_lock_a);
		omp_set_lock(lock_b);
		g_b++;
		omp_unset_lock(lock_b);
	}
}

void decrementer(omp_lock_t* lock_b) {
	for(int i=0; i< LOOP_ITS; ++i) {
		while (!omp_test_lock(&g_lock_a));
		g_a--;
		omp_unset_lock(&g_lock_a);
		while (!omp_test_lock(lock_b));
		g_b--;
		omp_unset_lock(lock_b);
	}
}

int main() {
	omp_init_lock(&g_lock_a);
	omp_lock_t lock_b;
	omp_init_lock(&lock_b);

	#pragma omp parallel num_threads(THREADS)
	incrementer(&lock_b);

	if(!(g_a == LOOP_ITS*THREADS && g_b == LOOP_ITS*THREADS)) {
		printf("FAILURE\n");
		return 0;
	}

	#pragma omp parallel num_threads(THREADS)
	decrementer(&lock_b);

	if (!(g_a == 0 && g_b == 0)) {
		printf("FAILURE\n");
		return 0;
	}

	printf("success\n");
	return 0;
}

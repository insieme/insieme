
#include <stdio.h>


int main() {
	int i;

	// count up using < and simple ++
	#pragma omp parallel for
	for(int j = 0; j < 10; j++) {
		printf("Aj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i < 10; i++) {
		printf("Ai%d\n", i);
	}

	#pragma omp parallel for
	for(int j = 0; j < 10; ++j) {
		printf("Bj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i < 10; ++i) {
		printf("Bi%d\n", i);
	}

	// count up using < and +=
	#pragma omp parallel for
	for(int j = 0; j < 10; j+=2) {
		printf("Cj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i < 10; i+=2) {
		printf("Ci%d\n", i);
	}

	// count up using < and x = x + z
	#pragma omp parallel for
	for(int j = 0; j < 10; j = j + 2) {
		printf("Dj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i < 10; i = i + 2) {
		printf("Di%d\n", i);
	}

	// count up using < and simple ++ with no iterations
	#pragma omp parallel for
	for(int j = 10; j < 0; j++) {
		printf("Ej%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i < 0; i++) {
		printf("Ei%d\n", i);
	}


	// count up using <= and simple ++
	#pragma omp parallel for
	for(int j = 0; j <= 10; j++) {
		printf("Fj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i <= 10; i++) {
		printf("Fi%d\n", i);
	}

	#pragma omp parallel for
	for(int j = 0; j <= 10; ++j) {
		printf("Gj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i <= 10; ++i) {
		printf("Gi%d\n", i);
	}

	// count up using <= and +=
	#pragma omp parallel for
	for(int j = 0; j <= 10; j+=2) {
		printf("Hj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i <= 10; i+=2) {
		printf("Hi%d\n", i);
	}

	// count up using <= and x = x + z
	#pragma omp parallel for
	for(int j = 0; j <= 10; j = j + 2) {
		printf("Ij%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i <= 10; i = i + 2) {
		printf("Ii%d\n", i);
	}

	// count up using <= and simple ++ with no iterations
	#pragma omp parallel for
	for(int j = 10; j <= 0; j++) {
		printf("Jj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i <= 0; i++) {
		printf("Ji%d\n", i);
	}


	// count down using > and simple --
	#pragma omp parallel for
	for(int j = 10; j > 0; j--) {
		printf("Kj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i > 0; i--) {
		printf("Ki%d\n", i);
	}

	#pragma omp parallel for
	for(int j = 10; j > 0; --j) {
		printf("Lj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i > 0; --i) {
		printf("Li%d\n", i);
	}

	// count up using > and +=
	#pragma omp parallel for
	for(int j = 10; j > 0; j-=2) {
		printf("Mj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i > 0; i-=2) {
		printf("Mi%d\n", i);
	}

	// count up using > and x = x - z
	#pragma omp parallel for
	for(int j = 10; j > 0; j = j - 2) {
		printf("Nj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i > 0; i = i - 2) {
		printf("Ni%d\n", i);
	}

	// count up using > and simple -- with no iterations
	#pragma omp parallel for
	for(int j = 0; j > 10; j--) {
		printf("Oj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i > 10; i--) {
		printf("Oi%d\n", i);
	}


	// count down using >= and simple --
	#pragma omp parallel for
	for(int j = 10; j >= 0; j--) {
		printf("Pj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i >= 0; i--) {
		printf("Pi%d\n", i);
	}

	#pragma omp parallel for
	for(int j = 10; j >= 0; --j) {
		printf("Qj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i >= 0; --i) {
		printf("Qi%d\n", i);
	}

	// count up using > and +=
	#pragma omp parallel for
	for(int j = 10; j >= 0; j-=2) {
		printf("Rj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i >= 0; i-=2) {
		printf("Ri%d\n", i);
	}

	// count up using > and x = x - z
	#pragma omp parallel for
	for(int j = 10; j >= 0; j = j - 2) {
		printf("Sj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 10; i >= 0; i = i - 2) {
		printf("Si%d\n", i);
	}

	// count up using > and simple -- with no iterations
	#pragma omp parallel for
	for(int j = 0; j >= 10; j--) {
		printf("Tj%d\n", j);
	}
	#pragma omp parallel for
	for(i = 0; i >= 10; i--) {
		printf("Ti%d\n", i);
	}

	return 0;
}

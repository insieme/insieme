
#include <stdio.h>
#include <vector>
#include <iterator>

#define N 50

using namespace std;

int main() {


	vector<int> a(N);

	#pragma omp parallel for
	for(auto p = a.begin(); p < a.end(); p++) {
		*p = 2;
	}

	int sum = 0;


	#pragma omp parallel for reduction(+ : sum)
	for(auto p = a.begin(); p < a.end(); p++) {
		sum += *p;
	}


	sum = 0;
	auto h = a.begin();
	#pragma omp parallel for reduction(+ : sum)
	for(decltype(std::distance(a.begin(), a.end())) i = 0; i < std::distance(a.begin(), a.end()); i++) {
		sum += h[i];
//		h[i] = 4;
	}

	printf("sum=%d\n", sum);
	return (sum == N*2)?0:1;
}


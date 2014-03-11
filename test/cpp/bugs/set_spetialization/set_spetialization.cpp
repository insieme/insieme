#include <set>

struct order;

struct order{


	bool operator() (int* a, int *b){
		return *a > *b;
	}
};


int main (){

	std::set<int*, order> collection;
	

	return 0;
}


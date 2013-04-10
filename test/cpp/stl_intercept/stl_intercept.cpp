
#include <stdio.h>
#include <vector>
#include <set>

using namespace std;

//vector<int> globalList;
//int globalInt;

int main() {

	//globalInt = 0;
	//globalList.push_back(10);

	vector<int> list;
	list.push_back(12);
	list.push_back(14);

	vector<int>::value_type x = list.at(0);

	//printf("[%d,%d]\n", list[0], list[1]);

	vector<char> list1;
	//list1.push_back(L'c');
	list1.push_back('c');

//	printf("[%d,%d]\n", list1[0], list1[1]);

//	set<bool> universe;
//	universe.insert(true); 

//	set<int> universe1;
//	universe1.insert(1); 

	return 0; 
}

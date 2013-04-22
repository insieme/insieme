#include <vector>
#include <set>
#include <list>
#include <iostream>
#include <cstdio>
//#include <stdio.h>

using namespace std;

//vector<int> globalList;
//int globalInt;

int main() {
	//wprintf("%lc",L'c');

	std::cout << "asd";
	//globalInt = 0;
	//globalList.push_back(10);
	int a;

	vector<int> v;
	v.push_back(12);
	v.push_back(14);

	a= 1;

	vector<int>::value_type x = v.at(0);

	printf("[%d,%d]\n", v[0], v[1]);

	vector<char> v1;
	//v1.push_back(L'c');
	v1.push_back('c');

	printf("[%d,%d]\n", v1[0], v1[1]);

	set<bool> universe;
	universe.insert(true); 

	set<int> universe1;
	universe1.insert(1); 

	list<int> l;
	l.push_back(12);
	l.push_back(14);

	list<int>::iterator it = l.begin();
	list<int>::iterator e = l.end();
	for(; it!=e; it++ ) { }

	return 0; 
}

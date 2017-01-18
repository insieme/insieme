#include <vector>
#include <set>
#include <list>
#include <iostream>
#include <cstdio>

using namespace std;

//vector<int> globalList;
//int globalInt;

int main() {
	//wprintf("%lc",L'c');

	//std::cout << "STL Intercept Test -- std::cout" << endl;
	std::cout << "STL Intercept Test -- std::cout";

	//globalInt = 0;
	//globalList.push_back(10);

	vector<int> v;
	v.push_back(12);
	v.push_back(14);

	vector<int>::value_type x = v.at(0);
	cout << x;

	printf("[%d,%d]\n", v[0], v[1]);

	vector<char> v1;
	v1.push_back('a');
	v1.push_back('b');
	printf("[%d,%d]\n", v1[0], v1[1]);

	//vector<wchar> v2;
	//v2.push_back(L'a');
	//v2.push_back(L'b');
	//printf("[%d,%d]\n", v2[0], v2[1]);

	set<bool> universe;
	universe.insert(true); 

	set<int> universe1;
	universe1.insert(1); 

	list<int> l;
	l.push_back(12);
	l.push_back(14);

	list<int>::iterator it = l.begin();
	list<int>::iterator e = l.end();
	for(; it!=e; it++ ) {
		cout << "List: " << *it;
	}

	return 0; 
}


#include <vector>
//#include <iostream>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <iostream>

#include "timer.h"

using namespace std;



////////////////////////////////////////////////////////////////////////////
//
bool checkResult(vector<int>& v){
	float last = *v.begin();
	vector<int>::iterator it;
	for (it = v.begin()+1; it != v.end(); it ++){
		if (last > *it)
			return false;
		last = *it;
	}
	return true;
}

////////////////////////////////////////////////////////////////////////////
//
void bubleSort(vector<int>& v){

	bool swapped;
	do{
		swapped = false;
		for (long long i =1; i < v.size(); i++){
			if (v[i-1] > v[i]){

				int aux = v[i];
				v[i] = v[i-1];
				v[i-1] = aux;
				
				swapped = true;
			}
		}
	}while (swapped);
}

////////////////////////////////////////////////////////////////////////////
//
void bucketSort (vector<int>& v, unsigned classes = 1000){
	vector<vector<int> > buckets(classes);
	
	for (size_t i=0; i< v.size(); i++){
		unsigned index = v[i];
		buckets[index].push_back(v[i]);
	}

	v.clear();
	for (int i=0; i< classes; i++){
		if (buckets[i].empty())
			continue;
		bubleSort(buckets[i]);
		v.insert( v.end(), buckets[i].begin(), buckets[i].end());
	}
}

////////////////////////////////////////////////////////////////////////////
//

void testAlgorithms (vector<int>& buffer){

	{
		vector<int> data(buffer);
		bubleSort(data);
		bool ok = checkResult(data);
		cout << "BubbleSort " << (ok?"OK":"WRONG") << "\n";
	}

	{
		vector<int> data(buffer);
		bucketSort(data);
		bool ok = checkResult(data);
		cout << "BucketSort " << (ok?"OK":"WRONG") << "\n";
	}

}

////////////////////////////////////////////////////////////////////////////
//
#define BUFFSIZE 1024*512
int main (int argc, char **argv){
	vector<size_t> sizes;
	sizes.push_back(5);
	sizes.push_back(20);
	sizes.push_back(512);
	sizes.push_back(1024);
	sizes.push_back(1024*16);
	for (size_t i = 0; i < sizes.size(); i++){

		cout << "Running Test for size " << sizes[i] << "\n";

		// generate speudo random (same data every run)
		srand(1234);
		vector<int> vData(sizes[i]);
		for (size_t j=0; j < sizes[i]; j++) {
			vData[j] = rand()%1000;
		}

		testAlgorithms(vData);
	}

	return 0;
}

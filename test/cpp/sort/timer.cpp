
#include <vector>
//#include <iostream>

#include <stdlib.h>
#include <float.h>

#include "timer.h"

using namespace std;


////////////////////////////////////////////////////////////////////////////
//
template <class T>
void print(vector<T>& v){
	for (size_t i =0; i< v.size(); i++){
		//cout << v[i] << ", ";
	}
	//cout  << endl;
}

////////////////////////////////////////////////////////////////////////////
//
template <class T>
bool checkResout(vector<T>& v){
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
template  <class T>
void bubleSort(vector<T>& v){

	bool swapped;
	do{
		swapped = false;
		for (long long i =1; i < v.size(); i++){
			if (v[i-1] > v[i]){

				T aux = v[i];
				v[i] = v[i-1];
				v[i-1] = aux;
				
				swapped = true;
			}
		}
	}while (swapped);
}

////////////////////////////////////////////////////////////////////////////
//
template <class T>
void bucketSort (vector<T>& v, unsigned classes = 1000){
	vector<T> buckets[classes];
	
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
template <unsigned byte>
void radix (vector<int>& in, vector<int>& out){
	unsigned count[256];
	unsigned index[256];
	memset(count, 0x0, sizeof(unsigned) * 256);
	
	for (size_t i=0; i< in.size(); i++)
		count[ ((in[i]) >> (byte*8)) & 0xff] ++;
	
	index[0] =0;	
	for (size_t i=1; i< 256; i++)
		index[i] = index[i-1] + count[i-1];

	for (size_t i=0; i< in.size(); i++)
		out[index[((in[i]) >> (byte*8)) &0xff]++] = in[i];
}

void radixSort (vector<int>& v){

	vector<int> temp(v.size());
	radix<0> (v, temp);
	radix<1> (temp, v);
	radix<2> (v, temp);
	radix<3> (temp, v);
}

////////////////////////////////////////////////////////////////////////////
//
template <class T>
void insertionSort(vector<T>&v){

	for(size_t i=1; i< v.size()-1; i++){
		T    item = v[i];
		size_t hole = i;
		
		while ((hole > 0) && (v[hole-1] > item)){
			v[hole] = v[hole-1];
			hole = hole-1;
		}
		v[hole] = item;
	}
}

////////////////////////////////////////////////////////////////////////////
//

vector<double> bubleTimes;
vector<double> bucketTimes;
vector<double> radixTimes;
vector<double> insertionTimes;

template <class T>
bool testAlgorithms (vector<T>& buffer){

	// 4 different copys to have 4 unsorted sets
	vector<T> a(buffer.begin(), buffer.end());
	vector<T> b(buffer.begin(), buffer.end());
	vector<T> c(buffer.begin(), buffer.end());

	ayuso::timer time;

	time.start();
	bubleSort(a);
	bubleTimes.push_back(time.stop());

	time.start();
	bucketSort(b);
	bucketTimes.push_back(time.stop());

	time.start();
	radixSort(c);
	radixTimes.push_back(time.stop());

	time.start();
	insertionSort(c);
	insertionTimes.push_back(time.stop());

	// check validity
	if(!checkResout(a)){
		//std::cout << "buble fails with " << buffer.size() << " elements\n";
		return false;
	}
	if(!checkResout(b)){
		//std::cout << "bucket fails with " << buffer.size() << " elements\n";
		return false;
	}
	if(!checkResout(c)){
		//std::cout << "radix fails with " << buffer.size() << " elements\n";
		return false;
	}

	//std::cout << "iteration complete: " << buffer.size() << " elems\n";
	return true;
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
	sizes.push_back(1024*128);
	sizes.push_back(1024*1024);
	sizes.push_back(1024*1024*16);

	for (size_t i = 0; i < sizes.size(); i++){
		vector<int> vData(sizes[i]);

		//cout << "generating " << sizes[i] << " elements\n";

		// generate speudo random (same data every run)
		srand(1234);
		for (size_t i=0; i< sizes[i]; i++)
			vData.insert(vData.end(), rand()%1000);

		if (!testAlgorithms(vData))
			break;
	}


	// print sumary:
	//cout << "Num\t\tbuble\t\tinsert\t\tbucket\t\tradix\n";
	for (size_t i=0; i< sizes.size(); i++){
		////cout << sizes[i] <<"\t\t";
		//cout << bubleTimes[i] << "\t\t";
		//cout << insertionTimes[i] << "\t\t";
		//cout << bucketTimes[i] << "\t\t";
		//cout << radixTimes[i] << "\n";
	}

	return 0;
}

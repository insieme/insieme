
#include <iostream>
#include <vector>

#define N 200

using namespace std;

template<typename T = double>
class Matrix {

	vector< vector<T> > data;

public:

	Matrix(int s) : data(s) {
		for(int i=0; i<s; i++) {
			data[i].resize(s);
		}
	}

	Matrix(int x, int y) : data(x) {
		for(int i=0; i<x; i++) {
			data[i].resize(y);
		}
	}

	// get matrix dimension
	size_t getX() const { return data.size(); }
	size_t getY() const { return data[0].size(); }


	// two access operators
	vector<T>& operator[](int x) { return data[x]; }
	const vector<T>& operator[](int x) const { return data[x]; }

	bool operator==(const Matrix<T>& other) const {
		// just compare the data arrays
		return data == other.data;
	}
};

template<typename T>
ostream& operator<<(ostream& out, const Matrix<T>& m) {
	for(size_t i=0; i<m.getX(); i++) {
		for(size_t j=0; j<m.getY(); j++) {
			out << m[i][j] << " ";
		}
		out << "\n";
	}
	return out;
}

template<typename T>
Matrix<T> id(int s) {
	Matrix<T> res(s);
	for(size_t i=0; i<s; i++) {
		for (size_t j=0; j<s; j++) {
			res[i][j] = ((i==j)?1:0);
		}
	}
	return res;
}

template<typename T>
Matrix<T> operator*(const Matrix<T>& a, const Matrix<T>& b) {

	Matrix<T> res(a.getX(), b.getY());

	for(size_t i=0; i<a.getX(); i++) {
		for(size_t j=0; j<b.getY(); j++) {
			for(size_t k=0; k<a.getY(); k++) {
				res[i][j] = res[i][j] + a[i][k] * b[k][j];
			}
		}
	}

	return res;
}

template<typename T>
void runTest(int size) {

	Matrix<T> A(size);
	Matrix<T> B = id<T>(size);

	// fill A
	for(size_t i=0; i<size; i++) {
		for(size_t j=0; j<size; j++) {
			A[i][j] = i+j;
		}
	}

	Matrix<T> R = A * B;

/*
	std::cout << "A:\n" << A << "\n";
	std::cout << "B:\n" << B << "\n";
	std::cout << "R:\n" << R << "\n";
*/
	
	std::cout << "Verification: " << ((A == R)?"OK":"ERR") << "\n";

}


int main() {

	std::cout << "Problem size: " << N << "\n";

//	std::cout << "int ...\n";
	runTest<int>(N);

//	std::cout << "double ...\n";
	runTest<double>(N);

	return 0;
}

// check naming of operator= in template

template<typename T>
struct X { 
	T y; 
	X<T>& operator=(const X<T>& other) {
		y = other.y;
		return *this;
	}
};

int main() {
	X<int> a {4};
	X<int> b;
	b = a;
}
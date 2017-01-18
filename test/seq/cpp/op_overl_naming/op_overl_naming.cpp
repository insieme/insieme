
// The point here is that the code can compile again in the backend. If so, we got the naming correct it seems...
struct File {
	File& operator<<=(int i) { return *this; }
	File& operator>>=(int i) { return *this; }
	File& operator->*(int i) { return *this; }
	File& operator+=(const File& other) { return *this; }
	File& operator-=(const File& other) { return *this; }
	File& operator*=(const File& other) { return *this; }
	File& operator/=(const File& other) { return *this; }
	File& operator%=(const File& other) { return *this; }
	File& operator^=(const File& other) { return *this; }
	File& operator&=(const File& other) { return *this; }
	File& operator|=(const File& other) { return *this; }
	File& operator<<(int i) { return *this; }
	File& operator>>(int i) { return *this; }
	File& operator==(const File& other) { return *this; }
	File& operator!=(const File& other) { return *this; }
	File& operator<=(const File& other) { return *this; }
	File& operator>=(const File& other) { return *this; }
	File& operator&&(const File& other) { return *this; }
	File& operator||(const File& other) { return *this; }
	File& operator++() { return *this; }
	File& operator--() { return *this; }
	File& operator->() { return *this; }
	File& operator()(const File& other) { return *this; }
	File& operator[](const File& other) { return *this; }
	File& operator+(const File& other) { return *this; }
	File& operator-(const File& other) { return *this; }
	File& operator*(const File& other) { return *this; }
	File& operator/(const File& other) { return *this; }
	File& operator%(const File& other) { return *this; }
	File& operator^(const File& other) { return *this; }
	File& operator&(const File& other) { return *this; }
	File& operator|(const File& other) { return *this; }
	File& operator~() { return *this; }
	File& operator=(const File& other) { return *this; }
	File& operator<(const File& other) { return *this; }
	File& operator>(const File& other) { return *this; }
	bool operator!() { return false; }
	File& operator,(const File& other) { return *this; }
};

int main() {

	File f;

	return 0;
}

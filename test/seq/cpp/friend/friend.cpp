#include <stdio.h>
class Class;
class FriendClass;

class Class {
	friend int friendFunc(Class& c);
	friend class FriendClass;
	int x;
	int y;
public:
	Class() : x(1), y(2) {}
};

class FriendClass {
	int y;
public:
	FriendClass() : y(10) {}
	int friendFunc(Class& c) {
		return c.x;
	}
};

int friendFunc(Class& c) {
	return c.x;
}

int main() {
	Class c;
	printf("friendFunc(c) %d\n", friendFunc(c));

	FriendClass fC;
	printf("fC.friendFunc(c) %d\n", fC.friendFunc(c));

	return 0;
}

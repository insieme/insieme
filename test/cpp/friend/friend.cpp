class Class;
class FriendClass;

class Class {
	friend int friendFunc(Class& c);
	friend class FriendClass;
	int x;
	int y;
};

class FriendClass {
	int y;
public:
	int friendFunc(Class& c) {
		return c.x;
	}
};

int friendFunc(Class& c) {
	return c.x;
}

int main() {
	Class c;
	friendFunc(c);

	FriendClass fC;
	fC.friendFunc(c);

	return 0;
}

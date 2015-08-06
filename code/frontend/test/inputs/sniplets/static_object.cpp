class K {
public:
	K() {};
};

void check() {
	//create a static object, ctor of K will be called
	static K s;
}

int main() {
	check();
	return 0;
}


class A {
public:
A() {};
~A() {};

typedef const A& result_type;

template <typename T>
result_type operator()(const A& b) {
	return intersect(T());
}

};


class B : public A{

};

const A& intersect(const A& a) {
	return a;
}

const B& intersect(const B& a) {
	return a;
}



int main() {
	A a;
	A b = a.operator()<B>(a);
	return 0;
}



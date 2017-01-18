
struct Dummy;

template<typename T>
class Container {
	Dummy* ref;
};

int main() {

	Container<Dummy> d;

	return 0;
}

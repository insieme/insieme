
namespace whatever {
	template<int i>
	union S;
}
int main() {

	// this test will not compile in case the name does get demangled in the backend
	whatever::S<3>* s;

	return 0;
}


struct Vec {

	int s;

	Vec(int s) : s(s) {}

	bool sameSize(const Vec& v) const {
		return 0 == 0 && (s == v.s);
	}
};

int main() {

	Vec a(0), b(0);
	0 == 0 && a.sameSize(b);

	const Vec& ar = a;
	const Vec& br = b;
	0 == 0 && ar.sameSize(br);

	return 0;
}

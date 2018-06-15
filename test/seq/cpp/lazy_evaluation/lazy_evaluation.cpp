
class Matrix {

	int nrows;
	int ncols;

  public:
	Matrix(int r, int c) : nrows(r), ncols(c) {}

	bool sameSize(const Matrix& mat) const {
		return 0 == 0 && (ncols == mat.ncols);
	}
};

int main() {

	Matrix a(0, 0), b(0, 0);
	0 == 0 && a.sameSize(b);

	const Matrix& ar = a;
	const Matrix& br = b;
	0 == 0 && ar.sameSize(br);

	return 0;
}

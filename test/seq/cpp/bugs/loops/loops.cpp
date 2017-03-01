


void makeband(int r, int c, int *rband, int *cband, int *uband, int *dband) {
	int k, m, min;

	for (k = 0; k < r; k++) {
		rband[k] = c;
	}

	for (k = 0; k < c; k++) {
		cband[k] = r;
	}

	for (k = 0, m = r + c - 2; k < r && k < c ; k++, m--) {
		uband[k] = k + 1;
		uband[m] = k + 1;

		dband[k] = k + 1;
		dband[m] = k + 1;
	}

	min = k;
	for (k = k; k <= m; k++, m--) {
		uband[k] = min;
		uband[m] = min;

		dband[k] = min;
		dband[m] = min;
	}
}


int main (){

	int a[10];
	int b[10];
	int c[10];
	int d[10];

	makeband ( 1, 2, a, b, c, d);

	return 0;
}

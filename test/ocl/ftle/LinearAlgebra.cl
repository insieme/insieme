

typedef float   fvec2[2];
typedef fvec2   fmat2[2];


float fmat2det(fmat2 a) {

	return a[0][0]*a[1][1] - a[0][1]*a[1][0];
}


void fmat2invariants(fmat2 m, fvec2 pqr) {

	// invariant0 = det(M)
	pqr[0] = fmat2det(m);

	// invariant1 = -trace M
	pqr[1] = -(m[0][0] + m[1][1]);
}


int fvec2squareroots(fvec2 a, fvec2 r) {

	/*
	*    Solves equation
	*        1 * x^2 + a[1]*x + a[0] = 0
	*
	*    On output,
	*        r[0], r[1] or
	*        r[0] +- i*r[1] are the roots
	*
	*    returns number of real solutions
	*/

	float discrim, root;

	discrim = a[1] * a[1] - 4 * a[0];

	if (discrim >= 0) {

		root = sqrt(discrim);
		r[0] = (-a[1] - root) / 2.0;
		r[1] = (-a[1] + root) / 2.0;
		return (2);
	}
	else {
		root = sqrt(-discrim);
		r[0] = -a[1] / 2.0;
		r[1] = root / 2.0;
		return(0);
	}
}


int fmat2eigenvalues(fmat2 m, fvec2 lambda) {

	fvec2 pqr;
	
	fmat2invariants(m, pqr);

	return (fvec2squareroots(pqr, lambda));
}


void fmat2copy(fmat2 a, fmat2 b) {

	b[0][0] = a[0][0];
	b[0][1] = a[0][1];
	b[1][0] = a[1][0];
	b[1][1] = a[1][1];
}


void fmat2mul(fmat2 a, fmat2 b, fmat2 c)
{
    fmat2 d;
    d[0][0] = a[0][0]*b[0][0] + a[0][1]*b[1][0];
    d[0][1] = a[0][0]*b[0][1] + a[0][1]*b[1][1];
    d[1][0] = a[1][0]*b[0][0] + a[1][1]*b[1][0];
    d[1][1] = a[1][0]*b[0][1] + a[1][1]*b[1][1];
    fmat2copy(d, c);
}


void fmat2trp(fmat2 a, fmat2 b) {

	if(a != b)
		fmat2copy(a, b);

	float x;
	x = b[0][1];
	b[0][1] = b[1][0];
	b[1][0] = x;
}


void fvec2add(fvec2 a, fvec2 b, fvec2 c) {
	
	c[0] = a[0] + b[0];
	c[1] = a[1] + b[1];
}


void fmat2add(fmat2 a, fmat2 b, fmat2 c) {

	fvec2add(a[0], b[0], c[0]);
	fvec2add(a[1], b[1], c[1]);
}


void fvec2scal(fvec2 a, float b, fvec2 c) {

	c[0] = a[0] * b;
	c[1] = a[1] * b;
}


void fmat2scal(fmat2 a, float b, fmat2 c) {

	fvec2scal(a[0], b, c[0]);
	fvec2scal(a[1], b, c[1]);
}


void fmat2symm(fmat2 m, fmat2 s) {

	fmat2 mT;
	fmat2trp(m, mT);
	fmat2add(m, mT, s);
	fmat2scal(s, 0.5, s);
}

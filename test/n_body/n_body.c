#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// the problem size - the number of particles
#ifndef N
  #define N 1000
#endif

// the second problem size = number of iterations
#ifndef M
	#define M 100
#endif

#define SPACE_SIZE 1000

// the type used to represent a triple of doubles
typedef struct {
	double x, y, z;
} triple;


// the types used to model position, speed and forces
typedef triple position;
typedef triple velocity;
typedef triple force;
typedef triple impulse;

// the type used to model one body
typedef struct {
	// mass is considered to be 1
	position pos;		// the position in space
	velocity v;			// the velocity of the body
} body;

// the list of bodies
body B[N];

// the forces effecting the particless
force F[N];


// ----- utility functions ------
double rand_val(double min, double max) {
	return (rand() / (double) RAND_MAX) * (max - min) + min;
}

triple triple_zero() {
	return (position) {0.0, 0.0, 0.0};
}

triple triple_rand() {
	return (position) {
		rand_val(-SPACE_SIZE,SPACE_SIZE),
		rand_val(-SPACE_SIZE,SPACE_SIZE),
		rand_val(-SPACE_SIZE,SPACE_SIZE)
	};
}

void triple_print(triple t) {
	printf("(%f,%f,%f)", t.x, t.y, t.z);
}

// some operators
#define eps 0.0001
#define abs(V) (((V)<0)?-(V):(V))

#define ADD(T1,T2) 		(triple) { (T1).x + (T2).x, (T1).y + (T2).y, (T1).z + (T2).z }
#define SUB(T1,T2) 		(triple) { (T1).x - (T2).x, (T1).y - (T2).y, (T1).z - (T2).z }

#define MULS(T,S) 		(triple) { (T).x * (S), (T).y * (S), (T).z * (S) }

#define EQ(T1,T2) 		(abs((T1).x-(T2).x) < eps && abs((T1).y-(T2).y) < eps && abs((T1).z-(T2).z) < eps)

#define ABS(T)			sqrt((T).x*(T).x + (T).y*(T).y + (T).z*(T).z)

#define NORM(T) 		MULS(T,(1/ABS(T)))

// --- main ----

int main() {

	// distribute bodies in space (randomly)
	for(int i=0; i<N; i++) {
		B[i].pos = triple_rand();
//		B[i].pos = (position) { 0, -10 + 20*(i/2), -10 + 20*(i%2) };		// for debugging!
		B[i].v   = triple_zero();
	}


	// run simulation for M steps
	#pragma omp parallel
	for(int i=0; i<M; i++) {
		
		// set forces to zero
		#pragma omp for
		for(int j=0; j<N; j++) {
			F[j] = triple_zero();
		}

		// compute forces for each body (very naive)
		#pragma omp for
		for(int j=0; j<N; j++) {
			for(int k=0; k<N; k++) {

				if(j!=k) {
					// comput distance vector
					triple dist = SUB(B[k].pos, B[j].pos);

					// compute absolute distance
					double r = ABS(dist);
				
					// compute strength of force (m1 = m2 = 1, G = 1 (who cares))
					//			F = G * (m1 * m2) / r^2
					double f = 1 / (r*r);

					// compute current contribution to force
					force cur = MULS(NORM(dist), f);

					// accumulate force
					F[j] = ADD(F[j], cur);
				}
			}
		}

		// apply forces
		#pragma omp for
		for(int j=0; j<N; j++) {
			// update speed
			//		F = m * a
			//		a = F / m		// m=1
			//		v' = v + a
			B[j].v = ADD(B[j].v, F[j]);

			// update position
			//		pos = pos + v * dt		// dt = 1
			B[j].pos = ADD(B[j].pos, B[j].v);
		}

/*		// debug print of positions and speed
		for(int i=0; i<N; i++) {
			printf("%2d - ", i); 
			triple_print(B[i].pos);
			printf(" - ");
			triple_print(B[i].v);
			printf("\n");
		}
		printf("\n");
*/

	}

	// check result (impulse has to be zero)
	impulse sum = triple_zero();
	for(int i=0; i<N; i++) {
		sum = ADD(sum, B[i].v);
	}
	int success = EQ(sum, triple_zero());
	printf("Verification: %s\n", ((success)?"OK":"ERR"));
	if (!success) {
		triple_print(triple_zero());
		triple_print(sum); printf("\n");
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

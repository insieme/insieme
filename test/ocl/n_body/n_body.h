#pragma once

#define M 5 // number of iterations
#define SPACE_SIZE 100

#define eps 0.001
#define abs(V) (((V)<0)?-(V):(V))
#define min(A,B) (((A)<(B))?(A):(B))

#define ADD(T1,T2) 		(triple) { (T1).x + (T2).x, (T1).y + (T2).y, (T1).z + (T2).z }
#define SUB(T1,T2) 		(triple) { (T1).x - (T2).x, (T1).y - (T2).y, (T1).z - (T2).z }
#define DIV(T1,T2) 		(triple) { (T1).x / (T2).x, (T1).y / (T2).y, (T1).z / (T2).z }

#define MULS(T,S) 		(triple) { (T).x * (S), (T).y * (S), (T).z * (S) }
#define DIVS(T,S) 		(triple) { (T).x / (S), (T).y / (S), (T).z / (S) }

#define EQ(T1,T2) 		(fabs((T1).x-(T2).x) < eps && fabs((T1).y-(T2).y) < eps && fabs((T1).z-(T2).z) < eps)

#define ABS(T)			sqrt((T).x*(T).x + (T).y*(T).y + (T).z*(T).z + 0.00001)
#define NORM(T) 		MULS(T,(1/ABS(T)))

// the type used to represent a triple of floats 
typedef struct {
	float x, y, z;
} triple;


// the types used to model position, speed and forces
typedef triple position;
typedef triple velocity;
typedef triple force;
typedef triple impulse;

// the type used to model one body
typedef struct {
	float m;		// the mass of the body
	position pos;		// the position in space
	velocity v;		// the velocity of the body
} body;

triple triple_zero() {
	return (position) {0.0, 0.0, 0.0};
}


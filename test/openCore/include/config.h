/*
 * config.h
 *
 *  Created on: 16.08.2010
 *      Author: clemens
 */

#ifndef CONFIG_H_
#define CONFIG_H_

struct icoSSData {
	const char* matName;
	double ug;
	double og;
	int intPoints;
};

struct elData {
	double kte[24][24];
	double rhs[24];
};
struct int4 {
	int i4[4];
};

//version
#define VERSION_COUNT 1
#define VERSION_NAME_CALC "oetz"
#define VERSION_ALL 2
#define VERSION_PARSER 1.8
#define VERSION_CALC 3.3
#define VERSION_NAME_VIEWER "ederplan"
#define VERSION_VIEWER 1.2

//number of arguments that have to be given
#define CALC_SUCCESS 0
#define CALC_FAIL -1

//input-parameter-errors
#define ERR_TOO_FEW_ARGS 1		//error-code
#define ERR_NO_INPUTFILE 2		//error-code - no inputfile found
#define ERR_IN_INPUTFILE 3		//error-code
#define ERR_NO_LOGFILE 4		//error-code - cannot create logfile

#define NODE_EXISTS 5
#define NSET_EXISTS 6
#define UNKNOWN_ELEMENTTYPE 7
#define ELEMENT_EXISTS 8
#define MATERIAL_EXISTS 9
#define ELSET_EXISTS 10
#define STEP_EXISTS 11
#define SOLIDSECTION_EXISTS 12
#define ERROR_STATIC 13
#define ICOSHELLSECTION_EXISTS 14

//elements etc
#define EL_HC8A8E9 1
#define EL_C3D8 2
#define EL_QMHS4 3

#define SOLIDSECTION 0
#define SHELLSECTION 1
#define ICO_SHELLSECTION 2

//maths
#define PI 3.1415926535897932384626

//gui
#define ICO_WIN_WIDTH 600
#define ICO_WIN_HEIGHT 600
#define ICO_VIEW_ANGLE 40.0
#define ICO_ROT_ANGLE 1.5

#define IVM_NODES 1
#define IVM_ELEMENTS 2
#define IVM_NSETS 3
#define IVM_ELSETS 4
#define IVM_LOADS 5
#define IVM_BOUNDARIES 6
#define IVM_SOLUTION 7
#define IVM_RESET_ALL 8

#define IVM_SHOW_NODES_ON 9
#define IVM_SHOW_NODES_OFF 10
#define IVM_SHOW_NODE_NUMBERS_ON 11
#define IVM_SHOW_NODE_NUMBERS_OFF 12
#define IVM_SHOW_ELEMENT_NUMBERS_ON 13
#define IVM_SHOW_ELEMENT_NUMBERS_OFF 14
#define IVM_SHOW_SOLUTION_ON 15
#define IVM_SHOW_SOLUTION_OFF 16
#define IVM_SHOW_SOLUTION_ON_ORIG 17

#endif /* CONFIG_H_ */

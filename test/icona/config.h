/*
 * config.h
 *
 *  Created on: 16.08.2010
 *      Author: clemens
 */

#ifndef CONFIG_H_
#define CONFIG_H_

//version
#define VERSION_COUNT 3
#define VERSION_NAME "glungezer"
#define VERSION_ALL 1
#define VERSION_PARSER 1.7
#define VERSION_CALC 2.1
#define VERSION_VIEWER 1.1

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

//maths
#define PI 3.1415926535897932384626

//gui
#define WIN_WIDTH 800
#define WIN_HEIGHT 800
#define ROT_ANGLE 1.5

#endif /* CONFIG_H_ */

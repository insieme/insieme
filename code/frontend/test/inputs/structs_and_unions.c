/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#include <stdlib.h>
//typedef int bool;
#define bool int

#define true 1
#define false 0

#include <string.h>

// ------------------------ Structs -------------------

typedef struct pStruct {
	char name[30];
	unsigned age;
} Person;

Person getPerson() {
	Person res;
	strcpy(res.name, "John Doe");
	res.age = 101;
	
	return res;
}

bool isTeenager(Person person) {
	return person.age >= 10 && person.age < 20;
}

// ------------------------ Unions -------------------

typedef union convert {
	int integer;
	float real;
} Converter;

Converter getForInt(int value) {
	Converter res;
	res.integer = value;
	return res;
}

Converter getForFloat(float value) {
	Converter res;
	res.real = value;
	return res;
}

// -------------------Union with Structs -------------

typedef union {
	char s[4];
	__extension__ struct {
		char  x, y, z, w;
	};
	__extension__ struct {
		char  s0, s1, s2, s3;
	};
} char4;


// ------------------------ Main -------------------

int main(int argc, char* argv[]) {
	Person mrX = getPerson();
	int res = isTeenager(mrX);
	
	Converter c = getForFloat(0.0);
	res += c.integer;
	
	char scalars[8] = { 0xf , 0x5c, 0xb , 0x51, 0xc9, 0x22, 0xe1, 0xd3 };
	
	
	char4* vector = (char4*)malloc(sizeof(char4) * 2);
	for(int i = 0; i < 8; i+=4) {
		vector[i] = (char4) {
			(char)i, (char)(i+1), (char)(i+2), (char)(i+3)
		};
	}
	
	return 0;
}

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
/**
 * A simple test case covering some arithmetic.
 */
#include <stdlib.h>

#include "../../input_tests/cba.h"

typedef struct {
	unsigned** data;
	int x, y;
} Image;


Image create_image(int x, int y) {

	unsigned* block = (unsigned*)malloc(sizeof(unsigned)*x*y);
	unsigned** index = (unsigned**)malloc(sizeof(unsigned*)*x);

	for(int i=0; i<x; i++) {
		index[i] = &(block[i*x]);
	}

	return (Image){index, x, y};
}

int main(int argc, char** argv) {

	cba_print_code();

	// create an image
	Image i = create_image(20,40);

	cba_print_int(i.data);

	i.data[2][4] = 5;

	cba_print_int(i.data);
	cba_print_int(i.data[2]);
	cba_print_int(i.data[2][4]);
	cba_expect_eq_int(i.data[2][4], 5);
	cba_dump_equations();

	return 0;
}

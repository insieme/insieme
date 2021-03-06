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
 */

int initedGlobal = 5;

typedef struct S {
	int x;
	unsigned y;
} S;

S y = { 1, 5u };

const S klaus_test[] = {{1,2u},{3,4u},{5,6u}};

char char_arr[255] = "";

int arr[2] = { 42, 43 };

#pragma test expect_ir(R"INSPIRE(
def struct IMP_S { x: int<4>; y: uint<4>; };
def IMP_main = ()->int<4> {
	<ref<int<4>,f,f,plain>>(lit("initedGlobal" : ref<int<4>,f,f,plain>)) {5};
	<ref<IMP_S,f,f,plain>>(lit("y" : ref<IMP_S,f,f,plain>)) {1, 5u};
	<ref<array<IMP_S,3u>,t,f,plain>>(lit("klaus_test" : ref<array<IMP_S,3u>,t,f,plain>)) {<ref<IMP_S,f,f,plain>>(ref_decl(type_lit(ref<IMP_S,f,f,plain>))) {1, 2u}, <ref<IMP_S,f,f,plain>>(ref_decl(type_lit(ref<IMP_S,f,f,plain>))) {3, 4u}, <ref<IMP_S,f,f,plain>>(ref_decl(type_lit(ref<IMP_S,f,f,plain>))) {5, 6u}};
	<ref<array<char,255u>,f,f,plain>>(lit("char_arr" : ref<array<char,255u>,f,f,plain>)) {'\0'};
	<ref<array<int<4>,2u>,f,f,plain>>(lit("arr" : ref<array<int<4>,2u>,f,f,plain>)) {42, 43};

	lit("initedGlobal" : ref<int<4>,f,f,plain>);
	lit("y" : ref<IMP_S,f,f,plain>);
	ptr_from_array(ref_reinterpret(lit("klaus_test" : ref<array<IMP_S,3u>,t,f,plain>), type_lit(array<IMP_S,inf>)));
	ptr_from_array(ref_reinterpret(lit("char_arr" : ref<array<char,255u>,f,f,plain>), type_lit(array<char,inf>)));
	ptr_from_array(ref_reinterpret(lit("arr" : ref<array<int<4>,2u>,f,f,plain>), type_lit(array<int<4>,inf>)));
	return 0;
};
IMP_main
)INSPIRE")
int main() {
	initedGlobal;
	y;
	klaus_test;
	char_arr;
	arr;
	return 0;
}

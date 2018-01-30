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

int& forward(int& x) {
	return x;
}

void consumeValue(int i) {}
void consumeRef(int& i) {}

int main() {
	;

	#pragma test expect_ir(R"(
		def IMP_forward = function (v0 : ref<int<4>,f,f,cpp_ref>) -> ref<int<4>,f,f,cpp_ref> {
			return v0;
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<int<4>,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref));
			v1;
			IMP_forward(v1);
			var ref<int<4>,f,f,cpp_ref> v2 = IMP_forward(v1);
		}
	)")
	{
		int i;
		int& r = i;
		r;
		forward(r);
		int& r2 = forward(r);
	}

	#pragma test expect_ir(R"(
		def IMP_consumeValue = function (v0 : ref<int<4>,f,f,plain>) -> unit { };
		def IMP_forward = function (v0 : ref<int<4>,f,f,cpp_ref>) -> ref<int<4>,f,f,cpp_ref> {
			return v0;
		};
		def IMP_consumeRef = function (v0 : ref<int<4>,f,f,cpp_ref>) -> unit { };
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<int<4>,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref));
			IMP_consumeValue(*v0);
			IMP_consumeValue(*v1);
			IMP_consumeValue(
					*ref_kind_cast(
							IMP_forward(ref_kind_cast(v0, type_lit(cpp_ref))),
							type_lit(plain)
					)
			);
			IMP_consumeValue(
					*ref_kind_cast(IMP_forward(v1), type_lit(plain))
			);
			IMP_consumeRef(ref_kind_cast(v0, type_lit(cpp_ref)));
			IMP_consumeRef(v1);
			IMP_consumeRef(
					IMP_forward(ref_kind_cast(v0, type_lit(cpp_ref)))
			);
			IMP_consumeRef(IMP_forward(v1));
		}
	)")
	{
		int i;
		int& r = i;
		consumeValue(i);
		consumeValue(r);
		consumeValue(forward(i));
		consumeValue(forward(r));
		consumeRef(i);
		consumeRef(r);
		consumeRef(forward(i));
		consumeRef(forward(r));
	}

	return 0;
}

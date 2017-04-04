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

struct C {
	int x;
  public:
	int get() const {
		return x;
	}
};

int main() {
	;

	// Test capturing of structs (implicit copy construction)

	#pragma test expect_ir(R"(
		def struct IMP_C {
			x : int<4>;
			const function IMP_get = () -> int<4> {
				return *(this).x;
			}
		};
		def struct __any_string__lambda_class {
			capture_0 : IMP_C;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_0.IMP_get();
			}
		};
		{
			var ref<IMP_C,f,f,plain> v0 = IMP_C::(ref_decl(type_lit(ref<IMP_C,f,f,plain>)));
			var ref<__any_string__lambda_class,f,f,plain> v1 = <ref<__any_string__lambda_class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__lambda_class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {
				ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))
			};
		}
	)")
	{
		C c;
		auto a = [=] {
			c.get();
		};
	}

	return 0;
}

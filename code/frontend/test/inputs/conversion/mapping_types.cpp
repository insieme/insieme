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

struct SimpleClass { };

template<typename T0, typename T1, typename T2, typename T3>
struct ClassWithTemplates { };

namespace ns {
	template<typename ... T>
	struct ToTuple { };

	template<typename ... T>
	struct ToTupleType { };
}

template<typename ... T>
struct EnclosingType {
	struct NestedType { };
};

int main() {
	; // this is required because of the clang compound source location bug

	// Note that we are dealing with pointers here to test only the type mapping

	// simple mapping
	#pragma test expect_ir(R"({
		var ref<ptr<simple>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<simple>,f,f,plain>));
	})")
	{
		SimpleClass* m;
	}

	// template parameter extraction
	#pragma test expect_ir(R"({
		var ref<ptr<templates<int<4>,simple,bool>>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<templates<int<4>,simple,bool>>,f,f,plain>));
	})")
	{
		ClassWithTemplates<SimpleClass, int, double, bool>* m;
	}

	// tuple creation from variadic template arguments
	#pragma test expect_ir(R"({
		var ref<ptr<(simple)>,f,f,plain> v2 = ref_decl(type_lit(ref<ptr<(simple)>,f,f,plain>));
	})")
	{
		ns::ToTuple<SimpleClass>* m;
	}

	// extraction of tuple element type
	#pragma test expect_ir(R"({
		var ref<ptr<extracted_tuple<simple>>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<extracted_tuple<simple>>,f,f,plain>));
	})")
	{
		ns::ToTupleType<ns::ToTuple<SimpleClass>>* m;
	}

	// extraction of tuple element type from enclosing scope
	#pragma test expect_ir(R"({
		var ref<ptr<nested<simple>>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<nested<simple>>,f,f,plain>));
	})")
	{
		EnclosingType<SimpleClass>::NestedType* m;
	}

	return 0;
}

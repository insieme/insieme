/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#pragma once

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	 * This class offers a list of IR extensions required within the class meta-info
	 * object to model IR++ concepts.
	 */
	class IRppExtensions : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		IRppExtensions(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:

		/**
		 * A literal to be used to represent pure virtual functions.
		 */
		LANG_EXT_LITERAL(PureVirtual, "<pure virtual>", "(type<'a>)->'a");

		/**
		 * A construct supporting the construction and initialization of an array
		 * of objects.
		 */
		LANG_EXT_DERIVED(ArrayCtor,
				"let int = uint<8> in "
				""
				"(('a)->ref<'a> allocator, 'b::() ctor, int size)->ref<array<'b,1>> { "
				"	// define the type to be allocated \n"
 				"	let wrapper = struct { int size; array<'b,1> data; }; "
				"	"
				"	// allocate the memory \n"
				"	ref<wrapper> res = allocator((wrapper){ size, array.create.1D(lit('b), size) }); "
				"	"
				"	// init elements \n"
				"	for(int i=0u .. size) {"
				"		ctor(res->data[i]);"
				"	}"
				"	"
				"	// return array reference \n"
				"	return res->data;"
				"}"
		);

		/**
		 * A construct supporting the construction and initialization of a vector
		 * of objects.
		 */
		LANG_EXT_DERIVED(VectorCtor,
				"let int = uint<8> in "
				""
				"(('a)->ref<'a> allocator, 'b::() ctor, intTypeParam<#s> size)->ref<vector<'b,#s>> { "
				"	// define the type to be allocated \n"
				"	let wrapper = struct { int size; vector<'b,#s> data; }; "
				"	"
				"	// allocate the memory \n"
				"	ref<wrapper> res = allocator((wrapper){ to.uint(size), vector.init.undefined(lit('b), size) });"
				"	"
				"	// init elements \n"
				"	for(int i=0u .. *res->size) {"
				"		ctor(res->data[i]);"
				"	}"
				"	"
				"	// return array reference \n"
				"	return res->data;"
				"}"
		);

		/**
		 * A destructor supporting the destruction of an array of objects.
		 */
		LANG_EXT_DERIVED(ArrayDtor,
				"let int = uint<8> in "
				""
				"(ref<array<'b,1>> data, (ref<'a>)->unit deallocator, ~'b::() dtor) -> unit { "
				"	// define the type to be allocated \n"
				"	let wrapper = struct { int size; array<'b,1> data; }; "
				"	"
				"	// access wrapper struct \n"
				"	ref<wrapper> block = ref.expand(data, dp.member(dp.root, lit(\"data\")), lit(wrapper)); "
				"	"
				"	// destroy all elments within the array \n"
				"	for(int i=0u .. *(block->size)) {"
				"		dtor(block->data[i]);"
				"	}"
				"	"
				"	// free memory \n"
				"	deallocator(block);"
				"}"
		);


		/**
		 * An operator converting a C++ reference into an IR reference.
		 */
		LANG_EXT_DERIVED(RefCppToIR,
				"(struct { ref<'a> _cpp_ref } x)->ref<'a> { return x._cpp_ref; }"
		);

		/**
		 * An operator converting an IR reference into a C++ reference.
		 */
		LANG_EXT_DERIVED(RefIRToCpp,
				"let cppRef = struct { ref<'a> _cpp_ref } in "
				"(ref<'a> x)->cppRef { return (cppRef) { x }; }"
		);

		/**
		 * An operator converting a const C++ reference into an IR reference.
		 */
		LANG_EXT_DERIVED(RefConstCppToIR,
				"(struct { ref<'a> _const_cpp_ref } x)->ref<'a> { return x._const_cpp_ref; }"
		);

		/**
		 * An operator converting an IR reference into a const C++ reference.
		 */
		LANG_EXT_DERIVED(RefIRToConstCpp,
				"let cppRef = struct { ref<'a> _const_cpp_ref } in "
				"(ref<'a> x)->cppRef { return (cppRef) { x }; }"
		);

		/**
		 * An operator converting a C++ reference into a const C++ reference.
		 */
		LANG_EXT_DERIVED(RefCppToConstCpp,
				"let cppRef = struct { ref<'a> _cpp_ref } in "
				"let constCppRef = struct { ref<'a> _const_cpp_ref } in "
				"(cppRef x)->constCppRef { return (constCppRef) { x._cpp_ref }; }"
		);


	};

} // end namespace lang
} // end namespace core
} // end namespace insieme

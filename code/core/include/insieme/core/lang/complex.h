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

#pragma once

#include "insieme/core/lang/extension.h"

#include "insieme/core/lang/reference.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	 * An extension covering the derived complex type and all its
	 * associated operators.
	 */
	class ComplexExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ComplexExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		// import the reference module for ref-operations
		IMPORT_MODULE(ReferenceExtension);


		// -------------------- complex ---------------------------

		/**
		 * Defines a complex number as a pair of a real and imaginary value
		 */
		TYPE_ALIAS("complex", "struct _ir_complex { rel : 'a; img : 'a; }");

		/**
		 * Defines the generic complex type.
		 */
		LANG_EXT_TYPE(GenComplex, "struct _ir_complex { rel : 'a; img : 'a; }")



		/**
		 * Get real part of complex.
		 */
		LANG_EXT_DERIVED(ComplexReal,
			"(x : struct _ir_complex { _real : 'a; _img : 'a; })->'a { return x._real; }"
		);

		/**
		 * Get real part of complex ref.
		 */
		LANG_EXT_DERIVED(RefComplexReal,
			"(x : ref<struct _ir_complex { _real : 'a; _img : 'a; },'c,'v>)->ref<'a,'c,'v> { return x._real; }"
		);

		/**
		 * Get imaginary part of complex.
		 */
		LANG_EXT_DERIVED(ComplexImg,
			"(x : struct _ir_complex { _real : 'a; _img : 'a; })->'a { return x._img; }"
		);

		/**
		 * Get imaginary part of complex ref.
		 */
		LANG_EXT_DERIVED(RefComplexImg,
			"(x : ref<struct _ir_complex { _real : 'a; _img : 'a; },'c,'v>)->ref<'a,'c,'v> { return x._img; }"
		);

		/**
		 * Create a Complex out of a constant value.
		 */
		LANG_EXT_DERIVED(ConstantToComplex,
							"alias res_t = struct _ir_complex {_real : 'a; _img : 'a; };"
							"(c : 'a)->res_t {"
								"return <res_t> {c, CAST('a) 0};"
							"}");

		/**
		 * Check if the real and imaginary part of the complex number are zero.
		 */
		LANG_EXT_DERIVED(ComplexToBool,
			"(x : struct _ir_complex { _real : 'a; _img : 'a; })->bool { return (x._img != num_cast(0.0, type_lit('a))) || (x._real != num_cast(0.0, type_lit('a))); }"
		);

		/**
		 * Cast a complex number of type a to a complex number of type b
		 */
		LANG_EXT_DERIVED(ComplexToComplex,
							"alias res_t = struct _ir_complex {_real : 'b; _img : 'b; };"
							"(c : struct _ir_complex {_real : 'a; _img : 'a; }, t : type<'b>)->res_t {"
								"return <res_t> { num_cast(c._real, type_lit('b)), num_cast(c._img, type_lit('b)) };"
							"}");

	};


	// --------------------- Utilities ----------------------------

	/**
	 * Determines whether a given node is the complex type or an expression of
	 * the complex type.
	 */
	bool isComplexType(const NodePtr& node);
	
} // end namespace lang
} // end namespace core
} // end namespace insieme

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


	class SIMDVectorExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		SIMDVectorExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:


		
		//supported operators according to: http://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html
	
		// binary operators (+, -, *, /, %, &, |, ^)
		LANG_EXT_LITERAL(SIMDAdd, "simd.add",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDSub, "simd.sub",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDMul, "simd.mul",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDDiv, "simd.div",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDMod, "simd.mod",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDAnd, "simd.and",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDOr, "simd.or",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDXor, "simd.xor",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDLShift, "simd.lshift",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDRShift, "simd.rshift",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")

		// comparison operators are supported from gcc 4.7
		// GCC 4.6.3 does NOT support them
		//comparison operators: ==, !=, <, <=, >, >=. 
		LANG_EXT_LITERAL(SIMDEq, "simd.eq",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDNe, "simd.ne",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDLt, "simd.lt",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDLe, "simd.le",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDGt, "simd.gt",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDGe, "simd.ge",  "(simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'elem1, #l>>")

		//unary operators ~, -
		LANG_EXT_LITERAL(SIMDNot, "simd.not",  "(simd<vector<'elem1,#l>>) -> simd<vector<'elem1, #l>>")
		LANG_EXT_LITERAL(SIMDMinus, "simd.minus",  "(simd<vector<'elem1,#l>>) -> simd<vector<'elem1, #l>>")
		
		//LANG_EXT_LITERAL(SIMDInitUniform,    	"simd.init.uniform",    	"('elem, intTypeParam<#a>) -> simd<vector<'elem,#a>>")
		LANG_EXT_LITERAL(SIMDInitUndefined,  	"simd.init.undefined",  	"(type<'elem>, intTypeParam<#a>) -> simd<vector<'elem,#a>>")
		LANG_EXT_LITERAL(SIMDInitPartial, 		"simd.init.partial",		"(list<'elem>, intTypeParam<#n>) -> simd<vector<'elem,#n>>")

		/*

		LANG_EXT_LITERAL(VectorToSIMD, "ref.vector.to.ref.simd",  "(ref<vector<'elem1,#l>>) -> ref<simd<vector<'elem1, #l>>>")
		LANG_EXT_LITERAL(SIMDToVector, "ref.simd.to.ref.vector",  "(ref<simd<vector<'elem1,#l>>>) -> ref<vector<'elem1, #l>>")


		LANG_EXT_LITERAL(SIMDVectorPointwise, "simd.pointwise", 
				"(('elem1, 'elem2) -> 'res, simd<vector<'elem1,#l>>, simd<vector<'elem2,#l>>) -> simd<vector<'res, #l>>")

		LANG_EXT_LITERAL(SIMDVectorPointwiseUnary, "simd.pointwise.unary", 
				"(('elem) -> 'res , simd<vector<'elem,#l>>) -> simd<vector<'res, #l>>")
		*/
	};

	/**
		* Checks if the given type isj a SIMD vector
		* @param type the type to check
		* @return bool true if given type is SIMD, false otherwise
		*/
	bool isSIMDVector(const TypePtr& type);

	/**
		* Gets the inner vector type of an SIMD vector
		* @param type the type to retrive the inner vector from
		* @return a vectorType representing the number of elements and element of the simd vector
		*/
	VectorTypePtr getSIMDVectorType(const TypePtr& type);

	/**
		* Creates a SIMD vector from an Vector type
		* @param type the type to generate a SIMD vector type from
		* @return the SIMD vector type
		*/
	GenericTypePtr toSIMDVector(const VectorTypePtr& type);

} // end namespace lang
} // end namespace core
} // end namespace insieme

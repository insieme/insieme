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

namespace {
    insieme::core::ExpressionPtr getComplexMember(const insieme::core::ExpressionPtr& expr, const insieme::core::ExpressionPtr& mem) {
        //ret type of inner element
        insieme::core::IRBuilder builder( expr->getNodeManager() );

        insieme::core::StructTypePtr structType = expr->getType().as<insieme::core::RefTypePtr>().getElementType().as<insieme::core::StructTypePtr>();
		insieme::core::TypePtr innerType = structType->getEntries()[0]->getType();
        return builder.callExpr(builder.refType(innerType), mem, expr, builder.getTypeLiteral(innerType));
    }
}

namespace insieme {
namespace core {
namespace lang {

	/**
	 * This class offers a list of IR extensions required within the class meta-info
	 * object to model IR++ concepts.
	 */
	class ComplexExtensions : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ComplexExtensions(core::NodeManager& manager)
				: core::lang::Extension(manager) {}


	public:

		/**
		 * Complex type.
		 */
		LANG_EXT_TYPE(Complex, "struct { 'a _real; 'a _img; }");

        /**
		 * Get real part of complex.
		 */
		LANG_EXT_DERIVED(ComplexReal,
            "(ref<struct { 'a _real; 'a _img; }> x, type<'a> t)->ref<'a> { return x->_real; }"
        );

        /**
		 * Get imaginary part of complex.
		 */
		LANG_EXT_DERIVED(ComplexImg,
            "(ref<struct { 'a _real; 'a _img; }> x, type<'a> t)->ref<'a> { return x->_img; }"
        );


		TypePtr getComplexType(const TypePtr& elementType) const {
			IRBuilder builder(elementType->getNodeManager());
			return builder.structType(toVector(
					builder.namedType("_real", elementType),
					builder.namedType("_img", elementType)
			));
		}


		ExpressionPtr getReal(const ExpressionPtr& expr) const {
		    assert(expr->getType().isa<RefTypePtr>());
		    return getComplexMember(expr, getComplexReal());
            //expr->getType().i)
		}

        ExpressionPtr getImg(const ExpressionPtr& expr) const {
           	assert(expr->getType().isa<RefTypePtr>());
            return getComplexMember(expr, getComplexImg());
		}


	};
}
}
}

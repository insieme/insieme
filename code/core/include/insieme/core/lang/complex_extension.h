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
            "(struct { 'a _real; 'a _img; } x)->'a { return x._real; }"
        );

        /**
		 * Get real part of complex ref.
		 */
		LANG_EXT_DERIVED(RefComplexReal,
            "(ref<struct { 'a _real; 'a _img; }> x)->ref<'a> { return x->_real; }"
        );

        /**
		 * Get imaginary part of complex.
		 */
		LANG_EXT_DERIVED(ComplexImg,
            "(struct { 'a _real; 'a _img; } x)->'a { return x._img; }"
        );

        /**
		 * Get imaginary part of complex ref.
		 */
		LANG_EXT_DERIVED(RefComplexImg,
            "(ref<struct { 'a _real; 'a _img; }> x)->ref<'a> { return x->_img; }"
        );

        /**
		 * Create a Complex out of a constant value.
		 */
        LANG_EXT_DERIVED(ConstantToComplex,
                            "let res_t = struct {'a _real; 'a _img} in"
                            "('a c)->res_t {"
                                "return (res_t) {c, ('a) 0};"
                            "}");

        /**
		 * Check if the real and imaginary part of the complex number are zero.
		 */
		LANG_EXT_DERIVED(ComplexToBool,
            "(struct { 'a _real; 'a _img; } x)->bool { return ((x._img != ('a) 0.0) || (x._real != ('a) 0.0)); }"
        );

        /**
		 * Cast a complex number of type a to a complex number of type b
		 */
        LANG_EXT_DERIVED(ComplexToComplex,
                            "(struct {'a _real; 'a _img} c, type<'b> t)->struct {'b _real; 'b _img} {"
                                "return (struct {'b _real; 'b _img}) { ('b) c._real, ('b) c._img };"
                            "}");


        /**
		 * Creates a complex type out of a type
		 * @param elementType the inner type of the complex number
		 * @return complex type
		 */
		TypePtr getComplexType(const TypePtr& elementType) const {
			IRBuilder builder(elementType->getNodeManager());
			return builder.structType(toVector(
					builder.namedType("_real", elementType),
					builder.namedType("_img", elementType)
			));
		}

        /**
		 * Get the real part out of a complex number
		 * @param expr the complex number expression
		 * @return real part of complex number
		 */
		ExpressionPtr getReal(const ExpressionPtr& expr) const {
            if(expr->getType().isa<StructTypePtr>())
                return getComplexMember(expr, getComplexReal());
            else
                return getComplexMember(expr, getRefComplexReal());
            assert(false && "this is no ref or struct type and so it cannot be a complex type.");
		}

        /**
		 * Get the imaginary part out of a complex number
		 * @param expr the complex number expression
		 * @return imaginary part of complex number
		 */
        ExpressionPtr getImg(const ExpressionPtr& expr) const {
            if(expr->getType().isa<StructTypePtr>())
                return getComplexMember(expr, getComplexImg());
            else
                return getComplexMember(expr, getRefComplexImg());
            assert(false && "this is no ref or struct type and so it cannot be a complex type.");
		}

        /**
		 * Cast a complex number to a bool. Check if the complex number equals 0+0*i.
		 * @param expr the complex number expression
		 * @return boolean expression pointer
		 */
        ExpressionPtr castComplexToBool(const ExpressionPtr& expr) const {
            assert(expr);
            IRBuilder builder(expr->getNodeManager());
            ExpressionPtr tmp = expr;
            if(expr->getType().isa<RefTypePtr>()) {
                tmp = builder.deref(expr);
            }
            assert(tmp->getType().isa<insieme::core::StructTypePtr>());
            return builder.callExpr(getComplexToBool(), tmp);
        }

        /**
		 * Cast a complex number of type a to a complex number of type b
		 * @param expr the complex number expression
		 * @param targetTy the target type (e.g. int)
		 * @return complex number
		 */
        ExpressionPtr castComplexToComplex(const ExpressionPtr& expr, const TypePtr& targetTy) const {
            assert(expr);
            assert(targetTy);
            IRBuilder builder(expr->getNodeManager());
            TypePtr target = targetTy.as<insieme::core::StructTypePtr>()->getEntries()[0]->getType();
            return builder.callExpr(getComplexToComplex(), expr, builder.getTypeLiteral(target));
        }


        /**
        * Check if the given expression is a complex number
        * @param expr the complex number expression
        * @return boolean value
        */
        bool isComplexType(const insieme::core::ExpressionPtr& expr) const {
            if(expr->getType().isa<insieme::core::StructTypePtr>())
                return true;
            if(expr->getType().isa<insieme::core::RefTypePtr>()) {
                if(expr->getType().as<insieme::core::RefTypePtr>().getElementType().isa<insieme::core::StructTypePtr>())
                    return true;
            }
            return false;
        }

        /**
        * Returns the inner type of a complex number
        * @param expr the complex number expression
        * @return the inner type of the complex number
        */
        insieme::core::TypePtr getComplexMemberType(const insieme::core::ExpressionPtr& expr) const {
            assert(isComplexType(expr) && "This is not a complex type");
            if(expr->getType().isa<insieme::core::StructTypePtr>())
                return expr->getType().as<insieme::core::StructTypePtr>()->getEntries()[0]->getType();
            return expr->getType().as<insieme::core::RefTypePtr>().getElementType().as<insieme::core::StructTypePtr>()->getEntries()[0]->getType();
        }

        /**
        * Returns the real or the imaginary part a complex number
        * @param expr the complex number expression
        * @param mem real or imaginary part
        * @return call expression to retrieve the real/imaginary part of a complex number
        */
        insieme::core::ExpressionPtr getComplexMember(const insieme::core::ExpressionPtr& expr, const insieme::core::ExpressionPtr& mem) const {
            //ret type of inner element
            insieme::core::IRBuilder builder( expr->getNodeManager() );
            if(expr->getType().isa<insieme::core::StructTypePtr>()) {
                return builder.callExpr(getComplexMemberType(expr), mem, expr);
            } else {
                return builder.callExpr(builder.refType(getComplexMemberType(expr)), mem, expr);
            }
        }


	};
}
}
}

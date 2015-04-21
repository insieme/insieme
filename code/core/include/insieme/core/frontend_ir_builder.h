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

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {

    /**
     *  This class wraps the IRBuilder and provides the
     *  methods that are needed to generate IR elements.
     */
    class FrontendIRBuilder : public core::IRBuilder {
    public:
        FrontendIRBuilder(core::NodeManager& mgr) : core::IRBuilder(mgr) { }

		// --------------------------- C++ -----------------------------

		/**
		 * Creates an expression representing a pure virtual function of the given type.
		 *
		 * @param memberFunctionType the type of the resulting pure virtual function
		 * @return an expression representing a pure virtual function of the given type
		 */
		ExpressionPtr getPureVirtual(const FunctionTypePtr& memberFunctionType) const;


		/**
		 * Converts a given IR reference into a C++ reference.
		 */
		ExpressionPtr toCppRef(const ExpressionPtr& ref) const;

		/**
		 * Converts a given IR reference into a const C++ reference.
		 */
		ExpressionPtr toConstCppRef(const ExpressionPtr& ref) const;

		/**
		 * Converts a given IR reference into a const C++ right side reference.
		 */
		ExpressionPtr toConstRValCppRef(const ExpressionPtr& ref) const;

		/**
		 * Converts a (const) C++ reference in an IR reference.
		 */
		ExpressionPtr toIRRef(const ExpressionPtr& ref) const;

		// --------------------- static variables ----------------------


		ExpressionPtr initStaticVariable(const LiteralPtr& staticVariable, const ExpressionPtr& initValue, bool constant= false) const;

		StatementPtr createStaticVariable(const LiteralPtr& staticVariable) const;

    };

}   //namespace core
}   //namespace insieme

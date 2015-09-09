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

#include <map>

#include "insieme/frontend/extensions/variable_length_array_extension.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/converter.h"
#include "insieme/annotations/c/include.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace frontend {
namespace extensions {
	
	insieme::core::TypePtr VariableLengthArrayExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		//we iterate trough the dimensions of 
		//the array from left to right. There is no hint 
		//in the C standard how this should be handled
		//and therefore the normal operator precedence
		//is used.
		if(const clang::VariableArrayType* arrType = llvm::dyn_cast<clang::VariableArrayType>(type.getTypePtr())) {
			//only consider variable array types
			//create a decl stmt e.g., decl uint v1 = i;
			core::IRBuilder builder = converter.getIRBuilder();
			auto index = converter.convertExpr(arrType->getSizeExpr());
			//cast needed from rhs type to int<inf>?!
			if(index->getType() != builder.getLangBasic().getIntInf()) {
				index = builder.numericCast(index, builder.getLangBasic().getIntInf());
			} 
			auto decl = builder.declarationStmt(builder.getLangBasic().getIntInf(), index);
			sizes.push_back(decl);

			//convert the element type (in nested array case this is again a variable array type)
			core::TypePtr elementType = converter.convertType(arrType->getElementType());
			core::TypePtr arrayType = builder.arrayType(elementType, decl->getVariable());

			return arrayType;
		}
		return nullptr;
	}
	
	stmtutils::StmtWrapper VariableLengthArrayExtension::Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) {
		sizes.clear();
		return stmtutils::StmtWrapper();
	}

	stmtutils::StmtWrapper VariableLengthArrayExtension::PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt, 
					insieme::frontend::conversion::Converter& converter) {
		if(sizes.size()>0) {
			//insert the variable declarations of the 
			//indices before the array is declared
			stmtutils::StmtWrapper newIRStmt = irStmt;
			while(sizes.size()>0) {
				newIRStmt.insert(newIRStmt.begin(), sizes.back());
				sizes.pop_back();
			}	
			return newIRStmt;
		}
		return irStmt;
	}

	
	core::ExpressionPtr VariableLengthArrayExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		auto& basic = converter.getNodeManager().getLangBasic();
		auto& builder = converter.getIRBuilder();

		//HINT: This will change to clang::SizeOfAlignOfExpr with clang 3.7
		if(clang::UnaryExprOrTypeTraitExpr* sizeofexpr = const_cast<clang::UnaryExprOrTypeTraitExpr*>(llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(expr))) {
			
			// if it's a VLA type, clang gives us the components we need to multiply in the child list (up to the innermost VLA)
			if(sizeofexpr->isArgumentType()) {
				// run through the conversion to see if it's VLA
				auto irType = converter.convertType(sizeofexpr->getTypeOfArgument());
				// if sizes is not empty, we just converted a VLA
				if(!sizes.empty()) {
					// use sizes to navigate to innermost VLA and get its element type
					core::TypePtr elemType;
					do {
						elemType = core::lang::ArrayType(irType).getElementType();
						if(core::lang::ArrayType(irType).getSize() == sizes.front()->getVariable()) sizes.pop_front();
						irType = elemType;
					} while(!sizes.empty());
					// multiply all children with sizeof of inner type
					auto retExpr = builder.callExpr(basic.getSizeof(), builder.getTypeLiteral(elemType)); 
					for(auto child : sizeofexpr->children()) {
						retExpr = builder.mul(retExpr, builder.numericCast(converter.convertExpr(llvm::dyn_cast<clang::Expr>(child)), basic.getUInt8()));
					}
					return retExpr;
				}
			}
			// if argument of UnaryExprOrTypeTraitExpr is any DeclRef, then handle like every other UnaryExprOrTypeTraitExpr, outside of this FE extension
		}
		return nullptr;
	}
	
} // end namespace extensions
} // end namespace frontend
} // end namespace insieme

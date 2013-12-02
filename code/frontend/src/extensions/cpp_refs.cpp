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

#include "insieme/frontend/extensions/cpp_refs.h"

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/assert.h"

 namespace insieme {
 namespace frontend {

 namespace {
	typedef std::map<core::LambdaExprPtr, core::LambdaExprPtr> memberReplace_t;
	typedef std::map<core::TypePtr, memberReplace_t> memberFunctionReplacement_t;

	core::TypePtr lookupTypeDetails(const core::TypePtr& type, insieme::frontend::tu::IRTranslationUnit& tu){
		if (!type.isa<core::GenericTypePtr>()) return type;
		core::TypePtr res = tu[type.as<core::GenericTypePtr>()];
		if (!res) return type;
		if (res.isa<core::GenericTypePtr>() && type != res){
			return lookupTypeDetails(res,tu);
		}
		return res;
	}
 }

	insieme::frontend::tu::IRTranslationUnit CppRefsCleanup::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu){

		// only cpp
		if (!tu.isCXX()) return tu;

		memberFunctionReplacement_t memberMap;

		// for each of the functions
		for (auto& pair : tu.getFunctions()){
			core::ExpressionPtr lit = pair.first;
			core::LambdaExprPtr func = pair.second;
			core::LambdaExprPtr oldFunc = pair.second;
			core::TypePtr retType = lit->getType().as<core::FunctionTypePtr>()->getReturnType();
			assert( retType == func->getType().as<core::FunctionTypePtr>()->getReturnType());

			core::IRBuilder builder(func->getNodeManager());
			const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();
			const core::lang::IRppExtensions& ext = func->getNodeManager().getLangExtension<core::lang::IRppExtensions>();
	
			// filter to filter out the recursive transition to another called function
			auto filter = [&func] (const core::NodePtr& node) ->bool{
				if(core::LambdaExprPtr call = node.isa<core::LambdaExprPtr>()){
					if (call == func) return true;
					else return false;
				}
				return true;
			};

			// if return type is a cpp ref
			if (core::analysis::isCppRef(retType) || core::analysis::isConstCppRef(retType)){

				// wrap all returns with a cpp ref conversion
				auto fixer = [&](const core::NodePtr& node)-> core::NodePtr{
					if (core::ReturnStmtPtr retStmt = node.isa<core::ReturnStmtPtr>()){
						core::ExpressionPtr expr = retStmt->getReturnExpr();

						// sometimes is easyer to return by value, ammend this
						if (core::analysis::isCallOf(expr, gen.getRefDeref())){
							expr = expr.as<core::CallExprPtr>()[0];
						}

						// wrap the return into the righful type
						if (core::analysis::isConstCppRef(retType)){
							// convert from cpp ref to const cpp ref
							if (core::analysis::isCppRef(expr->getType())){
								expr = builder.callExpr(retType, ext.getRefCppToConstCpp(), expr);
							}
							// convert form non ref to const ref
							else if (!core::analysis::isConstCppRef(expr->getType())){
								expr = builder.callExpr(retType, ext.getRefIRToConstCpp(), expr);
							}
						}
						else
						{
							if (!core::analysis::isCppRef(expr->getType())){
								expr = builder.callExpr(retType, ext.getRefIRToCpp(), expr);
							}
						}
						
						return builder.returnStmt(expr);
					}
					return node;
				};

				// modify all returns at once!
				auto returnFixer = core::transform::makeCachedLambdaMapper(fixer, filter);
				func = returnFixer.map(func);
			}
			// but it could be also that we return a cppref and the return type is not cppref
			else {

				// unwrap all returns of cppref exprs
				auto fixer = [&](const core::NodePtr& node)-> core::NodePtr{
					if (core::ReturnStmtPtr retStmt = node.isa<core::ReturnStmtPtr>()){
						core::ExpressionPtr expr = retStmt->getReturnExpr();

						// we are returning a non cpp ref, uwrap it
						if (core::analysis::isConstCppRef(expr->getType())){
							expr = builder.callExpr(retType, ext.getRefConstCppToIR(), expr);
						}
						else if (core::analysis::isCppRef(expr->getType())){
							expr = builder.callExpr(retType, ext.getRefCppToIR(), expr);
						}

						// are we returning by value? deref
						if (!retType.isa<core::RefTypePtr>() && retType.isa<core::RefTypePtr>()){
							expr = builder.deref(expr);
						}

						return builder.returnStmt(expr);
					}
					return node;
				};

				// modify all returns at once!
				auto returnFixer = core::transform::makeCachedLambdaMapper(fixer, filter);
				func = returnFixer.map(func);
			}

			// fix the function
			tu.replaceFunction(lit.as<core::LiteralPtr>(), func);


			// now a little problem for the brave souls:
			// 	the metainfo class has not being modified, and still holds pointers to functions that no longer exist
			//	we need to update those, store them in a map, and well do it later on
			if (func->getType().as<core::FunctionTypePtr>()->isMemberFunction()){
				auto& memberReplacements = memberMap[func->getType().as<core::FunctionTypePtr>()->getObjectType()];
				memberReplacements [builder.normalize(oldFunc)]  = func;
			}
		}

		// now fix the meta info of the classes
		for (auto pair : memberMap){
			// meta info is attached to retrieve the full type
			core::TypePtr objTy = lookupTypeDetails(pair.first, tu);
			core::IRBuilder builder(objTy->getNodeManager());
			assert(objTy);

			assert(core::hasMetaInfo(objTy));
			memberReplace_t replacements = pair.second;
			core::ClassMetaInfo meta = core::getMetaInfo(objTy);
			vector<core::MemberFunction> members = meta.getMemberFunctions();
			for (core::MemberFunction& member : members){
				auto fit = replacements.find(builder.normalize(member.getImplementation().as<core::LambdaExprPtr>()));
				if (fit != replacements.end()){
					member = core::MemberFunction(member.getName(), fit->second, member.isVirtual(), member.isConst());
				}
			}
			meta.setMemberFunctions(members);
			core::setMetaInfo(objTy, meta);
		}

		return tu;
	}

} // frontend
} // insieme

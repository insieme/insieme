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

#include "insieme/backend/opencl/opencl_analysis.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace analysis {
	
	using namespace insieme::annotations::opencl;
	
	namespace {
		bool isPrimitive(const core::TypePtr& type) {
			// return type->getNodeManager().getLangBasic().isPrimitive(type);
			auto& basic = type->getNodeManager().getLangBasic();
			return basic.isChar(type) || basic.isBool(type) || basic.isScalarType(type);
		}
		// @TODO: removeReference/Pointer sauberer lösen
		core::TypePtr removeReference(const core::TypePtr& node) {
			if (core::lang::isReference(node))
				return core::lang::ReferenceType(node).getElementType();
			else
				return node;
		}
		
		core::TypePtr removePointer(const core::TypePtr& node) {
			if (core::lang::isPointer(node))
				return core::lang::PointerType(node).getElementType();
			else
				return node;
		}
		
		bool isBuiltIn(const core::NodePtr& node) {
			// represents a white-list of all IMP__fun which are allowed in the kernel
			static std::unordered_set<std::string> builtins = {
				"acos", "acosh", "asin", "asinh", "atan", "atan2",
				"cbrt", "ceil", "copysign", "cos", "cosh", "erfc",
				"erf", "exp", "exp2", "expm1", "fabs", "fdim",
				"floor", "fma", "fmax", "fmin", "fmod", "frexp",
				"hypot", "ilogb", "ldexp", "lgamma", "log", "log2",
				"log10", "log1p", "logb", "modf", "nan", "nextafter",
				"pow", "remainder", "remquo", "rint", "round", "sin",
				"sqrt", "tan", "tanh", "tgamma", "trunc"};
			// first of all, it must be a literal atm
			if (node->getNodeType() != core::NT_Literal)
				return false;
			// convert eg. IMP__cos to cos
			const std::string& name = insieme::utils::demangle(node.as<core::LiteralPtr>()->getStringValue());
			return builtins.find(name) != builtins.end();
		}
		
		bool hasBuiltInLiterals(const core::NodePtr& node) {
			bool result = true; 
			auto& refExt = node->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			// check for invalid usage of global and externally declared literals
			core::visitDepthFirstOnceInterruptible(node, [&](const core::LiteralPtr& literal) {
				// if the literal is reference, it is a global var
				if (core::lang::isReference(literal)) {
					result = false;
					return true;
				}
				// IR constructs are always legal
				if (core::lang::isBuiltIn(literal) || core::lang::isDerived(literal)) {
					// special check built-ins which allocate memory on the heap
					if (core::analysis::isCallOf(refExt.getRefAlloc(), literal)) {
						core::ExpressionPtr memloc = core::analysis::getArgument(literal, 1);
						if (memloc == refExt.getMemLocHeap())
							// whoops.. malloc & friends cannot be used in kernels
							return false;
					}
					return false;
				}
				// handle potential external functions
				if (literal->getType().isa<core::FunctionTypePtr>()) {
					// in this case we need to check if it is an opencl builtin
					if(!isBuiltIn(literal)) {
						result = false;
						return true;
					}
				}
				return false;
			});
			return result;
		}
		
		bool hasMultipleIndirections(const core::NodePtr& node) {
			/*
			ref<ref<...>> is a ptr!
			
			bool checkDoublePointer(const VariablePtr& var) const {
				// also check if the argument is not a pointer to a pointer
				auto type = var->getType();
				// if we hold a reference, get the element type instead
				if(lang::isReference(type)) type = lang::ReferenceType(type).getElementType();
				// if we hold a pointer, get the element type instead
				if(lang::isPointer(type)) type = lang::PointerType(type).getElementType();
				// at this point, it is not legal to hold a pointer (OpenCL does not support it!)
				if(lang::isPointer(type)) {
					LOG(WARNING) << "OpenCL: illegal pointer to pointer as argument " << dumpColor(var) << " of type: " << dumpColor(var->getType());
					return false;
				}
				return true;
			}
			*/
			return false;
		}
		
		bool isTrivial(const core::TypePtr& type, bool isParameter = false) {
			// @TODO: this case of ptr casting is currently not handeled
			//
			// decl IMP_printf : (ptr<char,t,f>, var_list) -> int<4>;
			// decl fun000 : (int<4>, ptr<ptr<char>>) -> int<4>;
			// decl fun001 : (ptr<unit>) -> unit;
			// decl fun002 : () -> unit;
			// def fun000 = function (v379 : ref<int<4>,f,f,plain>, v380 : ref<ptr<ptr<char>>,f,f,plain>) -> int<4> {
			//     fun001(ptr_cast(ptr_reinterpret(ptr_of_function(fun002), type_lit(unit)), type_lit(f), type_lit(f)));
			//     return 0;
			// };
			// def fun001 = function (v341 : ref<ptr<unit>,f,f,plain>) -> unit {
			//     ptr_deref(ptr_cast(ptr_reinterpret(*v341, type_lit(() -> unit)), type_lit(t), type_lit(f)))();
			//     return unit;
			// };
			// def fun002 = function () -> unit {
			//     IMP_printf(ptr_cast(ptr_from_array("hihi\n"), type_lit(t), type_lit(f)), varlist_pack(()));
			//     return unit;
			// };
			//
			// if we hold a reference, get the element type instead
			if (core::lang::isReference(type)) {
				if (!core::lang::isPlainReference(type))
					// in this case we cannot simply strip it off since OpenCL requires plain refs only
					return false;
				// in case of a parameter the indirection is limited
				if (isParameter) {
					// eg. ref<ref<ptr<'a>>> is invalid as argument
					if (hasMultipleIndirections(type))
						return false;
				}
				// remove it and recurse the check
				return isTrivial(removeReference(type), isParameter);
			}
			// check for funtion pointers
			if (core::lang::isPointer(type)) {
				if (getElementType(type).isa<core::FunctionTypePtr>())
					return false;
			}
			// in case we are a parameter a referenced record cannot contain pointers
			if (isParameter) {
				// simply strip off all indirections and check the element type
				if (auto tagType = getReferencedType(type).isa<core::TagTypePtr>()) {
					for(const auto& field : tagType->getRecord()->getFields()) {
						if (core::lang::isPointer(field->getType()))
							// whoops, we fail quite late tho
							return false;
					}
				}
			}
			// the general triviality must be fulfilled as-well
			return core::analysis::isTrivial(type);
		}
		
		/*
		bool checkCompoundStmt(const BaseAnnotationPtr& anno, const CompoundStmtPtr& compoundStmt) const {
			LOG(DEBUG) << "OpenCL: checking compoundStmt:";
			LOG(DEBUG) << "OpenCL: " << dumpColor(compoundStmt);
			// check if the compound is outlineAble, otherwise transform::outline will trigger an assertion			
			if(!analysis::isOutlineAble(compoundStmt, false)) return false;
			
			auto callExpr = transform::outline(nodeMan, compoundStmt);
			// @TODO: is is just a proof of concept
			if(!checkLambdaExpr(anno, callExpr->getFunctionExpr().as<LambdaExprPtr>())) return false;
			
			DEBUG opencl_sema.cpp:317 - OpenCL: checking compoundStmt:
			DEBUG opencl_sema.cpp:318 - OpenCL: {
				var ref<int<4>,f,f,plain> v363 = ref_var_init(*ptr_to_ref(*v354));
			}

			DEBUG opencl_sema.cpp:289 - OpenCL: checking lambdaExpr:
			DEBUG opencl_sema.cpp:290 - OpenCL: decl fun000 : (ref<ptr<int<4>>,f,f,plain>) -> unit;
			def fun000 = function (v391 : ref<ref<ptr<int<4>>,f,f,plain>,f,f,plain>) -> unit {
				var ref<int<4>,f,f,plain> v363 = ref_var_init(*ptr_to_ref(**v391));
			};
			fun000

			WARN  opencl_sema.cpp:282 - OpenCL: illegal OpenCL annotated fragment: {
				var ref<int<4>,f,f,plain> v363 = ref_var_init(*ptr_to_ref(**v391));
			}
			
			LOG(DEBUG) << "OpenCL: compoundStmt passed check for offloading";
			return true;
		}
		*/
		
		VariableRequirement::AccessMode tryDeduceAccessModeIntern(const core::TypePtr& type) {
			// @TODO: this is not a real impl!
			return VariableRequirement::AccessMode::RW;
		}
		
		VariableRequirement::AccessMode tryDeduceAccessMode(const core::TypePtr& type) {
			core::TypePtr ptr = type;
			if (core::lang::isPlainReference(ptr))
				// strip the ref<> away (legel due to the following check!)
				ptr = removeReference(ptr);
			// if the type is primitive at this point, we can return early.
			// this is legal as isOutlineAble returns true only if the input parameters
			// themselfs are not modified
			if (isPrimitive(ptr))
				return VariableRequirement::AccessMode::RO;
			// ok, hop into the recursion
			return tryDeduceAccessModeIntern(type);
		}
		
		VariableRequirementPtr tryDeduceVariableRequirement(core::NodeManager& manager, const core::CallExprPtr& callExpr, unsigned index) {
			core::IRBuilder builder(manager);
			
			core::ExpressionPtr arg = callExpr->getArgument(index);
			// the deduction is atm purely based on the type
			core::TypePtr type = getReferencedType(arg->getType());
			// at this point we try to deduce the correct requirement
			core::ExpressionPtr size;
			core::ExpressionPtr start;
			core::ExpressionPtr end;
			if (isPrimitive(type)) {
				size = builder.uintLit(1);
				start = builder.uintLit(0);
				end = size;
			} else if (core::lang::isFixedSizedArray(type)) {
				core::lang::ArrayType arrayType = core::lang::ArrayType(type);
				// element type must be primitive
				if (!isPrimitive(arrayType.getElementType()))
					return nullptr;
				
				unsigned numElements = arrayType.getNumElements();
				// at this point we assume full-range
				size = builder.uintLit(numElements);
				start = builder.uintLit(0);
				end = size;
			} else
				return nullptr;
			// right now the access mode is determined solely using the type
			VariableRequirement::AccessMode accessMode = tryDeduceAccessMode(arg->getType());
			// generate a temporary variable to hold the type inforation
			return std::make_shared<VariableRequirement>(core::Variable::get(manager, arg->getType()), size, start, end, accessMode);
		}
		
		// @TODO: an argument wih an incomplete type is not allowed!!!
		// @TODO: while to for extension is a problem if produces e.g. ref<ref<'a>...>
	}
	
	core::TypePtr getElementType(const core::TypePtr& type) {
		if (core::lang::isReference(type))
			return removeReference(type);
		else if (core::lang::isPointer(type))
			return removePointer(type);
		else {
			return type;
		}
	}
	
	core::TypePtr getReferencedType(const core::NodePtr& node) {
		auto expr = node.isa<core::ExpressionPtr>();
		assert_true(expr) << "node must be an expression";
		return getReferencedType(expr->getType());
	}
	
	core::TypePtr getReferencedType(const core::TypePtr& type) {
		if (core::lang::isPointer(type) || core::lang::isReference(type))
			return getReferencedType(getElementType(type));
		else
			return type;
	}
	
	bool isOutlineAble(core::NodeManager& manager, const core::NodePtr& node) {
		// if it is not a statement then we cannot outline it anyway
		core::StatementPtr stmt = node.isa<core::StatementPtr>();
		if (!stmt) return false;
	
		// however, we do not want expressions here
		if (auto expr = node.isa<core::ExpressionPtr>()) return false;
	
		// if the generic check fails, we do not need to proceed any further
		if (!core::analysis::isOutlineAble(stmt, false))
			return false;
		
		// obtain a list for all & only free variables
		const auto& free = core::analysis::getFreeVariables(manager.get(stmt));
		const auto& vars = core::analysis::getAllVariables(manager.get(stmt));
		for (const auto& cur : vars) {
			bool isParameter = std::find(free.begin(), free.end(), cur) != free.end();
			// parameters must meet additional constraints
			if (!isTrivial(cur->getType(), isParameter)) {
				LOG(DEBUG) << "not outlineable due to non triviality of " << (isParameter ? "parameter: " : ": ") << dumpColor(cur->getType());
				return false;
			}
		}

		/*
		@TODO: this check needs to be enabled once the readOnly check is fixed
		
		// most expensive check is done at last
		for (const auto& cur : free) {
			LOG(DEBUG) << "isReadOnly called with: " << dumpColor(stmt) << " for arg: " << dumpColor(cur);
		
			if (!core::analysis::isReadOnlyWithinScope(stmt, cur)) {
				LOG(DEBUG) << "not outlineable due to non-read-only of: " << dumpColor(cur);
				return false;
			}
		}
		*/
		return true;
	}
	
	VariableRequirementList getVariableRequirements(core::NodeManager& manager, const core::CallExprPtr& callExpr) {
		VariableRequirementList result;
		VariableRequirementList candidates;
		// all annotations are attached to the outlined lambda itself
		core::LambdaExprPtr lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		// grab all annotations which relate to OpenCL (and have been attached by FE)
		const auto& annos = lambdaExpr->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
		// loop through all of them and pick only those who correspond to variables
		for (const auto& anno : annos) {
			if (auto rq = std::dynamic_pointer_cast<VariableRequirement>(anno))
				candidates.push_back(rq);
		}
		// map candidates to actual vars
		for (unsigned index = 0; index < callExpr->getArgumentList().size(); ++index) {
			auto iter = candidates.end();
			core::ExpressionPtr arg = callExpr->getArgument(index);
			// check if argument is a variable in the first place
			if (arg->getNodeType() == core::NT_Variable)
				// excellent, we can try to locate the corresponding one in the annos
				iter = std::find_if(candidates.begin(), candidates.end(), [&](const VariableRequirementPtr& var) { return *var->getVar() == *arg; });
			// were we able to locate a user supplied anno?
			if (iter != candidates.end()) {
				result.push_back(*iter);
				continue;
			}
			LOG(DEBUG) << "trying to deduce requirements for: " << dumpColor(arg);
			// user did not supply any hints .. try to deduce them on our own
			auto req = tryDeduceVariableRequirement(manager, callExpr, index);
			if (!req) {
				assert_fail() << "cannot deduce requirement for: " << dumpColor(arg);
				// in case the assertion is a no-op return an empty result and our caller
				// can handle the error properly
				return {};
			}
			result.push_back(req);
		}
		return result;
	}
}
}
}
}

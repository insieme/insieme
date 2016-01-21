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

#include "insieme/annotations/meta_info/meta_infos.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/annotations/meta_info/meta_infos.h"
#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/backend/opencl/opencl_backend.h"
#include "insieme/backend/opencl/opencl_preprocessor.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/backend/opencl/opencl_entities.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {
namespace opencl {
	using namespace insieme::core;
	using namespace insieme::annotations::opencl;
	using namespace insieme::annotations::opencl_ns;
	using namespace insieme::utils::map;

	// @TODO: compound only does not work  --> does a for not have an own one?
	class Filter {
		NodeManager& nodeMan;
		const lang::BasicGenerator& basic;
		std::unordered_set<std::string> builtins;

	public:
		Filter(NodeManager& nodeMan) :
			nodeMan(nodeMan), basic(nodeMan.getLangBasic()),
			// builtins represents the intersection of cmath and opencl 1.0 builtin math functions
			builtins({"acos", "acosh", "asin", "asinh", "atan", "atan2",
					  "cbrt", "ceil", "copysign", "cos", "cosh", "erfc",
					  "erf", "exp", "exp2", "expm1", "fabs", "fdim",
					  "floor", "fma", "fmax", "fmin", "fmod", "frexp",
					  "hypot", "ilogb", "ldexp", "lgamma", "log", "log2",
					  "log10", "log1p", "logb", "modf", "nan", "nextafter",
					  "pow", "remainder", "remquo", "rint", "round", "sin",
					  "sqrt", "tan", "tanh", "tgamma", "trunc"})
		{ }
		/*
		ProgramPtr run(void) {
			insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;
			auto&& transformer = [&](const NodePtr& node) {
				if(auto anno = node->getAnnotation(BaseAnnotation::KEY)) {
					// a lambdaExpr does not modify the "IR-Tree" yet
					if(auto lambdaExpr = node.isa<LambdaExprPtr>()) handleLambdaExpr(anno, lambdaExpr);
					// if it is a compound, grab the replacements
					else if(auto compoundStmt = node.isa<CompoundStmtPtr>()) {
						auto outline = handleCompoundStmt(anno, compoundStmt);
						replacements.insert(std::make_pair(compoundStmt, outline));
					}
				}
			};
			visitDepthFirstOnce(prog, makeLambdaVisitor(transformer));
			return static_pointer_cast<const ProgramPtr>(transform::replaceAll(nodeMan, prog, replacements, transform::globalReplacement));
		}
		*/
		
		// return true if the given node satisfies the constraints of opencl offloading
		bool operator()(const NodePtr& node) const {
			if(auto anno = node->getAnnotation(BaseAnnotation::KEY)) {
				switch(node->getNodeType()) {
				// case core::NT_LambdaExpr: 	return checkLambdaExpr(anno, node.as<LambdaExprPtr>());
				case core::NT_CompoundStmt:	return checkCompoundStmt(anno, node.as<CompoundStmtPtr>());
				default:					return false;
				}
			}
			return false;
		}
		
	protected:
		bool isBuiltIn(const LiteralPtr& literal) const {
			return builtins.find(insieme::utils::demangle(literal->getStringValue())) != builtins.end();
		}
	
		bool checkReturnType(const LambdaExprPtr& lambdaExpr) const {
			auto funType = lambdaExpr->getFunctionType();
			auto retType = funType->getReturnType();
			// make sure that the return type equals 'void'
			if(basic.getUnit() != retType) {
				LOG(WARNING) << "OpenCL: expected void as return type but got type: " << dumpColor(retType);
				return false;
			}
			
			return true;
		}
		
		bool checkGlobal(const LiteralPtr& literal) const {
			// a global variable is always a reference
			if(!lang::isReference(literal)) return true;
			
			LOG(WARNING) << "OpenCL: illegal global variable " << dumpColor(literal) << " of type: " << dumpColor(literal->getType());
			return false;
		}
		
		bool checkExternal(const LiteralPtr& literal) const {
			if(lang::isBuiltIn(literal)) return true;
			if(lang::isDerived(literal)) return true;
		
			if(auto funType = literal->getType().isa<FunctionTypePtr>()) {
				// in this case we need to check if it is an opencl builtin
				if(!isBuiltIn(literal)) {
					LOG(WARNING) << "OpenCL: illegal external function: " << dumpColor(literal);
					return false;
				}
			}
			return true;
		}
		
		bool checkLiterals(const LambdaExprPtr& lambdaExpr) const {
			bool foundBadLiteral = false; 
			// check for the usage of global literals
			visitDepthFirstOnce(lambdaExpr->getBody(), [&](const LiteralPtr& literal) {
				foundBadLiteral |= !checkGlobal(literal);
				foundBadLiteral |= !checkExternal(literal);
			});
			return !foundBadLiteral;
		}
		
		bool checkRecursive(const LambdaExprPtr& lambdaExpr) const {
			// check for recursive calls which are not permitted by opencl
			if(lambdaExpr->isRecursive()) {
				LOG(WARNING) << "OpenCL: recursive lambdaExpr is not allowed";
				return false;
			}
			return true;
		}
		
		bool checkTrivial(const VariablePtr& var) const {
			// this nasty case of ptr casting is currently not handeled
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
			// check all variables which are located within the body
			auto type = var->getType();
			// if we hold a reference, get the element type instead
			if(lang::isReference(type)) type = lang::ReferenceType(type).getElementType();
			// each variable which is used within the body must be trivial
			if(!analysis::isTrivial(type)) {
				LOG(WARNING) << "OpenCL: illegal non-trivial variable " << dumpColor(var) << " of type: " << dumpColor(type);
				return false;
			}
			// check for special case: ptr<() -> unit,t,f>
			if (lang::isPointer(type)) {
				auto elementType = lang::PointerType(type).getElementType();
				if (elementType.isa<FunctionTypePtr>()) {
					LOG(WARNING) << "OpenCL: illegal function pointer variable " << dumpColor(var) << " of type: " << dumpColor(elementType);
					return false;
				}
			}
			return true;
		}
		
		bool checkVariables(const LambdaExprPtr& lambdaExpr) const {
			for(auto& var : analysis::getAllVariables(lambdaExpr->getBody())) {
				if(!checkTrivial(var)) return false;
			}
			return true;
		}
		
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
		
		bool checkRecord(const VariablePtr& var) const {
			auto varType = var->getType();
			// if we hold a reference, get the element type instead
			if(lang::isReference(varType)) varType = lang::ReferenceType(varType).getElementType();
			// if we hold a pointer, get the element type instead
			if(lang::isPointer(varType)) varType = lang::PointerType(varType).getElementType();
			// do we have a tagType now?
			auto tagType = varType.isa<TagTypePtr>();
			if(!tagType) return true;
			
			for(const auto& field : tagType->getRecord()->getFields()) {
				if(lang::isPointer(field->getType())) {
					LOG(WARNING) << "OpenCL: illegal pointer in argument record of type: " << dumpColor(field->getType());
					return false;
				}
			};
			return true;
		}

		bool checkParameters(const LambdaExprPtr& lambdaExpr) const {
			const vector<VariablePtr>& params = lambdaExpr->getLambda()->getParameters()->getElements();
			for(const auto& param : params) {
				if(!checkTrivial(param)) return false;
				if(!checkDoublePointer(param)) return false;
				if(!checkRecord(param)) return false;
			}
			return true;
		}
		
		bool checkCallExprs(const LambdaExprPtr& lambdaExpr) const {
			bool foundBadCall = false; 
			visitDepthFirstOnce(lambdaExpr->getBody(), [&](const CallExprPtr& callExpr) {
			});
			return !foundBadCall;
		}
		
		bool checkConflictingAnnotations(const LambdaExprPtr& lambdaExpr) const {
			bool foundBadAnno = false;
			visitDepthFirstOnce(lambdaExpr->getBody(), [&](const NodePtr& node) {
				/*if(auto anno = node->getAnnotation(omp::BaseAnnotation::KEY)) {
					foundBadAnno = true;
					LOG(WARNING) << "OpenCL: illegal OpenMP annotated fragment: " << dumpColor(node);
				}*//* else if(auto anno = node->getAnnotation(BaseAnnotation::KEY)) {
					foundBadAnno = true;
					LOG(WARNING) << "OpenCL: illegal OpenCL annotated fragment: " << dumpColor(node);
				}*/
			});
			return !foundBadAnno;
		}
		
		bool checkLambdaExpr(const BaseAnnotationPtr& anno, const LambdaExprPtr& lambdaExpr) const {
			LOG(DEBUG) << "OpenCL: checking lambdaExpr:";
			LOG(DEBUG) << "OpenCL: " << dumpColor(lambdaExpr);
			
			if(!checkReturnType(lambdaExpr)) return false;
			if(!checkRecursive(lambdaExpr)) return false;
			if(!checkParameters(lambdaExpr)) return false;
			if(!checkLiterals(lambdaExpr)) return false;
			if(!checkVariables(lambdaExpr)) return false;
			if(!checkCallExprs(lambdaExpr)) return false;
			if(!checkConflictingAnnotations(lambdaExpr)) return false;
						
			LOG(DEBUG) << "OpenCL: lambdaExpr passed check for offloading";
			return true;
		}
		
		bool checkCompoundStmt(const BaseAnnotationPtr& anno, const CompoundStmtPtr& compoundStmt) const {
			LOG(DEBUG) << "OpenCL: checking compoundStmt:";
			LOG(DEBUG) << "OpenCL: " << dumpColor(compoundStmt);
			// check if the compound is outlineAble, otherwise transform::outline will trigger an assertion			
			if(!analysis::isOutlineAble(compoundStmt, false)) return false;
			
			auto callExpr = transform::outline(nodeMan, compoundStmt);
			// @TODO: is is just a proof of concept
			if(!checkLambdaExpr(anno, callExpr->getFunctionExpr().as<LambdaExprPtr>())) return false;
			
			/*
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
			*/
			
			LOG(DEBUG) << "OpenCL: compoundStmt passed check for offloading";
			return true;
		}
	};

	// @TODO: put this into opencl_transform.cpp/h ?	
	class Transformer {
		IRBuilder& builder;
		NodeManager& manager;
		unsigned int uniqueId;
		OpenCLKernelBackendPtr backend;
		const OpenCLExtension& oclExt;
	public:
		Transformer(IRBuilder& builder, NodeManager& manager) : 
			builder(builder), manager(manager), uniqueId(0), 
			backend(OpenCLKernelBackend::getDefault()), oclExt(manager.getLangExtension<OpenCLExtension>())
		{ }
		
		LambdaExprPtr operator()(const LambdaExprPtr& lambdaExpr, const DataRequirementList& requirements) { 
			unsigned int id;
			StatementList body;
			// first of all, we need to register the kernel itself
			body.push_back(buildRegisterKernel(id, buildSource(lambdaExpr)));
			body.push_back(buildRegisterNDRange(id));
			for (const auto& requirement : requirements)
				body.push_back(buildRegisterDataRequirement(id, requirement));
			// now the body is populated with all required IR constructs, build the wrapper
			LambdaExprPtr wrapperExpr = buildWrapper(lambdaExpr->getParameterList(), body);
			// tag this lambda as OpenCL capable
			info_type meta_data;
			meta_data.opencl = true;
			meta_data.kernel_id = id;			
			wrapperExpr->attachValue(meta_data);
			return wrapperExpr;
		}
	private:
		CallExprPtr buildRegisterKernel(unsigned int& id, const LiteralPtr& literal) {
			// generate a new unique id for this kernel
			id = uniqueId++;
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getRegisterKernel(),
									builder.uintLit(id), lang::buildPtrFromArray(literal));
		}
		
		CallExprPtr buildRegisterNDRange(unsigned int id) {
			// @TODO: this must match the actual ND-Range of the kernel!
			// work_dim = 1, global_work_offset = NULL, global_work_size[0] set to 1, and local_work_size[0] set to 1.
			ExpressionList workSize;
			workSize.push_back(builder.uintLit(1));
			workSize.push_back(builder.uintLit(0));
			workSize.push_back(builder.uintLit(0));
			
			LiteralPtr workDim = builder.uintLit(1);
			// now build the actual register call
			return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getRegisterNDRange(),
									builder.uintLit(id), workDim,
									lang::buildArrayCreate(manager, workSize.size(), workSize),
									lang::buildArrayCreate(manager, workSize.size(), workSize));
		}
		
		CallExprPtr buildRegisterDataRequirement(unsigned int id, const DataRequirementPtr& requirement) {
			// use the default IRBuilder to generate the callExpr
			return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getRegisterDataRequirement(),
									builder.uintLit(id), DataRequirement::encode(manager, requirement));
		}
	
		LiteralPtr buildSource(const LambdaExprPtr& lambdaExpr) {
			TargetCodePtr target = backend->convert(lambdaExpr);
			return builder.stringLit(toString(*target));
		}
		
		LambdaExprPtr buildWrapper(const VariableList& params, const StatementList& body) {
			return builder.lambdaExpr(manager.getLangBasic().getUnit(), params, builder.compoundStmt(body));
		}
	};
	
	// @TODO: replace reserved names within the context of OpenCL .. SpirKeywordReplacer .. or something like that
	OffloadSupport::OffloadSupport() :
		PreProcessor()
	{ }

	NodePtr OffloadSupport::process(const Converter& converter, const NodePtr& node) {
		// node manager used by this extension
		NodeManager& manager = converter.getNodeManager();
		// instantiate a filter to guide the visitor
		Filter filter(manager);
		// used to transform IR to KernelIR
		IRBuilder builder(manager);
		Transformer transformer(builder, manager);
		// this map will be filled by the visitor
		PointerMap<NodePtr, NodePtr> replacements;
		// traverse through the tree and find nodes which are valid for offloading
		visitDepthFirstOnce(node, makeLambdaVisitor(filter, [&](const NodePtr& node) {
			// lets print a message to see if a node has passed our filter or not
			LOG(DEBUG) << "OpenCL: found offloading candiate ...";
			if (node->getNodeType() == core::NT_CompoundStmt) {
				// we outline the compound such that we can implement our pick between default & opencl kernel
				CallExprPtr callExpr = transform::outline(manager, node.as<CompoundStmtPtr>());
				// grab the lambdaExpr as the pick stores "function-pointer"-like objects
				LambdaExprPtr lambdaExpr = callExpr->getFunctionExpr().as<LambdaExprPtr>();
				// put together the offloadExpr
				DataRequirementList requirements;
				for (const auto& arg : callExpr->getArguments()) {
					DataRequirementPtr requirement = std::make_shared<DataRequirement>();
					requirement->setAccessMode(DataRequirement::AccessMode::RW);
					requirement->setType(arg->getType());
					// @TODO: this is hardcoded!
					requirement->addDim(1000);
					requirement->addRange(DataRange::get(manager, 0, 1000));
					requirements.push_back(requirement);
				}
				// put together the variants used by the pick
				ExpressionList variants;
				variants.push_back(lambdaExpr);
				variants.push_back(transformer(lambdaExpr, requirements));
				CallExprPtr pickExpr = builder.pickVariant(variants);
				// ... and call it to let the runtime take over
				replacements.insert(std::make_pair(node, builder.callExpr(manager.getLangBasic().getUnit(), pickExpr, callExpr->getArguments())));
			}
		}));
		
		// fast-path
		if(replacements.empty()) return node;
		// slow-path
		return transform::replaceAll(manager, node, replacements, core::transform::globalReplacement);
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme

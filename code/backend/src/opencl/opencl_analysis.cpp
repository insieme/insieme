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
#include "insieme/backend/opencl/opencl_extension.h"

#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/iterator_utils.h"

#include <set>

namespace insieme {
namespace backend {
namespace opencl {
namespace analysis {

	using namespace insieme::annotations::opencl;

	namespace {
		core::TypePtr removeReference(const core::TypePtr& node) {
			if (core::lang::isReference(node))
				return core::lang::ReferenceType(node).getElementType();
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

		bool hasMultipleIndirections(core::TypePtr type) {
			// if we hold a reference, get the element type instead
			if (core::lang::isReference(type)) {
				type = core::lang::ReferenceType(type).getElementType();
				// if we hold a pointer, get the element type instead
				if (core::lang::isPointer(type)) {
					type = core::lang::PointerType(type).getElementType();
					// at this point, it is not legal to hold a pointer (OpenCL does not support it!)
					if (core::lang::isPointer(type)) return true;
				}
			}
			return false;
		}

		bool isTrivial(const core::TypePtr& type, bool isParameter = false) {
			// if we hold a reference, get the element type instead
			if (core::lang::isReference(type)) {
				// allow undefined references along plain ones
				if (core::lang::isCppReference(type) || core::lang::isCppRValueReference(type))
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

		bool tryDeduceIndependence(const core::StatementPtr& stmt) {
			LOG(WARNING) << "pessimistic independence assumption of: " << dumpColor(stmt);
			return false;
		}

		VariableRequirement::AccessMode tryDeduceAccessModeIntern(const core::TypePtr& type) {
			LOG(WARNING) << "pessimistic access mode assumption of: " << dumpColor(type);
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

		VariableRequirementPtr tryDeduceVariableRequirement(core::NodeManager& manager, const core::NodePtr& code,
															const core::VariablePtr& var) {
			core::IRBuilder builder(manager);

			// the deduction is atm purely based on the type
			auto type = getReferencedType(var->getType());
			// at this point we try to deduce the correct requirement
			core::ExpressionList size;
			core::ExpressionList start;
			core::ExpressionList end;
			if (core::lang::isPlainReference(var->getType()) && core::lang::isPointer(getElementType(var->getType()))) {
				// build the literal which represents malloc only once
				auto declMalloc = builder.literal(insieme::utils::mangle("malloc"), builder.parseType("(uint<8>) -> ptr<unit>"));

				bool success = false;
				auto& ptrExt = manager.getLangExtension<core::lang::PointerExtension>();
				// in this we try to find the following:
				// var ref<ptr<real<4>>,f,f,plain> v66 = ptr_reinterpret(IMP_malloc(
				//     sizeof(type_lit(real<4>))*num_cast(*v55, type_lit(uint<8>))), type_lit(real<4>));
				core::visitDepthFirstOnceInterruptible(code, [&](const core::DeclarationStmtPtr& declStmt) {
					// if this is not the variable we are seraching for continue at another level
					if (*(declStmt->getVariable()) != *var) return false;

					core::ExpressionPtr init = declStmt->getInitialization();
					if (!core::analysis::isCallOf(init, ptrExt.getPtrReinterpret())) return true;

					// obtain the first argument of the cast
					auto castee = core::analysis::getArgument(init, 0);
					if (!core::analysis::isCallOf(castee, declMalloc)) return true;

					// transform::outline will use our analysis module to pass any
					// required variables into the lambda later on!
					size.push_back(builder.div(core::analysis::getArgument(castee, 0),
						builder.callExpr(manager.getLangBasic().getSizeof(), builder.getTypeLiteral(getUnderlyingType(type)))));
					start.push_back(builder.uintLit(0));
					end.push_back(size.back());
					// mark this as success
					success = true;
					return true;
				});

				if (!success) return nullptr;
			} else if (isPrimitive(type)) {
				size.push_back(builder.uintLit(1));
				start.push_back(builder.uintLit(0));
				end.push_back(size.back());
			} else if (core::lang::isFixedSizedArray(type)) {
				// recurse as long as it stays a fixed size one
				do {
					core::lang::ArrayType arrayType = core::lang::ArrayType(type);
					unsigned numElements = arrayType.getNumElements();
					// at this point we assume full-range
					size.push_back(builder.uintLit(numElements));
					start.push_back(builder.uintLit(0));
					end.push_back(size.back());
					// hope one level deeper
					type = arrayType.getElementType();
				} while (core::lang::isFixedSizedArray(type));

				// the last inner-most element type must be primitive
				if (!isPrimitive(type)) return nullptr;
			} else
				return nullptr;
			// right now the access mode is determined solely using the type
			auto accessMode = tryDeduceAccessMode(var->getType());
			// generate a temporary variable to hold the type inforation
			VariableRangeList ranges;
			for (unsigned i = 0; i < size.size(); ++i)
				ranges.push_back(std::make_shared<VariableRange>(size[i], start[i], end[i]));
			return std::make_shared<VariableRequirement>(builder.variable(var->getType()), accessMode, ranges);
		}

		bool isOffloadWorth(const core::NodePtr& node) {
			// if it is not a statement then we cannot outline it anyway
			if (node->getNodeCategory() != core::NC_Statement) return false;

			std::vector<core::NodeAddress> candidates;
			core::visitBreadthFirst(core::NodeAddress(node), [&](const core::StatementAddress& addr) {
				// assure that we do not face an expression
				auto stmt = addr.getAddressedNode();
				if (stmt->getNodeCategory() != core::NC_Statement) return;

				// now check of independence
				if (!isIndependentStmt(stmt)) return;
				// @TODO: implement e.g. heuristic if the independent stmts are worth it
				candidates.push_back(addr);
			});
			return candidates.size() > 0;
		}

		core::VariableList getFreeVariables(core::NodeManager& manager, const VariableRequirementPtr& requirement) {
			// allocate a derived instance manager as we do not want to pollute the original one!
			core::NodeManager derived(manager);
			core::IRBuilder builder(derived);
			// use a little trick to skip the merge of lists
			core::StatementList body;
			for (const auto& range : requirement->getRanges()) {
				body.push_back(range->getSize());
				body.push_back(range->getStart());
				body.push_back(range->getEnd());
			}
			return core::analysis::getFreeVariables(derived.get(builder.compoundStmt(body)));
		}

		DeviceAnnotationPtr tryDeduceDeviceInfo(const core::StatementPtr& stmt) {
			// assume that it works _well_ on all types of devices
			// thus taking the optimistic approach here instead of other tryXxx fallbacks
			return std::make_shared<DeviceAnnotation>(std::make_shared<Device>(Device::Type::ALL));
		}
	}

	bool isPrimitive(const core::NodePtr& node) {
		if (node->getNodeCategory() != core::NC_Type) return false;
		return isPrimitive(node.as<core::TypePtr>());
	}

	bool isPrimitive(const core::TypePtr& type) {
		// return type->getNodeManager().getLangBasic().isPrimitive(type);
		auto& basic = type->getNodeManager().getLangBasic();
		return basic.isChar(type) || basic.isBool(type) || basic.isScalarType(type);
	}

	core::TypePtr getElementType(const core::TypePtr& type) {
		if (core::lang::isReference(type))
			return core::lang::ReferenceType(type).getElementType();
		else if (core::lang::isPointer(type))
			return core::lang::PointerType(type).getElementType();
		else if (core::lang::isArray(type))
			return core::lang::ArrayType(type).getElementType();
		else if (isKernelType(type)) {
			return KernelType(type).getElementType();
		} else {
			return type;
		}
	}

	core::TypePtr getUnderlyingType(const core::TypePtr& type) {
		core::TypePtr newType = getElementType(type);
		// if we point to the same memory location getElementType returned the identity
		if (newType == type)
			return newType;
		else
			return getUnderlyingType(newType);
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

	bool isReadOnly(const core::StatementPtr& stmt, const core::VariablePtr& var) {
		if (core::lang::isReference(var->getType())) {
			auto type = core::lang::ReferenceType(var->getType());
			if (type.isConst()) return true;
			// otherwise fall through
		}

		return core::analysis::isReadOnly(stmt, var);
	}

	bool isOffloadAble(core::NodeManager& manager, const core::NodePtr& node) {
		// if it is not a statement then we cannot outline it anyway
		core::StatementPtr stmt = node.isa<core::StatementPtr>();
		if (!stmt) return false;

		// however, we do not want expressions here
		if (auto expr = node.isa<core::ExpressionPtr>()) return false;

		// if the generic check fails, we do not need to proceed any further
		if (!core::analysis::isOutlineAble(stmt, false))
			return false;

		// check if it contain builtin literals only
		if (!hasBuiltInLiterals(node)) return false;

		// obtain a list for all & only free variables
		core::VariableSet vars = core::analysis::getAllVariables(manager.get(stmt));
		// this set can be ro as we do not need to modify it -- the latter does not hold for vars tho
		const core::VariableList& free = core::analysis::getFreeVariables(manager.get(stmt));
		for (const auto& cur : vars) {
			bool isParameter = std::find(free.begin(), free.end(), cur) != free.end();
			// parameters must meet additional constraints
			if (!isTrivial(cur->getType(), isParameter)) {
				LOG(ERROR) << "not outlineable due to non triviality of " << (isParameter ? "parameter: " : ": ") << dumpColor(cur->getType());
				return false;
			}
		}

		// re-use the variable list
		vars.clear();
		// most expensive check is done at last
		for (const auto& cur : free) {
			core::TypePtr type = cur->getType();
			LOG(DEBUG) << "checking " << dumpColor(cur) << " of type: " << dumpColor(type);
			// in case is it non a refType it is readOnly anyway
			if (!core::lang::isReference(type)) continue;
			// take a look at the enclosed type
			type = getElementType(type);
			// iff it is an array, is is readOnly as well (modeled as 'a *const in backend)
			if (core::lang::isArray(type)) continue;
			// in case of a primitive, the standard check can be used
			if (isPrimitive(type)) {
				// fail if it is not!
				if (opencl::analysis::isReadOnly(stmt, cur)) continue;
				// whooops!
				LOG(ERROR) << "not outlineable due to non-read-only of: " << dumpColor(cur);
				return false;
			}

			// grab all alias names as we need to check them later on
			auto names = core::analysis::getVariableNames(cur, stmt);
			vars.insert(names.begin(), names.end());
			vars.insert(cur);
		}
		// in case vec is empty everything is fine already
		if (vars.empty()) return true;
		// in this case a more complicated check is required
		bool fail = false;
		for (const auto& cur : vars) {
			// if it is read-only straight along we can skip further checks
			if (opencl::analysis::isReadOnly(stmt, cur)) continue;

			fail = true;
			LOG(WARNING) << "pessimistic read-only assumption of: " << dumpColor(cur) << " with type: " << dumpColor(cur->getType());
		}
		if (fail) { LOG(DEBUG) << "offending IR fragment: " << dumpColor(stmt); return false; }
		return true;
	}

	VariableRequirementList getVariableRequirements(core::NodeManager& manager, const core::NodePtr& code,
													const core::StatementPtr& stmt) {
		VariableRequirementList result;
		VariableRequirementList candidates;
		// all annotations, if present, must be attached to the given stmt
		if (stmt->hasAnnotation(BaseAnnotation::KEY)) {
			// grab all annotations which relate to OpenCL (and have been attached by FE)
			const auto& annos = stmt->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
			// loop through all of them and pick only those who correspond to variables
			for (const auto& anno : annos) {
				if (auto rq = std::dynamic_pointer_cast<VariableRequirement>(anno))
					candidates.push_back(rq);
			}
		}

		core::VariableList free = core::analysis::getFreeVariables(manager.get(stmt));
		for (const auto& cur : free) {
			auto iter = std::find_if(candidates.begin(), candidates.end(),
				[&](const VariableRequirementPtr& var) {
					return *var->getVar() == *cur;
				});
			// were we able to locate a user supplied anno?
			if (iter != candidates.end()) {
				result.push_back(*iter);
				continue;
			}
			LOG(DEBUG) << "trying to deduce requirements for: " << dumpColor(cur);
			// user did not supply any hints .. try to deduce them on our own
			auto req = tryDeduceVariableRequirement(manager, code, cur);
			if (!req) {
				assert_fail() << "cannot deduce requirement for: " << dumpColor(cur);
				// in case the assertion is a no-op return an empty result and our caller
				// can handle the error properly
				return {};
			}
			result.push_back(req);
		}
		return result;
	}

	bool isVariableRequirementOf(const core::LambdaExprPtr& lambdaExpr, const VariableRequirementList& requirements) {
		const auto& params = lambdaExpr->getParameterList();
		for (const auto& requirement : requirements) {
			// check if each free variable is coped by the lambdaExpr
			for (const auto& free : getFreeVariables(lambdaExpr->getNodeManager(), requirement)) {
				auto it = std::find_if(params.begin(), params.end(), [&](const core::VariablePtr& var) { return *var == *free; });
				if (it == params.end()) return false;
			}
		}
		return true;
	}

	core::VariableList getFreeVariables(core::NodeManager& manager, const core::StatementPtr& stmt,
										const VariableRequirementList& requirements) {
			// obtain a list of free variables within stmt
			core::VariableList free = core::analysis::getFreeVariables(manager.get(stmt));
			// sort to obtain stable results for the essential requirements
			std::sort(free.begin(), free.end(), compare_target<core::VariablePtr>());
			// interate over all requirements to check what else we might need
			for (const auto& cur : requirements) {
				auto vars = getFreeVariables(manager, cur);
				// unite them with the essentials
				std::copy_if(vars.begin(), vars.end(), std::back_inserter(free),
					[&](const core::VariablePtr& candidate) {
						auto it = std::find_if(free.begin(), free.end(),
							[&](const core::VariablePtr& element) {
								return *element == *candidate;
							});
						return it == free.end();
					});
			}
			return free;
		}

	bool isIndependentStmt(const core::StatementPtr& stmt) {
		// if it is not a for statement it is per-design not independent -- however this may be changed in the future!
		if (stmt->getNodeType() != core::NT_ForStmt) return false;
		// if annotations are attached check them first
		if (stmt->hasAnnotation(BaseAnnotation::KEY)) {
			const auto& annos = stmt->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
			// the first annotation wins the rest is ignored
			for (const auto& anno : annos) {
				if (auto rq = std::dynamic_pointer_cast<LoopAnnotation>(anno))
					return rq->getIndependent();
			}
		}
		// in this case we have to deduce it
		LOG(DEBUG) << "trying to deduce independence of: " << dumpColor(stmt);
		return tryDeduceIndependence(stmt);
	}

	core::NodeList getOffloadAbleStmts(const core::NodePtr& node) {
		core::NodeList result;
		core::NodeManager& manager = node->getNodeManager();
		// instantiate a filter to guide the visitor
		auto filter = [&](const core::NodePtr& node) {
			if (node->getNodeCategory() != core::NC_Statement) return false;

			if (auto anno = node->getAnnotation(BaseAnnotation::KEY)) {
				// first of all it must meet the requirements of OpenCL
				if (!isOffloadAble(manager, node)) return false;
				// secondly it must be worth it
				return isOffloadWorth(node);
			}
			return false;
		};
		// traverse through the tree and find nodes which are valid for offloading
		core::visitDepthFirstOncePrunable(node, core::makeLambdaVisitor(filter, [&](const core::NodePtr& node) { result.push_back(node); return true; }));
		return result;
	}

	core::TypePtr getLWDataItemType(const core::LambdaExprPtr& lambdaExpr) {
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		return runtime::DataItem::toLWDataItemType(builder.tupleType(lambdaExpr->getType().as<core::FunctionTypePtr>().getParameterTypeList()));
	}

	DeviceAnnotationPtr getDeviceInfo(const core::StatementPtr& stmt) {
		// all annotations, if present, must be attached to the given stmt
		if (stmt->hasAnnotation(BaseAnnotation::KEY)) {
			// grab all annotations which relate to OpenCL (and have been attached by FE)
			const auto& annos = stmt->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
			// loop through all of them and pick only those who correspond to variables
			for (const auto& anno : annos) {
				if (auto da = std::dynamic_pointer_cast<DeviceAnnotation>(anno))
					return da;
			}
		}

		LOG(DEBUG) << "trying to deduce device info for: " << dumpColor(stmt);
		return tryDeduceDeviceInfo(stmt);
	}

	namespace {
		class DependencyGraphBuilder {
			core::NodePtr root;
			DependencyGraph& graph;
			std::set<core::NodeAddress> all;
			const core::lang::ReferenceExtension& refExt;
		public:
			DependencyGraphBuilder(const core::NodePtr& root, DependencyGraph& graph) :
				root(root), graph(graph), all(),
				refExt(root->getNodeManager().getLangExtension<core::lang::ReferenceExtension>())
			{ }

			void visit(const core::NodePtr& node) {
				static bool debug = true;

				// find all addresses using bfs
				auto addrs = core::Address<const core::Node>::findAll(node, root, false);
				if (debug) std::cout << "found " << addrs.size() << " addresses referencing: " << dumpColor(node);
				// iterate over each occurance of the given variable within the stmt
				for (const core::NodeAddress& addr : addrs) {
					// explicitly add the source
					graph.addVertex(addr.getAddressedNode());

					if (debug) std::cout << "visit vertex: " << dumpColor(addr.getAddressedNode());

					core::NodeAddress source = addr;
					core::visitPathBottomUpInterruptible(addr.getParentAddress(), [&](const core::NodeAddress& sink) {
						if (!all.insert(sink).second) return true;
						if (debug) std::cout << "visit sink: " << dumpColor(sink.getAddressedNode());

						auto node = sink.getAddressedNode();
						if (node->getNodeType() != core::NT_CallExpr) return true;

						auto call = node.as<core::CallExprPtr>();
						if (core::analysis::isCallOf(call, refExt.getRefAssign())) {
							// add a dependency to the variable and recurse
							auto arg = core::analysis::getArgument(call, 0);
							graph.addEdge(source, arg);

							if (debug) std::cout << "ref_assign: " << dumpColor(arg);

							// remove all derefs
							while (core::analysis::isCallOf(arg, refExt.getRefDeref()))
								arg = core::analysis::getArgument(arg, 0);

							// hop into the recursion
							visit(arg);
							return true;
						} else if (core::analysis::isCallOf(call, refExt.getRefDeref())) {
							if (debug) std::cout << "ref_deref: " << dumpColor(call);

							// in this case we ignore it as *v3 is the same dependency as v3
							// return false;
						}
						// add a general dependency iff source and sink differ
						if (source != sink) {
							if (debug) std::cout << "add edge" << std::endl;
							graph.addEdge(source.getAddressedNode(), sink.getAddressedNode());
						}
						// setup the source for the next round
						source = sink;
						return false;
					});
				}
			}
		};
	}

	DependencyGraph& getDependencyGraph(const core::StatementPtr& stmt, const core::VariableSet& vars, DependencyGraph& base) {
		DependencyGraphBuilder builder(stmt, base);
		for (const core::VariablePtr& var : vars) builder.visit(var);
		return base;
	}

	DependencyGraph& getDependencyGraph(const core::CallExprPtr& callExpr, core::NodeMap& mapping, DependencyGraph& base) {
		// 1. map input vars to actual parameters
		auto lambdaExpr = callExpr->getFunctionExpr().isa<core::LambdaExprPtr>();
		if (!lambdaExpr) return base;

		core::VariableSet vars;
		for_each(make_paired_range(callExpr->getArgumentList(), lambdaExpr->getLambda()->getParameters()),
		[&](const std::pair<const core::ExpressionPtr, const core::VariablePtr>& pair) {
			// note vars for later
			vars.insert(pair.second);
			// generic mapping from input args to output args
			mapping[pair.first] = pair.second;
		});
		// nothing more we can do here .. we ignore internals
		if (core::lang::isBuiltIn(callExpr) || core::lang::isDerived(callExpr)) return base;
		// 2. foreach parameter build the dependency subgraph
		return getDependencyGraph(lambdaExpr->getBody(), vars, base);
	}
}
}
}
}

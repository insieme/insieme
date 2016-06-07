/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/addons/static_variables.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/preprocessor.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace addons {

	namespace {

		class StaticVarBackendExtension : public core::lang::Extension {
			/**
			 * Allow the node manager to create instances of this class.
			 */
			friend class core::NodeManager;

			/**
			 * Creates a new instance based on the given node manager.
			 */
			StaticVarBackendExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

		  public:

			// import reference extension to utilize aliases
			IMPORT_MODULE(core::lang::ReferenceExtension);

			/**
			 * A literal masking the initialization of a static literal using constants.
			 */
			LANG_EXT_LITERAL(InitStaticConst, "BE_InitStaticConst", "('a, 'b)->ref<'a>");

			/**
			 * A literal masking the initialization of a static literal using lazy expressions.
			 */
			LANG_EXT_LITERAL(InitStaticLazy, "BE_InitStaticLazy", "(()=>'a, 'b)->ref<'a>");
		};

		// a marker for annotation for globals only only initialized once
		struct SingleStaticInitMarker {};


		// a pre-processor marking all static variables only used once
		class StaticInitVisitor : public PreProcessor {
			virtual core::NodePtr process(const Converter& converter, const core::NodePtr& code) {
				core::NodeManager& mgr = code->getNodeManager();
				auto& ext = mgr.getLangExtension<core::lang::StaticVariableExtension>();

				std::map<core::LiteralPtr, std::set<core::CallExprPtr>> inits;
				auto collector = [&](const core::CallExprPtr& call) {
					// only interested in init-static-lazy calls
					if(core::analysis::isCallOf(call, ext.getInitStaticLazy())) { inits[core::transform::extractInitExprFromDecl(call[0]).as<core::LiteralPtr>()].insert(call); }
				};

				// search all InitStaticLazy calls
				core::visitDepthFirstOnce(code, collector);

				// mark init calls that are unique
				for(const auto& cur : inits) {
					if(cur.second.size() == 1u) { cur.first->attachValue<SingleStaticInitMarker>(); }
				}

				// that's it
				return code;
			}
		};


		OperatorConverterTable getStaticVariableOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;

			const auto& ext = manager.getLangExtension<core::lang::StaticVariableExtension>();
			const auto& ext2 = manager.getLangExtension<StaticVarBackendExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getCreateStatic()] = OP_CONVERTER {
				return nullptr; // no instruction required at this point
			};


			// -------------------- Constant initialization -----------------------

			res[ext.getInitStaticConst()] = OP_CONVERTER {

				// we have to build a new function for this init call containing a static variable
				//
				//		A* initXY(lazy) {
				//			static A a = lazy();
				//			return &a;
				//		}
				//

				// ... and since the construction of a function and all its dependencies is anoying
				// in the C-AST we create a new IR function including an artificial construct
				// InitStatic that only covers the magic part ...

				auto fun = call->getFunctionExpr().as<core::LambdaExprPtr>();
				auto retType = call->getType();

				LOG(DEBUG) << "ext.getInitStaticConst() retType" << *retType << "\n";

				auto& mgr = NODE_MANAGER;
				auto& ext = mgr.getLangExtension<StaticVarBackendExtension>();
				core::IRBuilder builder(mgr);

				auto init = builder.callExpr(builder.refType(call[1]->getType()), ext.getInitStaticConst(), toVector(call[1], call[0]));
				auto lambda = builder.lambdaExpr(retType, core::VariableList(), init);

				// this function call is equivalent to a call to the new artificial lambda
				auto ret = builder.callExpr(retType, lambda, core::ExpressionList());
				LOG(DEBUG) << "ISC: " << dumpColor(ret) << " : " << dumpColor(ret->getType()) << "\n";
				return CONVERT_EXPR(ret);
			};

			res[ext2.getInitStaticConst()] = OP_CONVERTER {
				// a call to this is translated to the following:
				//
				//			static A a = lazy();
				//			return &a;
				//
				// where A is the type of the resulting object.
				auto A = core::lang::ReferenceType(call).getElementType();
				LOG(DEBUG) << "ext2.getInitStaticConst() A:\n" << *A << "\n";

				// get meta-type
				auto& infoA = GET_TYPE_INFO(A);

				// add dependency to definition
				context.addDependency(infoA.definition);

				// create variable
				auto var = C_NODE_MANAGER->create<c_ast::Variable>(infoA.lValueType, C_NODE_MANAGER->create("a"));

				// create init value
				auto& mgr = NODE_MANAGER;
				core::IRBuilder builder(mgr);
				c_ast::ExpressionPtr init;

				if(call[0] != builder.getZero(A)) {
					auto initValue = ARG(0);

					// Let's avoid a cast in static variable initializations when dealing with string literals
					// Char vectors are wrapped into structs by Insieme
					if(core::lang::isFixedSizedArray(initValue)
					   && mgr.getLangBasic().isChar(core::lang::ArrayType(initValue).getElementType())) {
						const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(initValue->getType());
						c_ast::InitializerPtr structInit =
						    c_ast::init(C_NODE_MANAGER->create<c_ast::StructType>(static_pointer_cast<c_ast::NamedType>(info.lValueType)->name));
						assert_true(core::analysis::isCallOf(initValue, mgr.getLangExtension<core::lang::ReferenceExtension>().getRefDeref()));
						structInit->values.push_back(CONVERT_EXPR(core::analysis::getArgument(initValue, 0)));
						return structInit;
					}

					// convert the init expression
					init = CONVERT_EXPR(core::transform::extractInitExprFromDecl(call[0]));
				}

				// built the static initialization
				auto decl = C_NODE_MANAGER->create<c_ast::VarDecl>(var, init);
				decl->isStatic = true;

				// build return statement
				auto ret = C_NODE_MANAGER->create<c_ast::Return>(c_ast::ref(var));

				// build compound
				auto comp = C_NODE_MANAGER->create<c_ast::Compound>(toVector<c_ast::NodePtr>(decl, ret));

				// done
				return C_NODE_MANAGER->create<c_ast::StmtExpr>(comp);
			};

			// ---------------- lazy initialization -------------------------

			res[ext.getInitStaticLazy()] = OP_CONVERTER {
				LOG(DEBUG) << "--------------------------\n------ ext.getInitStaticLazy()\n";
				LOG(DEBUG) << "call type: " << call->getType() << "\n";

				// we have to build a new function for this init call containing a static variable
				//
				//		A* initXY(lazy) {
				//			static A a = lazy();
				//			return &a;
				//		}
				//

				// ... and since the construction of a function and all its dependencies is anoying
				// in the C-AST we create a new IR function including an artificial construct
				// InitStatic that only covers the magic part ...

				auto fun = call->getFunctionExpr().as<core::LambdaExprPtr>();
				// auto retType = fun->getFunctionType()->getReturnType();
				auto retType = call->getType();

				auto& mgr = NODE_MANAGER;
				auto& ext = mgr.getLangExtension<StaticVarBackendExtension>();
				core::IRBuilder builder(mgr);

				// special case for static variables not depending on free variables and is a single static init marker
				// TODO: if this ever causes a problem that only closed init values may be used fix it here
				if(!core::analysis::hasFreeVariables(ARG(1)) && ARG(0)->hasAttachedValue<SingleStaticInitMarker>()) {
					// use a constant initialization by inlining the bind
					auto value = core::transform::evalLazy(mgr, ARG(1));
					LOG(DEBUG) << "value:\n" << dumpColor(ARG(1)) << "\n value type: " << dumpColor(ARG(1)->getType()) << "\n";
					LOG(DEBUG) << "value TEXT:\n" << dumpText(ARG(1)) << "\n";

					auto init = builder.callExpr(retType, ext.getInitStaticConst(), value, core::transform::extractInitExprFromDecl(call[0]));
					LOG(DEBUG) << ">>>>>>>INIT: \n" << dumpPretty(init) << " : " << init->getType() << "\n";
					auto lambda = builder.lambdaExpr(retType, core::VariableList(), init);
					LOG(DEBUG) << ">>>>>>>LAMBDA: \n" << dumpPretty(lambda) << " : " << lambda->getType() << "\n";

					// this function call is equivalent to a call to the new artificial lambda
					auto rcall = builder.callExpr(retType, lambda);
					LOG(DEBUG) << "ext.getInitStaticLazy() BUILT1:\n" << *rcall << "\n";
					return CONVERT_EXPR(rcall);
				}

				auto param = fun->getParameterList()[1];
				param = core::transform::replaceAllGen(mgr, param, {{builder.typeVariable("a"), core::analysis::getReferencedType(retType)}});

				auto init = builder.callExpr(builder.refType(builder.deref(param)->getType()), ext.getInitStaticLazy(), builder.deref(param),
					                         core::transform::extractInitExprFromDecl(call[0]));
				LOG(DEBUG) << ">>>>>>>INIT: \n" << dumpPretty(init) << " : " << init->getType() << "\n";
				auto newFunTy = builder.functionType(param->getType(), init->getType());
				LOG(DEBUG) << ">>>>NEWFT: \n" << dumpPretty(newFunTy) << "\n";
				auto lambda = builder.lambdaExpr(newFunTy->getReturnType(), toVector(param), init);
				LOG(DEBUG) << ">>>>>>>LAMBDA: \n" << dumpPretty(lambda) << " : " << lambda->getType() << "\n";

				// this function call is equivalent to a call to the new artificial lambda
				auto rcall = builder.callExpr(newFunTy->getReturnType(), lambda, core::transform::extractInitExprFromDecl(call[1]));
				LOG(DEBUG) << "ext.getInitStaticLazy() BUILT2:\n" << *rcall << "\n";
				return CONVERT_EXPR(rcall);
			};

			res[ext2.getInitStaticLazy()] = OP_CONVERTER {
				// a call to this is translated to the following:
				//
				//			static A a = lazy();
				//			return &a;
				//
				// where A is the type of the resulting object.

				auto A = core::lang::ReferenceType(call).getElementType();

				// get meta-type
				auto& infoA = GET_TYPE_INFO(A);

				// add dependency to definition
				context.addDependency(infoA.definition);

				// create variable
				auto var = C_NODE_MANAGER->create<c_ast::Variable>(infoA.lValueType, C_NODE_MANAGER->create("a"));

				// create init value
				auto& mgr = NODE_MANAGER;
				core::IRBuilder builder(mgr);
				auto init = CONVERT_EXPR(builder.callExpr(core::transform::extractInitExprFromDecl(call[0]), toVector<core::ExpressionPtr>()));

				// built the static initialization
				auto decl = C_NODE_MANAGER->create<c_ast::VarDecl>(var, init);
				decl->isStatic = true;

				// build return statement
				auto ret = C_NODE_MANAGER->create<c_ast::Return>(c_ast::ref(var));

				// build compound
				auto comp = C_NODE_MANAGER->create<c_ast::Compound>(toVector<c_ast::NodePtr>(decl, ret));

				// done
				return C_NODE_MANAGER->create<c_ast::StmtExpr>(comp);
			};

			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}

	} // namespace

	void StaticVariables::installOn(Converter& converter) const {
		// install pre-processor
		converter.setPreProcessor(makePreProcessorSequence(converter.getPreProcessor(), makePreProcessor<StaticInitVisitor>()));

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getStaticVariableOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

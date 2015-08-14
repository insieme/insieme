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

#include "insieme/frontend/extensions/frontend_cleanup_extension.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/pattern/rule.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/annotations/c/decl_only.h"
#include "insieme/annotations/c/include.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/tu/ir_translation_unit.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/frontend/tu/ir_translation_unit_io.h"

#include "insieme/annotations/data_annotations.h"

#include "insieme/utils/assert.h"

#include "insieme/frontend/omp/omp_annotation.h"

#include <functional>

namespace insieme {
namespace frontend {
namespace extensions {

	namespace {


		/**
		 * here the little trick, the translation unit is converted into a single expression to guarantee the
		 * the visiting of every single little node
		 */
		insieme::frontend::tu::IRTranslationUnit applyCleanup(insieme::frontend::tu::IRTranslationUnit& tu,
		                                                      std::function<core::NodePtr(const core::NodePtr&)> pass) {
			core::ExpressionPtr singlenode = insieme::frontend::tu::toIR(tu.getNodeManager(), tu);
			assert_true(singlenode) << "empty tu?";
			singlenode = pass(singlenode).as<core::ExpressionPtr>();

			return insieme::frontend::tu::fromIR(singlenode);
		}
		
		//////////////////////////////////////////////////////////////////////
		// Support alloca call
		// ==============
		//
		// This step converts alloca calls to var calls which are semantically the same
		insieme::core::NodePtr allocaCall(const insieme::core::NodePtr& prog) {
			using namespace core::pattern;
			namespace p = core::pattern::irp;
			namespace g = core::pattern::generator::irg;

			//auto& base = prog->getNodeManager().getLangBasic();

			//// get a few function symbols
			//auto reinterpret = base.getRefReinterpret();
			//auto alloca = p::literal("__builtin_alloca");
			//auto var = base.getRefVar();
			//auto createArray = base.getArrayCreate1D();

			//// build a transformation rule

			//Variable resType;
			//Variable elementType;

			//TreeGenerator gen = elementType;
			//Variable arrayType = Variable(p::arrayType(elementType));
			//Variable arrayTypeLiteral = Variable(p::typeLiteral(arrayType));
			//Variable size;
			//auto rule = Rule(p::callExpr(resType, reinterpret, p::callExpr(alloca, p::binaryOp(size, any)) << arrayTypeLiteral),
			//                 g::callExpr(resType, var, g::callExpr(arrayType, createArray, g::typeLiteral(elementType), size)));

			//// apply the transformation
			//auto allocaReplacer = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node) -> core::NodePtr {
			//	// apply rule on local node
			//	auto mod = rule(node);
			//	return (mod) ? mod : node;
			//});
			//return allocaReplacer.map(prog);
			return prog;
		}

		//////////////////////////////////////////////////////////////////////
		// Superfluous code
		// ================
		//
		// remove empty constructs from functions bodies, this prevents multiple implementations of the same
		// equivalent code produced in different translation units with different macro expansions
		// this removes empty compounds and nested compounds with single elements
		insieme::core::NodePtr superfluousCode(const insieme::core::NodePtr& prog) {
			core::IRBuilder builder(prog->getNodeManager());
			auto noop = builder.getNoOp();
			auto unitExpr = builder.getLangBasic().getUnitConstant();

			auto clean = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node) -> core::NodePtr {
				if(core::CompoundStmtPtr cmpnd = node.isa<core::CompoundStmtPtr>()) {
					vector<core::StatementPtr> stmtList;
					for(core::StatementPtr stmt : cmpnd->getStatements()) {
						// if not empty
						core::CompoundStmtPtr inCmpd = stmt.isa<core::CompoundStmtPtr>();
						if(inCmpd && inCmpd.empty()) { continue; }
						// if not NoOp
						if(stmt == noop) { continue; }
						// no unit constant
						if(unitExpr == stmt) { continue; }

						// any other case, just add it to the new list
						stmtList.push_back(stmt);
					}

					// once done, clean nested compounds with a single compound inside
					std::function<core::CompoundStmtPtr(const std::vector<core::StatementPtr>&)> unNest = [&builder, &unNest](
					    const std::vector<core::StatementPtr>& list) -> core::CompoundStmtPtr {
						if(list.size() == 1 && list[0].isa<core::CompoundStmtPtr>() && !list[0]->hasAnnotation(annotations::DataRangeAnnotation::KEY)) {
							return unNest(list[0].as<core::CompoundStmtPtr>()->getStatements());
						}
						// any last stmt being a compound can be inlined since cleanups will be made anyway at the end of the function
						// any aliased variable declared in the body will be renamed anyway
						else if(list.size() > 0 && list[list.size() - 1].isa<core::CompoundStmtPtr>()
						        && !list[list.size() - 1]->hasAnnotation(annotations::DataRangeAnnotation::KEY)) {
							core::CompoundStmtPtr tmp = unNest(list[list.size() - 1].as<core::CompoundStmtPtr>()->getStatements());
							std::vector<core::StatementPtr> newList(list.begin(), list.end() - 1);
							newList.insert(newList.end(), tmp.getStatements().begin(), tmp.getStatements().end());
							return builder.compoundStmt(newList);
						}
						return builder.compoundStmt(list);
					};
					return unNest(stmtList);
				}
				return node;
			});
			return clean.map(prog);
		}

	} // anonymous namespace


	boost::optional<std::string> FrontendCleanupExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// last or second-last
		auto it = setup.getExtensions().crbegin();
		if(it->get() != this) {
			std::advance(it, 1);
			if(it->get() != this) { return boost::optional<std::string>("FrontendCleanup needs to be the last or second-to-last Extension"); }
		}

		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	insieme::core::ProgramPtr FrontendCleanupExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		//////////////////////////////////////////////////////////////////////
		// Assure return statements for "Main" functions typed as int
		// ==========================================================
		//
		// In C, it's allowed for the main function to by typed as () -> int, but not actually contain a "return x".
		// Since in the backend we move that function to a "normal" one, this will generate a warning in the BE compiler.
		// We fix this by generating a "return 0" in that case.
		//
		auto& mgr = prog->getNodeManager();
		auto& basic = mgr.getLangBasic();
		const auto& build = core::IRBuilder(mgr);
		namespace icp = insieme::core::pattern;
		namespace irp = insieme::core::pattern::irp;

		// if entry point returns int ensure that it has a return statement
		auto eplist = prog->getEntryPoints();
		// we only need to care about C programs which do not have a return because of C semantics on "main"
		if(eplist.size() == 1) {
			auto eP = prog->getEntryPoints()[0];
			if(eP.isa<core::LambdaExprPtr>()) {
				auto lambdaExp = eP.as<core::LambdaExprPtr>();
				auto funType = lambdaExp->getFunctionType();
				if(basic.isInt(funType->getReturnType())) {
					icp::TreePattern compoundPattern = icp::outermost(icp::var("compound", irp::compoundStmt(icp::anyList)));

					auto optCompoundMatch = compoundPattern.matchAddress(core::LambdaExprAddress(lambdaExp));
					if(optCompoundMatch) {
						icp::AddressMatch compoundMatch = optCompoundMatch.get();
						auto compound = compoundMatch.getVarBinding("compound").getFlattened()[0].as<core::CompoundStmtAddress>();

						icp::TreePattern compoundPattern =
						    irp::compoundStmt(icp::empty | icp::single(!irp::returnStmt(icp::any)) | (icp::anyList << !irp::returnStmt(icp::any)));
						if(compoundPattern.match(compound)) {
							// outermost compound does not contain return, add it
							auto newRoot = core::transform::append(mgr, compound, toVector<core::StatementPtr>(build.returnStmt(build.intLit(0))))
							                   .as<core::ExpressionPtr>();
							prog = core::Program::remEntryPoint(mgr, prog, eP);
							prog = core::Program::addEntryPoint(mgr, prog, newRoot);
						}
					}
				}
			}
		}


		return prog;
	}


	insieme::frontend::tu::IRTranslationUnit FrontendCleanupExtension::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {
		tu = applyCleanup(tu, allocaCall);
		tu = applyCleanup(tu, superfluousCode);

		////////////////////////////////////////////////////////////////////////
		//// Malloc
		//// ==============
		//// used to replace all malloc and calloc calls with the correct IR expression
		//{
		//	for(auto& pair : tu.getFunctions()) {
		//		core::ExpressionPtr lit = pair.first;
		//		core::LambdaExprPtr func = pair.second;

		//		core::TypePtr retType = lit->getType().as<core::FunctionTypePtr>()->getReturnType();
		//		assert_eq(retType, func->getType().as<core::FunctionTypePtr>()->getReturnType());

		//		core::IRBuilder builder(func->getNodeManager());
		//		const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();


		//		// check if return type is volatile or not and adapt return statements accordingly
		//		bool funcIsVolatile = core::analysis::isVolatileType(retType);

		//		core::LambdaExprAddress fA(func);
		//		std::map<core::NodeAddress, core::NodePtr> replacements;

		//		// loock at all return types and check if a volatile has to be added/removed
		//		visitDepthFirst(fA, [&](const core::ReturnStmtAddress& ret) {
		//			core::TypePtr returningType = ret->getReturnExpr()->getType();
		//			core::StatementPtr newRet;
		//			if(newRet) { replacements[ret] = newRet; }
		//		});

		//		// replace return statements if necessary
		//		if(!replacements.empty()) { func = core::transform::replaceAll(func->getNodeManager(), replacements).as<core::LambdaExprPtr>(); }


		//		// filter to filter out the recursive transition to another called function
		//		auto filter = [&func](const core::NodePtr& node) -> bool {
		//			if(core::LambdaExprPtr call = node.isa<core::LambdaExprPtr>()) {
		//				if(call == func) {
		//					return true;
		//				} else {
		//					return false;
		//				}
		//			}
		//			return true;
		//		};

		//		// fix all malloc and calloc calls
		//		auto fixer = [&](const core::NodePtr& node) -> core::NodePtr {
		//			if(core::analysis::isCallOf(node, gen.getRefReinterpret())) {
		//				if(core::CallExprPtr call = node.as<core::CallExprPtr>()[0].isa<core::CallExprPtr>()) {
		//					if(core::LiteralPtr lit = call->getFunctionExpr().isa<core::LiteralPtr>()) {
		//						if(lit->getStringValue() == "malloc" || lit->getStringValue() == "calloc") {
		//							return frontend::utils::handleMemAlloc(builder, node.as<core::CallExprPtr>()->getType(), call);
		//						}
		//					}
		//				}
		//			}
		//			return node;
		//		};

		//		// modify all returns at once!
		//		auto memallocFixer = core::transform::makeCachedLambdaMapper(fixer, filter);
		//		func = memallocFixer.map(func);

		//		// update function of translation unit
		//		tu.replaceFunction(lit.as<core::LiteralPtr>(), func);
		//	}
		//}
		return tu;
	}

	namespace {

		core::StatementPtr collectAssignments(const core::StatementPtr& stmt, const core::NodePtr& feAssign, core::StatementPtr& lastExpr,
		                                      core::StatementList& preProcess, bool isCXX) {
			assert_true(stmt) << "no stmt, no fun";
			core::IRBuilder builder(stmt->getNodeManager());

			//// the maper should never leave the first nested scope
			//auto doNotCheckInBodies = [&](const core::NodePtr& node) {
			//	if(node.isa<core::CompoundStmtPtr>()) { return false; }
			//	return true;
			//};

			//// substitute usage by left side
			//auto assignMapper = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node) -> core::NodePtr {

			//	if(core::CallExprPtr call = node.isa<core::CallExprPtr>()) {
			//		if(core::analysis::isCallOf(call, builder.getLangBasic().getGenPostDec())
			//		   || core::analysis::isCallOf(call, builder.getLangBasic().getGenPostInc())
			//		   || core::analysis::isCallOf(call, builder.getLangBasic().getArrayViewPostDec())
			//		   || core::analysis::isCallOf(call, builder.getLangBasic().getArrayViewPostInc())) {
			//			auto var = builder.variable(call->getType());
			//			auto decl = builder.declarationStmt(var, call);
			//			preProcess.push_back(decl);
			//			return var;
			//		}

			//		if(core::analysis::isCallOf(call, feAssign)) {
			//			// build a new operator with the final shape
			//			auto assign = builder.assign(call[0], call[1]);
			//			core::transform::utils::migrateAnnotations(call, assign);

			//			// extract the operation
			//			preProcess.push_back(assign);

			//			// Cpp by ref, C by value
			//			if(isCXX) {
			//				lastExpr = call[0];
			//			} else {
			//				lastExpr = builder.deref(call[0]);
			//			}
			//			return lastExpr;
			//		}
			//	}
			//	return node;
			//}, doNotCheckInBodies);

			//return assignMapper.map(stmt);
			return stmt;
		}

	} // annonymous namespace

	stmtutils::StmtWrapper FrontendCleanupExtension::PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmts,
	                                                           conversion::Converter& converter) {
		stmtutils::StmtWrapper newStmts;

		//////////////////////////////////////////////////////////////
		// remove frontend assignments and extract them into different statements
		{
			const auto& feExt = converter.getFrontendIR();
			core::IRBuilder builder(converter.getNodeManager());

			// stores the last expression returned to avoid writting a dead read
			core::StatementPtr lastExpr;
			// the extracted statements, only if this list is not empty the transformation is done
			core::StatementList prependStmts;

			auto justRemove = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node) -> core::NodePtr {
				if(core::CallExprPtr call = node.isa<core::CallExprPtr>()) {
					if(core::analysis::isCallOf(call, feExt.getRefAssign())) {
						// build a new operator with the final shape
						auto assign = builder.assign(call[0], call[1]);
						core::transform::utils::migrateAnnotations(call, assign);
						return assign;
					}
				}
				return node;
			});

			// this is not necesary, but makes code prettier
			auto allPostOps = [&](const core::StatementList& list) -> bool {
				for(auto stmt : list)
					if(!stmt.isa<core::DeclarationStmtPtr>()) { return false; }
				return true;
			};

			for(auto stmt : irStmts) {
				// do nothing on declarations
				if(stmt.isa<core::DeclarationStmtPtr>() || stmt.isa<core::CompoundStmtPtr>() || stmt.isa<core::ForStmtPtr>()) {
					newStmts.push_back(stmt);
				}
				// marked stmts?  just remove the Frontend node and write a good assign call
				else if(stmt.isa<core::MarkerExprPtr>() || stmt.isa<core::MarkerStmtPtr>()) {
					newStmts.push_back(justRemove.map(stmt));
				} else if(core::WhileStmtPtr whilestmt = stmt.isa<core::WhileStmtPtr>()) {
					// any assignment extracted from the condition expression of a while stmt needs to be replicated
					// at the end of the loop body
					core::StatementList conditionBody;
					core::ExpressionPtr conditionExpr;

					auto res = collectAssignments(stmt, feExt.getRefAssign(), lastExpr, conditionBody, converter.getCompiler().isCXX());
					conditionBody.push_back(builder.returnStmt(res.as<core::WhileStmtPtr>().getCondition()));

					// reconstruct the conditional expression, if this became more than one statements we'll capture it in a lambda
					// whenever free variable will be captured by ref and therefore modified in the outer scope
					if(conditionBody.size() == 1) {
						conditionExpr = res.as<core::WhileStmtPtr>().getCondition();
					} else {
						conditionExpr = builder.createCallExprFromBody(builder.compoundStmt(conditionBody), builder.getLangBasic().getBool());
					}

					// rebuild the statement with the added elements
					core::StatementList bodyList = res.as<core::WhileStmtPtr>()->getBody()->getStatements();
					{
						core::StatementList newBody;
						core::StatementPtr lastBodyExpr;
						core::StatementList preBodyProcess;
						for(auto bodyStmt : bodyList) {
							auto bodyRes = collectAssignments(bodyStmt, feExt.getRefAssign(), lastBodyExpr, preBodyProcess, converter.getCompiler().isCXX());
							if(preBodyProcess.empty() || allPostOps(preBodyProcess)) {
								if(res != lastBodyExpr) { newBody.push_back(bodyStmt); }
							} else {
								newBody.insert(newBody.end(), preBodyProcess.begin(), preBodyProcess.end());
								if(bodyRes != lastBodyExpr) { newBody.push_back(bodyRes); }
							}
							preBodyProcess.clear();
						}

						res = builder.whileStmt(conditionExpr, stmtutils::tryAggregateStmts(builder, newBody));
					}

					newStmts.push_back(res);
				} else {
					auto res = collectAssignments(stmt, feExt.getRefAssign(), lastExpr, prependStmts, converter.getCompiler().isCXX());
					if(prependStmts.empty() || allPostOps(prependStmts)) {
						newStmts.push_back(stmt);
					} else {
						newStmts.insert(newStmts.end(), prependStmts.begin(), prependStmts.end());
						// if the assignment is used as expression, well have a remainng tail which is not needed
						if(res != lastExpr) { newStmts.push_back(res); }
					}
				}
				prependStmts.clear();
				lastExpr = core::StatementPtr();
			}
		}

		//////////////////////////////////////////////////////////////
		// check for declaration statements like int x=x;
		// this is valid in C/C++ but not in IR. Create
		// something like
		// decl ref<type> x = var(undefined(type));
		// x = *x;
		{
			// do we have a single statement that is a declaration statement?!
			if(newStmts.size() == 1 && newStmts[0].isa<core::DeclarationStmtPtr>()) {
				// check if the variable uses itself in the initialization
				core::DeclarationStmtPtr decl = newStmts[0].as<core::DeclarationStmtPtr>();
				core::VariablePtr var = decl->getVariable();
				bool usesVar = false;
				core::visitDepthFirstOnce(decl->getInitialization(), [&](const core::VariablePtr& use) {
					if(use == var) { usesVar = true; }
				}, true, false);
				// if it is not using itself in the initialization don't touch it
				if(!usesVar) { return newStmts; }
				// now we know that we have something like int x=x;
				// create decl ref<type> a = undefined(type);
				auto& builder = converter.getIRBuilder();
				// keep the old var name (instead if creating a new one)
				// makes life easier, because we dont have to replace
				// the name of the var everywhere..
				core::DeclarationStmtPtr newDecl =
				    core::transform::replaceAllGen(converter.getNodeManager(), decl, decl->getInitialization(), builder.undefinedVar(var->getType()));
				// this is our new declaration statement
				newStmts[0] = newDecl;
				// if it is a ctor call we have to call a deref operation
				// on the constructor. otherwise we just use the inner part of the
				// var call.. (e.g., var(*v1) -> *v1)
				if(core::analysis::isConstructorCall(decl->getInitialization())) {
					newStmts.push_back(builder.assign(newDecl->getVariable(), builder.deref(decl->getInitialization())));
				} else {
					newStmts.push_back(builder.assign(newDecl->getVariable(), decl->getInitialization().as<core::CallExprPtr>()->getArgument(0)));
				}
				// fix uses on right side.. var(*v1) -> *v1
				newStmts[1] = core::transform::replaceAllGen(converter.getNodeManager(), newStmts[1], builder.refVar(builder.deref(newDecl->getVariable())),
				                                             builder.deref(newDecl->getVariable()));
			}
		}

		return newStmts;
	}

} // extensions
} // frontend
} // insieme

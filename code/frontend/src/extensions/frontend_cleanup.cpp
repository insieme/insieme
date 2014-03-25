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

#include "insieme/frontend/extensions/frontend_cleanup.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/annotations/c/decl_only.h"
#include "insieme/annotations/c/include.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/tu/ir_translation_unit.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/assert.h"


#include "insieme/frontend/omp/omp_annotation.h"

#include <functional>

//#include <boost/algorithm/string/predicate.hpp>


 namespace insieme {
 namespace frontend {

	namespace {

		core::NodePtr applyCleanup(const core::NodePtr& node, std::function<core::NodePtr(const core::NodePtr&)>  pass){

			core::NodePtr res;

			if (!node.isa<core::TypePtr>())
				res = pass(node);

			core::visitDepthFirstOnce(res, [&] (const core::TypePtr& type){
				if (core::hasMetaInfo(type)){
					auto meta = core::getMetaInfo(type);

					vector<core::LambdaExprPtr> ctors = meta.getConstructors();
					for (auto& ctor : ctors){
						ctor = pass(ctor).as<core::LambdaExprPtr>();
					}
					if (!ctors.empty()) meta.setConstructors(ctors);

					if (meta.hasDestructor()){
						auto dtor = meta.getDestructor();
						dtor = pass(dtor).as<core::LambdaExprPtr>();
						meta.setDestructor(dtor);
					}

					vector<core::MemberFunction> members = meta.getMemberFunctions();
					for (core::MemberFunction& member : members){
						member = core::MemberFunction(member.getName(), pass(member.getImplementation()).as<core::ExpressionPtr>(),
													  member.isVirtual(), member.isConst());
					}
					if(!members.empty()) meta.setMemberFunctions(members);
					core::setMetaInfo(type, meta);

				}
			});
			return res;
		}


		//////////////////////////////////////////////////////////////////////
		//  Long long cleanup
		//  =================
		//
		//  during the translation, long long behaves like a built in type, but in the backend it is mapped into the same bitwith as a long
		//  the problem comes when we pass it as parameter to functions, because it produces overload, even when in the backend both functions
		//  are going to have the same parameter type
		//
		//  after all translation units have being merged, we can safely remove this type, all the function calls are already mapped and statically
		//  resolved, so we wont find any problem related with typing.
		//
		//  The only unresolved issue is 3rd party compatibility, now we wont have long long variables in the generated code, so we can not exploit
		//  overloads in 3rd party libraries (they do not make much sense anyway, do they? )
		insieme::core::NodePtr longLongCleanup (const insieme::core::NodePtr& prog){

			core::NodePtr res;

			// remove all superfluous casts
			auto castRemover = core::transform::makeCachedLambdaMapper([](const core::NodePtr& node)-> core::NodePtr{
						if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
							core::IRBuilder builder (node->getNodeManager());
							const auto& ext = node->getNodeManager().getLangExtension<core::lang::IRppExtensions>();

							// TO
							if (core::analysis::isCallOf(call, ext.getLongToLongLong()))  return call[0];
							if (core::analysis::isCallOf(call, ext.getULongToULongLong())) return call[0];

							// From
							if (core::analysis::isCallOf(call, ext.getLongLongToLong()))  return call[0];
							if (core::analysis::isCallOf(call, ext.getULongLongToULong())) return call[0];

							// between
							if (core::analysis::isCallOf(call, ext.getLongLongToULongLong()))
								return builder.callExpr(builder.getLangBasic().getUInt8(),
														builder.getLangBasic().getSignedToUnsigned(),
														toVector (call[0], builder.getIntParamLiteral(8)));
							if (core::analysis::isCallOf(call, ext.getULongLongToLongLong()))
								return builder.callExpr(builder.getLangBasic().getInt8(),
														builder.getLangBasic().getUnsignedToInt(),
														toVector (call[0], builder.getIntParamLiteral(8)));
						}
						return node;
					});
			res = castRemover.map(prog);

			// finaly, substitute any usage of the long long types
			core::IRBuilder builder (prog->getNodeManager());
			core::TypePtr longlongTy = builder.structType(toVector( builder.namedType("longlong_val", builder.getLangBasic().getInt8())));
			core::TypePtr ulonglongTy = builder.structType(toVector( builder.namedType("longlong_val", builder.getLangBasic().getUInt8())));
			core::NodeMap replacements;
			replacements [ longlongTy ] = builder.getLangBasic().getInt8();
			replacements [ ulonglongTy ] = builder.getLangBasic().getUInt8();
			res = core::transform::replaceAllGen (prog->getNodeManager(), res, replacements, false);

			return res;
		}

		//////////////////////////////////////////////////////////////////////
		// CleanUp "deref refVar" situation
		// ================================
		//
		//	sometimes, specially during initializations, a value is materialized in memory to be deref then and accessing the value again.
		//	this is actually equivalent to not doing it
		insieme::core::NodePtr refDerefCleanup (const insieme::core::NodePtr& prog){

			auto derefVarRemover = core::transform::makeCachedLambdaMapper([](const core::NodePtr& node)-> core::NodePtr{
						if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
							const auto& gen = node->getNodeManager().getLangBasic();
							if (core::analysis::isCallOf(call, gen.getRefDeref())) {
								if (core::CallExprPtr inCall = call[0].isa<core::CallExprPtr>()){
									if (core::analysis::isCallOf(inCall, gen.getRefVar())) {
										return inCall[0];
									}
								}
							}
						}
						return node;
					});
			return derefVarRemover.map(prog);
		}

		//////////////////////////////////////////////////////////////////////
		// Unnecesary casts
		// ==============
		//
		// During translation it might be easier to cast values to a different type to enforce type matching, but after resolving all the aliases
		// in the translation unit unification it might turn out that those types were actually the same one. Clean up those casts since they might drive
		// output code into error (specially with c++)
		insieme::core::NodePtr castCleanup (const insieme::core::NodePtr& prog){
			auto castRemover = core::transform::makeCachedLambdaMapper([](const core::NodePtr& node)-> core::NodePtr{
						if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
							const auto& gen = node->getNodeManager().getLangBasic();
							if (core::analysis::isCallOf(call, gen.getRefReinterpret())) {
								if (call->getType() == call[0]->getType())
									return call[0];
							}
						}
						return node;
					});
			return castRemover.map(prog);
		}

		//////////////////////////////////////////////////////////////////////
		// Superfluous code
		// ================
		//
		// remove empty constructs from functions bodies, this prevents multiple implementations of the same
		// equivalent code produced in different translation units with different macro expansions
		// this removes empty compounds and nested compunds with single elements
		insieme::core::NodePtr superfluousCode (const insieme::core::NodePtr& prog){

			core::IRBuilder builder (prog->getNodeManager());
			auto noop = builder.getNoOp();
			auto unitExpr = builder.getLangBasic().getUnitConstant();

			auto clean = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
						if (core::CompoundStmtPtr cmpnd = node.isa<core::CompoundStmtPtr>()){
							vector<core::StatementPtr> stmtList;
							for (core::StatementPtr stmt : cmpnd->getStatements()){
								// if not empty 
								core::CompoundStmtPtr inCmpd = stmt.isa<core::CompoundStmtPtr>();
								if (inCmpd && inCmpd.empty())
									continue;
								// if not NoOp
								if (stmt == noop)
									continue;
								// no unit constant
								if(unitExpr == stmt)
									continue;

								// any other case, just add it to the new list
								stmtList.push_back (stmt);
							}

							// once done, clean nested compounds with a single compound inside
							std::function<core::CompoundStmtPtr(const std::vector<core::StatementPtr>&)> unNest =
								[&builder, &unNest] 	(const std::vector<core::StatementPtr>& list) -> core::CompoundStmtPtr{
									if (list.size() ==1 && list[0].isa<core::CompoundStmtPtr>())
										return unNest(list[0].as<core::CompoundStmtPtr>()->getStatements());
									return builder.compoundStmt(list);
								};
							return unNest(stmtList);
						}
						return node;
					});
			return clean.map(prog);
		}


	} // anonymous namespace


	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	insieme::core::ProgramPtr FrontendCleanup::IRVisit(insieme::core::ProgramPtr& prog){

		prog = applyCleanup(prog, longLongCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, refDerefCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, castCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, superfluousCode).as<core::ProgramPtr>();


		/////////////////////////////////////////////////////////////////////////////////////
		//		DEBUG
		//			some code to fish bugs
//		core::visitDepthFirstOnce(prog, [&] (const core::NodePtr& node){
//			if (core::StructTypePtr type = node.isa<core::StructTypePtr>()){
//				if (boost::starts_with(toString( type), "AP(struct CGAL_Lazy_class_CGAL_Interval_nt_false_")){
//					if (core::hasMetaInfo(type)){
//						auto meta = core::getMetaInfo(type);
//						std::cout << " ========= \n" << std::endl;
//						std::cout << "CLASS: - " <<  toString(type ) << std::endl;
//						for (auto ctor : meta.getConstructors()){
//							dumpPretty(ctor);
//							std::cout << "------------------" << std::endl;
//						}
//						std::cout << " ========= " << std::endl;
//					}
//				}
//			}
//
//
//			if (core::LambdaExprPtr expr = node.isa<core::LambdaExprPtr>()){
//				core::FunctionTypePtr fty = expr->getType().as<core::FunctionTypePtr>();	
//				if (fty->getParameterTypes().size() == 3){
//					if (boost::starts_with(toString( fty->getParameterTypes()[0]), "AP(ref<struct CGAL_Lazy_class_struct")){
//						dumpPretty(expr);
//						std::cout << " ######## " << std::endl;
//					}
//				}
//			}
//		});
//
//	//	abort();
//


		return prog;
	}


    insieme::frontend::tu::IRTranslationUnit FrontendCleanup::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {

		//////////////////////////////////////////////////////////////////////
		// Malloc
		// ==============
    	// used to replace all malloc and calloc calls with the correct IR expression
		{ 
			for (auto& pair : tu.getFunctions()) {
				core::ExpressionPtr lit = pair.first;
				core::LambdaExprPtr func = pair.second;

				core::TypePtr retType = lit->getType().as<core::FunctionTypePtr>()->getReturnType();
				assert( retType == func->getType().as<core::FunctionTypePtr>()->getReturnType());

				core::IRBuilder builder(func->getNodeManager());
				const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();

				// filter to filter out the recursive transition to another called function
				auto filter = [&func] (const core::NodePtr& node) ->bool{
					if(core::LambdaExprPtr call = node.isa<core::LambdaExprPtr>()){
						if (call == func) return true;
						else return false;
					}
					return true;
				};

				// fix all malloc and calloc calls
				auto fixer = [&](const core::NodePtr& node)-> core::NodePtr{
					if(core::analysis::isCallOf(node,gen.getRefReinterpret())) {
						if(core::CallExprPtr call = node.as<core::CallExprPtr>()[0].isa<core::CallExprPtr>()) {
							if (core::LiteralPtr lit = call->getFunctionExpr().isa<core::LiteralPtr>()) {
								if(lit->getStringValue() == "malloc" || lit->getStringValue() == "calloc") {
									return frontend::utils::handleMemAlloc(builder, node.as<core::CallExprPtr>()->getType(), call);
								}
							}
						}
					}
					return node;
				};

				// modify all returns at once!
				auto memallocFixer = core::transform::makeCachedLambdaMapper(fixer, filter);
				func = memallocFixer.map(func);

				//update function of translation unit
				tu.replaceFunction(lit.as<core::LiteralPtr>(), func);
			}
		}
        return tu;
    }

    stmtutils::StmtWrapper FrontendCleanup::PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmts, conversion::Converter& convFact){
		stmtutils::StmtWrapper newStmts;

		//////////////////////////////////////////////////////////////
		// remove frontend assignments and extract them into different statements
		{
			const auto& feExt = convFact.getFrontendIR();
			core::IRBuilder builder (convFact.getNodeManager());

			// the maper should never leave the first nested scope
			auto doNotCheckInBodies = [&] (const core::NodePtr& node){
				if (node.isa<core::CompoundStmtPtr>())
					return false;
				return true;
			};

			core::ExpressionPtr lastExpr;
			// substitute usage by left side
			auto assignMapper = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
					if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
						if (core::analysis::isCallOf(call, feExt.getRefAssign())){

							// build a new operator with the final shape
							auto assign = builder.assign(call[0], call[1]);
							core::transform::utils::migrateAnnotations(call, assign);

							// extract the operation
							newStmts.push_back(assign);

							// Cpp by ref, C by value
							if (convFact.getCompiler().isCXX())
								lastExpr = call[0];
							else
								lastExpr = builder.deref(call[0]);
							return lastExpr;
						}
					}
					return node;
			},doNotCheckInBodies);

			auto justRemove = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
					if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
						if (core::analysis::isCallOf(call, feExt.getRefAssign())){
							// build a new operator with the final shape
							auto assign = builder.assign(call[0], call[1]);
							core::transform::utils::migrateAnnotations(call, assign);
							return assign;
						}
					}
					return node;
			});

			for (auto stmt : irStmts){

				if (stmt.isa<core::DeclarationStmtPtr>() || stmt.isa<core::CompoundStmtPtr>() || 
					stmt.isa<core::ForStmtPtr>())
					newStmts.push_back( stmt);
				else if (stmt.isa<core::MarkerExprPtr>() || stmt.isa<core::MarkerStmtPtr>())
					newStmts.push_back( justRemove.map(stmt));
				else if (core::WhileStmtPtr whilestmt = stmt.isa<core::WhileStmtPtr>()){

					// any assignment extracted from the condition expression of a while stmt needs to be replicated
					// at the end of the loop body
					stmtutils::StmtWrapper  stashStmts = newStmts;
					newStmts.clear();
					core::StatementList toAdd;

					auto res = assignMapper.map(stmt);

					for (core::StatementPtr stmt : newStmts){
						toAdd.push_back(stmt);
					}

					// rebuild the statement with the added elements
					core::StatementList bodyList = res.as<core::WhileStmtPtr>()->getBody()->getStatements();
					{
						// clean inner scope
						stmtutils::StmtWrapper  stashBody = newStmts;
						newStmts.clear();

						for (auto bodyStmt : bodyList){
							auto whileline = assignMapper.map(bodyStmt);
							if (res != lastExpr)
								newStmts.push_back( whileline);
						}

						newStmts.insert(newStmts.end(), toAdd.begin(), toAdd.end());
						res = builder.whileStmt( res.as<core::WhileStmtPtr>()->getCondition(), stmtutils::tryAggregateStmts(builder,newStmts));
						newStmts.clear();
						newStmts = stashBody;
					}


					newStmts.insert(newStmts.begin(), stashStmts.begin(), stashStmts.end());
					newStmts.push_back(res);
				}
				else{
					auto res = assignMapper.map(stmt);

					// if the assignment is used as expression, well have a remainng tail which is not needed
					if (res != lastExpr){
						newStmts.push_back( res);
					}
				}
			}
		}

		return newStmts;
	}

} // frontend
} // insieme

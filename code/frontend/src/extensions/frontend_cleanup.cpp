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

#include "insieme/frontend/expr_converter.h"

#include "insieme/utils/assert.h"

#include <functional>

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
		// Declaration Only structs
		// ========================
		//
		// If we encounter a sturct declaration without a definition we generate a GenericType with
		// its name and mark it as DeclOnly, if in a different TU we encounter and definition for
		// that type we drop the genericType. all genericTypes with an DeclOnlyAnnotation are turned
		// into empty structs
		insieme::core::NodePtr declarationCleanup (const insieme::core::NodePtr& prog){
			core::NodeMap replacements;
			core::IRBuilder builder(prog->getNodeManager());

			visitDepthFirstOnce(prog, [&](const core::GenericTypePtr& cur) {
					if(insieme::annotations::c::isDeclOnly(cur)) {
						insieme::annotations::c::markDeclOnly(cur,false);
						core::NamedCompositeType::Entries structElements;
						auto replacement = builder.structType( builder.stringValue(cur.toString()),structElements );

						insieme::core::transform::utils::migrateAnnotations(cur.as<core::NodePtr>(), replacement.as<core::NodePtr>());
						replacements[cur] = replacement;
					}
				}, true, true);

			return  core::transform::replaceAllGen (prog->getNodeManager(), prog, replacements, false);

		}

	} // anonymous namespace

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	void printErrors ( const core::TypePtr& obj, const core::checks::MessageList& ml , const char* msg, bool& flag){
		if (!ml.empty()){

			if (flag){
				dumpPretty(obj);
				flag = false;
			}

			for (unsigned i=0 ; i < ml.size(); i++){
				std::cout << " == Sema error " << msg << "  ==== " << std::endl;
				std::cout << " - " <<  ml[i].getErrorCode() << std::endl;
			//	dumpPretty(ml[i].getOrigin());

				std::string msg = ml[i].getMessage();
				std::cout << " msg: " << std::string(msg.begin(), (msg.length() > 80)? msg.begin()+80 : msg.end()) << std::endl;
			}
		}
	}


	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	insieme::core::ProgramPtr FrontendCleanup::IRVisit(insieme::core::ProgramPtr& prog){

		prog = applyCleanup(prog, longLongCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, refDerefCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, castCleanup).as<core::ProgramPtr>();
		prog = applyCleanup(prog, declarationCleanup).as<core::ProgramPtr>();


	//	core::NodeManager& nm (prog.getNodeManager());
	//	core::IRBuilder builder (nm);

	//	auto triangleThing = builder.genericType("Triangle_3");
	//	if (core::hasMetaInfo(triangleThing)){
	//		std::cout << "Hey: there is meta" << std::endl << std::flush;
	//	}
	//	else{
	//		std::cout << " no meta at all" << std::endl << std::flush;
	//	}

	//	if ( annotations::c::hasIncludeAttached(triangleThing) ){
	//		std::cout << " there is an include" << std::endl << std::flush;
	//	}
	//	else{
	//		std::cout << " no include at all" << std::endl << std::flush;
	//	}

	//  ///////////////////////////////////////////////////////////////
	//  //   make independent semantic checks
	//	core::visitDepthFirstOnce(prog, [&] (const core::TypePtr& type){
	//			if (core::hasMetaInfo(type)){
	//				bool dumpClass =true;
	//				auto meta = core::getMetaInfo(type);

	//				vector<core::LambdaExprPtr> ctors = meta.getConstructors();
	//				for (auto& ctor : ctors){
	//					auto semaErrors = core::checks::check(ctor);
	//					printErrors (type, semaErrors, "ctor", dumpClass);
	//				}

	//				if (meta.hasDestructor()){
	//					auto dtor = meta.getDestructor();
	//					auto semaErrors = core::checks::check(dtor);
	//					printErrors (type, semaErrors, "dtor", dumpClass);
	//				}

	//				vector<core::MemberFunction> members = meta.getMemberFunctions();
	//				for (core::MemberFunction& member : members){
	//					auto semaErrors = core::checks::check(member.getImplementation());
	//					printErrors (type, semaErrors, "member", dumpClass);
	//				}
	//			}
	//	});
		
		return prog;
	}


    //used to replace all malloc and calloc calls with the correct IR expression
    insieme::frontend::tu::IRTranslationUnit FrontendCleanup::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {
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
                                return exprutils::handleMemAlloc(builder, node.as<core::CallExprPtr>()->getType(), call);
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
        return tu;
    }


} // frontend
} // insieme

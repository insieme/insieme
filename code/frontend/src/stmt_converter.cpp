/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/frontend/stmt_converter.h"

#include <algorithm>

#include "insieme/frontend/converter.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/expr_to_bool.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/materialize.h"

using namespace clang;

namespace {
	struct litCompare {
		bool operator()(const insieme::core::LiteralPtr& a, const insieme::core::LiteralPtr& b) const {
			return a->getStringValue() < b->getStringValue();
		}
	};
}


namespace insieme {
namespace frontend {
namespace conversion {

	stmtutils::StmtWrapper Converter::StmtConverter::BaseVisit(clang::Stmt* stmt, std::function<stmtutils::StmtWrapper(clang::Stmt*)> self) {

		auto retStmt = converter.applyExtensions<stmtutils::StmtWrapper>(stmt, [&](clang::Stmt* param) {
			converter.trackSourceLocation(param);
			auto retIr = self(param);
			converter.untrackSourceLocation();
			return retIr;
		});

		// print diagnosis messages
		converter.printDiagnosis(stmt->getLocStart());

		// deal with pragmas
		core::NodeList list(retStmt.begin(), retStmt.end());
		list = pragma::handlePragmas(list, stmt, converter);
		retStmt.clear();
		for(const auto& e : list) {
			retStmt.push_back(e.as<core::StatementPtr>());
		}

		// attach location from clang
		for(auto s: retStmt) {
			utils::attachLocationFromClang(s, converter.getSourceManager(), stmt->getLocStart(), stmt->getLocEnd());
		}

		return retStmt;
	}

	stmtutils::StmtWrapper Converter::CStmtConverter::Visit(clang::Stmt* stmt) {
		VLOG(2) << "CStmtConverter";
		return BaseVisit(stmt, [&](clang::Stmt* stmt) { return StmtVisitor<CStmtConverter, stmtutils::StmtWrapper>::Visit(stmt); });
	}

	//---------------------------------------------------------------------------------------------------------------------
	//							DECL STATEMENT
	//---------------------------------------------------------------------------------------------------------------------

	namespace {
		core::StatementPtr convertVarDeclToStmt(const clang::VarDecl* varDecl, const Converter& converter) {
			const core::IRBuilder& builder = converter.getIRBuilder();
			// external declaration statement as per very early K&R C -> ignore
			if(varDecl->hasExternalStorage()) { return builder.getNoOp(); }
			// global storage handled in decl visit
			if(varDecl->hasGlobalStorage()) {
				converter.getDeclConverter()->VisitVarDecl(varDecl);
				return builder.getNoOp();
			}
			// convert decl
			auto convertedDecl = converter.getDeclConverter()->convertVarDecl(varDecl);
			auto refDecl = core::lang::buildRefDecl(convertedDecl.first->getType());
			// check if we have an init expression
			core::ExpressionPtr initExp;
			if(convertedDecl.second) {
				initExp = convertedDecl.second;
				initExp = utils::fixTempMemoryInInitExpression(refDecl, initExp);
			} else {
				// generate undefined initializer
				initExp = refDecl;
			}

			// maybe add necessary casts if not materializing
			initExp = core::transform::castInitializationIfNotMaterializing(convertedDecl.first->getType(), initExp);

			// build ir declaration
			return builder.declarationStmt(convertedDecl.first, initExp);
		}
	}

	stmtutils::StmtWrapper Converter::StmtConverter::VisitDeclStmt(clang::DeclStmt* declStmt) {
		stmtutils::StmtWrapper retIr;
		LOG_STMT_CONVERSION(declStmt, retIr);

		for(auto decl : declStmt->decls()) {
			// a decl can be either a variable decl, or e.g. a typedef
			if(clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(decl)) {
				retIr.push_back(convertVarDeclToStmt(varDecl, converter));
			}
		}

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitReturnStmt(clang::ReturnStmt* retStmt) {
		stmtutils::StmtWrapper retIr;
		LOG_STMT_CONVERSION(retStmt, retIr);

		auto irRetStmt = builder.returnStmt();
		core::TypePtr retType = converter.getVarMan()->getRetType();
		bool isUnitRet = converter.getNodeManager().getLangBasic().isUnit(retType);

		// check if we have a return value
		if(clang::Expr* expr = retStmt->getRetValue()) {
			auto returnExpr = converter.convertCxxArgExpr(expr);
			// maybe add necessary casts if not materializing
			returnExpr = core::transform::castInitializationIfNotMaterializing(retType, returnExpr);
			// case of return void_function();
			if(isUnitRet) {
				retIr.push_back(returnExpr);
				retIr.push_back(builder.returnStmt());
				return retIr;
			}
			returnExpr = utils::fixTempMemoryInInitExpression(core::lang::buildRefDecl(retType), returnExpr);
			irRetStmt = builder.returnStmt(returnExpr, retType);
		}
		// case of return in a constructor or destructor
		else if(converter.getVarMan()->getRetType() && !isUnitRet) {
			irRetStmt = builder.returnStmt(converter.getVarMan()->getThis(), retType);
		}

		retIr.push_back(irRetStmt);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FOR STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitForStmt(clang::ForStmt* forStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(forStmt, retStmt);

		if(forStmt->getInit()) { retStmt.push_back(converter.convertStmt(forStmt->getInit())); }

		core::ExpressionPtr condExpr;
		if(forStmt->getCond()) {
			condExpr = utils::exprToBool(converter.convertExpr(forStmt->getCond()));
		} else {
			condExpr = builder.boolLit(true);
		}

		core::StatementPtr newBody;

		// only generate non-empty body in IR if not a clang::NullStmt (for() ;) and not an empty compound (for() { })
		clang::Stmt* clangBody = forStmt->getBody();
		if(clangBody && !dyn_cast<clang::NullStmt>(clangBody)) {
			stmtutils::StmtWrapper irOldBody = converter.convertStmtToWrapper(clangBody);
			if(auto compound = irOldBody.getSingleStmt().isa<core::CompoundStmtPtr>()) {
				if(!compound->empty()) { newBody = aggregateStmts(builder, irOldBody); }
			} else {
				newBody = irOldBody.getSingleStmt();
			}
		}

		if(forStmt->getInc()) {
			// convert increment statement
			auto incStmt = converter.convertExpr(forStmt->getInc()).as<core::StatementPtr>();
			newBody = frontend::utils::addIncrementExprBeforeAllExitPoints(newBody, incStmt);
		}

		// if we still have no body (because we had no body and no increment
		if(!newBody) newBody = builder.compoundStmt();

		auto whileStmt = builder.whileStmt(condExpr, newBody);

		// attach location to while explicitly
		utils::attachLocationFromClang(whileStmt, converter.getSourceManager(), forStmt->getLocStart(), forStmt->getLocEnd());

		retStmt.push_back(whileStmt);

		// compound statement required for correct scoping of variables declared in init statement of for header
		return builder.compoundStmt(retStmt);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								IF STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitIfStmt(clang::IfStmt* ifStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(ifStmt, retStmt);

		core::StatementPtr thenBody = stmtutils::aggregateStmts(builder, converter.convertStmtToWrapper(ifStmt->getThen()));
		frontend_assert(thenBody) << "Couldn't convert 'then' body of the IfStmt";

		core::ExpressionPtr condExpr;
		if(const clang::VarDecl* condVarDecl = ifStmt->getConditionVariable()) {
			/*
			 * we are in the situation where a variable is declared in the if condition, i.e.:
			 *
			 * 		if(int a = exp) { }
			 *
			 * this will be converted into the following IR representation:
			 *
			 * 		{
			 * 			int a = exp;
			 * 			if(cast<bool>(a)){ }
			 * 		}
			 */
			core::StatementPtr declStmt = convertVarDeclToStmt(condVarDecl, converter);
			retStmt.push_back(declStmt);

			frontend_assert(declStmt.isa<core::DeclarationStmtPtr>()) << "declaring static variables within an if is not very polite";
		}

		const clang::Expr* cond = ifStmt->getCond();
		frontend_assert(cond) << "If statement with no condition";
		condExpr = converter.convertExpr(cond);
		frontend_assert(condExpr) << "Couldn't convert 'condition' expression of the IfStmt";

		core::StatementPtr elseBody = builder.compoundStmt();
		// check for else statement
		if(Stmt* elseStmt = ifStmt->getElse()) { elseBody = stmtutils::aggregateStmts(builder, converter.convertStmtToWrapper(elseStmt)); }
		frontend_assert(elseBody) << "Couldn't convert 'else' body of the IfStmt";

		// adding the ifstmt to the list of returned stmts
		retStmt.push_back(builder.ifStmt(utils::exprToBool(condExpr), thenBody, elseBody));

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							WHILE STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitWhileStmt(clang::WhileStmt* whileStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(whileStmt, retStmt);

		core::StatementPtr body = aggregateStmts(builder, converter.convertStmtToWrapper(whileStmt->getBody()));
		frontend_assert(body) << "Couldn't convert body of the WhileStmt";

		frontend_assert(!whileStmt->getConditionVariable()) << "WhileStmt with a declaration of a condition variable not supported";

		const clang::Expr* cond = whileStmt->getCond();
		frontend_assert(cond) << "WhileStmt with no condition";

		core::ExpressionPtr condExpr = converter.convertExpr(cond);

		frontend_assert(!core::analysis::isCallOf(condExpr, mgr.getLangExtension<core::lang::ReferenceExtension>().getRefAssign()))
			<< "Assignments in while condition not supported";

		frontend_assert(condExpr && "Couldn't convert 'condition' expression of the WhileStmt");

		retStmt.push_back(builder.whileStmt(utils::exprToBool(condExpr), body));
		retStmt = aggregateStmts(builder, retStmt);

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DO STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitDoStmt(clang::DoStmt* doStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(doStmt, retStmt);

		core::CompoundStmtPtr body = builder.wrapBody(stmtutils::aggregateStmts(builder, converter.convertStmtToWrapper(doStmt->getBody())));
		frontend_assert(body) << "Couldn't convert body of the DoStmt";

		const clang::Expr* cond = doStmt->getCond();
		frontend_assert(cond) << "DoStmt must have a condition";

		core::ExpressionPtr condExpr = utils::exprToBool(converter.convertExpr(cond));
		frontend_assert(condExpr) << "Couldn't convert 'condition' expression of the DoStmt";

		frontend_assert(!core::analysis::isCallOf(condExpr, mgr.getLangExtension<core::lang::ReferenceExtension>().getRefAssign()))
			<< "Assignments in do while condition not supported";

		core::VariablePtr exitTest = builder.variable(builder.refType(gen.getBool()));
		condExpr = builder.logicOr(builder.logicNeg(builder.deref(exitTest)), condExpr);
		body = builder.compoundStmt({builder.assign(exitTest, gen.getTrue()), body});
		retStmt.push_back(builder.compoundStmt({builder.declarationStmt(exitTest, gen.getFalse()), builder.whileStmt(condExpr, body)}));

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							SWITCH STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitSwitchStmt(clang::SwitchStmt* switchStmt) {
		stmtutils::StmtWrapper retStmt;
		LOG_STMT_CONVERSION(switchStmt, retStmt);

		frontend_assert(!switchStmt->getConditionVariable()) << "Switch statements with variable declarations not supported";

		const clang::Expr* cond = switchStmt->getCond();
		frontend_assert(cond) << "SwitchStmt with no condition";
		core::ExpressionPtr condExpr = converter.convertExpr(cond);

		std::map<core::LiteralPtr, std::vector<core::StatementPtr>> caseMap;
		vector<core::StatementPtr> decls;
		std::vector<core::LiteralPtr> openCases;
		auto defLit = builder.literal("__insieme_default_case", gen.getUnit());

		auto addStmtToOpenCases = [&caseMap, &openCases](const core::StatementPtr& stmt) {
			// for each of the open cases, add the statement to their own stmt list
			for(const auto& caseLit : openCases) {
				auto& vec = caseMap[caseLit];
				// skip obviously closed cases
				if(!vec.empty() && (vec.back().isa<core::ReturnStmtPtr>() || vec.back().isa<core::BreakStmtPtr>())) continue;
				vec.push_back(stmt);
			}
		};

		// converts the cases to literals
		auto convertCase = [this, defLit](const clang::SwitchCase* switchCase) -> core::LiteralPtr {
			frontend_assert(switchCase) << "Invalid SwitchCase";
			if(llvm::isa<clang::DefaultStmt>(switchCase)) { return defLit; }

			core::LiteralPtr caseLiteral;
			const clang::Expr* caseExpr = llvm::cast<clang::CaseStmt>(switchCase)->getLHS();

			// if the expr is an integerConstantExpr
			if(caseExpr->isIntegerConstantExpr(converter.getCompiler().getASTContext())) {
				llvm::APSInt result;
				// reduce it and store it in result -- done by clang
				caseExpr->isIntegerConstantExpr(result, converter.getCompiler().getASTContext());
				core::TypePtr type = converter.convertType(caseExpr->getType());
				caseLiteral = builder.literal(type, result.toString(10));
			} else {
				core::ExpressionPtr caseExprIr = converter.convertExpr(caseExpr);
				if(caseExprIr->getNodeType() == core::NT_CastExpr) {
					core::CastExprPtr cast = static_pointer_cast<core::CastExprPtr>(caseExprIr);
					if(cast->getSubExpression()->getNodeType() == core::NT_Literal) {
						core::LiteralPtr literal = static_pointer_cast<core::LiteralPtr>(cast->getSubExpression());
						caseExprIr = builder.literal(cast->getType(), literal->getValue());
					}
				}

				if(!caseExprIr.isa<core::LiteralPtr>()) {
					// clang casts the literal to fit the condition type... and is not a literal anymore
					// it might be a scalar cast, we retrieve the literal
					caseLiteral = caseExprIr.as<core::CallExprPtr>()->getArgument(0).as<core::LiteralPtr>();
				} else {
					caseLiteral = caseExprIr.as<core::LiteralPtr>();
				}
			}

			return caseLiteral;
		};

		auto handleDeclStmt = [this, &decls](const clang::DeclStmt* declStmt) {
			// first, remove existing mappings (we may have a 1:N mapping
			for(auto decl: declStmt->decls()) {
				if(clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(decl)) converter.getVarMan()->undefine(varDecl);
			}
			// then, convert
			auto result = converter.convertStmt(declStmt);
			core::DeclarationStmtPtr decl;
			if(result.isa<core::CompoundStmtPtr>()) {
				core::CompoundStmtPtr comp = result.as<core::CompoundStmtPtr>();
				for(auto st : comp) {
					frontend_assert(st.isa<core::DeclarationStmtPtr>()) << "Declaration stmt inside of SwitchStmt contains unknown elements.";
					decl = st.as<core::DeclarationStmtPtr>();
					// remove the init, use undefinedvar
					// this is what GCC does, VC simply errors out
					decl = builder.declarationStmt(decl->getVariable());
					decls.push_back(decl);
				}
			} else {
				decl = result.as<core::DeclarationStmtPtr>();
				// remove the init, use undefinedvar
				// this is what GCC does, VC simply errors out
				decl = builder.declarationStmt(decl->getVariable());
				decls.push_back(decl);
			}
		};

		// looks for inner cases inside of cases stmt, and returns the compound attached
		// 			case A
		// 				case B
		// 					stmt1
		// 					stmt2
		// 			break
		auto lookForCases = [this, &caseMap, &openCases, convertCase, addStmtToOpenCases, handleDeclStmt](const clang::SwitchCase* caseStmt) {
			const clang::Stmt* stmt = caseStmt;

			// we might find some chained stmts
			while(stmt && llvm::isa<clang::SwitchCase>(stmt)) {
				const clang::SwitchCase* inCase = llvm::cast<clang::SwitchCase>(stmt);
				openCases.push_back(convertCase(inCase));
				caseMap[openCases.back()] = std::vector<core::StatementPtr>();
				stmt = inCase->getSubStmt();
			}

			// after the case statements, we might find the statements to be executed
			if(stmt) {
				// it may happen that the first stmt after the case is a declstmt.
				// in this case we need to convert it and save it in the decl list.
				if(const clang::DeclStmt* declStmt = dyn_cast<clang::DeclStmt>(stmt)) {
					handleDeclStmt(declStmt);
				} else {
					addStmtToOpenCases(converter.convertStmt(stmt));
				}
			}
		};

		// iterate over statements inside of switch
		auto body = switchStmt->getBody();
		vector<clang::Stmt*> subStatments;
		clang::CompoundStmt* compStmt = dyn_cast<clang::CompoundStmt>(body);
		if(compStmt) std::copy(compStmt->body_begin(), compStmt->body_end(), std::back_inserter(subStatments));
		else subStatments.push_back(body);
		for(auto currStmt : subStatments) {
			// if it is a case stmt, create a literal and open it
			if(const clang::SwitchCase* switchCaseStmt = llvm::dyn_cast<clang::SwitchCase>(currStmt)) {
				lookForCases(switchCaseStmt);
				continue;
			} else if(const clang::DeclStmt* declStmt = llvm::dyn_cast<clang::DeclStmt>(currStmt)) {
				// collect all declarations which are in the switch body and add them (without init) to the cases
				handleDeclStmt(declStmt);
				continue;
			}

			// if it is whatever other kind of stmt append it to each of the open cases list
			addStmtToOpenCases(converter.convertStmt(currStmt));
		}

		// we need to sort the elements to assure same output for different memory alignment, valgrind problem
		std::set<core::LiteralPtr, litCompare> caseLiterals;
		for(auto pair : caseMap) {
			caseLiterals.insert(pair.first);
		}

		vector<core::SwitchCasePtr> cases;
		// initialize the default case with an empty compoundstmt
		core::CompoundStmtPtr defStmt = builder.compoundStmt();
		for(auto literal : caseLiterals) {
			if(literal != defLit) {
				cases.push_back(builder.switchCase(literal, builder.wrapBody(stmtutils::aggregateStmts(builder, caseMap[literal]))));
			} else {
				defStmt = builder.wrapBody(stmtutils::aggregateStmts(builder, caseMap[literal]));
			}
		}

		core::StatementPtr irSwitch = builder.switchStmt(condExpr, cases, defStmt);

		// add declarations at switch scope
		for(auto decl : decls) {
			retStmt.push_back(decl);
		}

		retStmt.push_back(irSwitch);
		retStmt = aggregateStmts(builder, retStmt);

		return retStmt;
	}

	/*
	 * A switch case should have been converted as part of its enclosing switch statement
	 */
	stmtutils::StmtWrapper Converter::StmtConverter::VisitSwitchCase(clang::SwitchCase* caseStmt) {
		frontend_assert(false) << "Visitor is visiting a 'case' stmt";
		return stmtutils::StmtWrapper();
	}

	stmtutils::StmtWrapper Converter::StmtConverter::VisitBreakStmt(clang::BreakStmt* breakStmt) {
		return stmtutils::StmtWrapper(builder.breakStmt());
	}

	stmtutils::StmtWrapper Converter::StmtConverter::VisitContinueStmt(clang::ContinueStmt* contStmt) {
		return stmtutils::StmtWrapper(builder.continueStmt());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {
		core::StatementPtr retStmt;
		LOG_STMT_CONVERSION(compStmt, retStmt);

		converter.getVarMan()->pushScope(true);
		core::StatementList stmtList;
		for(auto stmt : compStmt->body()) {
			stmtutils::StmtWrapper convertedStmt = converter.convertStmtToWrapper(stmt);
			for(auto stmt : convertedStmt) {
				if(stmt != builder.getNoOp()) stmtList.push_back(stmt);
			}
		}
		converter.getVarMan()->popScope();

		retStmt = builder.compoundStmt(stmtList);

		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitNullStmt(clang::NullStmt* nullStmt) {
		return builder.getNoOp();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							GOTO  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitGotoStmt(clang::GotoStmt* gotoStmt) {
		core::StatementPtr retStmt;
		LOG_STMT_CONVERSION(gotoStmt, retStmt);

		frontend_assert(false) << "Goto not implemented.";
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							LABEL  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitLabelStmt(clang::LabelStmt* labelStmt) {
		// core::StatementPtr retIr;
		// LOG_STMT_CONVERSION(labelStmt, retIr);
		core::StringValuePtr str = builder.stringValue(labelStmt->getName());
		stmtutils::StmtWrapper retIr = stmtutils::StmtWrapper(builder.labelStmt(str));

		clang::Stmt* stmt = labelStmt->getSubStmt();
		stmtutils::StmtWrapper retIr2 = converter.convertStmtToWrapper(stmt);
		std::copy(retIr2.begin(), retIr2.end(), std::back_inserter(retIr));
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ASM STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitAsmStmt(clang::AsmStmt* asmStmt) {
		// two subclasses - gccasmstmt/msasmstmt
		frontend_assert(false && "currently not implemented");
		return stmtutils::StmtWrapper();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							  STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::StmtConverter::VisitStmt(clang::Stmt* stmt) {
		// side effects?
		std::for_each(stmt->child_begin(), stmt->child_end(), [&](clang::Stmt* stmt) { converter.convertStmtToWrapper(stmt); });
		return stmtutils::StmtWrapper();
	}
}
}
}

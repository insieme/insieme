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

#include "insieme/core/printer/lua_printer.h"

#include <sstream>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace printer {

	namespace {

		class LuaConverter;

		typedef void(* OperatorConverter)(LuaConverter&, const CallExprPtr&);
		typedef utils::map::PointerMap<ExpressionPtr, OperatorConverter> OperatorConverterTable;

		const OperatorConverterTable& getDefaultConverterTable();



		class LuaConverter : public IRVisitor<> {

			const OperatorConverterTable& operatorTable;

			std::ostream& out;

			unsigned intend;

			bool outermostCall;

		public:

			LuaConverter(std::ostream& out)
				: operatorTable(getDefaultConverterTable()), out(out), intend(0), outermostCall(true) {}

			std::ostream& getOut() {
				return out;
			}

			void setOutermost(bool value = true) {
				outermostCall = value;
			}

		protected:

			void newLine() {
				out << "\n" << times("    ", intend);
			}

			void visitLiteral(const LiteralPtr& lit) {
				out << lit->getStringValue();
			}

			void visitCastExpr(const CastExprPtr& cast) {
				visit(cast->getSubExpression()); // cast are ignored!
			}

			void visitVariable(const VariablePtr& var) {
				out << *var;
			}

			void visitCallExpr(const CallExprPtr& call) {

				bool outermost = outermostCall;
				outermostCall = false;

				// test whether for the current call a special format has been registered
				auto function = call->getFunctionExpr();
				if (function->getNodeType() == NT_Literal) {
					auto pos = operatorTable.find(function);
					if (pos != operatorTable.end()) {
						if (!outermost) out << "(";
						pos->second(*this, call);
						if (!outermost) out << ")";
						return;
					}

					// check whether it is a build in
					if (call->getNodeManager().getLangBasic().isBuiltIn(function)) {
						throw LuaConversionException(call, "Unsupported built-in function!");
					}
				}

				// default formating
				this->visit(function);
				auto arguments = call->getArguments();
				if (arguments.empty()) {
					out << "()";
					return;
				}

				out << "(";
				auto begin = arguments.begin();
				this->visit(*begin);
				for_each(begin+1, arguments.end(), [&](const NodePtr& cur) {
					out << ", ";
					this->visit(cur);
				});
				out << ")";

			}


			// -- Statements --

			void visitBreakStmt(const BreakStmtPtr& stmt) {
				out << "break";
			}

//			void visitContinueStmt(const ContinueStmtPtr& stmt) {
//				// continue is not natively supported by lua => realized using label
//				out << "goto loop-end";
//			}

			void visitReturnStmt(const ReturnStmtPtr& stmt) {
				out << "return ";
				const auto& res = stmt.getReturnExpr();
				if (stmt->getNodeManager().getLangBasic().isUnitConstant(res)) {
					return;
				}
				visit(res);
			}

			void visitDeclarationStmt(const DeclarationStmtPtr& decl) {
				out << "local ";
				visit(decl->getVariable());
				out << " = ";
				visit(decl->getInitialization());
			}

			void visitCompoundStmt(const CompoundStmtPtr& stmts) {

				// start a block
				out << "do ";
				intend++;

				// process statements
				for_each(stmts->getStatements(), [&](const StatementPtr& cur) {
					this->newLine();
					this->outermostCall = true;
					this->visit(cur);
					out << ";";
				});

				// finish the block
				intend--;
				newLine();
				out << "end";
			}

			void visitIfStmt(const IfStmtPtr& stmt) {
				out << "if ";
				visit(stmt->getCondition());
				out << " then ";
				visit(stmt->getThenBody());
				out << " else ";
				visit(stmt->getElseBody());
				out << "end";
			}

			void visitWhileStmt(const WhileStmtPtr& stmt) {
				out << "while ";
				visit(stmt->getCondition());
				out << " ";
				visit(stmt->getBody());
			}

			void visitForStmt(const ForStmtPtr& stmt) {
				out << "for ";
				visit(stmt->getIterator());
				out << " = ";
				visit(stmt->getStart());
				out << " , (";

				visit(stmt->getEnd());
				out << " - 1";

				out << ") , ";
				visit(stmt->getStep());
				out << " ";
				visit(stmt->getBody());
			}

			void visitLambdaExpr(const LambdaExprPtr& lambda) {

				// skip recursive functions for now
				if (lambda->isRecursive()) {
					throw LuaConversionException(lambda, "Recursive functions are not supported yet!");
				}

				out << "(function ";

				const auto& params = lambda->getParameterList()->getParameters();
				out << "(";
				auto begin = params.begin();
				this->visit(*begin);
				for_each(begin+1, params.end(), [&](const NodePtr& cur) {
					out << ", ";
					this->visit(cur);
				});
				out << ") ";

				this->visit(lambda->getBody());
				out << " end)";
			}

			void visitNode(const NodePtr& node) {
				throw LuaConversionException(node, "Not implemented!");
			}

		};


		OperatorConverterTable buildDefaultConverterTable(NodeManager& manager);

		const OperatorConverterTable& getDefaultConverterTable() {
			static NodeManager manager;
			const static OperatorConverterTable table = buildDefaultConverterTable(manager);
			return table;
		}

		OperatorConverterTable buildDefaultConverterTable(NodeManager& manager) {
			const auto& basic = manager.getLangBasic();

			OperatorConverterTable res;

			#define OP_CONVERTER(Conversion) \
					&(((struct { \
						int dummy; \
						static void f(LuaConverter& converter, const CallExprPtr& call) Conversion \
					 }){0}).f)

			#define PRINT_ARG(index) ( converter.visit(call->getArgument(index)) )
			#define PRINT_EXPR(expr) ( converter.visit(expr) )
			#define OUT(code) ( converter.getOut() << code )


			// arithmetic operators

			res[basic.getSignedIntAdd()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" + "); PRINT_ARG(1); });
			res[basic.getSignedIntSub()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" - "); PRINT_ARG(1); });
			res[basic.getSignedIntMul()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" * "); PRINT_ARG(1); });
			res[basic.getSignedIntDiv()] = OP_CONVERTER({ OUT("math.floor("); PRINT_ARG(0); OUT(" / "); PRINT_ARG(1); OUT(")"); });
			res[basic.getSignedIntMod()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" % "); PRINT_ARG(1); });

			res[basic.getUnsignedIntAdd()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" + "); PRINT_ARG(1); });
			res[basic.getUnsignedIntSub()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" - "); PRINT_ARG(1); });
			res[basic.getUnsignedIntMul()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" * "); PRINT_ARG(1); });
			res[basic.getUnsignedIntDiv()] = OP_CONVERTER({ OUT("math.floor("); PRINT_ARG(0); OUT(" / "); PRINT_ARG(1); OUT(")"); });
			res[basic.getUnsignedIntMod()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" % "); PRINT_ARG(1); });

			res[basic.getRealAdd()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" + "); PRINT_ARG(1); });
			res[basic.getRealSub()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" - "); PRINT_ARG(1); });
			res[basic.getRealMul()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" * "); PRINT_ARG(1); });
			res[basic.getRealDiv()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" / "); PRINT_ARG(1); });


			// relational operators

			res[basic.getSignedIntEq()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" == "); PRINT_ARG(1); });
			res[basic.getSignedIntNe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" ~= "); PRINT_ARG(1); });
			res[basic.getSignedIntLt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" < "); PRINT_ARG(1); });
			res[basic.getSignedIntGt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" > "); PRINT_ARG(1); });
			res[basic.getSignedIntLe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" <= "); PRINT_ARG(1); });
			res[basic.getSignedIntGe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" >= "); PRINT_ARG(1); });

			res[basic.getUnsignedIntEq()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" == "); PRINT_ARG(1); });
			res[basic.getUnsignedIntNe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" ~= "); PRINT_ARG(1); });
			res[basic.getUnsignedIntLt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" < "); PRINT_ARG(1); });
			res[basic.getUnsignedIntGt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" > "); PRINT_ARG(1); });
			res[basic.getUnsignedIntLe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" <= "); PRINT_ARG(1); });
			res[basic.getUnsignedIntGe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" >= "); PRINT_ARG(1); });

			res[basic.getRealEq()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" == "); PRINT_ARG(1); });
			res[basic.getRealNe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" ~= "); PRINT_ARG(1); });
			res[basic.getRealLt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" < "); PRINT_ARG(1); });
			res[basic.getRealGt()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" > "); PRINT_ARG(1); });
			res[basic.getRealLe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" <= "); PRINT_ARG(1); });
			res[basic.getRealGe()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" >= "); PRINT_ARG(1); });


			// logical operators

			res[basic.getBoolEq()]   = OP_CONVERTER({ PRINT_ARG(0); OUT(" == "); PRINT_ARG(1); });
			res[basic.getBoolNe()]   = OP_CONVERTER({ PRINT_ARG(0); OUT(" ~= "); PRINT_ARG(1); });
			res[basic.getBoolLAnd()] = OP_CONVERTER({ PRINT_ARG(0); OUT(" and "); PRINT_ARG(1); });
			res[basic.getBoolLOr()]  = OP_CONVERTER({ PRINT_ARG(0); OUT(" or "); PRINT_ARG(1); });
			res[basic.getBoolLNot()] = OP_CONVERTER({ OUT(" not "); PRINT_ARG(0); });


			// ref operators

			res[basic.getRefAlloc()] = OP_CONVERTER({ converter.visit( static_pointer_cast<const core::CallExpr>(call->getArgument(0))->getArgument(0) ); });
			res[basic.getRefDeref()] = OP_CONVERTER({ PRINT_ARG(0); });
			res[basic.getRefDelete()] = OP_CONVERTER({ });
			res[basic.getRefAssign()] = OP_CONVERTER({
				converter.setOutermost();
				PRINT_ARG(0); OUT(" = "); PRINT_ARG(1);
			});


			// array operators

			res[basic.getArrayCreate1D()]     = OP_CONVERTER({ OUT("{}"); });
			res[basic.getArraySubscript1D()]  = OP_CONVERTER({ PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
			res[basic.getArrayRefElem1D()]    = OP_CONVERTER({ PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });


			// vector operators

			res[basic.getVectorSubscript()]  	= OP_CONVERTER({ PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
			res[basic.getVectorRefElem()]    	= OP_CONVERTER({ PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
			res[basic.getRefVectorToRefArray()] = OP_CONVERTER({ PRINT_ARG(0); });

			// struct operators

			res[basic.getCompositeRefElem()]      = OP_CONVERTER({ PRINT_ARG(0); OUT("."); PRINT_ARG(1); });
			res[basic.getCompositeMemberAccess()] = OP_CONVERTER({ PRINT_ARG(0); OUT("."); PRINT_ARG(1); });



			// special functions
			res[basic.getIfThenElse()]				= OP_CONVERTER({
				OUT("(");
				PRINT_ARG(0);
				NodeManager& manager = call->getNodeManager();
				OUT(" and ");
				PRINT_EXPR(transform::evalLazy(manager, call->getArgument(1)));
				OUT(" or ");
				PRINT_EXPR(transform::evalLazy(manager, call->getArgument(2)));
				OUT(")");
			});
			res[basic.getScalarToArray()] 			= OP_CONVERTER({ PRINT_ARG(0); });
			res[basic.getStringToCharPointer()] 	= OP_CONVERTER({ PRINT_ARG(0); });

			res[basic.getSelect()]	= OP_CONVERTER({

				IRBuilder builder(call->getNodeManager());

				auto& args = call->getArguments();
				OUT("(");
				PRINT_EXPR(builder.callExpr(call->getType(), args[2], args[0], args[1]));
				OUT(" and ");
				PRINT_ARG(0);
				OUT(" or ");
				PRINT_ARG(1);
				OUT(")");

			});

			#undef PRINT_ARG
			#undef OP_CONVERTER
			#undef OUT

			return res;
		}

	}


	std::ostream& LuaPrinter::printTo(std::ostream& out) const {
		// some helper functions
		LuaConverter(out).visit(stmt);
		return out;
	}

	string toLuaScript(const StatementPtr& stmt) {
		std::stringstream out;
		out << LuaPrinter(stmt);
		return out.str();
	}

	namespace {

		string buildMsg(const NodePtr& source, const string& msg) {
			std::stringstream res;
			res << "Unable to convert " << *source << " of type " << source->getNodeType() << " into Lua code.";
			if (!msg.empty()) {
				res << " Reason: " << msg;
			}
			return res.str();
		}
	}

	LuaConversionException::LuaConversionException(const NodePtr& source, const string& msg)
		: source(source), msg(buildMsg(source, msg)) {}

} // end namespace printer
} // end namespace core
} // end namespace insieme


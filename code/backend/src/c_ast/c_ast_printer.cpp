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

#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace backend {
namespace c_ast {

	namespace {


		// the actual printer of the C AST
		class CPrinter {

			std::ostream& out;
			string indentStep;
			int indent;

		public:

			CPrinter(std::ostream& out, const string& indentStep = "    ")
				: out(out), indentStep(indentStep), indent(0) {}

			std::ostream& print(NodePtr node) {
				switch(node->getType()) {
					#define CONCRETE(name) case NT_ ## name: print ## name (static_pointer_cast<name>(node)); return out;
					#include "insieme/backend/c_ast/c_nodes.def"
					#undef CONCRETE
				}
				assert(false && "Unknown C-Node type encountered!");
				return out;
			}

		private:

			std::ostream& newLine() {
				return out << "\n" << times(indentStep, indent);
			}

			void incIndent() {
				indent++;
			}

			void decIndent() {
				indent--;
				assert(indent >= 0 && "Should never become < 0");
			}

			#define PRINT(name) std::ostream& print ## name (name ## Ptr node)

			PRINT(Identifier) {
				return out << node->name;
			}

			PRINT(Comment) {
				return out << "/* " <<  node->comment << " */";
			}

			PRINT(OpaqueCode) {
				return out << node->code;
			}

			PRINT(PrimitiveType) {
				return print(node->name);
			}

			PRINT(PointerType) {
				return print(node->elementType) << "*";
			}

			PRINT(VectorType) {
				print(node->elementType);
				out << "[";
				print(node->size);
				out << "]";
				return out;
			}

			PRINT(StructType) {
				// TODO: implement a struct
				return out << "TODO - struct";
			}

			PRINT(UnionType) {
				// TODO: implement a struct
				return out << "TODO - union";
			}

			PRINT(VarDecl) {
				// print a variable declaration
				print(node->var->type);
				out << " ";
				print(node->var->name);

				if (!node->init) {
					return out;
				}
				out << " = ";
				return print(node->init);
			}

			PRINT(Compound) {
				out << "{";
				incIndent();
				newLine();

				std::size_t size = node->statements.size();
				for (std::size_t i=0; i<size; i++) {
					print(node->statements[i]);
					out << ";";
					if (i < size-1) newLine();
				}

				decIndent();
				newLine();
				out << "}";

				return out;
			}

			PRINT(If) {
				out << "if (";
				print(node->condition);
				out << ") ";
				print(node->thenStmt);
				if (node->elseStmt) {
					out << " else ";
					print(node->elseStmt);
				}
				return out;
			}

			PRINT(Switch) {
				out << "switch(";
				print(node->value);
				out << ") {";
				incIndent();
				newLine();

				std::size_t size = node->cases.size();
				for (std::size_t i=0; i<size; i++) {
					const std::pair<ExpressionPtr, StatementPtr>& cur = node->cases[i];
					out << "case ";
					print(cur.first);
					out << ": ";
					print(cur.second);
					if (i < size-1) newLine();
				}

				if (node->defaultBranch) {
					newLine();
					out << "default: ";
					print(node->defaultBranch);
				}

				decIndent();
				newLine();
				out << "}";
				return out;
			}

			PRINT(For) {
				out << "for (";
				print(node->init);
				out << "; ";
				print(node->check);
				out << "; ";
				print(node->step);
				out << ") ";
				print(node->body);
				return out;
			}

			PRINT(While) {
				out << "while (";
				print(node->condition);
				out << ") ";
				print(node->body);
				return out;
			}

			PRINT(Continue) {
				return out << "continue";
			}

			PRINT(Break) {
				return out << "break";
			}

			PRINT(Return) {
				out << "return ";
				return print(node->value);
			}

			PRINT(Literal) {
				return out << node->value;
			}

			PRINT(Variable) {
				printIdentifier(node->name);
				return out;
			}

			PRINT(Initializer) {
				out << "((";
				print(node->type);
				out << "){";

				std::size_t size = node->values.size();
				for(std::size_t i = 0; i<size; i++) {
					print(node->values[i]);
					if (i < size-1) out << ", ";
				}
				out << "})";

				return out;
			}

			PRINT(UnaryOperation) {

				// handle operations
				switch (node->operation) {
					case UnaryOperation::UnaryPlus: 	out << "+"; return print(node->operand);
					case UnaryOperation::UnaryMinus: 	out << "-"; return print(node->operand);
					case UnaryOperation::PrefixInc: 	out << "++"; return print(node->operand);
					case UnaryOperation::PrefixDec: 	out << "--"; return print(node->operand);
					case UnaryOperation::PostFixInc: 	print(node->operand); return out << "++";
					case UnaryOperation::PostFixDec: 	print(node->operand); return out << "--";
					case UnaryOperation::LogicNot: 		out << "!"; return print(node->operand);
					case UnaryOperation::BitwiseNot: 	out << "~"; return print(node->operand);
					case UnaryOperation::Indirection: 	out << "*"; return print(node->operand);
					case UnaryOperation::Reference: 	out << "&"; return print(node->operand);
					case UnaryOperation::SizeOf: 		out << "sizeof("; print(node->operand); return out << ")";
				}

				assert(false && "Invalid unary operation encountered!");
				return out;
			}

			PRINT(BinaryOperation) {

				string op = "";
				switch (node->operation) {
					case BinaryOperation::Assignment: 				op = " = "; break;
					case BinaryOperation::Additon: 					op = "+"; break;
					case BinaryOperation::Subtraction: 				op = "-"; break;
					case BinaryOperation::Multiplication: 			op = "*"; break;
					case BinaryOperation::Division: 				op = "/"; break;
					case BinaryOperation::Modulo: 					op = "%"; break;
					case BinaryOperation::Equal: 					op = "=="; break;
					case BinaryOperation::NotEqual: 				op = "!="; break;
					case BinaryOperation::GreaterThan: 				op = ">"; break;
					case BinaryOperation::LessThan: 				op = "<"; break;
					case BinaryOperation::GreaterOrEqual: 			op = ">="; break;
					case BinaryOperation::LessOrEqual: 				op = "<="; break;
					case BinaryOperation::LogicAnd: 				op = "&&"; break;
					case BinaryOperation::LogicOr: 					op = "||"; break;
					case BinaryOperation::BitwiseAnd: 				op = "&"; break;
					case BinaryOperation::BitwiseOr: 				op = "|"; break;
					case BinaryOperation::BitwiseXOr: 				op = "^"; break;
					case BinaryOperation::BitwiseLeftShift: 		op = "<<"; break;
					case BinaryOperation::BitwiseRightShift:		op = ">>"; break;
					case BinaryOperation::AdditionAssign: 			op = "+="; break;
					case BinaryOperation::SubtractionAssign: 		op = "-="; break;
					case BinaryOperation::MultiplicationAssign: 	op = "*="; break;
					case BinaryOperation::DivisionAssign: 			op = "/="; break;
					case BinaryOperation::ModuloAssign: 			op = "%="; break;
					case BinaryOperation::BitwiseAndAssign: 		op = "&="; break;
					case BinaryOperation::BitwiseOrAssign: 			op = "|="; break;
					case BinaryOperation::BitwiseXOrAssign: 		op = "^="; break;
					case BinaryOperation::BitwiseLeftShiftAssign: 	op = "<<="; break;
					case BinaryOperation::BitwiseRightShiftAssign: 	op = ">>="; break;
					case BinaryOperation::MemberAccess: 			op = "."; break;

					// special handling for subscript and cast
					case BinaryOperation::Subscript: print(node->operandA); out << "["; print(node->operandB); return out << "]";
					case BinaryOperation::Cast: out << "("; print(node->operandA); out << ")"; return print(node->operandB);

				}

				assert(op != "" && "Invalid binary operation encountered!");

				print(node->operandA);
				out << op;
				print(node->operandB);
				return out;
			}

			PRINT(TernaryOperation) {

				switch (node->operation) {
				case TernaryOperation::TernaryCondition : {
					print(node->operandA);
					out << "?";
					print(node->operandB);
					out << ":";
					print(node->operandC);
					return out;
				}
				}

				assert(false && "Invalid ternary operation encountered!");
				return out;
			}

			PRINT(Call) {

				printIdentifier(node->function);
				out << "(";

				std::size_t size = node->arguments.size();
				for(std::size_t i = 0; i<size; i++) {
					print(node->arguments[i]);
					if (i < size-1) out << ", ";
				}

				return out << ")";
			}

			PRINT(Parentheses) {
				out << "(";
				print(node->expression);
				return out << ")";
			}

			PRINT(TypePrototype) {
				return out << "TODO";
			}

			PRINT(TypeDef) {
				return out << "TODO";
			}

			PRINT(FunctionPrototype) {
				return out << "TODO";
			}

			PRINT(Function) {
				return out << "TODO";
			}

			#undef PRINT

		};


	}


	std::ostream& CPrint::printTo(std::ostream& out) const {
		// use internal printer to generate code
		return CPrinter(out).print(fragment);
	}

	string toString(const NodePtr& node) {
		return ::toString(CPrint(node));
	}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

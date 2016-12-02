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

#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/printable.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include <boost/algorithm/string/predicate.hpp>

namespace insieme {
namespace backend {
namespace c_ast {

	namespace {

		class CPrinter;

		struct PrintWrapper : public utils::Printable {

			CPrinter& printer;
			const NodePtr& node;

		  public:

			PrintWrapper(CPrinter& printer, const NodePtr& node) : printer(printer), node(node){};

			std::ostream& printTo(std::ostream& out) const;
		};

		// the actual printer of the C AST
		class CPrinter {
			string indentStep;
			int indent = 0;

		  public:
			CPrinter(const string& indentStep = "    ") : indentStep(indentStep) {}

			std::ostream& print(NodePtr node, std::ostream& out) {
				if(!node) { return out; }
				switch(node->getType()) {
				#define CONCRETE(name)                                                                                                                         \
					case NT_##name:                                                                                                                            \
						return print##name(static_pointer_cast<name>(node), out);
				#include "insieme/backend/c_ast/c_nodes.def"
				#undef CONCRETE
				}
				assert_fail() << "Unknown C-Node type encountered!";
				return out;
			}

		  private:
			PrintWrapper print(const NodePtr& node) {
				return PrintWrapper(*this, node);
			}

			template <typename... A>
			ParameterPrinter printParam(A... param) {
				return ParameterPrinter(param...);
			}

			ParameterPrinter printMemberParam(const vector<VariablePtr>& params) {
				vector<VariablePtr> subset(params.begin() + 1, params.end());
				return ParameterPrinter(subset);
			}

			std::ostream& newLine(std::ostream& out) {
				return out << "\n" << times(indentStep, indent);
			}

			void incIndent() {
				indent++;
			}

			void decIndent() {
				indent--;
				assert_ge(indent, 0) << "Should never become < 0";
			}

			#define PRINT(name) std::ostream& print##name(name##Ptr node, std::ostream& out)

			PRINT(Identifier) {
				return out << node->name;
			}

			PRINT(Comment) {
				return out << "/* " << node->comment << " */";
			}

			PRINT(OpaqueCode) {
				return out << node->code;
			}

			PRINT(IntegralType) {
				return out << print(node->value);
			}

			PRINT(PrimitiveType) {

				// print qualifiers
				if(node->isConst())    { out << "const "; }
				if(node->isVolatile()) { out << "volatile "; }

				switch(node->type) {
				case PrimitiveType::Void: return out << "void";
				case PrimitiveType::Bool: return out << "bool";
				case PrimitiveType::Char: return out << "char";
				case PrimitiveType::Int8: return out << "int8_t";
				case PrimitiveType::Int16: return out << "int16_t";
				case PrimitiveType::Int32: return out << "int32_t";
				case PrimitiveType::Int64: return out << "int64_t";
				case PrimitiveType::Int128: return out << "int128_t";
				case PrimitiveType::UInt8: return out << "uint8_t";
				case PrimitiveType::UInt16: return out << "uint16_t";
				case PrimitiveType::UInt32: return out << "uint32_t";
				case PrimitiveType::UInt64: return out << "uint64_t";
				case PrimitiveType::UInt128: return out << "uint128_t";
				case PrimitiveType::Float: return out << "float";
				case PrimitiveType::Double: return out << "double";
				case PrimitiveType::LongLong: return out << "long long";
				case PrimitiveType::ULongLong: return out << "unsigned long long";
				}
				assert_fail() << "Unsupported primitive type!";
				return out << "/* unsupported primitive type */";
			}

			PRINT(NamedType) {

				// print qualifiers
				if(node->isConst())    { out << "const "; }
				if(node->isVolatile()) { out << "volatile "; }

				// print name
				out << print(node->name);
				if(node->isGenericType || !node->parameters.empty()) {
					out << "<" << join(", ", node->parameters, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << " >";
				}
				return out;
			}

			PRINT(PointerType) {
				// to print a pointer type we just use the variable facility but we trick it by using a
				// variable with no name so it will never show off
				return out << ParameterPrinter(node, node->getManager()->create(""));
			}

			PRINT(ReferenceType) {
				// print referenced type
				out << print(node->elementType);

				// print qualifiers
				if(node->isConst())    { out << " const"; }
				if(node->isVolatile()) { out << " volatile"; }

				return out << "&";
			}

			PRINT(RValueReferenceType) {
				// print referenced type
				out << print(node->elementType);

				// print qualifiers
				if(node->isConst())    { out << " const"; }
				if(node->isVolatile()) { out << " volatile"; }

				return out << "&&";
			}

			PRINT(VectorType) {
				return out << print(node->elementType) << "[" << print(node->size) << "]";
			}

			PRINT(ComplexType) {
				return out << "_Complex " << print(node->elementType);
			}

			PRINT(StructType) {
				return out << print(node->name);
			}

			PRINT(UnionType) {
				return out << print(node->name);
			}

			PRINT(FunctionType) {
				return out << print(node->returnType) << "("
				           << join(",", node->parameterTypes, [&](std::ostream& out, const TypePtr& cur) { out << print(cur); }) << ")";
			}

			PRINT(VarArgsType) {
				return out << "...";
			}

			PRINT(AttributedType) {
				return out << node->attribute << " " << print(node->type);
			}

			PRINT(EnumType) {
				out << "typedef enum " << print(node->name);
				// int type should only be printed when using c++11 enum int types
				if(node->intType) { out << " : " << print(node->intType); }
				out << " { " <<
					join(", ", node->values, [&](std::ostream& out, const std::pair<IdentifierPtr,LiteralPtr>& cur) {
						out << print(cur.first);
						if(!cur.second->value.empty()) {
							out << "=" << print(cur.second);
						}
				});
				return out << " } " << print(node->name) << ";";
			}

			PRINT(MemberFieldPointer) {
				assert_fail() << "this should never be reached, we can not define a member pointer without name";
				return out << " /* this type should not be here*/ ";
			}


			PRINT(VarDecl) {
				// handle static modifier
				if(node->isStatic) { out << "static "; }

				// a utility function printing the init part
				auto printInit = [&](const c_ast::ExpressionPtr& expr) {
					if(!expr) { return; }
					out << " = " << print(expr);
				};

				// handle single-variable declaration ...
				if(node->varInit.size() == 1u) {
					// print a variable declaration
					out << printParam(node->varInit[0].first);

					// add constructor call if necessary
					ConstructorCallPtr call = node->varInit[0].second.isa<ConstructorCallPtr>();
					auto vt = node->varInit[0].first->type;
					if(call && !vt.isa<c_ast::ReferenceTypePtr>() && !vt.isa<c_ast::RValueReferenceTypePtr>()) {
						// do nothing if it is default constructed
						if(call->arguments.empty()) { return out; }

						// just add list of parameters
						return out << "(" << join(", ", call->arguments, [&](std::ostream& out, const NodePtr& cur) { out << "(" << print(cur) << ")"; })
						           << ")";
					}

					// add init value
					printInit(node->varInit[0].second);

					// done
					return out;
				}

				// multiple declarations
				assert_gt(node->varInit.size(), 1u);

				// start with type
				out << print(node->varInit[0].first->type);

				// add name/value pair
				return out << " " << join(", ", node->varInit, [&](std::ostream& out, const pair<VariablePtr, ExpressionPtr>& cur) {
					       out << print(cur.first);
					       printInit(cur.second);
					   });
			}

			PRINT(Compound) {
				// short-cut for empty blocks
				if(node->statements.empty()) { return out << "{ }"; }

				out << "{";
				incIndent();
				newLine(out);

				std::size_t size = node->statements.size();
				for(std::size_t i = 0; i < size; i++) {
					const NodePtr& cur = node->statements[i];
					out << print(cur);
					//					auto type = cur->getType();
					//					if (type != NT_For && type != NT_While && type != NT_If && type != NT_Switch && type != NT_Compound) {
					out << ";";
					//					}
					if(i < size - 1) { newLine(out); }
				}

				decIndent();
				newLine(out);
				out << "}";

				return out;
			}

			PRINT(If) {
				c_ast::StatementPtr thenStmt = node->thenStmt;
				c_ast::StatementPtr elseStmt = node->elseStmt;

				// wrap then stmt into compound block if necessary
				if(thenStmt->getType() != c_ast::NT_Compound) { thenStmt = thenStmt->getManager()->create<c_ast::Compound>(thenStmt); }
				out << "if (" << print(node->condition) << ") " << print(thenStmt);

				// skip else stmt if not present
				if(!node->elseStmt) { return out; }

				// wrap else stmt into compound block if necessary
				if(elseStmt->getType() != c_ast::NT_Compound) { elseStmt = elseStmt->getManager()->create<c_ast::Compound>(elseStmt); }
				return out << " else " << print(elseStmt);
			}

			PRINT(Switch) {
				out << "switch(" << print(node->value) << ") {";
				incIndent();
				newLine(out);

				std::size_t size = node->cases.size();
				for(std::size_t i = 0; i < size; i++) {
					const std::pair<ExpressionPtr, StatementPtr>& cur = node->cases[i];
					out << "case " << print(cur.first) << ": " << print(cur.second);
					if(cur.second->getType() != c_ast::NT_Compound) { out << ";"; }
					out << " break;";
					if(i < size - 1) { newLine(out); }
				}

				if(node->defaultBranch) {
					newLine(out);
					out << "default: " << print(node->defaultBranch);
					if(node->defaultBranch->getType() != c_ast::NT_Compound) { out << ";"; }
				}

				decIndent();
				newLine(out);
				out << "}";
				return out;
			}

			PRINT(For) {
				out << "for (" << print(node->init) << "; " << print(node->check) << "; " << print(node->step) << ") ";

				NodePtr body = node->body;
				if(body->getType() != NT_Compound) { body = c_ast::compound(body); }
				return out << print(body);
			}

			PRINT(While) {
				return out << "while (" << print(node->condition) << ") " << print(node->body);
			}

			PRINT(TryCatch) {
				out << "try " << print(node->body);
				for(auto cur : node->clauses) {
					out << " catch(";
					if(cur.var) {
						out << printParam(cur.var);
					} else {
						out << "...";
					}
					out << ") " << print(cur.body);
				}
				return out;
			}

			PRINT(Continue) {
				return out << "continue";
			}

			PRINT(Break) {
				return out << "break";
			}

			PRINT(Return) {
				out << "return";
				if(node->value) { // if there is a return value ...
					out << " " << print(node->value);
				}
				return out;
			}

			PRINT(Throw) {
				return out << "throw " << print(node->value);
			}

			PRINT(Goto) {
				return out << "goto " << node->value;
			}

			PRINT(Label) {
				return out << node->value << ": ";
			}

			PRINT(Literal) {
				return out << node->value;
			}

			PRINT(Variable) {
				return printIdentifier(node->name, out);
			}

			PRINT(Initializer) {
				if(node->type) { out << "INS_INIT(" << print(node->type) << ")"; }
				return out << "{" << join(", ", node->values, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << "}";
			}

			PRINT(DesignatedInitializer) {
				out << "INS_INIT(" << print(node->type) << ")";
				out << "{ ";
				// special handling for anonymous inner records
				if(!node->member->name.empty()) {
					out << "." << print(node->member) << " = ";
				}
				out << print(node->value) << " }";
				return out;
			}

			PRINT(ArrayInit) {
				out << print(node->type);
				for(auto cur : node->size) {
					out << "[" << print(cur) << "]";
				}
				return out;
			}

			PRINT(VectorInit) {
				return out << "{" << join(", ", node->values, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << "}";
			}

			PRINT(UnaryOperation) {
				// handle operations
				switch(node->operation) {
				case UnaryOperation::UnaryPlus: return out << "+" << print(node->operand);
				case UnaryOperation::UnaryMinus: return out << "-" << print(node->operand);
				case UnaryOperation::PrefixInc: return out << "++" << print(node->operand);
				case UnaryOperation::PrefixDec: return out << "--" << print(node->operand);
				case UnaryOperation::PostfixInc: return out << print(node->operand) << "++";
				case UnaryOperation::PostfixDec: return out << print(node->operand) << "--";
				case UnaryOperation::LogicNot: return out << "!" << print(node->operand);
				case UnaryOperation::BitwiseNot: return out << "~" << print(node->operand);
				case UnaryOperation::Indirection: return out << "*" << print(node->operand);
				case UnaryOperation::Reference: return out << "&" << print(node->operand);
				case UnaryOperation::SizeOf: return out << "sizeof(" << print(node->operand) << ")";
				case UnaryOperation::Typeid: return out << "typeid(" << print(node->operand) << ")";
				case UnaryOperation::New: return out << "new " << print(node->operand);
				case UnaryOperation::Delete: return out << "delete " << print(node->operand);
				case UnaryOperation::DeleteArray: return out << "delete[] " << print(node->operand);
				case UnaryOperation::ComplexReal: return out << "__real__ " << print(node->operand);
				case UnaryOperation::ComplexImag: return out << "__imag__ " << print(node->operand);
				}

				assert_fail() << "Invalid unary operation encountered!";
				return out;
			}

			PRINT(BinaryOperation) {
				string op = "";
				switch(node->operation) {
				case BinaryOperation::Assignment: op = " = "; break;
				case BinaryOperation::Additon: op = " + "; break;
				case BinaryOperation::Subtraction: op = " - "; break;
				case BinaryOperation::Multiplication: op = " * "; break;
				case BinaryOperation::Division: op = " / "; break;
				case BinaryOperation::Modulo: op = " % "; break;
				case BinaryOperation::Equal: op = " == "; break;
				case BinaryOperation::NotEqual: op = " != "; break;
				case BinaryOperation::GreaterThan: op = " > "; break;
				case BinaryOperation::LessThan: op = " < "; break;
				case BinaryOperation::GreaterOrEqual: op = " >= "; break;
				case BinaryOperation::LessOrEqual: op = " <= "; break;
				case BinaryOperation::LogicAnd: op = " && "; break;
				case BinaryOperation::LogicOr: op = " || "; break;
				case BinaryOperation::BitwiseAnd: op = " & "; break;
				case BinaryOperation::BitwiseOr: op = " | "; break;
				case BinaryOperation::BitwiseXOr: op = "^"; break;
				case BinaryOperation::BitwiseLeftShift: op = "<<"; break;
				case BinaryOperation::BitwiseRightShift: op = ">>"; break;
				case BinaryOperation::AdditionAssign: op = "+="; break;
				case BinaryOperation::SubtractionAssign: op = "-="; break;
				case BinaryOperation::MultiplicationAssign: op = "*="; break;
				case BinaryOperation::DivisionAssign: op = "/="; break;
				case BinaryOperation::ModuloAssign: op = "%="; break;
				case BinaryOperation::BitwiseAndAssign: op = "&="; break;
				case BinaryOperation::BitwiseOrAssign: op = "|="; break;
				case BinaryOperation::BitwiseXOrAssign: op = "^="; break;
				case BinaryOperation::BitwiseLeftShiftAssign: op = "<<="; break;
				case BinaryOperation::BitwiseRightShiftAssign: op = ">>="; break;
				case BinaryOperation::MemberAccess: op = "."; break;
				case BinaryOperation::IndirectMemberAccess: op = "->"; break;
				case BinaryOperation::Comma: op = ","; break;

				// special handling for subscript and cast
				case BinaryOperation::Subscript: return out << print(node->operandA) << "[" << print(node->operandB) << "]";
				case BinaryOperation::Cast: return out << "(" << print(node->operandA) << ")" << print(node->operandB);

				case BinaryOperation::StaticCast: return out << "static_cast<" << print(node->operandA) << ">(" << print(node->operandB) << ")";
				case BinaryOperation::DynamicCast: return out << "dynamic_cast<" << print(node->operandA) << ">(" << print(node->operandB) << ")";

				case BinaryOperation::ScopeResolution: op = "::"; break;
				case BinaryOperation::PointerToMember: op = "->*"; break;
				}

				assert(op != "" && "Invalid binary operation encountered!");

				// avoid /* literal by composition of / and dereferencing *
				if(node->operation == BinaryOperation::Division) {
					if(node->operandB->getType() == NT_UnaryOperation) {
						c_ast::UnaryOperationPtr opB = static_pointer_cast<UnaryOperation>(node->operandB);
						if(opB->operation == UnaryOperation::Indirection) { return out << print(node->operandA) << op << " " << print(node->operandB); }
					}
				}

				// handle as usual
				return out << print(node->operandA) << op << print(node->operandB);
			}

			PRINT(TernaryOperation) {
				switch(node->operation) {
				case TernaryOperation::TernaryCondition: {
					return out << print(node->operandA) << "?" << print(node->operandB) << ":" << print(node->operandC);
				}
				case TernaryOperation::NewArray: {
					out << "new " << print(node->operandA) << "[" << print(node->operandB) << "]";
					if(node->operandC) out << print(node->operandC);
					return out;
				}
				}

				assert_fail() << "Invalid ternary operation encountered!";
				return out;
			}

			PRINT(ExplicitInstantiation) {
				// <subexpr> "<" instantiation types ">"
				out << print(node->subExpr);
				return out << "<" << join(", ", node->instantiationTypes, [&](std::ostream& out, const TypePtr& cur) { out << print(cur); }) << " >";
			}

			PRINT(Call) {
				// <function> ( <arguments> )
				out << print(node->function);
				return out << "(" << join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << ")";
			}

			PRINT(MemberCall) {
				// <obj> . <function> ( <arguments> )
				out << print(node->object) << "." << print(node->memberFun);
				return out << "(" << join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << ")";
			}

			PRINT(ConstructorCall) {
				// <new> <className> ( <arguments> )
				out << ((node->location) ? "new " : "");

				// the location for a placement new
				if(node->location) { out << "(" << print(node->location) << ") "; }

				// the rest
				return out << print(node->classType) << "(" << join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); })
				           << ")";
			}

			PRINT(DestructorCall) {
				// <location> . <classType> :: ~<classType> ( )
				out << print(node->location) << ".";
				if(!node->isVirtual) { out << print(node->classType) << "::"; }
				out << "~" << print(node->classType) << "()";
				return out;
			}

			PRINT(Parentheses) {
				return out << "(" << print(node->expression) << ")";
			}

			PRINT(OpaqueExpr) {
				return out << node->value;
			}

			PRINT(StmtExpr) {
				return out << print(node->stmt);
			}

			PRINT(TypeDeclaration) {
				// forward declaration + type definition
				bool isStruct = (node->type->getNodeType() == NT_StructType);

				// forward declaration
				out << ((isStruct) ? "struct " : "union ") << print(node->type) << ";\n";

				// type definition
				out << "typedef " << ((isStruct) ? "struct " : "union ") << print(node->type) << " " << print(node->type) << ";\n";

				// done
				return out;
			}

			PRINT(FunctionPrototype) {
				// <returnType> name ( <parameter list> );
				FunctionPtr fun = node->function;
				return out << print(fun->returnType) << " " << print(fun->name) << "(" << printParam(fun->parameter) << ");\n";
			}

			PRINT(ConstructorPrototype) {
				// <class name> ( <parameter list > );
				auto ctor = node->ctor;
				return out << print(ctor->className) << "(" << printMemberParam(ctor->function->parameter) << ")" << node->flag;
			}

			PRINT(DestructorPrototype) {
				// <virtual> ~ <class name> ( <parameter list > );
				auto dtor = node->dtor;
				return out << (node->isVirtual ? "virtual " : "") << "~" << print(dtor->className) << "()" << node->flag;
			}

			PRINT(MemberFunctionPrototype) {
				// <virtual> <return type> <name> ( <parameter list > ) <const>;
				auto fun = node->fun->function;
				if (node->fun->isStatic) out << "static ";
				if (node->isVirtual) out << "virtual ";
				out << (boost::starts_with(fun->name->name, "operator ") ? "" : toC(fun->returnType) + " ");
				out << print(fun->name) << "(";
				if (node->fun->isStatic) {
					out << printParam(fun->parameter);
				} else {
					out << printMemberParam(fun->parameter);
				}
				out << ")";
				if (node->fun->isConstant) out << " const";
				if (node->fun->isVolatile) out << " volatile";
				out << node->flag;
				if (node->pureVirtual) out << " =0";
				return out;
			}

			PRINT(GlobalVarDecl) {
				out << (node->external ? "extern " : "") << ParameterPrinter(node->type, node->getManager()->create(node->name));

				// add constructor call if necessary
				if(ConstructorCallPtr call = node->init.isa<ConstructorCallPtr>()) {
					// do nothing if it is default constructed
					if(call->arguments.empty()) { return out << ";\n"; }

					// just add list of parameters
					return out << "(" << join(", ", call->arguments, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << ")"
					           << ";\n";
				}

				if(node->init) {
					out << " = " << print(node->init);
				}
				return out << ";\n";
			}

			PRINT(Parent) {
				return out << (node->isVirtual ? "virtual " : "") << print(node->parent);
			}

			std::ostream& printRecordDefinition(c_ast::TypePtr type, std::ostream& out, bool anonymous = false) {
				// handle struct / union types
				c_ast::NamedCompositeTypePtr composite = dynamic_pointer_cast<c_ast::NamedCompositeType>(type);
				assert_true(composite) << "Must be a struct or union type!";

				// special handling for struct types
				c_ast::StructTypePtr structType = dynamic_pointer_cast<c_ast::StructType>(composite);

				// define struct / type as part of a type definition (C/C++ compatible)
				out << ((structType) ? "struct" : "union") << " ";
				if(!anonymous) out << print(composite->name);

				// print parents
				if(structType && !structType->parents.empty()) {
					out << " : public " << join(", public ", structType->parents, [&](std::ostream& out, const ParentPtr& cur) { out << print(cur); });
				}

				// start definition
				out << " {";

				// add fields
				if(!composite->elements.empty()) { out << "\n    "; }
				out << join("\n    ", composite->elements, [&](std::ostream& out, const VariablePtr& cur) {
					// special handling for anonymous struct/union members
					if(cur->name->name.empty()) {
						printRecordDefinition(cur->type, out, true);
						return;
					}
					out << printParam(cur) << ";";
				});
				if(!composite->elements.empty()) { out << ";"; }

				// add member functions
				if(composite) {
					// add constructors
					if(!composite->ctors.empty()) { out << "\n    "; }
					out << join(";\n    ", composite->ctors, [&](std::ostream& out, const ConstructorPrototypePtr& cur) { out << print(cur); });
					if(!composite->ctors.empty()) { out << ";"; }

					// add destructor
					if(composite->dtor) { out << "\n    "; }
					out << print(composite->dtor);
					if(composite->dtor) { out << ";"; }


					// add member functions
					if(!composite->members.empty()) { out << "\n    "; }
					out << join(";\n    ", composite->members, [&](std::ostream& out, const MemberFunctionPrototypePtr& cur) { out << print(cur); });
					if(!composite->members.empty()) { out << ";"; }
				}

				// add remaining elements
				for(const auto& cur : composite->others) {
					out << "\n    " << print(cur);
				}

				// finish type definition
				return out << "\n};\n";
			}

			PRINT(TypeDefinition) {
				// handle type definitions
				if((bool)(node->name)) {
					// since here is the only place where we have type + name, we have to take care of
					// function type declarations

					// function type declaration need to be parametrized
					//	if (node->type->getNodeType() == NT_FunctionType  || node->type->getNodeType() == NT_PointerType ){
					return out << "typedef " << ParameterPrinter(node->type, node->name) << ";\n";
				}

				return printRecordDefinition(node->type, out);
			}

			PRINT(TypeAlias) {
				return out << "using " << print(node->type) << " = " << print(node->definition) << ";\n";
			}

			c_ast::StatementPtr wrapBody(const c_ast::StatementPtr& body) {
				if(body->getType() == c_ast::NT_Compound) { return body; }
				return body->getManager()->create<c_ast::Compound>(body);
			}

			PRINT(Function) {
				// <returnType> name ( <parameter list> ) <body> \n
				if(node->flags & Function::STATIC) { out << "static "; }
				if(node->flags & Function::INLINE) { out << "inline "; }
				if(node->flags & Function::OCL_KERNEL) { out << "__kernel "; }

				out << print(node->returnType) << " " << print(node->name) << "(" << printParam(node->parameter) << ") ";

				return out << print(wrapBody(node->body));
			}

			PRINT(Constructor) {
				// <className> :: <name> ( <parameter list> ) : <initializer_list>  <body> \n

				auto fun = node->function;

				// print header
				out << print(node->className) << "::" << print(node->className) << "(" << printMemberParam(fun->parameter) << ") ";

				// add initializer list
				if(!node->initialization.empty()) {
					out << ": " << join(", ", node->initialization, [&](std::ostream& out, const Constructor::InitializerListEntry& cur) {
						out << print(cur.first) << "(" << join(", ", cur.second, [&](std::ostream& out, const NodePtr& cur) { out << print(cur); }) << ")";
					}) << " ";
				}

				// print body
				return out << print(wrapBody(fun->body));
			}

			PRINT(Destructor) {
				// ~ <className> :: <name> ( ) <body> \n

				auto fun = node->function;

				// print header
				out << print(node->className) << "::~" << print(node->className) << "() ";

				// print body
				return out << print(wrapBody(fun->body));
			}

			PRINT(MemberFunction) {
				// <returnType> <className> :: <name> ( <parameter list> ) <const> <body> \n

				auto fun = node->function;

				// print header
				out << (boost::starts_with(fun->name->name, "operator ") ? "" : toC(fun->returnType) + " ") << print(node->className)
				    << "::" << print(fun->name) << "(";

				if (node->isStatic) {
					out << printParam(fun->parameter);
				} else {
					out << printMemberParam(fun->parameter);
				}

				out << ")" << (node->isConstant ? " const" : "") << (node->isVolatile ? " volatile" : "") << " ";

				// print body
				return out << print(wrapBody(fun->body));
			}

			PRINT(Namespace) {
				// namespace <name> { \n <inner def> \n }

				out << "namespace " << print(node->name) << "{";
				incIndent();
				newLine(out);

				// print definition
				out << print(node->definition);

				decIndent();
				newLine(out);
				out << "}";
				newLine(out);
				return out;
			}

			PRINT(ExternC) {
				// extern "C" { \n <inner def> \n }

				out << "extern \"C\" {";
				incIndent();
				newLine(out);

				// print definitions
				for(auto definition : node->definitions) {
					out << print(definition);
				}

				decIndent();
				out << "}";
				newLine(out);
				return out;
			}

			#undef PRINT
		};


		std::ostream& PrintWrapper::printTo(std::ostream& out) const {
			return (node) ? printer.print(node, out) : out;
		}

		enum PointerQualifier {
			PLAIN = 0, CONST = 1, VOLATILE = 2
		};

		struct TypeLevel {
			vector<PointerQualifier> qualifier; // pointer qualifiers
			vector<ExpressionPtr> subscripts;
			vector<TypePtr> parameters;
			IdentifierPtr owner;
			bool hasParameters;
			TypeLevel() : qualifier(), hasParameters(false) {}
		};

		IdentifierPtr getNameFrom(const TypePtr& type) {
			// could only be a struct or named type ...
			if(auto structType = type.isa<StructTypePtr>()) {
				return structType->name;
			} else if(auto namedType = type.isa<NamedTypePtr>()) {
				return namedType->name;
			}
			return IdentifierPtr();
		}

		typedef vector<TypeLevel> TypeNesting;
		typedef TypeNesting::const_iterator NestIterator;

		TypePtr computeNesting(TypeNesting& data, const TypePtr& type) {
			// check whether there is something to do
			if(type->getType() != NT_PointerType && type->getType() != NT_VectorType && type->getType() != NT_FunctionType
			   && type->getType() != NT_MemberFieldPointer) {
				return type;
			}

			TypePtr cur = type;
			TypeLevel res;

			// collect vector sizes
			while(cur->getType() == NT_VectorType) {
				VectorTypePtr vectorType = static_pointer_cast<VectorType>(cur);
				res.subscripts.push_back(vectorType->size);
				cur = vectorType->elementType;
			}

			// collect function parameters
			if((cur->getType() == NT_FunctionType) || (cur->getType() == NT_MemberFieldPointer)) {
				// if vectors have already been processed => continue with next level
				if(!res.subscripts.empty()) {
					auto innermost = computeNesting(data, cur);
					data.push_back(res);
					return innermost;
				}

				if(cur->getType() == NT_FunctionType) {
					FunctionTypePtr funType = static_pointer_cast<FunctionType>(cur);
					copy(funType->parameterTypes, std::back_inserter(res.parameters));
					res.hasParameters = true;
					res.owner = getNameFrom(cur.as<FunctionTypePtr>()->classType);
					cur = funType->returnType;
				}
				if(cur->getType() == NT_MemberFieldPointer) {
					res.hasParameters = false;
					res.owner = getNameFrom(cur.as<MemberFieldPointerPtr>()->parentType);
					cur = static_pointer_cast<MemberFieldPointer>(cur)->type;
					data.push_back(res);
					res.qualifier.push_back(PLAIN);
				}
			}

			// count pointers
			while(cur->getType() == NT_PointerType) {
				PointerTypePtr ptr = cur.as<PointerTypePtr>();
				cur = static_pointer_cast<PointerType>(cur)->elementType;
				res.qualifier.push_back(
						PointerQualifier(
							((ptr->isConst()) ? CONST : PLAIN) | ((ptr->isVolatile()) ? VOLATILE : PLAIN)
						)
				);
			}

			// resolve rest recursively
			auto innermost = computeNesting(data, cur);

			// add pair to result
			data.push_back(res);

			return innermost;
		}

		std::ostream& printName(std::ostream& out, const IdentifierPtr& name) {
			if(name->name.empty()) { return out; }
			return out << " " << CPrint(name);
		}

		std::ostream& printTypeNest(std::ostream& out, NestIterator level_it, NestIterator end, const IdentifierPtr& name) {
			// terminal case ...
			if(level_it == end) { return printName(out, name); }

			// print pointers ...
			const TypeLevel& cur = *level_it;
			for(auto it = cur.qualifier.rbegin(); it != cur.qualifier.rend(); it++) {
				out << "*";
				if (*it & CONST) out << " const";
				if (*it & VOLATILE) out << " volatile";
			}

			++level_it;
			if(level_it != end) {
				out << "(";

				// here is the place to print any membership of a function pointer
				if(cur.owner) {
					out << CPrint(cur.owner);
					out << "::";
				}

				// print nested recursively
				printTypeNest(out, level_it, end, name);

				out << ")";
			} else {
				printName(out, name);
			}

			// check whether one or the other is empty
			assert_true(!(!cur.parameters.empty() && !cur.subscripts.empty())) << "Only one component may be non-empty!";

			// print vector sizes
			out << join("", cur.subscripts, [&](std::ostream& out, const ExpressionPtr& cur) { out << "[" << CPrint(cur) << "]"; });


			// print parameter list
			if(cur.hasParameters) {
				out << "(" << join(",", cur.parameters, [&](std::ostream& out, const TypePtr& cur) { out << CPrint(cur); }) << ")";
			}

			return out;
		}
	}


	std::ostream& ParameterPrinter::printTo(std::ostream& out) const {
		c_ast::VariablePtr var;

		return out << join(", ", params, [](std::ostream& out, const c_ast::VariablePtr& var) {
			       // special handling for varargs
			       if(var->type->getType() == c_ast::NT_VarArgsType) {
				       out << "...";
				       return;
			       }

			       // special handling for pointer to vectors
			       TypeNesting nesting;
			       TypePtr innermost = computeNesting(nesting, var->type);
			       out << CPrint(innermost);
			       printTypeNest(out, nesting.begin(), nesting.end(), var->name);
			   });
	}


	std::ostream& CPrint::printTo(std::ostream& out) const {
		// use internal printer to generate code
		return CPrinter().print(fragment, out);
	}

	string toC(const NodePtr& node) {
		if(!node) { return "/* null */"; }
		return ::toString(CPrint(node));
	}

	string toC(const c_ast::CodeFragmentPtr& fragment) {
		if(!fragment) { return "/* null */"; }
		return ::toString(*fragment);
	}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

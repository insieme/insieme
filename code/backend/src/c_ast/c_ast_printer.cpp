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
#include "insieme/utils/printable.h"

#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace c_ast {

	namespace {

		class CPrinter;

		struct PrintWrapper : public utils::Printable {
			CPrinter& printer;
			const NodePtr& node;
		public:
			PrintWrapper(CPrinter& printer, const NodePtr& node)
				: printer(printer), node(node) {};
			virtual std::ostream& printTo(std::ostream& out) const;
		};


		// the actual printer of the C AST
		class CPrinter {

			string indentStep;
			int indent;

		public:

			CPrinter(const string& indentStep = "    ")
				: indentStep(indentStep), indent(0) {}

			std::ostream& print(NodePtr node, std::ostream& out) {
				if (!node) return out;
				switch(node->getType()) {
					#define CONCRETE(name) case NT_ ## name: return print ## name (static_pointer_cast<name>(node), out);
					#include "insieme/backend/c_ast/c_nodes.def"
					#undef CONCRETE
				}
				assert(false && "Unknown C-Node type encountered!");
				return out;
			}

		private:

			PrintWrapper print(const NodePtr& node) {
				return PrintWrapper(*this, node);
			}

			template<typename ... A>
			ParameterPrinter printParam(A ... param) {
				return ParameterPrinter(param...);
			}

			ParameterPrinter printMemberParam(const vector<VariablePtr>& params) {
				vector<VariablePtr> subset(params.begin()+1, params.end());
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
				assert(indent >= 0 && "Should never become < 0");
			}

			#define PRINT(name) std::ostream& print ## name (name ## Ptr node, std::ostream& out)

			PRINT(Identifier) {
				return out << node->name;
			}

			PRINT(Comment) {
				return out << "/* " <<  node->comment << " */";
			}

			PRINT(OpaqueCode) {
				return out << node->code;
			}

			PRINT(ModifiedType) {
				if (node->hasMod(ModifiedType::VOLATILE)) out << "volatile ";
				if (node->hasMod(ModifiedType::CONST)) out << "const ";
				return out << print(node->type);
			}

			PRINT(PrimitiveType) {
				switch(node->type) {
				case PrimitiveType::Void : return out << "void";
				case PrimitiveType::Bool : return out << "bool";
				case PrimitiveType::Char : return out << "char";
				case PrimitiveType::Int8 : return out << "int8_t";
				case PrimitiveType::Int16 : return out << "int16_t";
				case PrimitiveType::Int32 : return out << "int32_t";
				case PrimitiveType::Int64 : return out << "int64_t";
				case PrimitiveType::UInt8 : return out << "uint8_t";
				case PrimitiveType::UInt16 : return out << "uint16_t";
				case PrimitiveType::UInt32 : return out << "uint32_t";
				case PrimitiveType::UInt64 : return out << "uint64_t";
				case PrimitiveType::Float : return out << "float";
				case PrimitiveType::Double : return out << "double";
				}
				assert(false && "Unsupported primitive type!");
				return out << "/* unsupported primitive type */";
			}

			PRINT(NamedType) {
				out << print(node->name);
				if (!node->parameters.empty()) {
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
				if (node->isConst) out << "const ";
				return out << print(node->elementType) << "&";
			}

			PRINT(VectorType) {
				return out << print(node->elementType) << "[" << print(node->size) << "]";
			}

            PRINT(ComplexType) {
                return out  << "_Complex " << print(node->elementType);
            }

			PRINT(StructType) {
				return out << print(node->name);
			}

			PRINT(UnionType) {
				return out << print(node->name);
			}

			PRINT(FunctionType) {
				return out << print(node->returnType) << "(" <<
						join(",", node->parameterTypes, [&](std::ostream& out, const TypePtr& cur) {
								out << print(cur);
				}) << ")";
			}

			PRINT(VarArgsType) {
				return out << "...";
			}

			PRINT(AttributedType) {
				return out << node->attribute << " " << print(node->type);
			}

            PRINT(EnumType) {
                return out  << "enum " << node->name << " { " << node->annotation << " };";
            }

			PRINT(VarDecl) {
				// handle single-variable declaration ...
				if (node->varInit.size() == 1u) {
					// print a variable declaration
					out << printParam(node->varInit[0].first);

					// add constructor call if necessary
					if (ConstructorCallPtr call = node->varInit[0].second.isa<ConstructorCallPtr>()) {

						// do nothing if it is default constructed
						if (call->arguments.empty()) return out;

						// just add list of parameters
						return out << "("
								<< join(", ", call->arguments, [&](std::ostream& out, const NodePtr& cur) {
									out << print(cur);
						}) << ")";
					}

					// add init value
					if (node->varInit[0].second) {
						out << " = " << print(node->varInit[0].second);
					}

					// done
					return out;
				}

				// multiple declarations
				assert(node->varInit.size() > 1u);

				// start with type
				out << print(node->varInit[0].first->type);

				// add name/value pair
				return out << " " << join(", ", node->varInit, [&](std::ostream& out, const pair<VariablePtr, ExpressionPtr>& cur) {
					out << print(cur.first);
					if (cur.second) {
						out << " = " << print(cur.second);
					}
				});
			}

			PRINT(Compound) {

				// short-cut for empty blocks
				if (node->statements.empty()) return out << "{ }";

				out << "{";
				incIndent();
				newLine(out);

				std::size_t size = node->statements.size();
				for (std::size_t i=0; i<size; i++) {
					const NodePtr& cur = node->statements[i];
					out << print(cur);
//					auto type = cur->getType();
//					if (type != NT_For && type != NT_While && type != NT_If && type != NT_Switch && type != NT_Compound) {
						out << ";";
//					}
					if (i < size-1) newLine(out);
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
				if (thenStmt->getType() != c_ast::NT_Compound) {
					thenStmt = thenStmt->getManager()->create<c_ast::Compound>(thenStmt);
				}
				out << "if (" << print(node->condition) << ") " << print(thenStmt);

				// skip else stmt if not present
				if (!node->elseStmt) {
					return out;
				}

				// wrap else stmt into compound block if necessary
				if (elseStmt->getType() != c_ast::NT_Compound) {
					elseStmt = elseStmt->getManager()->create<c_ast::Compound>(elseStmt);
				}
				return out << " else " << print(elseStmt);
			}

			PRINT(Switch) {
				out << "switch(" << print(node->value) << ") {";
				incIndent();
				newLine(out);

				std::size_t size = node->cases.size();
				for (std::size_t i=0; i<size; i++) {
					const std::pair<ExpressionPtr, StatementPtr>& cur = node->cases[i];
					out << "case " << print(cur.first) << ": " << print(cur.second);
					if (cur.second->getType() != c_ast::NT_Compound) {
						out << ";";
					}
					out << " break;";
					if (i < size-1) newLine(out);
				}

				if (node->defaultBranch) {
					newLine(out);
					out << "default: " << print(node->defaultBranch);
					if (node->defaultBranch->getType() != c_ast::NT_Compound) {
						out << ";";
					}
				}

				decIndent();
				newLine(out);
				out << "}";
				return out;
			}

			PRINT(For) {
				out << "for (" << print(node->init) << "; "
				    << print(node->check) << "; "
				    << print(node->step) << ") ";

				NodePtr body = node->body;
				if (body->getType() != NT_Compound) {
					body = c_ast::compound(body);
				}
				return out << print(body);
			}

			PRINT(While) {
				return out << "while (" << print(node->condition) << ") " << print(node->body);
			}

			PRINT(TryCatch) {
				out << "try " << print(node->body);
				for(auto cur : node->clauses) {
					out << " catch(";
					if (cur.var) {
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
				if (node->value) { // if there is a return value ...
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
				if (node->explicitType) out << "(" << print(node->type) << ")";
				return out << "{"
						<< join(", ", node->values, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
				}) << "}";
			}

			PRINT(DesignatedInitializer) {
				return out << "(" << print(node->type) << "){ ." << print(node->member) << " = " << print(node->value) << " }";
			}

			PRINT(ArrayInit) {
				return out << print(node->type) << "[" << print(node->size) << "]";
			}

			PRINT(VectorInit) {
				return out << "{"
						<< join(", ", node->values, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
				}) << "}";
			}

			PRINT(OCLVectorInit) {
				return out << "(" << print(node->type) << ")("
						<< join(", ", node->values, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
				}) << ")";
			}

			PRINT(UnaryOperation) {

				// handle operations
				switch (node->operation) {
					case UnaryOperation::UnaryPlus: 	return out << "+" << print(node->operand);
					case UnaryOperation::UnaryMinus: 	return out << "-" << print(node->operand);
					case UnaryOperation::PrefixInc: 	return out << "++" << print(node->operand);
					case UnaryOperation::PrefixDec: 	return out << "--" << print(node->operand);
					case UnaryOperation::PostfixInc: 	return out << print(node->operand) << "++";
					case UnaryOperation::PostfixDec: 	return out << print(node->operand) << "--";
					case UnaryOperation::LogicNot: 		return out << "!" << print(node->operand);
					case UnaryOperation::BitwiseNot: 	return out << "~" << print(node->operand);
					case UnaryOperation::Indirection: 	return out << "*" << print(node->operand);
					case UnaryOperation::Reference: 	return out << "&" << print(node->operand);
					case UnaryOperation::SizeOf: 		return out << "sizeof(" << print(node->operand) << ")";
					case UnaryOperation::Typeid: 		return out << "typeid(" << print(node->operand) << ")";
					case UnaryOperation::New:			return out << "new " << print(node->operand);
					case UnaryOperation::Delete:		return out << "delete " << print(node->operand);
					case UnaryOperation::DeleteArray:	return out << "delete[] " << print(node->operand);
					case UnaryOperation::ComplexReal:	return out << "__real__ " << print(node->operand);
					case UnaryOperation::ComplexImag:	return out << "__imag__ " << print(node->operand);
				}

				assert(false && "Invalid unary operation encountered!");
				return out;
			}

			PRINT(BinaryOperation) {

				string op = "";
				switch (node->operation) {
					case BinaryOperation::Assignment: 				op = " = "; break;
					case BinaryOperation::Additon: 					op = " + "; break;
					case BinaryOperation::Subtraction: 				op = " - "; break;
					case BinaryOperation::Multiplication: 			op = " * "; break;
					case BinaryOperation::Division: 				op = " / "; break;
					case BinaryOperation::Modulo: 					op = " % "; break;
					case BinaryOperation::Equal: 					op = " == "; break;
					case BinaryOperation::NotEqual: 				op = " != "; break;
					case BinaryOperation::GreaterThan: 				op = " > "; break;
					case BinaryOperation::LessThan: 				op = " < "; break;
					case BinaryOperation::GreaterOrEqual: 			op = " >= "; break;
					case BinaryOperation::LessOrEqual: 				op = " <= "; break;
					case BinaryOperation::LogicAnd: 				op = " && "; break;
					case BinaryOperation::LogicOr: 					op = " || "; break;
					case BinaryOperation::BitwiseAnd: 				op = " & "; break;
					case BinaryOperation::BitwiseOr: 				op = " | "; break;
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
					case BinaryOperation::IndirectMemberAccess:		op = "->"; break;
					case BinaryOperation::Comma:					op = ","; break;

					// special handling for subscript and cast
					case BinaryOperation::Subscript: return out << print(node->operandA) << "[" << print(node->operandB) << "]";
					case BinaryOperation::Cast: return out << "(" << print(node->operandA) << ")" << print(node->operandB);

					case BinaryOperation::StaticCast:  return out << "static_cast<"  << print(node->operandA) << ">(" << print(node->operandB) << ")";
					case BinaryOperation::DynamicCast: return out << "dynamic_cast<" << print(node->operandA) << ">(" << print(node->operandB) << ")";

					case BinaryOperation::ScopeResolution:			op = "::"; break;
					case BinaryOperation::PointerToMember:			op = "->*"; break;
				}

				assert(op != "" && "Invalid binary operation encountered!");

				// avoid /* literal by composition of / and dereferencing *
				if (node->operation == BinaryOperation::Division) {
					if (node->operandB->getType() == NT_UnaryOperation) {
						c_ast::UnaryOperationPtr opB = static_pointer_cast<UnaryOperation>(node->operandB);
						if (opB->operation == UnaryOperation::Indirection) {
							return out << print(node->operandA) << op << " " << print(node->operandB);
						}
					}
				}

				// handle as usual
				return out << print(node->operandA) << op << print(node->operandB);
			}

			PRINT(TernaryOperation) {

				switch (node->operation) {
				case TernaryOperation::TernaryCondition : {
					return out << print(node->operandA) << "?" << print(node->operandB) << ":" << print(node->operandC);
				}
				}

				assert(false && "Invalid ternary operation encountered!");
				return out;
			}

			PRINT(Call) {
				// <function> ( <arguments> )
				return out << print(node->function) << "("
						<< join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
				}) << ")";
			}

			PRINT(MemberCall) {
				// <obj> . <function> ( <arguments> )
				return out << print(node->object) << "." << print(node->memberFun) << "("
						<< join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
				}) << ")";
			}

			PRINT(ConstructorCall) {
				// <new> <className> ( <arguments> )
				out << ((node->location)?"new ":"");

				// the location for a placement new
				if (node->location) {
					out << "(" << print(node->location) << ") ";
				}

				// the rest
				return out
						<< print(node->classType) << "("
						<< join(", ", node->arguments, [&](std::ostream& out, const NodePtr& cur) {
							out << print(cur);
						}) << ")";
			}

			PRINT(DestructorCall) {
				// <location> . <classType> :: ~<classType> ( )
				out << print(node->location) << ".";
				if (!node->isVirtual) out << print(node->classType) << "::";
				out << "~" << print(node->classType) << "()";
				return out;
			}

			PRINT(Parentheses) {
				return out << "(" << print(node->expression) << ")";
			}

			PRINT(OpaqueExpr) {
				return out << node->value;
			}

			PRINT(TypeDeclaration) {
				// forward declaration + type definition
				bool isStruct = (node->type->getNodeType() == NT_StructType);

				// forward declaration
				out << ((isStruct)?"struct ":"union ") << print(node->type) << ";\n";

				// type definition
				out << "typedef " << ((isStruct)?"struct ":"union ") << print(node->type) << " " << print(node->type) << ";\n";

				// done
				return out;
			}

			PRINT(FunctionPrototype) {
				// <returnType> name ( <parameter list> );
				FunctionPtr fun =  node->function;
				return out << print(fun->returnType) << " " << print(fun->name) << "(" << printParam(fun->parameter) << ");\n";
			}

			PRINT(ConstructorPrototype) {
				// <class name> ( <parameter list > );
				auto ctor = node->ctor;
				return out << print(ctor->className) << "(" << printMemberParam(ctor->function->parameter)<< ")";
			}

			PRINT(DestructorPrototype) {
				// <virtual> ~ <class name> ( <parameter list > );
				auto dtor = node->dtor;
				return out << (node->isVirtual?"virtual ":"") << "~" << print(dtor->className) << "()";
			}

			PRINT(MemberFunctionPrototype) {
				// <virtual> <return type> <name> ( <parameter list > ) <const>;
				auto fun = node->fun->function;
				return out
						<< (node->isVirtual?"virtual ":"")
						<< print(fun->returnType) << " "
						<< print(fun->name)
						<< "(" << printMemberParam(fun->parameter)<< ")"
						<< (node->fun->isConstant?" const":"")
						<< (node->pureVirtual?" =0":"");
			}

			PRINT(GlobalVarDecl) {
				return out << (node->external?"extern ":"") << ParameterPrinter(node->type, node->getManager()->create(node->name)) << ";\n";
			}

			PRINT(Parent) {
				return out << (node->isVirtual?"virtual ":"") << print(node->parent);
			}

			PRINT(TypeDefinition) {

				// handle type definitions
				if ((bool)(node->name)) {
					return out << "typedef " << print(node->type) << " " << print(node->name) << ";\n";
				}

				// handle struct / union types
				c_ast::NamedCompositeTypePtr composite = dynamic_pointer_cast<c_ast::NamedCompositeType>(node->type);
				assert(composite && "Must be a struct or union type!");

				// special handling for struct types
				c_ast::StructTypePtr structType = dynamic_pointer_cast<c_ast::StructType>(composite);

				// define struct / type as part of a type definition (C/C++ compatible)
				out << ((structType)?"struct":"union") << " " << print(composite->name);

				// print parents
				if (structType && !structType->parents.empty()) {
					out << " : public " << join(", public ", structType->parents, [&](std::ostream& out, const ParentPtr& cur) { out << print(cur); });
				}

				// start definition
				out << " {";

				// add fields
				if (!composite->elements.empty()) out << "\n    ";
				out << join(";\n    ", composite->elements,
						[&](std::ostream& out, const VariablePtr& cur) {
							out << printParam(cur);
				});
				if (!composite->elements.empty()) out << ";";

				// add member functions
				if (structType) {

					// todo: add ctor / dtor

					// add constructors
					if (!structType->ctors.empty()) out << "\n    ";
					out << join(";\n    ", structType->ctors,
							[&](std::ostream& out, const ConstructorPrototypePtr& cur) {
								out << print(cur);
					});
					if (!structType->ctors.empty()) out << ";";

					// add destructor
					if (structType->dtor) out << "\n    ";
					out << print(structType->dtor);
					if (structType->dtor) out << ";";


					// add member functions
					if (!structType->members.empty()) out << "\n    ";
					out << join(";\n    ", structType->members,
							[&](std::ostream& out, const MemberFunctionPrototypePtr& cur) {
								out << print(cur);
					});
					if (!structType->members.empty()) out << ";";

					// todo: add ctors / dtors / member function prototypes

				}

				// finish type definition
				return out << "\n};\n";
			}

			c_ast::StatementPtr wrapBody(const c_ast::StatementPtr& body) {
				if (body->getType() == c_ast::NT_Compound) {
					return body;
				}
				return body->getManager()->create<c_ast::Compound>(body);
			}

			PRINT(Function) {
				// <returnType> name ( <parameter list> ) <body> \n
				if (node->flags & Function::STATIC) {
					out << "static ";
				}
				if (node->flags & Function::INLINE) {
					out << "inline ";
				}
				if (node->flags & Function::OCL_KERNEL) {
					out << "__kernel ";
				}

				out << print(node->returnType) << " " << print(node->name) << "(" << printParam(node->parameter) << ") ";

				return out << print(wrapBody(node->body));
			}

			PRINT(Constructor) {
				// <className> :: <name> ( <parameter list> ) : <initializer_list>  <body> \n

				auto fun = node->function;

				// print header
				out << print(node->className) << "::" << print(node->className) << "(" << printMemberParam(fun->parameter) << ") ";

				// add initializer list
				if (!node->initialization.empty()) {
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
				out << print(fun->returnType) << " " << print(node->className) << "::" << print(fun->name)
						<< "(" << printMemberParam(fun->parameter) << ")" << (node->isConstant?" const ":" ");

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
			return (node)?printer.print(node, out):out;
		}

		struct TypeLevel {
			typedef bool Pointer;
			vector<Pointer> pointers; // true is a const pointer, false a standard pointer
			vector<ExpressionPtr> subscripts;
			vector<TypePtr> parameters;
			StructTypePtr owner;
			bool hasParameters;
			TypeLevel() : pointers(), hasParameters(false) {}
		};

		typedef vector<TypeLevel> TypeNesting;
		typedef TypeNesting::const_iterator NestIterator;

		TypePtr computeNesting(TypeNesting& data, const TypePtr& type) {
			// check whether there is something to do
			if (type->getType() != NT_PointerType && type->getType() != NT_VectorType && type->getType() != NT_FunctionType) {
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
			if (cur->getType() == NT_FunctionType) {

				// if vectors have already been processed => continue with next level
				if (!res.subscripts.empty()) {
					auto innermost = computeNesting(data, cur);
					data.push_back(res);
					return innermost;
				}

				FunctionTypePtr funType = static_pointer_cast<FunctionType>(cur);
				copy(funType->parameterTypes, std::back_inserter(res.parameters));
				res.hasParameters = true;

				res.owner = static_pointer_cast<StructType>(static_pointer_cast<FunctionType>(cur)->classType);

				cur = funType->returnType;
			}

			// count pointers
			while(cur->getType() == NT_PointerType) {
				PointerTypePtr ptr = cur.as<PointerTypePtr>();
				cur = static_pointer_cast<PointerType>(cur)->elementType;
				res.pointers.push_back(ptr->isConst);
			}

			// resolve rest recursively
			auto innermost = computeNesting(data, cur);

			// add pair to result
			data.push_back(res);

			return innermost;
		}

		std::ostream& printName(std::ostream& out, const IdentifierPtr& name) {
			if (name->name.empty()) {
				return out;
			}
			return out << " " << CPrint(name);
		}

		std::ostream& printTypeNest(std::ostream& out, NestIterator level_it, NestIterator end, const IdentifierPtr& name) {

			// terminal case ...
			if (level_it == end) {
				return printName(out, name);
			}

			// print pointers ...
			const TypeLevel& cur = *level_it;
			for(auto it = cur.pointers.rbegin(); it != cur.pointers.rend(); it++) {
				out << ((*it) ? "*const" : "*");
			}

			++level_it;
			if (level_it != end) {
				out << "(";

				// here is the place to print any membership of a function pointer
				if(cur.owner){
					out << CPrint(static_pointer_cast<StructType>(cur.owner)->name);	
					out << "::";
				}

				// print nested recursively
				printTypeNest(out, level_it, end, name);

				out << ")";
			} else {
				printName(out, name);
			}

			// check whether one or the other is empty
			assert(!(!cur.parameters.empty() && !cur.subscripts.empty()) && "Only one component may be non-empty!");

			// print vector sizes
			out << join("", cur.subscripts, [&](std::ostream& out, const ExpressionPtr& cur) {
				out << "[" << CPrint(cur) << "]";
			});


			// print parameter list
			if (cur.hasParameters) {
				out << "(" << join(",", cur.parameters, [&](std::ostream& out, const TypePtr& cur) {
					out << CPrint(cur);
				}) << ")";
			}

			return out;
		}

	}


	std::ostream& ParameterPrinter::printTo(std::ostream& out) const {
		c_ast::VariablePtr var;

		return out << join(", ", params, [](std::ostream& out, const c_ast::VariablePtr& var) {

			// special handling for varargs
			if (var->type->getType() == c_ast::NT_VarArgsType) {
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
		if (!node) return "/* null */";
		return ::toString(CPrint(node));
	}

	string toC(const c_ast::CodeFragmentPtr& fragment) {
		if (!fragment) return "/* null */";
		return ::toString(*fragment);
	}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

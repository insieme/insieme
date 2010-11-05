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

#include "printer/pretty_printer.h"

#include <cassert>
#include <memory>

#include <boost/unordered_map.hpp>

#include "ast_visitor.h"
#include "string_utils.h"
#include "lang_basic.h"

namespace insieme {
namespace core {
namespace printer {

namespace {

	// a forward declaration of the actual printer
	class InspirePrinter;

	/**
	 * This class allows the pretty printer to use special formats when printing the call of a specific function.
	 * For instance, this formatter allows to write the add operation + in infix notation.
	 */
	class Formatter {

		/**
		 * The type of literal this formatter is capable of handling
		 */
		const LiteralPtr literal;

	public:

		/**
		 * Creates a new instance of a formatter.
		 *
		 * @param literal the literal handled by this formatter
		 */
		Formatter(const LiteralPtr& literal) : literal(literal) {}

		/**
		 * Obtains the literal handled by this formatter.
		 *
		 * @return the handled literal
		 */
		const LiteralPtr& getLiteral() { return literal; }

		/**
		 * Performs the actual code formating. This method is pure abstract and
		 * has to be implemented within sub-classes.
		 *
		 * @param printer the pretty printer to be used for printing sub-expressions
		 * @param call the call statement invoking the handled literal
		 */
		virtual void format(InspirePrinter& printer, const CallExprPtr& call) =0;

	};

	/**
	 * Since formatter instances are polymorthic, they need to be handled via pointer or
	 * references. Further, the memory management needs to be considered. Therefore, formatter
	 * should be passed using this pointer type, which is based on a shared pointer.
	 */
	typedef std::shared_ptr<Formatter> FormatterPtr;

	/**
	 * The Lambda Formatter is a concrete generic implementation of the Formatter class. It uses
	 * a lambda expression passed in during the construction to format the actual output. This
	 * mechanism allows to write "anonymous inner" classes.
	 */
	template<typename Lambda>
	class LambdaFormatter : public Formatter {

		/**
		 * The lambda used to perform the formatting.
		 */
		Lambda lambda;

	public:

		/**
		 * Creates a new instance of this type printing the given literal using the
		 * given lambda during the formating.
		 *
		 * @param literal the literal to be handled by this formatter
		 * @param lambda the lambda performing the actual formatting
		 */
		LambdaFormatter(const LiteralPtr& literal, Lambda lambda) : Formatter(literal), lambda(lambda) {}

		/**
		 * Conducts the actual formatting of the given call expression.
		 *
		 * @param printer the printer to be used for printing sub-expressions
		 * @param call the call expression to be formated by this formatter
		 */
		virtual void format(InspirePrinter& printer, const CallExprPtr& call) {
			// TODO: re-enable when literals are no longer identified by their name
			// assert(*call->getFunctionExpr() == *getLiteral() && "Invoked for wrong literal!");
			lambda(printer, call);
		}
	};

	/**
	 * A utility function to create LiteralFormatter instances without the need of
	 * specifying generic types. Those types will be inferred automatically.
	 *
	 * @param literal the literal to be handled by the requested formatter
	 * @param lambda the formatting routine
	 * @return a new formatter handling the given literal using the given lambda
	 */
	template<typename Lambda>
	FormatterPtr make_formatter(const LiteralPtr& literal, Lambda lambda) {
		return std::make_shared<LambdaFormatter<Lambda>>(literal, lambda);
	}

	// TODO re-enable full literal comparison
	//	typedef boost::unordered_map<LiteralPtr, FormatterPtr, hash_target<LiteralPtr>, equal_target<LiteralPtr>> FormatTable;
	typedef boost::unordered_map<string, FormatterPtr, boost::hash<string>> FormatTable;

	// a forward declaration for a method assembling formater tables
	FormatTable initFormatTable(bool);



	/**
	 * The main visitor used by the pretty printer process.
	 */
	class InspirePrinter : public ASTVisitor<> {

		/**
		 * A table containing special formatting rules for particular functions.
		 */
		FormatTable formatTable;

		/**
		 * The current intention level.
		 */
		unsigned indent;

		/**
		 * The pretty print handled by this printer. It is stored since it contains
		 * various formating options.
		 */
		const PrettyPrint& print;

	public:

		/**
		 * The output stream this printer is printing to.
		 */
		std::ostream& out;

		/**
		 * Creates a new instance of this printer writing results to the given output
		 * stream.
		 *
		 * @param out the stream to be printed to
		 * @param print the setup of the pretty print
		 */
		InspirePrinter(std::ostream& out, const PrettyPrint& print)
				: formatTable(initFormatTable(print.hideDeref)), indent(0), print(print), out(out) { };


		/**
		 * A macro simplifying the definition for print routine of some node type.
		 */
		#define PRINT(NodeType, Print) \
			void visit ## NodeType (const NodeType ## Ptr& node) Print


		PRINT(Type, {
				// types can be handled easilly ...
				out << *node;
		});

		PRINT(BreakStmt, {
				out << "break";
		});

		PRINT(ContinueStmt, {
				out << "continue";
		});

		PRINT(ReturnStmt, {
				out << "return ";
				this->visit(node->getReturnExpr());
		});

		PRINT(DeclarationStmt, {
				// print type
				VariablePtr var = node->getVariable();
				out << "decl ";
				this->visit(var->getType());
				out << " " << *var << " = ";
				this->visit(node->getInitialization());
		});

		PRINT(CompoundStmt, {
				auto list = node->getStatements();
				if (list.empty()) {
					out << "{ }";
					return;
				}

				out << "{"; increaseIndent(); newLine();
				for_each(list.begin(), list.end()-1, [&](const NodePtr& cur) {
					this->visit(cur);
					out << ";";
					this->newLine();
				});
				this->visit(*(list.end()-1));
				out << ";";decreaseIndent(); newLine();
				out << "}";
		});

		PRINT(WhileStmt, {
				// variables can be directly printed
				out << "while(";
				this->visit(node->getCondition());
				out << ") ";
				this->visit(node->getBody());
		});

		PRINT(ForStmt, {
				// variables can be directly printed
				out << "for(";
				this->visit(node->getDeclaration());
				out << " .. ";
				this->visit(node->getEnd());
				out << " : ";
				this->visit(node->getStep());
				out << ") ";

				NodePtr body = node->getBody();
				if (body->getNodeType() != NT_CompoundStmt) {
					increaseIndent(); this->newLine();
					this->visit(body);
					decreaseIndent(); this->newLine();
				} else {
					this->visit(node->getBody());
				}
		});

		PRINT(IfStmt, {
				// variables can be directly printed
				out << "if(";
				this->visit(node->getCondition());
				out << ") ";
				this->visit(node->getThenBody());
				out << " else ";
				this->visit(node->getElseBody());
		});

		PRINT(SwitchStmt, {
				// variables can be directly printed
				out << "switch(";
				this->visit(node->getSwitchExpr());
				out << ") {"; increaseIndent(); this->newLine();
				for_each(node->getCases(), [&](const SwitchStmt::Case& cur) {
					out << "case ";
					this->visit(cur.first);
					out << ": ";
					this->visit(cur.second);
					this->newLine();
				});
				out << "default: ";
				this->visit(node->getDefaultCase());
				decreaseIndent(); this->newLine(); out << "}";
		});

		PRINT(Variable, {
				// variables can be directly printed
				out << *node;
		});

		PRINT(Literal, {
				out << *node;
		});

		PRINT(LambdaExpr, {
				out << "fun";
				if (!node->getCaptureList().empty()) {
					out << "[" << ::join(", ", node->getCaptureList(), [&](std::ostream&, const DeclarationStmtPtr& cur) {
						this->visit(cur);
					}) << "]";
				}
				out << "(" << ::join(", ", node->getParams(), [&](std::ostream& out, const VariablePtr& cur) {
					out << *cur->getType() << " " << *cur;
				}) << ") ";
				this->visit(node->getBody());
		});

		PRINT(CallExpr, {
				// test whether for the current call a special format has been registerd
				auto function = node->getFunctionExpr();
				if (function->getNodeType() == NT_Literal) {
					auto pos = formatTable.find(static_pointer_cast<const Literal>(function)->getValue());
					if (pos != formatTable.end()) {
						FormatterPtr formatter = (*pos).second;
						if (!print.hideBrackets) out << "(";
						formatter->format(*this, node);
						if (!print.hideBrackets) out << ")";
						return;
					}
				}

				// default formating
				this->visit(node->getFunctionExpr());
				auto arguments = node->getArguments();
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
		});

		PRINT(CastExpr, {
				if (print.hideCasts) {
					this->visit(node->getSubExpression());
				} else {
					out << "CAST<" << *node->getType() << ">(";
					this->visit(node->getSubExpression());
					out << ")";
				}
		});

		PRINT(TupleExpr, {
				out << "(" << ::join(", ", node->getExpressions(), [&](std::ostream&, const ExpressionPtr& cur) {
					this->visit(cur);
				}) << ")";
		});

		PRINT(VectorExpr, {
				std::vector<ExpressionPtr> elements = node->getExpressions();

				const int limit = 5;
				bool cut = false;
				if (elements.size() > limit) {
					elements = std::vector<ExpressionPtr>(elements.begin(), elements.begin()+limit);
					cut = true;
				}

				out << "[" << ::join(", ", elements, [&](std::ostream&, const ExpressionPtr& cur) {
					this->visit(cur);
				}) << ((cut)?", ...":"") << "]";
		});

		PRINT(RecLambdaExpr, {
				out << "recFun ";
				this->visit(node->getVariable());
				out << " ";
				this->visit(node->getDefinition());
		});

		PRINT(JobExpr, {
				// prints the job expression quite similar to a switch expression
				out << "job";
				if (!node->getLocalDecls().empty()) {
					out << "[" << ::join(", ", node->getLocalDecls(), [&](std::ostream&, const DeclarationStmtPtr& cur) {
						this->visit(cur);
					}) << "]";
				}
				out << "{"; increaseIndent(); this->newLine();
				for_each(node->getGuardedStmts(), [&](const JobExpr::GuardedStmt& cur) {
					out << "if ";
					this->visit(cur.first);
					out << " do: ";
					this->visit(cur.second);
					this->newLine();
				});
				out << "default: ";
				this->visit(node->getDefaultStmt());
				decreaseIndent(); this->newLine(); out << "}";
		});

	//	AST_TERMINAL(StructExpr, NamedCompositeExpr)
	//	AST_TERMINAL(UnionExpr, NamedCompositeExpr)

		PRINT(RecTypeDefinition, {
				auto defs = node->getDefinitions();
				if (defs.empty()) {
					out << "{ }";
					return;
				}

				out << "{"; increaseIndent(); newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const std::pair<const TypeVariablePtr, TypePtr>& cur) {
					out << *cur.first << " = ";
					this->visit(cur.second);
					out << ";";
					if (count++ < defs.size() -1) this->newLine();
				});

				decreaseIndent();
				newLine();
				out << "}";
		});

		PRINT(RecLambdaDefinition, {
				auto defs = node->getDefinitions();
				if (defs.empty()) {
					out << "{ }";
					return;
				}

				out << "{"; increaseIndent(); newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const std::pair<const VariablePtr, LambdaExprPtr>& cur) {
					out << *cur.first << " = ";
					this->visit(cur.second);
					out << ";";
					if (count++ < defs.size() -1) this->newLine();
				});

				decreaseIndent();
				newLine();
				out << "}";
		});


		PRINT(Program, {
			out << "// Inspire Program "; newLine();
			for_each(node->getEntryPoints(), [&](const NodePtr& cur) {
				this->out << "//  Entry Point: "; this->newLine();
				this->increaseIndent();
				this->visit(cur);
				this->decreaseIndent();
			});
		});

		/**
		 * A generic solution for unknown types. In this case, the
		 * default debug print is forwarded to the output stream.
		 */
		PRINT(Node, {
			out << "<?>" << *node << "</?>";
		});

		/**
		 * Creates a new line.
		 */
		void newLine() const {
			out << std::endl;
			for (unsigned i=0; i<indent; i++) {
				out << "    ";;
			}
		}

		/**
		 * Increases the indent for following lines.
		 */
		void increaseIndent() {
			indent++;
		}

		/**
		 * Decreases the indent for following lines.
		 */
		void decreaseIndent() {
			indent--;
		}
	};


	/**
	 * A utility function to obtain the n-th argument within the given call expression.
	 *
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the requested argument
	 * @return the requested argument or a NULL pointer in case there is no such argument
	 */
	NodePtr getArgument(const CallExprPtr& call, unsigned n) {
		auto arguments = call->getArguments();
		if (n < arguments.size()) {
			return arguments[n];
		}
		return NodePtr();
	}

	/**
	 * A utility function printing the n-th argument of a call expression.
	 *
	 * @param printer the printer to be used for the actual printing
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the argument to be printed; in case there is no such argument a ? is printed.
	 */
	void printArgument(InspirePrinter& printer, const CallExprPtr& call, unsigned n) {
		NodePtr argument = getArgument(call, n);
		if (argument) {
			printer.visit(argument);
		} else {
			printer.out << "?";
		}
	}

	/**
	 * Creates a format table defining the formatting of various build in functions.
	 * @param hideDeref if set to true, derefs will be invisible. Otherwise the uniary * operator will be used.
	 */
	FormatTable initFormatTable(bool hideDeref) {
		FormatTable res;

		#define OUT(Literal) printer.out << Literal
		#define ARG(N) getArgument(call, N)
		#define PRINT_ARG(N) printArgument(printer, call, N)
		#define ADD_FORMATTER(Literal, FORMAT) \
					res.insert(std::make_pair(Literal->getValue(), make_formatter(Literal, [](InspirePrinter& printer, const CallExprPtr& call) FORMAT ))).second;

		if (hideDeref) {
			ADD_FORMATTER(lang::OP_REF_DEREF_PTR, { PRINT_ARG(0); });
		} else {
			ADD_FORMATTER(lang::OP_REF_DEREF_PTR, { OUT(" *"); PRINT_ARG(0); });
		}

		ADD_FORMATTER(lang::OP_REF_ASSIGN_PTR, { PRINT_ARG(0); OUT(" := "); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_REF_VAR_PTR, { OUT(" var("); PRINT_ARG(0); OUT(")"); });
		ADD_FORMATTER(lang::OP_REF_NEW_PTR, { OUT(" new("); PRINT_ARG(0); OUT(")"); });
		ADD_FORMATTER(lang::OP_REF_DELETE_PTR, { OUT(" del("); PRINT_ARG(0); OUT(")"); });

		ADD_FORMATTER(lang::OP_SUBSCRIPT_PTR, { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(lang::OP_SUBSCRIPT_SINGLE_PTR, { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });

		ADD_FORMATTER(lang::OP_REAL_ADD_PTR, { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_SUB_PTR, { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_MUL_PTR, { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_DIV_PTR, { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });

		ADD_FORMATTER(lang::OP_UINT_ADD_PTR, { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_SUB_PTR, { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_MUL_PTR, { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_DIV_PTR, { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_MOD_PTR, { PRINT_ARG(0); OUT("%"); PRINT_ARG(1); });

		ADD_FORMATTER(lang::OP_INT_ADD_PTR, { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_SUB_PTR, { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_MUL_PTR, { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_DIV_PTR, { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_MOD_PTR, { PRINT_ARG(0); OUT("%"); PRINT_ARG(1); });

		ADD_FORMATTER(lang::OP_BOOL_AND_PTR, { PRINT_ARG(0); OUT("&&"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_OR_PTR, { PRINT_ARG(0); OUT("||"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_EQ_PTR, { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_NOT_PTR, { OUT("!"); PRINT_ARG(0); });

		ADD_FORMATTER(lang::OP_UINT_EQ_PTR, { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_GE_PTR, { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_GT_PTR, { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_LT_PTR, { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_LE_PTR, { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });

		ADD_FORMATTER(lang::OP_INT_EQ_PTR, { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_GE_PTR, { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_GT_PTR, { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_LT_PTR, { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_LE_PTR, { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });


		#undef ADD_FORMATTER
		#undef OUT
		#undef ARG
		#undef PRINT_ARG


		return res;
	}

} // end of anonymous namespace

} // end of namespace printer
} // end of namespace core
} // end of namespace insieme

namespace std {

/**
 * Prints the given pretty print to the given output stream.
 *
 * @param out the stream the output should be printed to
 * @param print the element to be printed
 * @return a reference to the output stream
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::printer::PrettyPrint& print) {

	out << std::endl;
	out << "// -------------- Pretty Print Inspire --------------" << std::endl;

	// use inspire printer to print the code ...
	insieme::core::printer::InspirePrinter printer(out, print);
	printer.visit(print.root);
	return out;
}

}


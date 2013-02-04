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

#include "insieme/core/printer/pretty_printer.h"

#include <cassert>
#include <memory>
#include <iomanip>

#include <boost/unordered_map.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/attributes.h"

#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/concepts.hpp> 

namespace insieme {
namespace core {
namespace printer {


// set up default formats for pretty printer
const unsigned PrettyPrinter::OPTIONS_DEFAULT = 0;
const unsigned PrettyPrinter::OPTIONS_DETAIL = PrettyPrinter::PRINT_BRACKETS | PrettyPrinter::PRINT_CASTS 
	| PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY;
const unsigned PrettyPrinter::OPTIONS_MAX_DETAIL = PrettyPrinter::PRINT_BRACKETS | PrettyPrinter::PRINT_CASTS 
	| PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ANNOTATIONS | PrettyPrinter::NO_LIST_SUGAR
	| PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY;
const unsigned PrettyPrinter::OPTIONS_SINGLE_LINE = PrettyPrinter::OPTIONS_DETAIL | PrettyPrinter::PRINT_SINGLE_LINE;

/**
 * Tests whether a certain option is set or not.
 *
 * @return true if the option is set, false otherwise
 */
bool PrettyPrinter::hasOption(Option option) const {
	// check corresponding bit field
	return flags & option;
}

/**
 * Updates a format option for the pretty printer.
 *
 * @param option the option to be updated
 * @param status the state this option should be set to
 */
void PrettyPrinter::setOption(Option option, bool status) {
	// update flag by setting / resetting the corresponding bit
	flags = (status)?(flags | option):( flags & ~option);
}


namespace {

	// a forward declaration of the actual printer
	class InspirePrinter;

	/**
	 * This class allows the pretty printer to use special formats when printing the call of a specific function.
	 * For instance, this formatter allows to write the add operation + in infix notation.
	 */
	class Formatter {

	public:

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
	 * Since formatter instances are polymorphic, they need to be handled via pointer or
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
		 * given formating lambda.
		 *
		 * @param lambda the lambda performing the actual formatting
		 */
		LambdaFormatter(Lambda lambda) : lambda(lambda) {}

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
	 * @param lambda the formatting routine
	 * @return a new formatter handling the given literal using the given lambda
	 */
	template<typename Lambda>
	FormatterPtr make_formatter(Lambda lambda) {
		return std::make_shared<LambdaFormatter<Lambda>>(lambda);
	}

	// defines the table used for indexing formatter entries
	typedef utils::map::PointerMap<ExpressionPtr, FormatterPtr> FormatTable;

	// a forward declaration for a method assembling formatter tables
	FormatTable initFormatTable(const PrettyPrinter&);



	/**
	 * The main visitor used by the pretty printer process.
	 */
	class InspirePrinter : private IRVisitor<> {

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
		const PrettyPrinter& printer;

		/**
		 * A counter for the current recursive depth of the print. The depth is checked when entering
		 * a visitXY method. In case it is exceeded, the recursion is terminated.
		 */
		unsigned depth;

		/**
		 * A list of nodes being bound to names to make the code more readable.
		 */
		std::map<NodePtr, std::string> letBindings;

		bool singleLineTypes;

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
		 * @param printer the setup of the pretty printer
		 */
		InspirePrinter(std::ostream& out, const PrettyPrinter& printer)
				: IRVisitor<>(true), formatTable(initFormatTable(printer)), indent(0), printer(printer), depth(0), out(out) { };

		const PrettyPrinter& getPrettyPrint() const {
			return printer;
		}

		/**
		 * The main entry point computing common sub-expressions before printing the actual code.
		 */
		void print(const NodePtr& node) {

			// reset setup
			letBindings.clear();
			singleLineTypes = true;

			// check whether bindings should be used
			if (printer.hasOption(PrettyPrinter::NO_LET_BINDINGS) || printer.hasOption(PrettyPrinter::PRINT_SINGLE_LINE)) {
				// skip computation of bindings
				visit(node);
				return;
			}

			// compute set of substitutions
			int funCounter = 0;
			int typeCounter = 0;
			singleLineTypes = false;		// enable multiline type definitions
			visitDepthFirstOnce(node, [&](const NodePtr& cur) {

				// do not let-bind build ins
				if (cur->getNodeManager().getLangBasic().isBuiltIn(cur)) {
					return;
				}

				NodeType type = cur->getNodeType();

				if(type == NT_RecType || type == NT_StructType || type == NT_UnionType || (!printer.hasOption(PrettyPrinter::NO_LET_BOUND_FUNCTIONS) && type == NT_LambdaExpr)) {

					// obtain a name (TODO: pick something more important)
					string name = (type == NT_LambdaExpr)?format("fun%03d", funCounter++):format("type%03d", typeCounter++);

					// print a let binding
					out << "let " << name << " = ";
					visit(cur);
					out << ";\n\n";

					// add a substitution rule
					letBindings[cur] = name;
				}

			}, false);	// iterate through IR in post-order

			singleLineTypes = true;
			visit(node);
		}

		/**
		 * Wrapper for general tasks
		 */
		virtual void visit(const NodePtr& element) {

			// check whether this one has been substituted
			auto pos = letBindings.find(element);
			if (pos != letBindings.end()) {
				out << pos->second;
				return;
			}

			if (depth > printer.maxDepth) {
				out << " ... ";
				return;
			}
			depth++;
			printAnnotations(element, true);
			IRVisitor<>::visit(element);
			printAnnotations(element, false);
			out.flush();
			depth--; 
		}

		/**
		 * A macro simplifying the definition for print routine of some node type.
		 */
		#define PRINT(NodeType, Print) \
		virtual	void visit ## NodeType (const NodeType ## Ptr& node) { Print }

		PRINT(Value, {
				// identifiers can be directly printed
				out << *node;
		});

		PRINT(GenericType, {
				out << *node->getName();
				const TypesPtr& types = node->getTypeParameter();
				const IntTypeParamsPtr& intTypes = node->getIntTypeParameter();
				
				if( types->empty() && intTypes->empty() ) {
					return;
				}

				out << "<" << join(",", types, [&](std::ostream&, const TypePtr& cur){ this->visit(cur); } );

				if ( !types->empty() && !intTypes->empty() ) {
			   		out << ",";	
				}

				out << join(",", intTypes,
							[&](std::ostream& jout, const IntTypeParamPtr& cur){ jout << *cur; } ) << ">"; 
		});

		PRINT(FunctionType, {

				auto printer = [&](std::ostream&, const TypePtr& cur){ this->visit(cur); };

				if (node->isConstructor()) {
					visit(node->getObjectType());
					auto begin = node->getParameterTypes().begin() + 1;
					auto end = node->getParameterTypes().end();
					out << "::(" << join(", ", begin, end, printer) << ")";
				} else if (node->isDestructor()) {
					out << "~";
					visit(node->getObjectType());
					out << "::()";
				} else if (node->isMemberFunction()) {
					visit(node->getObjectType());
					auto begin = node->getParameterTypes().begin() + 1;
					auto end = node->getParameterTypes().end();
					out << "::(" << join(", ", begin, end, printer) << ") -> ";
					visit(node->getReturnType());
				} else {
					out << "(" << join(", ", node->getParameterTypes(), printer) << ") ";
					out << ((node->isPlain())?"->":"=>");
					out << " ";
					visit( node->getReturnType() );
				}
		});

		PRINT(RecType, {
				out << "rec ";
				visit(node->getTypeVariable());

				string newItem = "\n\t";
				string newLine = "\n";

				if (singleLineTypes) {
					newItem = "";
					newLine = "";
				}

				out << "{" << newItem << join(", " + newItem, node->getDefinition()->getDefinitions(),
					[&](std::ostream& jout, const RecTypeBindingPtr& cur) {
						this->visit(cur->getVariable());
						jout << "=";
						this->visit(cur->getType());
				}) << newLine << "}";
		});

		PRINT(NamedCompositeType, {

			string newItem = "\n\t";
			string newLine = "\n";

			if (singleLineTypes) {
				newItem = "";
				newLine = "";
			}

			out << ((node->getNodeType() == NT_UnionType)?"union":"struct");

			if (!node->getParents().empty()) {
				out << " : " << join(", ", node->getParents(), [&](std::ostream& out, const ParentPtr& parent) {
					if (parent->isVirtual()) out << "virtual ";
					this->visit(parent->getType());
				}) << " ";
			}

			out << "<" << newItem << join("," + newItem, node->getEntries(),
				[&](std::ostream& out, const NamedTypePtr& cur) {
					this->visit(cur->getName());
					out << ":";
				   this->visit(cur->getType());
			    }) << newLine << ">";
		});

		PRINT(TupleType, {
				out << '(' << join(",", node->getElementTypes(), 
						[&](std::ostream&,const TypePtr& cur){ this->visit(cur); }) 
					<< ')';
		});

		PRINT(RefType, {
				out << "ref<";
				visit(node->getElementType());
				out << ">";
		});

		PRINT(ArrayType, {
				out << "array<";
				visit(node->getElementType());
				out << ",";
				visit(node->getDimension());
				out << ">";
		});

		PRINT(VectorType, {
				out << "vector<";
				visit(node->getElementType());
				out << ",";
				visit(node->getSize());
				out << ">";
		});

		PRINT(Type,{
				out << *node;
		});

		PRINT(IntTypeParam, {
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
				const VariablePtr& var = node->getVariable();
				out << "decl ";
				this->visit(var->getType());
				out << " ";
				this->visit(var);
			   	out << " = ";
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
				this->visit(list.back());
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
				out << "for(decl ";
				this->visit(node->getIterator()->getType());
				out << " ";
				this->visit(node->getIterator());
				out << " = ";
				this->visit(node->getStart());
				out << " .. ";
				this->visit(node->getEnd());
				out << " : ";
				this->visit(node->getStep());
				out << ") ";

				NodePtr&& body = node->getBody();
				if (body->getNodeType() != NT_CompoundStmt) {
					increaseIndent(); this->newLine();
					this->visit(body);
					decreaseIndent(); this->newLine();
				} else {
					this->visit(body);
				}
		});

		PRINT(IfStmt, {
				// variables can be directly printed
				out << "if(";
				this->visit(node->getCondition());
				out << ") ";
				this->visit(node->getThenBody());
				if (!analysis::isNoOp(node->getElseBody())) {
					out << " else ";
					this->visit(node->getElseBody());
				}
		});

		PRINT(SwitchStmt, {
				// variables can be directly printed
				out << "switch(";
				this->visit(node->getSwitchExpr());
				out << ") {"; increaseIndent(); this->newLine();
				for_each(node->getCases()->getCases(), [&](const SwitchCasePtr& cur) {
					out << "case ";
					this->visit(cur->getGuard());
					out << ": ";
					this->visit(cur->getBody());
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
				if (GenericTypePtr&& genTy = core::dynamic_pointer_cast<const GenericType>(node->getType()) ) {
					if(genTy->getName()->getValue() == "type") {
						visit(genTy);
						return;
					}
				}

				const string& str = node->getStringValue();
				if (printer.hasOption(PrettyPrinter::NAME_CONTRACTION) && str.size() > 9) {
					out << str.substr(0,3) << "..." << str.substr(str.size()-3, str.size());
				} else {
					out << str;
				}
		});

		PRINT(LambdaExpr, {
				bool noExpandLambdas = printer.hasOption(PrettyPrinter::NO_EXPAND_LAMBDAS);
				if(noExpandLambdas) {
					out << "fun{...}";
					return;
				}

				// some sanity check frequently encountered
				assert(node->getLambda() && "Accessing non-present Lambda Definition!");

				// short-cut for non-recursive functions
				if (!node->isRecursive()) {
					visit(node->getLambda());
					return;
				}
				
				// general case: recursive function
				out << "recFun ";
				this->visit(node->getVariable());
				out << " ";
				this->visit(node->getDefinition());
		});


		PRINT(LambdaDefinition, {
				auto defs = node->getDefinitions();
				if (defs.empty()) {
					return;
				}

				out << "{"; increaseIndent(); newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const LambdaBindingPtr& cur) {
					this->visit(cur->getVariable());
					out << " = ";
					this->visit(cur->getLambda());
					out << ";";
					if (count++ < defs.size() -1) this->newLine();
				});

				decreaseIndent();
				newLine();
				out << "}";
		});


		PRINT(Lambda, {
				auto paramPrinter = [&](std::ostream& out, const VariablePtr& cur) {
					this->visit(cur->getType());
				    out << " ";
				    this->visit(cur);
				};

				auto funType = node->getType();

				// print header ...
				if (funType->isConstructor()) {
					// print constructor header
					out << "ctor ";
					this->visit(funType->getObjectType());
					out << " ";
					this->visit(node->getParameters()->getElement(0));
					out << " :: (" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") ";

				} else if (funType->isDestructor()) {
					// print destructor header
					out << "dtor ~";
					this->visit(funType->getObjectType());
					out << " ";
					this->visit(node->getParameters()->getElement(0));
					out << " :: (" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") ";

				} else if (funType->isMemberFunction()) {
					// print member function header
					out << "mfun ";
					this->visit(funType->getObjectType());
					out << " ";
					this->visit(node->getParameters()->getElement(0));
					out << " :: (" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") -> ";
					this->visit(funType->getReturnType());
					out << " ";
				} else {
					// print plain header function
					out << "fun(" << join(", ", node->getParameterList(), paramPrinter) << ") -> ";
					this->visit(funType->getReturnType());
					out << " ";
				}

				// .. and body
				visit(node->getBody());
		});


		PRINT(CallExpr, {

				// obtain flag indicating format
				bool printBrackets = printer.hasOption(PrettyPrinter::PRINT_BRACKETS);

				// test whether for the current call a special format has been registered
				auto function = node->getFunctionExpr();
				auto pos = formatTable.find(function);
				if (pos != formatTable.end()) {
					FormatterPtr formatter = (*pos).second;
					if (printBrackets) out << "(";
					formatter->format(*this, node);
					if (printBrackets) out << ")";
					return;
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

		PRINT(BindExpr, {
				out << "bind(" << join(", ", node->getParameters(),[&](std::ostream& out, const ExpressionPtr& cur) {
					this->visit(cur);
				}) << "){";
				visit(node->getCall());
				out << "}";
		});

		PRINT(CastExpr, {
				if (printer.hasOption(PrettyPrinter::PRINT_CASTS)) {
					out << "CAST<";
				   	this->visit(node->getType());
				    out << ">(";
					this->visit(node->getSubExpression());
					out << ")";
				} else {
					this->visit(node->getSubExpression());
				}
		});

		PRINT(TupleExpr, {
				out << "(" << ::join(", ", node->getExpressions(), [&](std::ostream&, const ExpressionPtr& cur) {
					this->visit(cur);
				}) << ")";
		});

		PRINT(VectorExpr, {
			const size_t limit = 5;	// TODO: parametrize this?
			const std::vector<ExpressionPtr>& elements = node->getExpressions()->getElements();

			bool cut = (elements.size() > limit);
			
			//if (elements.size() > limit) {
				//elements = std::vector<ExpressionPtr>(elements.begin(), elements.begin()+limit);
				//cut = true;
			//}
			
			std::vector<ExpressionPtr>::const_iterator end = cut ? elements.begin()+limit : elements.end();

			out << "[" << ::join(", ", elements.begin(), end, [&](std::ostream&, const ExpressionPtr& cur) {
				this->visit(cur);
			}) << ((cut)?", ...":"") << "]";
		});

		PRINT(JobExpr, {
				// prints the job expression quite similar to a switch expression
				out << "job";
				out << "(";
				this->visit(node->getThreadNumRange());
				out << ")";
				if (!node->getLocalDecls().empty()) {
					out << "[" << ::join(", ", node->getLocalDecls(), [&](std::ostream&, const DeclarationStmtPtr& cur) {
						this->visit(cur);
					}) << "]";
				}
				out << "{"; increaseIndent(); this->newLine();
				for_each(node->getGuardedExprs()->getElements(), [&](const GuardedExprPtr& cur) {
					out << "if ";
					this->visit(cur->getGuard());
					out << " do: ";
					this->visit(cur->getExpression());
					this->newLine();
				});
				out << "default: ";
				this->visit(node->getDefaultExpr());
				decreaseIndent(); this->newLine(); out << "}";
		});

		PRINT(StructExpr, {
				out << "struct{" << ::join(", ", node->getMembers()->getElements(), [&](std::ostream& out, const NamedValuePtr& cur) {
					this->visit(cur->getName());
					out << ":=";
					this->visit(cur->getValue());
				}) << "}";
		});

		PRINT(UnionExpr, {
				out << "union{" << node->getMemberName() << ":=";
				visit(node->getMember());
				out << "}";
		});

		PRINT(RecTypeDefinition, {
				auto defs = node->getDefinitions();
				if (defs.empty()) {
					out << "{ }";
					return;
				}

				out << "{"; increaseIndent(); newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const RecTypeBindingPtr& cur) {
					this->visit(cur->getVariable());
					out << " = ";
					this->visit(cur->getType());
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
				this->newLine();
				this->newLine();
			});
		});

		PRINT(MarkerExpr, {
			bool showMarker = printer.hasOption(PrettyPrinter::Option::PRINT_MARKERS);
			if (showMarker) out << "<m id=" << node->getId() << ">";
			visit(node->getSubExpression());
			if (showMarker) out << "</m>";
		});

		PRINT(MarkerStmt, {
			bool showMarker = printer.hasOption(PrettyPrinter::Option::PRINT_MARKERS);
			if (showMarker) out << "<m id=" << node->getId() << ">";
			visit(node->getSubStatement());
			if (showMarker) out << "</m>";
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
			// check single-line flag
			if (printer.hasOption(PrettyPrinter::PRINT_SINGLE_LINE)) {
				return;
			}
			out.flush();
			// print a new line
			out << std::endl;
			for (unsigned i=0; i<indent; i++) {
				out << std::string(printer.tabSep.second, printer.tabSep.first);
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

		/**
		 * If enabled, prints annotations on Node node.
		 */
		void printAnnotations(const NodePtr& node, bool start) {
			if(printer.hasOption(PrettyPrinter::PRINT_ANNOTATIONS) && node->hasAnnotations()) {
				if(start) {
					out << "$[";
					auto iter = node->getAnnotations().begin(); 
					while(true) {
						out << *iter->second;
						if(++iter != node->getAnnotations().end()) out << ", ";
						else break;
					}
					out << ": ";
				} else {
					out << "]$";
				}
			}
		}
	};

	// OutputStreamWrapper: Wraps the output stream capturing all the operations performed on it
	// it used to keep the current position in the output stream and forward the characters to the 
	// real output stream
	class OutputStreamWrapper : public boost::iostreams::sink {
		std::ostream& out;
		// keep track of the current position in the output stream
		SourceLocation currLoc;
		static const size_t width = 8;
		const bool showLineNo;
		const bool colWrap;
		const size_t colWidth;

		void newLine() {
			++currLoc.first; 		// increment the line number
			currLoc.second = 0;		// set the column number to 0

			if (showLineNo) {
				out << std::setw(width) << std::setiosflags(std::ios::left) << currLoc.first;
			}
		}

	public:
		OutputStreamWrapper(std::ostream& out, bool showLineNo, int columnWrap) : 
			 out(out), currLoc(0,0), showLineNo(showLineNo), colWrap(columnWrap != -1), colWidth(columnWrap)
		{ 
			if(showLineNo) {
				out << std::setw(width) << std::setiosflags(std::ios::left) << 0; 
			}
		}

    	std::streamsize write(const char* s, std::streamsize n) {
			if ( colWrap && (n+currLoc.second) > colWidth ) {
				out << std::endl;
				newLine();
			}
			out.write(s,n);
			// new lines are printed from the pretty printer separately
			// therefore we can capture them easily
			if ( n == 1 && *s == '\n' ) {
				newLine();
				return n;
			}
			currLoc.second += n;
			return n;
		}

		SourceLocation getSrcLoc() const { return currLoc; }

	};

	// InspireMapPrinter: this visitor extend the basic InspirePrinter adding the 
	// capability to map each source range to the corresponding generating IR node 
	struct InspireMapPrinter : public InspirePrinter {
		
		// reference to the underlying output stream
		std::ostream& out;
		// reference to the stream wrapper used to get the current position in the 
		// generated code 
		const OutputStreamWrapper& wout;

		// Range -> IR nodes map
		SourceLocationMap& srcMap;

		InspireMapPrinter(boost::iostreams::stream<OutputStreamWrapper>& out, SourceLocationMap& srcMap, const PrettyPrinter& printer)
				: InspirePrinter(out, printer), out(out), wout(*out), srcMap(srcMap) { }

		void visit(const NodePtr& node) {

			out.flush();
			SourceLocation&& start = wout.getSrcLoc();
			InspirePrinter::visit(node);
			out.flush();
			SourceLocation&& end = wout.getSrcLoc();

			srcMap.insert( std::make_pair(SourceRange(start,end), node) );
		}
	};


	/**
	 * A utility function to obtain the n-th argument within the given call expression.
	 *
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the requested argument
	 * @return the requested argument or a NULL pointer in case there is no such argument
	 */
	ExpressionPtr getArgument(const CallExprPtr& call, unsigned n) {
		if (n < call.size()) {
			return call[n];
		}
		return ExpressionPtr();
	}

	/**
	 * A utility function printing the n-th argument of a call expression.
	 *
	 * @param printer the printer to be used for the actual printing
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the argument to be printed; in case there is no such argument a ? is printed.
	 */
	void printArgument(InspirePrinter& printer, const CallExprPtr& call, unsigned n) {
		NodePtr&& argument = getArgument(call, n);
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
	FormatTable initFormatTable(const PrettyPrinter& config) {
		FormatTable res;

		// get lang basic
		NodeManager& mgr = config.root->getNodeManager();
		const lang::BasicGenerator& basic = mgr.getLangBasic();

		#define OUT(Literal) printer.out << Literal
		#define ARG(N) getArgument(call, N)
		#define MGR call->getNodeManager()
		#define PRINT_EXPR(E) printer.visit(E)
		#define PRINT_ARG(N) printArgument(printer, call, N)
		#define HAS_OPTION(OPT) printer.getPrettyPrint().hasOption(PrettyPrinter::OPT)
		#define ADD_FORMATTER(Literal, FORMAT) \
					res.insert(std::make_pair(Literal, make_formatter([](InspirePrinter& printer, const CallExprPtr& call) FORMAT ))).second;


		if (config.hasOption(PrettyPrinter::PRINT_DEREFS)) {
			ADD_FORMATTER(basic.getRefDeref(), { OUT(" *"); PRINT_ARG(0); });
		} else {
			ADD_FORMATTER(basic.getRefDeref(), { PRINT_ARG(0); });
		}

		ADD_FORMATTER(basic.getRefAssign(), { PRINT_ARG(0); OUT(" := "); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRefVar(), { OUT(" var("); PRINT_ARG(0); OUT(")"); });
		ADD_FORMATTER(basic.getRefNew(), { OUT(" new("); PRINT_ARG(0); OUT(")"); });
		ADD_FORMATTER(basic.getRefDelete(), { OUT(" del("); PRINT_ARG(0); OUT(")"); });

		if (!config.hasOption(PrettyPrinter::PRINT_DEREFS)) {
			ADD_FORMATTER(basic.getStringToCharPointer(), { PRINT_ARG(0); });
		}

		ADD_FORMATTER(basic.getDataPathRoot(), { OUT("<>"); });
		ADD_FORMATTER(basic.getDataPathMember(),  { PRINT_ARG(0); OUT("."); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getDataPathElement(), { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getDataPathComponent(), { PRINT_ARG(0); OUT("."); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getDataPathParent(), {PRINT_ARG(0); OUT(".as<"); PRINT_ARG(1); OUT(">"); });

		ADD_FORMATTER(basic.getArraySubscript1D(), { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getArraySubscriptND(), { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getArrayRefElem1D(), { PRINT_ARG(0); OUT("&["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getArrayRefElemND(), { PRINT_ARG(0); OUT("&["); PRINT_ARG(1); OUT("]"); });

		ADD_FORMATTER(basic.getVectorSubscript(), { PRINT_ARG(0); OUT("["); PRINT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getVectorRefElem(), { PRINT_ARG(0); OUT("&["); PRINT_ARG(1); OUT("]"); });


		ADD_FORMATTER(basic.getCompositeRefElem(), { PRINT_ARG(0); OUT("->"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCompositeMemberAccess(), { PRINT_ARG(0); OUT("."); PRINT_ARG(1); });
	
		ADD_FORMATTER(basic.getRealAdd(), { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealSub(), { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealMul(), { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealDiv(), { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntAdd(), { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntSub(), { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntMul(), { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntDiv(), { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntMod(), { PRINT_ARG(0); OUT("%"); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntAdd(), { PRINT_ARG(0); OUT("+"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntSub(), { PRINT_ARG(0); OUT("-"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMul(), { PRINT_ARG(0); OUT("*"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntDiv(), { PRINT_ARG(0); OUT("/"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMod(), { PRINT_ARG(0); OUT("%"); PRINT_ARG(1); });

		// nicer inlined versions of the && and || operators
//		ADD_FORMATTER(basic.getBoolLAnd(), { PRINT_ARG(0); OUT(" && "); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getBoolLAnd(), { PRINT_ARG(0); OUT(" && "); if (HAS_OPTION(NO_EVAL_LAZY)) PRINT_ARG(1); else PRINT_EXPR(transform::evalLazy(MGR, ARG(1))); });
//		ADD_FORMATTER(basic.getBoolLOr(), { PRINT_ARG(0); OUT(" || "); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getBoolLOr(), { PRINT_ARG(0); OUT(" || "); if (HAS_OPTION(NO_EVAL_LAZY)) PRINT_ARG(1); else PRINT_EXPR(transform::evalLazy(MGR, ARG(1))); });
		ADD_FORMATTER(basic.getBoolEq(), { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getBoolLNot(), { OUT("!"); PRINT_ARG(0); });

		ADD_FORMATTER(basic.getCharNe(), { PRINT_ARG(0); OUT("!="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCharEq(), { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCharGe(), { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCharGt(), { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCharLt(), { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getCharLe(), { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntEq(), { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntNe(), { PRINT_ARG(0); OUT("!="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntGe(), { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntGt(), { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntLt(), { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntLe(), { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntEq(), { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntNe(), { PRINT_ARG(0); OUT("!="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntGe(), { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntGt(), { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntLt(), { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntLe(), { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getRealEq(), { PRINT_ARG(0); OUT("=="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealNe(), { PRINT_ARG(0); OUT("!="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealGe(), { PRINT_ARG(0); OUT(">="); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealGt(), { PRINT_ARG(0); OUT(">"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealLt(), { PRINT_ARG(0); OUT("<"); PRINT_ARG(1); });
		ADD_FORMATTER(basic.getRealLe(), { PRINT_ARG(0); OUT("<="); PRINT_ARG(1); });

		ADD_FORMATTER(basic.getCreateMinRange(), { OUT("["); PRINT_ARG(0); OUT("-inf]"); });
		ADD_FORMATTER(basic.getCreateBoundRange(), { OUT("["); PRINT_ARG(0); OUT("-"); PRINT_ARG(1); OUT("]"); });
		
		ADD_FORMATTER(basic.getIfThenElse(), {
				OUT("("); PRINT_ARG(0); OUT(")?");
				if (HAS_OPTION(NO_EVAL_LAZY)) PRINT_ARG(1); else PRINT_EXPR(transform::evalLazy(MGR, ARG(1)));
				OUT(":");
				if (HAS_OPTION(NO_EVAL_LAZY)) PRINT_ARG(2); else PRINT_EXPR(transform::evalLazy(MGR, ARG(2)));
		});

		ADD_FORMATTER(basic.getBarrier(), { OUT("barrier()"); });

		if (!config.hasOption(PrettyPrinter::NO_LIST_SUGAR)) {
			// add semantic sugar for list handling
			const encoder::ListExtension& ext = config.root->getNodeManager().getLangExtension<encoder::ListExtension>();

			typedef encoder::ListConverter<ExpressionPtr, encoder::DirectExprConverter> AttributConverter;

			ADD_FORMATTER(ext.empty, { OUT("[]"); });
			ADD_FORMATTER(ext.cons, {
					vector<ExpressionPtr> list = (encoder::toValue<vector<ExpressionPtr>, AttributConverter>(call));
					printer.out << "[" << join(",", list, [&](std::ostream& out, const ExpressionPtr& cur) {
						printer.visit(cur);
					}) << "]";
			});
		}

		if (!config.hasOption(PrettyPrinter::PRINT_ATTRIBUTES)) {
			const analysis::AttributeExtension& ext = mgr.getLangExtension<analysis::AttributeExtension>();
			ADD_FORMATTER(ext.getAttr(), { PRINT_ARG(0); });
		}


		#undef ADD_FORMATTER
		#undef OUT
		#undef ARG
		#undef PRINT_EXPR
		#undef PRINT_ARG


		return res;
	}

} // end of anonymous namespace

SourceLocationMap printAndMap( std::ostream& out, const insieme::core::printer::PrettyPrinter& print, bool showLineNo, int columnWrap) { 
	using namespace insieme::core::printer;
	// create a boost stream out of it and pass it to the visitor
	boost::iostreams::stream<OutputStreamWrapper> wrappedOutStream( out, showLineNo, columnWrap );
	
	// In order to avoid a copy when the map is returned, we pass it to the printer
	SourceLocationMap srcMap;
	
	InspireMapPrinter printer(wrappedOutStream, srcMap, print);
	printer.visit(print.root);
	wrappedOutStream.flush();

	return srcMap;
}

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
std::ostream& operator<<(std::ostream& out, const insieme::core::printer::PrettyPrinter& print) {
	// use inspire printer to print the code ...
	insieme::core::printer::InspirePrinter printer(out, print);
	printer.print(print.root);
	return out;
}


std::ostream& operator<<(std::ostream& out, const  insieme::core::printer::SourceLocationMap& srcMap) {
	using namespace insieme::core::printer;

	for(SourceLocationMap::const_iterator it = srcMap.begin(), end=srcMap.end(); it != end; ++it) {
		std::string&& stmt = toString(*it->second);
		size_t length = stmt.length();
		
		std::cout << "@ RANGE: " << it->first << std::endl 
			      << "\t-> IR node [addr: " << &*it->second << "] ";
	   
		if(length < 10)
			out << stmt;
		else {
			// we want to show the last 5 chars just to give an idea of the context
			size_t remains = (length-10)>5?5:length-10;
			out << stmt.substr(0,10) << "..." << stmt.substr(length-remains, length-1); 
		}
		out << std::endl;
	}
	return out;
}

std::ostream& operator<<(std::ostream& out, const  insieme::core::printer::SourceLocation& loc) {
	return out << loc.first << ":" << loc.second;
}

std::ostream& operator<<(std::ostream& out, const  insieme::core::printer::SourceRange& range) {
	return out << "[" << range.first << " - " << range.second << "]";
}


}

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

#include "insieme/core/printer/pretty_printer.h"

#include <cassert>
#include <memory>
#include <iomanip>
#include <stack>

#include <boost/unordered_map.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/lang/parallel.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/annotations/naming.h"

#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/concepts.hpp>

#include "insieme/core/analysis/parentheses.h"
#include "insieme/core/printer/lexer.h"


namespace insieme {
namespace core {
namespace printer {


	namespace detail {

		/**
		 * A factory for a empty plug-in.
		 */
		const PrinterPlugin& getEmptyPlugin() {
			static const struct EmptyPlugin : public PrinterPlugin {
				virtual bool covers(const NodeAddress&) const {
					return false;
				}
				virtual std::ostream& print(std::ostream& out, const NodeAddress&, const std::function<void(const NodeAddress&)>&) const {
					assert_fail() << "Should not be reached!";
					return out;
				}
				EmptyPlugin() {}
			} empty;
			return empty;
		}
	}


	// set up default formats for pretty printer
	const unsigned PrettyPrinter::OPTIONS_DEFAULT = 0;
	const unsigned PrettyPrinter::OPTIONS_DETAIL =
	    PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY;
	const unsigned PrettyPrinter::OPTIONS_MAX_DETAIL = PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
	                                                   | PrettyPrinter::PRINT_ANNOTATIONS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
	                                                   | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_LITERAL_TYPES | PrettyPrinter::PRINT_DERIVED_IMPL;
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
		flags = (status) ? (flags | option) : (flags & ~option);
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
			virtual void format(InspirePrinter& printer, const CallExprAddress& call) = 0;
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
		template <typename Lambda>
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
			virtual void format(InspirePrinter& printer, const CallExprAddress& call) {
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
		template <typename Lambda>
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
		class InspirePrinter : public IRVisitor<void, Address> {
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

            /**
             * A stack used to keep track of the "this"-operator
             */
            std::stack<VariablePtr> thisStack;

			bool singleLineTypes;
			int funCounter = 0;

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
			    : IRVisitor<void, Address>(true), formatTable(initFormatTable(printer)), indent(0), printer(printer), depth(0), out(out){};

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
				if(printer.hasOption(PrettyPrinter::NO_LET_BINDINGS) || printer.hasOption(PrettyPrinter::PRINT_SINGLE_LINE)) {
					// skip computation of bindings
					visit(NodeAddress(node));
					return;
				}

				// compute set of substitutions
				int typeCounter = 0;
				singleLineTypes = false; // enable multiline type definitions

				visitDepthFirstOnce(node, [&](const NodePtr& cur) {

					// do not let-bind build ins
					if(lang::isBuiltIn(cur)) { return; }

					// do not decent into derived implementations if not printed
					if(!printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(cur)) { return; }

					NodeType type = cur->getNodeType();

					if(type == NT_TagType || (!printer.hasOption(PrettyPrinter::NO_LET_BOUND_FUNCTIONS) && type == NT_LambdaExpr)) {
						string name;

						if(insieme::core::annotations::hasAttachedName(cur) &&
							!insieme::core::annotations::getAttachedName(cur).compare("main"))
							name = insieme::core::annotations::getAttachedName(cur);
						else

							name = (type == NT_LambdaExpr)?format("fun%03d", funCounter++):format("type%03d", typeCounter++);

						if(auto tagType = cur.isa<TagTypePtr>()) {


							out << "let " << name << " = ";
							out << ((tagType->isStruct()) ? "struct " : "union ");
							this->visit(NodeAddress(tagType->getName()));

							// include all inherited classes if there are any
							if (StructPtr structType = analysis::isStruct(tagType)) {
								if (!structType->getParents().empty()) {
									out << " : [";
									out << join(", ", structType->getParents(), [&](std::ostream& out, const ParentPtr& parent) {
										this->visit(NodeAddress(parent->getType()));
									});
									out << "]";
								}
							}

							out << " { ";

							// print fields
							out << join("; ", tagType->getFields(), [&](std::ostream&,const FieldPtr& cur) {
								this->visit(NodeAddress(cur->getType()));   //print type
								out << " ";
								this->visit(NodeAddress(cur->getName()));   //print field name
							});

							out << " };" << std::endl;

						}

						// avoid printing more than one scope
						if(!printer.hasOption(PrettyPrinter::JUST_OUTERMOST_SCOPE)) {
							// print a let binding

							if (auto lambda = cur.isa<LambdaExprPtr>()) {

								if (name.compare("main") != 0) {
									// not the main function
									out << "let " << name << " = ";
									visit(NodeAddress(cur));
									out << ";\n\n";
								} else {
									// the special case for the main function
									auto paramPrinter = [&](std::ostream &out, const VariablePtr &cur) {
										this->visit(NodeAddress(cur->getType()));                  // cur->getType());
										out << " ";
										this->visit(NodeAddress(cur));
									};

									out << std::endl;

									// printing the return Value
									visit(NodeAddress(lambda->getType().as<FunctionTypePtr>()->getReturnType()));

									// print the signature of the main function
									out << " " << name << "(" << join(", ", cur.as<LambdaExprPtr>()->getParameterList(), paramPrinter) << ")";

									// print the body of the main function
									visit(NodeAddress(cur.as<LambdaExprPtr>().getBody()));
									out << "\n\n";
								}
							}



						}

						// add a substitution rule
						letBindings[cur] = name;
					}

				}, false); // iterate through IR in post-order

				if(printer.hasOption(PrettyPrinter::JUST_OUTERMOST_SCOPE)) { letBindings.erase(node); }

				singleLineTypes = true;
				visit(NodeAddress(node));
			}

			~InspirePrinter() {
				// once the printer is done, the plugin might want to do something
				printer.plugin.afterAllDone(out);
			}

			/**
			 * Wrapper for general tasks
			 */
			virtual void visit(const NodeAddress& element) {
				// check whether this one is covered by the plug-in
				if(printer.plugin.covers(element)) {
					printer.plugin.print(out, element, *this);
					return;
				}

				// check whether this one has been substituted
				auto pos = letBindings.find(element);
				if(pos != letBindings.end()) {
					out << pos->second;
					return;
				}

				if(depth > printer.maxDepth) {
					out << " ... ";
					return;
				}
				depth++;
				//printAnnotations(element, true);
				IRVisitor<void,Address>::visit(element);
				//printAnnotations(element, false);
				out.flush();
				depth--;
			}

			/**
			 * A macro simplifying the definition for print routine of some node type.
			 */
			#define PRINT(NodeType)                                                                                                                      \
				virtual void visit##NodeType(const NodeType##Address& node)

			/**
			 * A macro for visiting the NodePtr
			 */
			#define VISIT(VNode) this->visit(NodeAddress(VNode));


			PRINT(Value) {
				// identifiers can be directly printed
				out << *node;
			}

			PRINT(GenericType) {
				out << *node->getName();
				const TypesPtr& types = node->getTypeParameter();

				if(types->empty()) { return; }

				out << "<" << join(",", types, [&](std::ostream&, const TypePtr& cur) { VISIT(cur); }) << ">";
			}

			PRINT(FunctionType) {

				auto printer = [&](std::ostream&, const TypePtr& cur) { VISIT(cur); };

				if(node->isConstructor()) {
					out << "ctor ";
					VISIT(node->getObjectType()->getChild(0));
					auto begin = node->getParameterTypes().begin() + 1;
					auto end = node->getParameterTypes().end();
					out << "::(" << join(", ", begin, end, printer) << ")";
				} else if(node->isDestructor()) {
					out << "~";
					VISIT(node->getObjectType()->getChild(0));
					out << "::()";
				} else if(node->isMemberFunction()) {
					out << "method ";
					//out << std::endl;
					//out << "node->getChildList(): " << node.getChildList() << std::endl;
					VISIT(node->getObjectType()->getChild(0));
					auto begin = node->getParameterTypes().begin() + 1;
					auto end = node->getParameterTypes().end();
					out << "::(" << join(", ", begin, end, printer) << ") -> ";
					VISIT(node->getReturnType());
				} else {
					out << "(" << join(", ", node->getParameterTypes(), printer) << ") ";
					out << ((node->isPlain()) ? "->" : "=>");
					out << " ";
					VISIT(node->getReturnType());
				}
			}

			PRINT(TagType) {

				// support simpler output of non-recursive types
				if (!node->isRecursive()) {
					VISIT(node->getRecord());
					return;
				}

				out << "rec ";
				VISIT(node->getTag());

				string newItem = "\n\t";
				string newLine = "\n";

				if(singleLineTypes) {
					newItem = "";
					newLine = "";
				}

				out << "{" << newItem << join(", " + newItem, node->getDefinition()->getDefinitions(), [&](std::ostream& jout, const TagTypeBindingPtr& cur) {
					VISIT(cur->getTag());
					jout << "=";
					VISIT(cur->getRecord());
				}) << newLine << "}";
			}

			PRINT(Record) {

				string newItem = "\n\t";
				string newLine = "\n";

				if(singleLineTypes) {
					newItem = "";
					newLine = "";
				}

				auto strct = analysis::isStruct(node);

				out << (strct ? "struct " : "union ");

				if(!node->getName()->getValue().empty()) {
					out << node->getName()->getValue() << " ";
				}

				if(strct && !strct->getParents().empty()) {
					out << ": " << join(", ", strct->getParents(), [&](std::ostream& out, const ParentPtr& parent) {
						if(parent->isVirtual()) { out << "virtual "; }
						VISIT(parent->getType());
					}) << " ";
				}

				out << "{" << newItem << join(";" + newItem, node->getFields(), [&](std::ostream& out, const FieldPtr& cur) {
					VISIT(cur->getType());
					out << " ";
					VISIT(cur->getName());
				});

				out << newLine << "}";
			}

			PRINT(TupleType) { out << "" << join(",", node->getElementTypes(), [&](std::ostream&, const TypePtr& cur) { VISIT(cur); }) << ""; }

			PRINT(Type) { out << *node; }

			PRINT(BreakStmt) { out << "break"; }

			PRINT(ContinueStmt) { out << "continue"; }

			PRINT(ReturnStmt) {
				out << "return ";
				VISIT(node->getReturnExpr());
			}

			PRINT(ThrowStmt) {
				out << "throw ";
				VISIT(node->getThrowExpr());
			}

			PRINT(GotoStmt) {
				out << "goto ";
				VISIT(node->getLabel());
			}

			PRINT(LabelStmt) {
				VISIT(node->getLabel());
				out << ":";
			}

			PRINT(DeclarationStmt) {
				// print type
				const VariablePtr& var = node->getVariable();
				out << "decl ";
				VISIT(var->getType());
				out << " ";
				VISIT(var);
				out << " = ";
				VISIT(node->getInitialization());
			}

			PRINT(CompoundStmt) {
				auto list = node->getStatements();
				if(list.empty()) {
					out << "{ }";
					return;
				}

				out << "{";
				increaseIndent();
				newLine();
				for_each(list.begin(), list.end() - 1, [&](const NodePtr& cur) {
					VISIT(cur);
					out << ";";
					this->newLine();
				});
				VISIT(list.back());
				out << ";";
				decreaseIndent();
				newLine();
				out << "}";
			}

			PRINT(WhileStmt) {
				// variables can be directly printed
				out << "while(";
				VISIT(node->getCondition());
				out << ") ";
				VISIT(node->getBody());
			}

			PRINT(ForStmt) {
				// variables can be directly printed
				out << "for( ";
				VISIT(node->getIterator()->getType());
				out << " ";
				VISIT(node->getIterator());
				out << " = ";
				VISIT(node->getStart());
				out << " .. ";
				VISIT(node->getEnd());
				out << " : ";
				VISIT(node->getStep());
				out << ") ";

				NodePtr&& body = node->getBody();
				if(body->getNodeType() != NT_CompoundStmt) {
					increaseIndent();
					this->newLine();
					VISIT(body);
					decreaseIndent();
					this->newLine();
				} else {
					VISIT(body);
				}
			}

			PRINT(IfStmt) {
				// variables can be directly printed
				out << "if(";
				VISIT(node->getCondition());
				out << ") ";
				VISIT(node->getThenBody());
				if(!analysis::isNoOp(node->getElseBody())) {
					out << " else ";
					VISIT(node->getElseBody());
				}
			}

			PRINT(SwitchStmt) {
				// variables can be directly printed
				out << "switch(";
				VISIT(node->getSwitchExpr());
				out << ") {";
				increaseIndent();
				this->newLine();
				for_each(node->getCases()->getCases(), [&](const SwitchCasePtr& cur) {
					out << "case ";
					VISIT(cur->getGuard());
					out << ": ";
					VISIT(cur->getBody());
					this->newLine();
				});
				out << "default: ";
				VISIT(node->getDefaultCase());
				decreaseIndent();
				this->newLine();
				out << "}";
			}

			PRINT(TryCatchStmt) {
				// variables can be directly printed
				out << "try ";
				VISIT(node->getBody());
				for(auto clause : node->getClauses()) {
					out << " catch(";
					VISIT(clause->getVariable()->getType());
					out << " ";
					VISIT(clause->getVariable());
					out << ") ";
					VISIT(clause->getBody());
				}
			}

			PRINT(Variable) {

				// print variable names if attached to the node... otherwise
				// a variable like "v..." will be used
		/*		if (insieme::core::annotations::hasAttachedName(node)) {
					out << insieme::core::annotations::getAttachedName(node);;
				} else*/ if(!thisStack.empty() && !(*node).toString().compare(thisStack.top().toString())) {
					out << "this";
				} else {
					out << *node;
				}

			}

			PRINT(Literal) {
				// special handling of type literals (ignore value)
				if(!printer.hasOption(PrettyPrinter::PRINT_LITERAL_TYPES) && analysis::isTypeLiteral(node.getParentNode(0))) {
					out << "type_lit(";
					visit(NodeAddress(node->getType().as<GenericTypePtr>()->getTypeParameter(0)));
					out << ")";
					return;
				}
 				const string& str = node->getStringValue();
				if(printer.hasOption(PrettyPrinter::NAME_CONTRACTION) && str.size() > 9) {
					out << str.substr(0, 3) << "..." << str.substr(str.size() - 3, str.size());
				} else {
					out << str;
					if(!node->getChild(0)->getChildList().empty() ) {
						if (!node->getChild(0)->getChild(0)->toString().compare("real")) {
							if (!node->getChild(0)->getChild(2)->getChild(0)->toString().compare("4"))
								out << "f";
						} else {
							if (!node->getChild(0)->getChild(0)->toString().compare("uint"))
								out << "u";
							if(node->getChild(0)->getChildList().size() > 2 && node->getChild(0)->getChild(2)->getChildList().size() > 0) {
								if (!node->getChild(0)->getChild(2)->getChild(0)->toString().compare("8"))
									out << "l";
								else if (!node->getChild(0)->getChild(2)->getChild(0)->toString().compare("16"))
									out << "ll";
							}
						}
					}
				}

				// add type if requested
				if(printer.hasOption(PrettyPrinter::PRINT_LITERAL_TYPES)) {
					out << ":";
					VISIT(node->getType());
				}
			}

			PRINT(LambdaExpr) {

				if(!printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(node)) {
					out << lang::getConstructName(node);
					return;
				}

				bool noExpandLambdas = printer.hasOption(PrettyPrinter::NO_EXPAND_LAMBDAS);
				if(noExpandLambdas) {
					out << "fun{...}";
					return;
				}

				// some sanity check frequently encountered
				assert_true(node->getLambda()) << "Accessing non-present Lambda Definition!";

				// short-cut for non-recursive functions
				if(!node->isRecursive()) {
					VISIT(node->getLambda());
					return;
				}

				// general case: recursive function
				out << "recFunc ";
				VISIT(node->getVariable());
				out << " ";
				VISIT(node->getDefinition());
			}


			PRINT(LambdaDefinition) {
				auto defs = node->getDefinitions();
				if(defs.empty()) { return; }

				out << "{";
				increaseIndent();
				newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const LambdaBindingPtr& cur) {
					VISIT(cur->getVariable());
					out << " = ";
					VISIT(cur->getLambda());
					out << ";";
					if(count++ < defs.size() - 1) { this->newLine(); }
				});

				decreaseIndent();
				newLine();
				out << "}";
			}


			PRINT(Lambda) {
				auto paramPrinter = [&](std::ostream& out, const VariablePtr& cur) {
					VISIT(cur->getType());
					out << " ";
					VISIT(cur);
				};

				auto funType = node->getType();

				// print header ...
				if(funType->isConstructor()) {
					// print constructor header
					out << "ctor ";
					VISIT(funType->getObjectType());
					out << " ";
					VISIT(node->getParameters()->getElement(0));
					out << " :: (" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isDestructor()) {
					// print destructor header
					out << "~";
					VISIT(funType->getObjectType());
					out << " ";
					VISIT(node->getParameters()->getElement(0));
					out << " :: (" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isMemberFunction()) {
					// print member function header
					out << "function ";
					VISIT(funType->getObjectType());
					out << "::(" << join(", ", node->getParameters().begin() + 1, node->getParameters().end(), paramPrinter) << ") -> ";
					VISIT(funType->getReturnType());
					out << " ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else {
					// print plain header function
					out << "function(" << join(", ", node->getParameterList(), paramPrinter) << ") -> ";
					VISIT(funType->getReturnType());
					out << " ";
					// .. and body
					VISIT(node->getBody());
				}
			}


			PRINT(CallExpr) {

				// obtain flag indicating format
				bool printBrackets = !printer.hasOption(PrettyPrinter::SKIP_BRACKETS);

				// don't print brackets for derefs
				if(printBrackets && !printer.hasOption(PrettyPrinter::PRINT_DEREFS)) {
					auto& refExt = node->getNodeManager().getLangExtension<lang::ReferenceExtension>();
					if (analysis::isCallOf(node.getParentNode(0), refExt.getRefDeref())) {
						printBrackets = false;
					}
				}

				// test whether for the current call a special format has been registered
				auto function = node->getFunctionExpr();
				auto pos = formatTable.find(function);
				if(pos != formatTable.end()) {
					FormatterPtr formatter = (*pos).second;
					formatter->format(*this, CallExprAddress(node));
					return;
				}

				// test whether function is a derived literal
				if(!printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(function)) {
					out << lang::getConstructName(function);
				} else {
					// default formating
					VISIT(node->getFunctionExpr());
				}

				// print argument list
				auto arguments = node->getArguments();
				if(arguments.empty()) {
					out << "()";
					return;
				}

				out << "(";
				auto begin = arguments.begin();
				VISIT(*begin);
				for_each(begin + 1, arguments.end(), [&](const NodePtr& cur) {
					out << ", ";
					VISIT(cur);
				});
				out << ")";
			}

			PRINT(BindExpr) {
				out << "function(" << join(", ", node->getParameters(),[&](std::ostream& out, const ExpressionPtr& cur) {
					VISIT(cur->getType());
					out << " ";
					VISIT(cur);
				}) << ")=> ";
				VISIT(node->getCall());                 // ???????????????????
			}

			PRINT(CastExpr) {
				if(printer.hasOption(PrettyPrinter::PRINT_CASTS)) {
					out << "CAST(";
					VISIT(node->getType());
					out << ") ";
					VISIT(node->getSubExpression());
				} else {
					VISIT(node->getSubExpression());
				}
			}

			PRINT(TupleExpr) {
				out << "(" << ::join(", ", node->getExpressions(), [&](std::ostream&, const ExpressionPtr& cur) { VISIT(cur); }) << ")";
			}

			PRINT(JobExpr) {
				// prints the job expression quite similar to a switch expression
				out << "job";
				out << "(";
				VISIT(node->getThreadNumRange());
				out << ")";
				out << "{";
				increaseIndent();
				this->newLine();
				VISIT(node->getBody());
				decreaseIndent();
				this->newLine();
				out << "}";
			}

			PRINT(StructExpr) {
				out << "struct{" << ::join(", ", node->getMembers()->getElements(), [&](std::ostream& out, const NamedValuePtr& cur) {
					VISIT(cur->getName());
					out << "=";
					VISIT(cur->getValue());
				}) << "}";
			}

			PRINT(UnionExpr) {
				out << "union{" << node->getMemberName()->getValue() << "=";
				visit(node->getMember());
				out << "}";
			}

			PRINT(TagTypeDefinition) {
				auto defs = node->getDefinitions();
				if(defs.empty()) {
					out << "{ }";
					return;
				}

				out << "{";
				increaseIndent();
				newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const TagTypeBindingPtr& cur) {
					VISIT(cur->getTag());
					out << " = ";
					VISIT(cur->getRecord());
					out << ";";
					if(count++ < defs.size() - 1) { this->newLine(); }
				});

				decreaseIndent();
				newLine();
				out << "}";
			}

			PRINT(Program) {
/*				out << "// Inspire Program ";
				newLine();
				for_each(node->getEntryPoints(), [&](const NodePtr& cur) {
					this->out << "//  Entry Point: ";
					this->newLine();
					this->increaseIndent();
					this->visit(cur);
					this->decreaseIndent();
					this->newLine();
					this->newLine();
				});
*/			}

			PRINT(MarkerExpr) {
				bool showMarker = printer.hasOption(PrettyPrinter::Option::PRINT_MARKERS);
				if(showMarker) out << "<m id=" << node->getId() << ">";
				visit(node->getSubExpression());
				if(showMarker) out << "</m>";
			}

			PRINT(MarkerStmt) {
				bool showMarker = printer.hasOption(PrettyPrinter::Option::PRINT_MARKERS);
				if(showMarker) out << "<m id=" << node->getId() << ">";
				visit(node->getSubStatement());
				if(showMarker) out << "</m>";
			}

			/**
			 * A generic solution for unknown types. In this case, the
			 * default debug print is forwarded to the output stream.
			 */
			PRINT(Node) { out << "<?>" << *node << "</?>"; }

			/**
			 * Creates a new line.
			 */
			void newLine() const {
				// check single-line flag
				if(printer.hasOption(PrettyPrinter::PRINT_SINGLE_LINE)) { return; }
				out.flush();
				// print a new line
				out << std::endl;

				printer.plugin.afterNewLine(out);

				for(unsigned i = 0; i < indent; i++) {
					out << printer.tabSep;
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
							if(++iter != node->getAnnotations().end()) {
								out << ", ";
							} else {
								break;
							}
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
				++currLoc.first;    // increment the line number
				currLoc.second = 0; // set the column number to 0

				if(showLineNo) { out << std::setw(width) << std::setiosflags(std::ios::left) << currLoc.first; }
			}

		  public:
			OutputStreamWrapper(std::ostream& out, bool showLineNo, int columnWrap)
			    : out(out), currLoc(0, 0), showLineNo(showLineNo), colWrap(columnWrap != -1), colWidth(columnWrap) {
				if(showLineNo) { out << std::setw(width) << std::setiosflags(std::ios::left) << 0; }
			}

			std::streamsize write(const char* s, std::streamsize n) {
				if(colWrap && (n + currLoc.second) > colWidth) {
					out << std::endl;
					newLine();
				}
				out.write(s, n);
				// new lines are printed from the pretty printer separately
				// therefore we can capture them easily
				if(n == 1 && *s == '\n') {
					newLine();
					return n;
				}
				currLoc.second += n;
				return n;
			}

			SourceLocation getSrcLoc() const {
				return currLoc;
			}
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
			    : InspirePrinter(out, printer), out(out), wout(*out), srcMap(srcMap) {}

			void visit(const NodeAddress& node) {
				out.flush();
				SourceLocation start = wout.getSrcLoc();
				InspirePrinter::visit(NodeAddress(node));
				out.flush();
				SourceLocation end = wout.getSrcLoc();

				srcMap.insert(std::make_pair(SourceRange(start, end), node.getParentNode(0)));
			}
		};


		/**
		 * A utility function to obtain the n-th argument within the given call expression.
		 *
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the requested argument
		 * @return the requested argument or a NULL pointer in case there is no such argument
		 */
		ExpressionPtr getArgument(const CallExprAddress& call, unsigned n) {
			if(n < call.size()) { return call[n]; }
			return ExpressionPtr();
		}

		/**
		 * A utility function to obtain the n-th argument within the given call expression.
		 *
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the requested argument
		 * @return the requested argument or a NULL pointer in case there is no such argument
		 */
		NodeAddress getAddressedArgument(const CallExprAddress& call ,unsigned n) {
			if (n < call.getChildAddresses().size()) {
				return call[n];
			}
			return NodeAddress();
		}

		/**
		 * A utility function printing the n-th argument of a call expression.
		 *
		 * @param printer the printer to be used for the actual printing
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the argument to be printed; in case there is no such argument a ? is printed.
		 */
		void printArgument(InspirePrinter& printer, const CallExprAddress& call, unsigned n) {
			NodeAddress&& argument = getAddressedArgument(call, n);
			if(argument) {
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
			const lang::ReferenceExtension& refExt = mgr.getLangExtension<lang::ReferenceExtension>();
			const lang::DatapathExtension& dpExt = mgr.getLangExtension<lang::DatapathExtension>();
			const lang::ParallelExtension& parExt = mgr.getLangExtension<lang::ParallelExtension>();


			#define OUT(Literal) printer.out << Literal
			#define ARG(N) getArgument(call, N)
			#define AARG(N) getAddressedArgument(call, N)
			#define MGR call->getNodeManager()
			#define PRINT_EXPR(E) printer.visit(NodeAddress(E))
			#define PRINT_ARG(N) printArgument(printer, call, N)
			#define HAS_OPTION(OPT) printer.getPrettyPrint().hasOption(PrettyPrinter::OPT)
			#define ADD_FORMATTER(Literal, FORMAT)                                                                                                             \
				res.insert(std::make_pair(Literal, make_formatter([](InspirePrinter & printer, const CallExprAddress& call)FORMAT))).second;

			#define ADD_BRACKET_FORMATTER(Literal, FORMAT) \
					ADD_FORMATTER(Literal, { if(!HAS_OPTION(SKIP_BRACKETS)) OUT("("); FORMAT; if(!HAS_OPTION(SKIP_BRACKETS)) OUT(")"); })


			#define ADD_INTELLIGENT_BRACKET_FORMATTER(Literal, FORMAT) \
					ADD_FORMATTER(Literal, { auto needB = insieme::core::analysis::needsParentheses(call); if(needB) OUT("("); FORMAT; if(needB) OUT(")"); })


			if(config.hasOption(PrettyPrinter::PRINT_DEREFS)) {
				ADD_FORMATTER(refExt.getRefDeref(), {
					OUT(" *");
					PRINT_ARG(0);
				});
			} else {
				ADD_FORMATTER(refExt.getRefDeref(), { PRINT_ARG(0); });
			}

			ADD_FORMATTER(refExt.getRefAssign(), {
				PRINT_ARG(0);
				OUT(" = ");
				PRINT_ARG(1);
			});
			ADD_FORMATTER(refExt.getRefVar(), {
				OUT(" var(");
				PRINT_ARG(0);
				OUT(")");
			});
			ADD_FORMATTER(refExt.getRefNew(), {
				OUT(" new(");
				PRINT_ARG(0);
				OUT(")");
			});
			ADD_FORMATTER(refExt.getRefDelete(), {
				OUT(" delete(");
				PRINT_ARG(0);
				OUT(")");
			});

			ADD_FORMATTER(dpExt.getDataPathRoot(), { OUT("<>"); });
			ADD_FORMATTER(dpExt.getDataPathMember(), {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			});
			ADD_FORMATTER(dpExt.getDataPathElement(), {
				PRINT_ARG(0);
				OUT("[");
				PRINT_ARG(1);
				OUT("]");
			});
			ADD_FORMATTER(dpExt.getDataPathComponent(), {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			});
			ADD_FORMATTER(dpExt.getDataPathParent(), {
				PRINT_ARG(0);
				OUT(".as<");
				PRINT_ARG(1);
				OUT(">");
			});

			ADD_FORMATTER(refExt.getRefArrayElement(), {
				PRINT_ARG(0);
				OUT("[");
				PRINT_ARG(1);
				OUT("]");
			});

			ADD_FORMATTER(refExt.getRefMemberAccess(), {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			});
			ADD_FORMATTER(basic.getCompositeMemberAccess(), {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealAdd(), {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealSub(), {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealMul(), {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealDiv(), {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntAdd(), {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntSub(), {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntMul(), {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntDiv(), {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntMod(), {
				PRINT_ARG(0);
				OUT("%");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntAdd(), {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntSub(), {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntMul(), {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntDiv(), {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntMod(), {
				PRINT_ARG(0);
				OUT("%");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntNot(), {
				OUT("~");
				PRINT_ARG(0);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntAnd(), {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntOr(), {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntXor(), {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntLShift(), {
				PRINT_ARG(0);
				OUT("<<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntRShift(), {
				PRINT_ARG(0);
				OUT(">>");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntNot(), {
				OUT("~");
				PRINT_ARG(0);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntAnd(), {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntOr(), {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntXor(), {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntLShift(), {
				PRINT_ARG(0);
				OUT("<<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntRShift(), {
				PRINT_ARG(0);
				OUT(">>");
				PRINT_ARG(1);
			});

			// nicer inlined versions of the && and || operators
			//		ADD_FORMATTER(basic.getBoolLAnd(), { PRINT_ARG(0); OUT(" && "); PRINT_ARG(1); });
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolLAnd(), {
				PRINT_ARG(0);
				OUT(" && ");
				if (HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else
					PRINT_EXPR(transform::evalLazy(MGR, ARG(1)));
			});
			//		ADD_FORMATTER(basic.getBoolLOr(), { PRINT_ARG(0); OUT(" || "); PRINT_ARG(1); });
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolLOr(), {
				PRINT_ARG(0);
				OUT(" || ");
				if (HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else
					PRINT_EXPR(transform::evalLazy(MGR, ARG(1)));
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolOr(), {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolAnd(), {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolEq(), {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolNe(), {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolXor(), {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolLNot(), {
				OUT("!");
				PRINT_ARG(0);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getBoolNot(), {
				OUT("!");
				PRINT_ARG(0);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharNe(), {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharEq(), {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharGe(), {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharGt(), {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharLt(), {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getCharLe(), {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntEq(), {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntNe(), {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntGe(), {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntGt(), {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntLt(), {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getUnsignedIntLe(), {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntEq(), {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntNe(), {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntGe(), {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntGt(), {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntLt(), {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getSignedIntLe(), {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			});

			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealEq(), {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealNe(), {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealGe(), {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealGt(), {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealLt(), {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			});
			ADD_INTELLIGENT_BRACKET_FORMATTER(basic.getRealLe(), {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			});

			ADD_FORMATTER(parExt.getCreateMinRange(), {
				OUT("[");
				PRINT_ARG(0);
				OUT("-inf]");
			});
			ADD_FORMATTER(parExt.getCreateBoundRange(), {
				OUT("[");
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
				OUT("]");
			});

			ADD_FORMATTER(basic.getIfThenElse(), {
				OUT("");
				PRINT_ARG(0);
				OUT("?");
				if(HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else {
					PRINT_EXPR(transform::evalLazy(MGR, ARG(1)));
				}
				OUT(":");
				if(HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(2);
				else {
					PRINT_EXPR(transform::evalLazy(MGR, ARG(2)));
				}
			});

			ADD_FORMATTER(parExt.getBarrier(), { OUT("barrier()"); });

			ADD_FORMATTER(parExt.getAtomic(), {
				OUT("atomic(");
				PRINT_ARG(0);
				OUT(", ");
				PRINT_ARG(1);
				OUT(", ");
				PRINT_ARG(2);
				OUT(")");
			});

			if(!config.hasOption(PrettyPrinter::NO_LIST_SUGAR)) {
				// add semantic sugar for list handling
				const lang::ListExtension& ext = config.root->getNodeManager().getLangExtension<lang::ListExtension>();

				typedef encoder::ListConverter<ExpressionPtr, encoder::DirectExprConverter> AttributConverter;
				typedef AttributConverter::is_encoding_of is_encoding_of_type;

				ADD_FORMATTER(ext.getListEmpty(), { OUT("[]"); });
				ADD_FORMATTER(ext.getListCons(), {
					const is_encoding_of_type is_encoding_of{};
					// check whether syntactic sugar is supported
					if(is_encoding_of(call)) {
						vector<ExpressionPtr> list = (encoder::toValue<vector<ExpressionPtr>, AttributConverter>(call));
						printer.out << "[" << join(",", list, [&](std::ostream& out, const ExpressionPtr& cur) { printer.visit(NodeAddress(cur)); }) << "]";
					} else {
						// use fall-back solution
						printer.out << "[";
						printer.visit(NodeAddress(call[0]));
						printer.out << ",";
						printer.visit(NodeAddress(call[1]));
						printer.out << "]";
					}
				});
			}

			if(!config.hasOption(PrettyPrinter::PRINT_ATTRIBUTES)) {
				const analysis::AttributeExtension& ext = mgr.getLangExtension<analysis::AttributeExtension>();
				ADD_FORMATTER(ext.getAttr(), { PRINT_ARG(0); });
			}

			#undef ADD_BRACKET_FORMATTER
			#undef ADD_FORMATTER
			#undef OUT
			#undef ARG
			#undef PRINT_EXPR
			#undef PRINT_ARG


			return res;
		}

	} // end of anonymous namespace

	SourceLocationMap printAndMap(std::ostream& out, const insieme::core::printer::PrettyPrinter& print, bool showLineNo, int columnWrap) {
		using namespace insieme::core::printer;
		// create a boost stream out of it and pass it to the visitor
		boost::iostreams::stream<OutputStreamWrapper> wrappedOutStream(out, showLineNo, columnWrap);

		// In order to avoid a copy when the map is returned, we pass it to the printer
		SourceLocationMap srcMap;

		InspireMapPrinter printer(wrappedOutStream, srcMap, print);
		printer.print(print.root);
		wrappedOutStream.flush();

		return srcMap;
	}

} // end of namespace printer
} // end of namespace core
} // end of namespace insieme

namespace std {

	namespace {

		const std::string RED = "\033[31m";
		const std::string GREEN = "\033[32m";
		const std::string BLUE = "\033[34m";
		const std::string BLACK = "\033[30m";
		const std::string CYAN = "\033[96m";
		const std::string YELLOW = "\033[33m";
		const std::string GREY = "\033[37m";

		const std::string RESET = "\033[0m";
		const std::string BOLD = "\033[1m";

	} // annon

	/**
	 * Prints the given pretty print to the given output stream.
	 *
	 * @param out the stream the output should be printed to
	 * @param print the element to be printed
	 * @return a reference to the output stream
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::PrettyPrinter& print) {
		// print code into string buffer
		std::stringstream buffer;
		insieme::core::printer::InspirePrinter(buffer, print).print(print.root);

		// use buffer content if there is no color highlighting required
		if(!print.hasOption(insieme::core::printer::PrettyPrinter::USE_COLOR)) { return out << buffer.str(); }


		using namespace insieme::core::printer::detail;
		auto tokens = lex(buffer.str(), false);

		// print tokens one-by-one
		for(auto cur : tokens) {
			// select formating of current token
			// color codes - see: http://en.wikipedia.org/wiki/ANSI_escape_code#graphics
			switch(cur.getType()) {
			case Token::Type::Symbol: out << YELLOW; break;
			case Token::Type::Keyword: out << CYAN; break;
			case Token::Type::Comment: out << GREY; break;
			case Token::Type::Identifier: out << RESET; break;
			case Token::Type::Bool_Literal: out << RED; break;
			case Token::Type::Char_Literal: out << RED; break;
			case Token::Type::Int_Literal: out << RED; break;
			case Token::Type::Float_Literal: out << RED; break;
			case Token::Type::Double_Literal: out << RED; break;
			case Token::Type::String_Literal: out << GREEN; break;
			case Token::Type::WhiteSpace: out << BOLD; break;
			}

			// special cases (differences between parser and printer)
			if(cur.getLexeme() == "fun") { out << CYAN; }
			if(cur.getLexeme() == "job") { out << CYAN; }
			if(cur.getLexeme() == "bind") { out << CYAN; }

			// print token
			out << cur.getLexeme();

			// clear formating
			out << "\033[0m";
		}

		// done
		return out;
	}


	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceLocationMap& srcMap) {
		using namespace insieme::core::printer;

		for(SourceLocationMap::const_iterator it = srcMap.begin(), end = srcMap.end(); it != end; ++it) {
			std::string&& stmt = toString(*it->second);
			size_t length = stmt.length();

			out << "@ RANGE: " << it->first << std::endl << "\t-> IR node [addr: " << &*it->second << "] ";

			if(length < 10) {
				out << stmt;
			} else {
				// we want to show the last 5 chars just to give an idea of the context
				size_t remains = (length - 10) > 5 ? 5 : length - 10;
				out << stmt.substr(0, 10) << "..." << stmt.substr(length - remains, length - 1);
			}
			out << std::endl;
		}
		return out;
	}

	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceLocation& loc) {
		return out << loc.first << ":" << loc.second;
	}

	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::SourceRange& range) {
		return out << "[" << range.first << " - " << range.second << "]";
	}
}

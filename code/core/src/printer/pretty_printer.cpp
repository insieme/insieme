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
#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/reference.h"

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
		 * Since formatter instances are polymorphic, they need to be handled via pointer or
		 * references. Further, the memory management needs to be considered. Therefore, formatter
		 * should be passed using this pointer type, which is based on a shared pointer.
		 */
		typedef std::function<void(InspirePrinter&,const CallExprAddress&)> Formatter;


		// defines the table used for indexing formatter entries
		typedef utils::map::PointerMap<ExpressionPtr, Formatter> FormatTable;

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
			utils::map::PointerMap<NodePtr, std::string> letBindings;

            /**
             * A stack used to keep track of the "this"-operator
             */
            std::stack<VariablePtr> thisStack;

			/**
			 * Used to format the printout
			 */
			bool singleLineTypes;
			mutable bool isFirstLine=true;

			/**
			 * A Map for lambdaexpr names
			 */
			utils::map::PointerMap<LambdaBindingPtr, std::string> lambdaNames;

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

				int funCounter = 0;

				// check whether bindings should be used
				if(printer.hasOption(PrettyPrinter::NO_LET_BINDINGS) || printer.hasOption(PrettyPrinter::PRINT_SINGLE_LINE)) {
					// skip computation of bindings
					visit(NodeAddress(node));
					return;
				}

				singleLineTypes = false; // enable multiline type definitions

				auto extractName = [&](const NodePtr& cur) {
					if (annotations::hasAttachedName(cur)) {
						return annotations::getAttachedName(cur);
					} else if (lang::isDerived(cur)) {
						return lang::getConstructName(cur);
					} else {
						return format("fun%03d", funCounter++);
					}
				};

				// get all lambda/function names
				// visit all lambdas to get names of all non-recursive functions
				visitDepthFirstOnce(node, [&](const LambdaExprPtr &cur) {
					const auto& defaultBinding = cur->getDefinition()->getBindingOf(cur->getVariable());
					if (lambdaNames.find(defaultBinding) != lambdaNames.end()) {
						return;
					}
					lambdaNames[defaultBinding] = extractName(cur);
					for(const auto& binding : cur->getDefinition()->getDefinitions()) {
						if (binding == defaultBinding || lambdaNames.find(binding) != lambdaNames.end()) {
							continue;
						}
						lambdaNames[binding] = extractName(binding);
					}
				});

				if(!printer.hasOption(PrettyPrinter::JUST_LOCAL_CONTEXT)) {
					//first: print all type declarations
					visitDepthFirstOnce(node, [&](const TagTypePtr& cur) {
						if (cur->getName()->getValue().compare("")) {
							newLine();
							out << "decl ";
							out << ((cur->isStruct()) ? "struct " : "union ");
							//this->visit(NodeAddress(tagType->getName()));
							out << cur->getName()->getValue();
							out << ";";
						}
					});

					//second: print all function declarations
					utils::set::PointerSet<LambdaBindingPtr> visitedBindings;

					visitDepthFirstOnce(node, [&](const LambdaExprPtr& cur) {
						// jump over this part, if lambda is derived, but printer don't have the option to print them
						if(lang::isDerived(cur) && !printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL)) {
							return;
						}
						if (lang::isBuiltIn(cur)) {
							return;
						}

						for(auto& binding : cur->getDefinition()->getDefinitions()) {
							if(!visitedBindings.insert(binding).second) {
								continue;
							}

							const auto& lambda = binding->getLambda();
							const auto& funType = lambda->getType();

							if (!funType->isMember()) {
								newLine();
								out << "decl " << lambdaNames[binding] << " : ";

								out << "(" <<
								join(",", funType->getParameterTypes(), [&](std::ostream& out, const TypePtr& curVar) {
									visit(NodeAddress(curVar));
								}) << ")" << (funType->isVirtualMemberFunction() ? " ~> " : " -> ");
								visit(NodeAddress(funType->getReturnType()));
								out << ";";
							}
						}

					});

					//third: print all declarations for memberFields, constructors, destructors, memberFunctions...
					visitDepthFirstOnce(node, [&](const RecordPtr &cur) {

						// TODO: print all memberFields declarations

						// print all constructors declarations
						auto constructors = cur->getConstructors();
						if (!constructors.empty()) {
							out << join(";", constructors, [&](std::ostream& out, const ExpressionPtr& constr) {
								if (auto ctor = constr.isa<LambdaExprPtr>()) {
									if (cur->getName()->getValue().compare("")) {
										newLine();
										out << "decl " << cur->getName()->getValue() << "::ctor";
										auto params = ctor->getParameterList();
										out << "(" << join(",", params.begin() + 1, params.end(),
														   [&](std::ostream &out, const VariablePtr &curVar) {
															   visit(NodeAddress(curVar->getType()));
														   });
									}
								}
							}) << ";";
						}

						// print the destructor declaration
						auto destructor = cur->getDestructor();
						if (auto dtor = destructor.isa<LambdaExprPtr>()) {
							if (cur->getName()->getValue().compare("")) {
								newLine();
								out << "decl " << cur->getName()->getValue() << "::dtor();";
							}
						}

						// print all memberFunctions declarations
						auto memberFunctions = cur->getMemberFunctions();
						if (!memberFunctions.empty()) {
							out <<
							join(";", memberFunctions, [&](std::ostream &out, const MemberFunctionPtr &memberFun) {
								if (auto member = memberFun->getImplementation().isa<LambdaExprPtr>()) {
									if (cur->getName()->getValue().compare("")) {
										newLine();
										out << "decl " << cur->getName()->getValue() << "::" <<
										memberFun->getName()->getValue();
										auto params = member->getParameterList();

										out << "(" << join(",", params.begin() + 1, params.end(),
														   [&](std::ostream &out, const VariablePtr &curVar) {
															   visit(NodeAddress(curVar->getType()));
														   }) << ") -> ";
										visit(NodeAddress(member->getFunctionType()->getReturnType()));
									}
								}
							}) << ";";
						}
						// TODO: wait unit pure virtual member funcs are implemented in the nodemanager

					});

					//third: print all record definitions
					visitDepthFirstOnce(node, [&](const RecordPtr &cur) {
						// only print named record types
						if (cur->getName()->getValue().compare("")) {

							auto paramPrinter = [&](std::ostream &out, const VariablePtr &cur) {
								visit(NodeAddress(cur));
								out << " : ";
								visit(NodeAddress(cur->getType()));
							};

							newLine();
							out << "def ";
							out << ((analysis::isStruct(cur)) ? "struct " : "union ");
							out << cur->getName()->getValue();

							auto parents = cur.as<StructPtr>()->getParents();
							if (!parents.empty()) {
								out << ": [ " << join(",", parents, [&](std::ostream &out, const ParentPtr &parent) {
									if (parent->isVirtual()) { out << "virtual "; }
									if (parent->isPrivate()) { out << "private "; }
									if (parent->isPublic()) { out << "public "; }
									if (parent->isProtected()) { out << "protected "; }
									visit(NodeAddress(parent->getType()));
								}) << " ]";
							}

							out << " {";
							increaseIndent();

							// print all memberFields definitions
							auto fields = cur->getFields();
							if (!fields.empty()) {
								out << join(";", fields, [&](std::ostream &out, const FieldPtr &field) {
									newLine();
									visit(NodeAddress(field->getName()));
									out << " : ";
									visit(NodeAddress(field->getType()));
								}) << ";";
							}

							// print all constructors definitions
							auto constructors = cur->getConstructors();
							if (!constructors.empty()) {
								out << join(";", constructors, [&](std::ostream &out, const ExpressionPtr &constr) {
									if (auto ctor = constr.isa<LambdaExprPtr>()) {
										newLine();
										auto params = ctor->getParameterList();
										out << "ctor (" << join(",", params.begin() + 1, params.end(), paramPrinter);
										out << ") ";
										visit(NodeAddress(ctor->getBody()));
									}
								}) << ";";
							}

							// print the destructor definitions
							auto destructor = cur->getDestructor();
							if (auto dtor = destructor.isa<LambdaExprPtr>()) {
								newLine();
								out << "dtor () ";
								visit(NodeAddress(dtor->getBody()));
							}

							// print all memberFunctions definitions
							auto memberFunctions = cur->getMemberFunctions();
							if (!memberFunctions.empty()) {
								out <<
								join("", memberFunctions, [&](std::ostream &out, const MemberFunctionPtr &memberFun) {
									if (auto impl = memberFun->getImplementation().isa<LambdaExprPtr>()) {
										newLine();
										if (memberFun->isVirtual()) out << "virtual ";

										const auto &params = impl->getParameterList();
										assert_true(params.size() >= 1);

										TypePtr thisParam = params[0]->getType();
										assert_true (analysis::isRefType(thisParam));
										if (analysis::isRefType(analysis::getReferencedType(thisParam))) {
											thisParam = analysis::getReferencedType(thisParam);
										}
										const auto thisParamRef = lang::ReferenceType(thisParam);
										if (thisParamRef.isConst()) { out << "const "; }
										if (thisParamRef.isVolatile()) { out << "volatile "; }

										out << "function " << memberFun->getName()->getValue();
										auto parameters = impl->getParameterList();
										thisStack.push(parameters.front());

										out << " : (" << join(",", parameters.begin() + 1, parameters.end(),
															  [&](std::ostream &out, const VariablePtr &curVar) {
																  visit(NodeAddress(curVar));
																  out << " : ";
																  visit(NodeAddress(curVar->getType()));
															  }) << ") -> ";
										visit(NodeAddress(impl->getFunctionType()->getReturnType()));
										out << " ";
										visit(NodeAddress(impl->getBody()));
										thisStack.pop();
									}
								});
							}
							// TODO: wait unit pure virtual member funcs are implemented in the nodemanager

							decreaseIndent();
							newLine();
							out << "};";
						}
					});


					//fourth: print all definitions for functions
					visitedBindings.clear();
					visitDepthFirstOnce(node, [&](const LambdaExprPtr& cur) {
						// jump over this part, if lambda is derived, but printer don't have the option to print them
						LambdaExprAddress curAddress{cur};

						if(lang::isDerived(cur) && !printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL)) {
								return;
						}
						if (lang::isBuiltIn(cur)) {
							return;
						}

						auto definition = curAddress->getDefinition();
						auto definitions = definition->getDefinitions();
						for(auto bindingAddress : definitions) {
							auto binding = bindingAddress.getAddressedNode();
							if (!visitedBindings.insert(binding).second) {
								continue;
							}

							const auto& lambda = binding->getLambda();
							const auto& funType = lambda->getType();

							if (!funType->isMember()) {
								if(!lambdaNames[binding].compare("main")) {
									return;
								} else {
									newLine();
									out << "def " << lambdaNames[binding];

									auto parameters = cur.getParameterList();
									out << " = function (" <<
									join(",", parameters, [&](std::ostream& out, const VariablePtr& curVar) {
										visit(NodeAddress(curVar));
										out << " : ";
										visit(NodeAddress(curVar->getType()));
									}) << ")" << (funType->isVirtualMemberFunction() ? " ~> " : " -> ");
									visit(NodeAddress(cur->getFunctionType()->getReturnType()));
									out << " ";
									visit(bindingAddress->getLambda()->getBody());
									out << ";";
								}
							}
						}
					});

					// print finally the main function, if there is one
					visitDepthFirstOnce(node, [&](const LambdaExprPtr& cur) {
						auto funType = cur->getFunctionType();
						if(!funType.isMember() && !lang::isBuiltIn(cur) && !lambdaNames[cur->getDefinition()->getBindingOf(cur->getVariable())].compare("main")) {
							newLine();
							visit(NodeAddress(cur->getFunctionType()->getReturnType()));
							auto parameters = cur->getParameterList();
							out << " main (" << join(",", parameters, [&](std::ostream& out, const VariablePtr& curVar) {
								visit(NodeAddress(curVar));
								out << " : ";
								visit(NodeAddress(analysis::getReferencedType(curVar->getType())));
							}) << ")";
							visit(NodeAddress(cur->getBody()));
						}
					});
					newLine();
				}

				if(printer.hasOption(PrettyPrinter::JUST_LOCAL_CONTEXT)) { letBindings.erase(node); }

				singleLineTypes = true;

				// skip program if let bindings have been used
				if (auto program = node.isa<ProgramPtr>()) {
					if (program->getEntryPoints().size() == 1u) {
						// main has already been printed => we are done
						return;
					}
				}

				// otherwise: print the rest
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
				printAnnotations(element, true);
				IRVisitor<void,Address>::visit(element);
				printAnnotations(element, false);
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
			#define VISIT(VNode) this->visit(VNode);


			PRINT(Value) {
				// identifiers can be directly printed
				out << *node;
			}

			PRINT(GenericType) {

				auto printer = [&](std::ostream&, const ParentAddress& cur) { VISIT(cur); };

				out << *node->getName();
				const auto& parent_types = node->getParents();

				if(!parent_types->empty()) {
					out << " : [ " << join(",", parent_types, printer) << " ]";
				}

				const auto& types = node->getTypeParameter();

				if(types->empty()) { return; }

				out << "<" << join(",", types, [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); }) << ">";
			}

			PRINT(FunctionType) {

				auto printer = [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); };

				if(node->isConstructor()) {
					VISIT(node->getObjectType());
					out << "::ctor";
					auto parameterTypes = node->getParameterTypes();
					out << "(" << join(",", parameterTypes.begin() + 1, parameterTypes.end(), printer) << ")";
				} else if(node->isDestructor()) {
					VISIT(node->getObjectType());
					out << "::dtor()";
				} else if(node->isMemberFunction() || node->isVirtualMemberFunction()) {
					VISIT(node->getObjectType());
					auto parameterTypes = node->getParameterTypes();
					out << "::(" << join(",", parameterTypes.begin() + 1, parameterTypes.end(), printer) << ")"
							<< (node->isMemberFunction() ? " -> " : " ~> ");
					VISIT(node->getReturnType());
				} else {
					out << "(" << join(",", node->getParameterTypes(), printer) << ") ";
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

				// open new TagType scope
				increaseIndent();
				out << "{";
				this->newLine();
				out << join(",", node->getDefinition()->getDefinitions(), [&](std::ostream& jout, const TagTypeBindingAddress& cur) {
					VISIT(cur->getTag());
					jout << "=";
					VISIT(cur->getRecord());
					this->newLine();
				});

				//close TagType scope
				decreaseIndent();
				this->newLine();
				out << "}";
			}

			PRINT(Record) {

				if(!node->getName()->getValue().compare("")) {

					auto paramPrinter = [&](std::ostream &out, const VariableAddress &cur) {
						VISIT(cur);
						out << " : ";
						VISIT(cur->getType());
					};

					auto strct = analysis::isStruct(node);

					out << (strct ? "struct " : "union ");

					if (strct) {
						auto parents = node.as<StructAddress>()->getParents();
						if (!parents.empty()) {
							out << ": [ " << join(",", parents, [&](std::ostream &out, const ParentAddress &parent) {
								if (parent->isVirtual()) { out << "virtual "; }
								if (parent->isPrivate()) { out << "private "; }
								if (parent->isPublic()) { out << "public "; }
								if (parent->isProtected()) { out << "protected "; }
								VISIT(parent->getType());
							}) << " ] ";
						}
					}

					// open new record scope
					out << "{";
					increaseIndent();

					// print all fields
					auto fields = node->getFields();
					if (!fields.empty()) {
						out << join(";", fields, [&](std::ostream &out, const FieldAddress &cur) {
							this->newLine();
							VISIT(cur->getName());
							out << " : ";
							VISIT(cur->getType());
						}) << ";";
					}

					// print all constructors
					auto constructors = node->getConstructors();
					if (!constructors.empty()) {
						out << join("", constructors, [&](std::ostream &out, const ExpressionAddress &cur) {
							if (auto ctor = cur.isa<LambdaExprAddress>()) {
								this->newLine();
								out << "ctor ";
								auto parameters = ctor->getParameterList();
								out << "(" << join(",", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
								VISIT(ctor->getBody());
							}
						});
					}

					// print all destructors
					auto destructor = node->getDestructor();
					if (auto dtor = destructor.isa<LambdaExprAddress>()) {
						this->newLine();
						out << "dtor ()";
						VISIT(dtor->getBody());
					}
					// print all member functions
					auto memberfuns = node->getMemberFunctions();
					if (!memberfuns.empty()) {
						out << join("", memberfuns, [&](std::ostream &out, const MemberFunctionAddress &member) {
							this->newLine();
							if (member->isVirtual()) { out << "virtual "; }
							const auto &impl = member->getImplementation().as<LambdaExprAddress>();
							const auto &params = impl->getParameterList();
							assert_true(params.size() >= 1);
							TypePtr thisParam = params[0]->getType();
							assert_true (analysis::isRefType(thisParam));
							if (analysis::isRefType(analysis::getReferencedType(thisParam))) {
								thisParam = analysis::getReferencedType(thisParam);
							}
							const auto thisParamRef = lang::ReferenceType(thisParam);
							if (thisParamRef.isConst()) { out << "const "; }
							if (thisParamRef.isVolatile()) { out << "volatile "; }
							out << "function ";
							out << member->getName()->getValue();
							out << " : (" << join(",", params.begin() + 1, params.end(), paramPrinter) << ") ";
							VISIT(impl->getBody());
						});
					}
					// print all pur virtual member functions


					// close record scope
					decreaseIndent();
					this->newLine();
					out << "}";
				} else {
					out << node->getName()->getValue();
				}
			}

			PRINT(TupleType) { out << join(",", node->getElementTypes(), [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); }); }

			PRINT(Type) {
				if(auto tagtype = node.isa<TagTypeAddress>())
					out << *tagtype.peel();
				else
					out << *node;
			}

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
				const auto& var = node->getVariable();
				out << "var ";
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
				for_each(list.begin(), list.end() - 1, [&](const NodeAddress& cur) {
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

				const auto& body = node->getBody();
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

				//print all cases
				for_each(node->getCases()->getCases(), [&](const SwitchCaseAddress& cur) {
					out << "case ";
					VISIT(cur->getGuard());
					out << ": ";
					VISIT(cur->getBody());
					this->newLine();
				});

				// print the default case, if existing
				if(!analysis::isNoOp(node->getDefaultCase())) {
					out << "default: ";
					VISIT(node->getDefaultCase());
				}

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
					VISIT(clause->getVariable());
					out << " : ";
					VISIT(clause->getVariable()->getType());
					out << ") ";
					VISIT(clause->getBody());
				}
			}

			PRINT(Variable) {

				// print this references as 'this'
				if(!thisStack.empty() && *node == *thisStack.top()) {
					out << "this";
				} else {
					out << *node;
				}

			}

			PRINT(Literal) {
				// special handling of type literals (ignore value)
				if(!printer.hasOption(PrettyPrinter::PRINT_LITERAL_TYPES) && analysis::isTypeLiteral(node.getAddressedNode())) {
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
					auto& basic = node->getNodeManager().getLangBasic();
					auto type = node->getType();

					if (basic.isFloat(type))  out << "f";

					if (basic.isUInt1(type))    out << "u";
					if (basic.isUInt2(type))    out << "u";
					if (basic.isUInt4(type))    out << "u";
					if (basic.isUInt8(type))    out << "ul";
					if (basic.isUInt16(type))   out << "ull";
					if (basic.isUIntGen(type))  out << "u";
					if (basic.isUIntInf(type))  out << "u";

					if (basic.isInt8(type))     out << "l";
					if (basic.isInt16(type))    out << "ll";

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
				out << lambdaNames[node->getDefinition()->getBindingOf(node->getVariable())];
			}


			PRINT(LambdaDefinition) {
				auto defs = node->getDefinitions();
				if(defs.empty()) { return; }

				out << "{";
				increaseIndent();
				newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const LambdaBindingAddress& cur) {
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
				auto paramPrinter = [&](std::ostream& out, const VariableAddress& cur) {
					VISIT(cur);
					out << " : ";
					VISIT(cur->getType());
				};

				auto funType = node->getType();
				// print header ...
				if(funType->isConstructor()) {
					// print constructor header
					out << "ctor ";
					VISIT(funType->getObjectType());
					out << " ";
					VISIT(node->getParameters()->getElement(0));
					auto parameters = node->getParameters();
					out << " :: (" << join(",", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isDestructor()) {
					// print destructor header
					VISIT(funType->getObjectType());
					auto parameters = node->getParameters();
					out << " :: (" << join(",", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isMemberFunction() || funType->isVirtualMemberFunction()) {
					// print member function header
					out << "function ";
					VISIT(funType->getObjectType());
					auto parameters = node->getParameters();
					out << "::(" << join(",", parameters.begin() + 1, parameters.end(), paramPrinter) << ")"
							<< (funType->isMemberFunction() ? " -> " : " ~> ");
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
					out << "(" << join(",", node->getParameterList(), paramPrinter) << ") -> ";
					VISIT(funType->getReturnType());
					out << " ";
					// .. and body
					VISIT(node->getBody());
				}
			}


			PRINT(CallExpr) {
				// test whether for the current call a special format has been registered
				auto function = node->getFunctionExpr();
				auto pos = formatTable.find(function);
				if(pos != formatTable.end()) {
					auto formatter = (*pos).second;
					auto needB = analysis::needsParentheses(node);
					if (needB) out << "(";
					formatter(*this, node);
					if (needB) out << ")";
					return;
				}

				// print name/variable/whatever
				if(printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(function)) {
					out << lang::getConstructName(function);
				} else if (printer.hasOption(PrettyPrinter::PRINT_ATTRIBUTES) && !analysis::getAttributes(node.getAddressedNode()).empty()) {
					out << "attr";
				} else {
					auto functionPtr = function.getAddressedNode();
					if (auto lambdaExpr = functionPtr.isa<LambdaExprPtr>()) {
						out << lambdaNames[lambdaExpr->getDefinition()->getBindingOf(lambdaExpr->getVariable())];
					} else if (auto var = functionPtr.isa<VariablePtr>()) {
						NodeAddress parent = node.getFirstParentOfType(NT_LambdaDefinition);
						if (parent) {
							auto bindingPtr = parent.getAddressedNode().as<LambdaDefinitionPtr>()->getBindingOf(var);
							if (bindingPtr) {
								out << lambdaNames[bindingPtr];
							} else {
								VISIT(function);
							}
						} else {
							VISIT(function);
						}
					} else if (auto call = functionPtr.isa<CallExprPtr>()) {
						const auto& refExt = call.getNodeManager().getLangExtension<lang::ReferenceExtension>();
						if (analysis::isCallOf(call, refExt.getRefDeref())) {
							out << "(";
							VISIT(function);
							out << ")";
						} else {
							VISIT(function);
						}
					} else if (auto call = functionPtr.isa<CastExprPtr>()) {
						out << "(";
						VISIT(function);
						out << ")";
					} else {
						VISIT(function);
					}
				}

				// print arguments
				auto args = node->getArguments();
				if(args.empty()) {
					out << "()";
				} else {
					out << "(" << join(",", args, [&](std::ostream& out, const NodePtr& curParam) {
						VISIT(NodeAddress(curParam));
					}) << ")";
				}
			}

			PRINT(BindExpr) {
				out << "(" << join(",", node->getParameters(),[&](std::ostream& out, const ExpressionAddress& cur) {
					VISIT(cur);
					out << " : ";
					VISIT(cur->getType());
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
				out << "(" << ::join(",", node->getExpressions(), [&](std::ostream&, const ExpressionAddress& cur) { VISIT(cur); }) << ")";
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
				out << "struct{" << ::join(",", node->getMembers()->getElements(), [&](std::ostream& out, const NamedValueAddress& cur) {
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
				for_each(defs.begin(), defs.end(), [&](const TagTypeBindingAddress& cur) {
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
				out << "// Inspire Program ";
				newLine();
				for(const auto& cur : node->getEntryPoints()) {
					this->out << "//  Entry Point: ";
					this->newLine();
					this->increaseIndent();
					this->visit(cur);
					this->decreaseIndent();
					this->newLine();
					this->newLine();
				}
			}

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
			PRINT(Node) { out << "<node type=" << node->getNodeType() << ">" << *node << "</node>"; }

			/**
			 * Creates a new line.
			 */
			void newLine() const {
				// check single-line flag
				if(isFirstLine) {
					isFirstLine = false;
					return;
				}
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
								out << ",";
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
				InspirePrinter::visit(node);
				out.flush();
				SourceLocation end = wout.getSrcLoc();

				srcMap.insert(std::make_pair(SourceRange(start, end), node));
			}
		};


		/**
		 * A utility function printing the n-th argument of a call expression.
		 *
		 * @param printer the printer to be used for the actual printing
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the argument to be printed; in case there is no such argument a ? is printed.
		 */
		void printArgument(InspirePrinter& printer, const CallExprAddress& call, unsigned n) {
			ExpressionAddress argument = call[n];
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
			#define ARG(N) call[N]
			#define MGR call->getNodeManager()
			#define PRINT_EXPR(E) printer.visit(E)
			#define PRINT_ARG(N) printArgument(printer, call, N)
			#define HAS_OPTION(OPT) printer.getPrettyPrint().hasOption(PrettyPrinter::OPT)
			#define ADD_FORMATTER(Literal)                                                                                                             \
				res[Literal] = [](InspirePrinter & printer, const CallExprAddress& call)

//			if(config.hasOption(PrettyPrinter::PRINT_DEREFS)) {
				ADD_FORMATTER(refExt.getRefDeref()) {
					OUT("*");
					PRINT_ARG(0);
				};
//			} else {
//				ADD_FORMATTER(refExt.getRefDeref()) { PRINT_ARG(0); };
//			}

			ADD_FORMATTER(refExt.getRefAssign()) {
				PRINT_ARG(0);
				OUT(" = ");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(refExt.getRefVar()) {
				OUT("ref_var(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefNew()) {
				OUT("ref_new(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefVarInit()) {
				OUT("ref_var_init(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefNewInit()) {
				OUT("ref_new_init(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefDelete()) {
				OUT("delete(");
				PRINT_ARG(0);
				OUT(")");
			};

			ADD_FORMATTER(dpExt.getDataPathRoot()) { OUT("<>"); };
			ADD_FORMATTER(dpExt.getDataPathMember()) {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(dpExt.getDataPathElement()) {
				PRINT_ARG(0);
				OUT("[");
				PRINT_ARG(1);
				OUT("]");
			};
			ADD_FORMATTER(dpExt.getDataPathComponent()) {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(dpExt.getDataPathParent()) {
				PRINT_ARG(0);
				OUT(".as<");
				PRINT_ARG(1);
				OUT(">");
			};

			ADD_FORMATTER(refExt.getRefArrayElement()) {
				PRINT_ARG(0);
				OUT("[");
				PRINT_ARG(1);
				OUT("]");
			};

			ADD_FORMATTER(refExt.getRefMemberAccess()) {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCompositeMemberAccess()) {
				PRINT_ARG(0);
				OUT(".");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getRealAdd()) {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealSub()) {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealMul()) {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealDiv()) {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getUnsignedIntAdd()) {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntSub()) {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntMul()) {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntDiv()) {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntMod()) {
				PRINT_ARG(0);
				OUT("%");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getSignedIntAdd()) {
				PRINT_ARG(0);
				OUT("+");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntSub()) {
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntMul()) {
				PRINT_ARG(0);
				OUT("*");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntDiv()) {
				PRINT_ARG(0);
				OUT("/");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntMod()) {
				PRINT_ARG(0);
				OUT("%");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getUnsignedIntNot()) {
				OUT("~");
				PRINT_ARG(0);
			};
			ADD_FORMATTER(basic.getUnsignedIntAnd()) {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntOr()) {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntXor()) {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntLShift()) {
				PRINT_ARG(0);
				OUT("<<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntRShift()) {
				PRINT_ARG(0);
				OUT(">>");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getSignedIntNot()) {
				OUT("~");
				PRINT_ARG(0);
			};
			ADD_FORMATTER(basic.getSignedIntAnd()) {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntOr()) {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntXor()) {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntLShift()) {
				PRINT_ARG(0);
				OUT("<<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntRShift()) {
				PRINT_ARG(0);
				OUT(">>");
				PRINT_ARG(1);
			};

			// nicer inlined versions of the && and || operators
			//		ADD_FORMATTER(basic.getBoolLAnd()) { PRINT_ARG(0); OUT(" && "); PRINT_ARG(1); };
			ADD_FORMATTER(basic.getBoolLAnd()) {
				PRINT_ARG(0);
				OUT(" && ");
				if (HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else
					PRINT_EXPR(ExpressionAddress(transform::evalLazy(MGR, ARG(1))));
			};
			//		ADD_FORMATTER(basic.getBoolLOr()) { PRINT_ARG(0); OUT(" || "); PRINT_ARG(1); };
			ADD_FORMATTER(basic.getBoolLOr()) {
				PRINT_ARG(0);
				OUT(" || ");
				if (HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else
					PRINT_EXPR(ExpressionAddress(transform::evalLazy(MGR, ARG(1))));
			};

			ADD_FORMATTER(basic.getBoolOr()) {
				PRINT_ARG(0);
				OUT("|");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getBoolAnd()) {
				PRINT_ARG(0);
				OUT("&");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getBoolEq()) {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getBoolNe()) {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getBoolXor()) {
				PRINT_ARG(0);
				OUT("^");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getBoolLNot()) {
				OUT("!");
				PRINT_ARG(0);
			};

			ADD_FORMATTER(basic.getBoolNot()) {
				OUT("!");
				PRINT_ARG(0);
			};

			ADD_FORMATTER(basic.getCharNe()) {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCharEq()) {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCharGe()) {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCharGt()) {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCharLt()) {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getCharLe()) {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getUnsignedIntEq()) {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntNe()) {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntGe()) {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntGt()) {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntLt()) {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getUnsignedIntLe()) {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getSignedIntEq()) {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntNe()) {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntGe()) {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntGt()) {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntLt()) {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getSignedIntLe()) {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(basic.getRealEq()) {
				PRINT_ARG(0);
				OUT("==");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealNe()) {
				PRINT_ARG(0);
				OUT("!=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealGe()) {
				PRINT_ARG(0);
				OUT(">=");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealGt()) {
				PRINT_ARG(0);
				OUT(">");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealLt()) {
				PRINT_ARG(0);
				OUT("<");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(basic.getRealLe()) {
				PRINT_ARG(0);
				OUT("<=");
				PRINT_ARG(1);
			};

			ADD_FORMATTER(parExt.getCreateMinRange()) {
				OUT("[");
				PRINT_ARG(0);
				OUT("-inf]");
			};
			ADD_FORMATTER(parExt.getCreateBoundRange()) {
				OUT("[");
				PRINT_ARG(0);
				OUT("-");
				PRINT_ARG(1);
				OUT("]");
			};

			ADD_FORMATTER(basic.getIfThenElse()) {
				OUT("");
				PRINT_ARG(0);
				OUT("?");
				if(HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(1);
				else {
					PRINT_EXPR(ExpressionAddress(transform::evalLazy(MGR, ARG(1))));
				}
				OUT(":");
				if(HAS_OPTION(NO_EVAL_LAZY))
					PRINT_ARG(2);
				else {
					PRINT_EXPR(ExpressionAddress(transform::evalLazy(MGR, ARG(2))));
				}
			};

			ADD_FORMATTER(parExt.getBarrier()) { OUT("barrier()"); };

			ADD_FORMATTER(parExt.getAtomic()) {
				OUT("atomic(");
				PRINT_ARG(0);
				OUT(",");
				PRINT_ARG(1);
				OUT(",");
				PRINT_ARG(2);
				OUT(")");
			};

			if(!config.hasOption(PrettyPrinter::NO_LIST_SUGAR)) {
				// add semantic sugar for list handling
				const lang::ListExtension& ext = config.root->getNodeManager().getLangExtension<lang::ListExtension>();

				typedef encoder::ListConverter<ExpressionPtr, encoder::DirectExprConverter> AttributConverter;
				typedef AttributConverter::is_encoding_of is_encoding_of_type;

				ADD_FORMATTER(ext.getListEmpty()) { OUT("[]"); };
				ADD_FORMATTER(ext.getListCons()) {
					const is_encoding_of_type is_encoding_of{};
					// check whether syntactic sugar is supported
					if(is_encoding_of(call)) {
						vector<ExpressionPtr> list = (encoder::toValue<vector<ExpressionPtr>, AttributConverter>(call));
						printer.out << "[" << join(",", list, [&](std::ostream& out, const ExpressionPtr& cur) { printer.visit(NodeAddress(cur)); }) << "]";
					} else {
						// use fall-back solution
						printer.out << "[";
						printer.visit(call[0]);
						printer.out << ",";
						printer.visit(call[1]);
						printer.out << "]";
					}
				};
			}

			if(!config.hasOption(PrettyPrinter::PRINT_ATTRIBUTES)) {
				const analysis::AttributeExtension& ext = mgr.getLangExtension<analysis::AttributeExtension>();
				ADD_FORMATTER(ext.getAttr()) { PRINT_ARG(0); };
			}

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

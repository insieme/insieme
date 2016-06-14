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

#include "insieme/core/printer/pretty_printer.h"

#include <cassert>
#include <memory>
#include <iomanip>
#include <stack>

#include <ctime>

#include <boost/unordered_map.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/concepts.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_statistic.h"

#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/parentheses.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/printer/lexer.h"
#include "insieme/core/types/match.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"

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

		// applies the given alias map to the given type
		TypePtr resolveTypeAliases(const TypePtr& type, const std::vector<std::pair<TypePtr,TypePtr>>& aliases) {
			// run through alias lists
			for(const auto& cur : aliases) {
				// check whether pattern matches
				if(auto sub = types::match(type->getNodeManager(), type, cur.first)) {
					// compute substitution
					auto next = (*sub).applyTo(cur.second);
					// apply pattern and start over again
					return resolveTypeAliases(next, aliases);
				}
			}
			// no matching alias found => done
			return type;
		}

		const std::string getObjectName(const core::TypePtr& ty) {
			auto objTy = analysis::getObjectType(ty);
			return analysis::getTypeName(objTy);
			throw;
		}

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
			mutable bool isFirstLine=true;

			/**
			 * A Map for lambdaexpr names
			 */
			utils::map::PointerMap<LambdaReferencePtr, std::string> lambdaNames;
			utils::set::PointerSet<LambdaReferencePtr> entryPoints;
			utils::set::PointerSet<TagTypePtr> visitedTagTypes;
			utils::set::PointerSet<LiteralPtr> visitedLiterals;
			utils::map::PointerMap<LambdaReferencePtr, std::tuple<std::string, std::string>> visitedFreeFunctions;
			utils::set::PointerSet<LambdaReferencePtr> visitedMemberFunctions;
			utils::set::PointerSet<LambdaReferencePtr> visitedConstructors;

			std::vector<std::pair<TypePtr,TypePtr>> aliases;

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

			const VariablePtr getTopOfThisStack() const {
				if (thisStack.empty()) {
					return VariablePtr();
				}
				return thisStack.top();
			}

			/**
			 * The main entry point computing common sub-expressions before printing the actual code.
			 */
			void print(const NodePtr& node) {
				// reset setup
				letBindings.clear();

				// get all entry-points
				if (const auto& program = node.isa<ProgramPtr>()) {
					for (auto& entry : program->getEntryPoints()) {
						const auto& lambdaExpr = entry.as<LambdaExprPtr>();
						entryPoints.insert(lambdaExpr->getReference());
					}
				}

				// perform reverse alias mapping
				// only pointers for now, TODO: extend
				auto& nm = node->getNodeManager();
				auto revAliases = nm.getLangExtension<lang::PointerExtension>().getDefinedTypeAliases();
				for(auto& entry: revAliases) {
					aliases.push_back({entry.second, entry.first});
				}
				// this sorting step is important to produce consistent results
				std::sort(aliases.begin(), aliases.end(), [](const std::pair<TypePtr, TypePtr>& a, const std::pair<TypePtr, TypePtr>& b) {
					return IRStatistic::evaluate(a.second).getNumSharedNodes() < IRStatistic::evaluate(b.second).getNumSharedNodes();
				});

				int funCounter = 0;
				auto extractName = [&](const NodePtr& cur) {
					std::string result = "_";
					if (annotations::hasAttachedName(cur)) {
						result = annotations::getAttachedName(cur);
					} else if (lang::isDerived(cur)) {
						result = lang::getConstructName(cur);
					} else if (auto binding = cur.isa<LambdaBindingPtr>()) {
						result = binding->getReference()->getNameAsString();
					} else if (auto expr = cur.isa<LambdaExprPtr>()) {
						result = expr->getReference()->getNameAsString();
					}

					if (result == "_") {
						result = format("fun%03d", funCounter++);
					}

					return result;
				};

				// get all lambda names out of member functions
				visitDepthFirstOnce(node, [&](const TagTypePtr& cur) {
					const auto& definition = cur->getDefinition();

					// iterate over all bindings
					for(auto& binding : definition) {
						// iterate over all member function of each binding
						for(auto& memberFun : binding->getRecord()->getMemberFunctions()) {
							if (const auto& lambdaExpr = memberFun->getImplementation().isa<LambdaExprPtr>()) {
								const auto& lambdaBinding = lambdaExpr->getDefinition()->getBindingOf(lambdaExpr->getReference());
								lambdaNames[cur->peel(lambdaBinding)->getReference()] = memberFun->getName()->getValue();
								lambdaNames[lambdaBinding->getReference()] = memberFun->getName()->getValue();

								// later used to find out which member functions are not visited -> free defined memFuns
								visitedMemberFunctions.insert(lambdaBinding->getReference());
								visitedMemberFunctions.insert(cur->peel(lambdaBinding)->getReference());
							}
						}

						// later used to find out which member functions are not visited -> free defined ctors
						auto constructors = binding->getRecord()->getConstructors();
						for (auto constr : constructors) {
							if (auto ctor = constr.isa<LambdaExprPtr>()) {
								visitedMemberFunctions.insert(ctor->getReference());
								visitedMemberFunctions.insert(cur->peel(ctor)->getReference());
							}
						}
					}
				});

				// get all lambda/function names
				// visit all lambdas to get names of all non-recursive functions
				visitDepthFirstOnce(node, [&](const LambdaExprPtr& cur) {
					const auto& defaultRef = cur->getReference();
					if (lambdaNames.find(defaultRef) != lambdaNames.end()) {
						return;
					}
					lambdaNames[defaultRef] = extractName(cur);
					for(const auto& binding : cur->getDefinition()->getDefinitions()) {
						const auto& bindRef = binding->getReference();
						if (bindRef == defaultRef || lambdaNames.find(bindRef) != lambdaNames.end()) {
							continue;
						}
						lambdaNames[bindRef] = extractName(binding);
					}
				}, true);

				// check whether bindings should be used
				if(printer.hasOption(PrettyPrinter::NO_LET_BINDINGS)) {
					// skip computation of bindings
					visit(NodeAddress(node));
					return;
				}

				// collect all tagtypes
				IRBuilder builder(nm);
				visitDepthFirstOnce(node, [&](const TagTypePtr& cur) {
					auto definition = cur->getDefinition();
					for(auto& binding : definition->getDefinitions()) {
						auto tag = binding->getTag();
						visitedTagTypes.insert(builder.tagType(
								tag,
								builder.tagTypeDefinition({{tag, definition->getDefinitionOf(tag)}})));
					}
				});

				if(!printer.hasOption(PrettyPrinter::JUST_LOCAL_CONTEXT)) {
					//first: print all type declarations
					for(auto& tag : visitedTagTypes) {
						if(tag->getName()->getValue().compare("")) {
							newLine();
							out << "decl ";
							out << ((tag->isStruct()) ? "struct " : "union ");
							out << tag->getName()->getValue();
							out << ";";
						}
					}

					// print all declarations for NOT-defined (but declared) functions
					if (!printer.hasOption(PrettyPrinter::FULL_LITERAL_SYNTAX)) {
						visitDepthFirstOnce(node, [&](const CallExprPtr& cur) {
							auto literalExpr = cur->getFunctionExpr().isa<LiteralPtr>();
							if (literalExpr && !lang::isBuiltIn(literalExpr)) {
								if(!visitedLiterals.insert(literalExpr).second) {
									return;
								}
								newLine();
								out << "decl ";
								visit(NodeAddress(literalExpr));
								out << " : ";
								visit(NodeAddress(literalExpr->getType()));
								out << ";";
							}
						});
					}

					//second: print all function declarations
					utils::set::PointerSet<LambdaBindingPtr> visitedBindings;
					visitDepthFirstOncePrunable(node, [&](const LambdaExprPtr& cur) {
						// jump over this part, if lambda is derived, but printer don't have the option to print them
						if(lang::isDerived(cur) && !printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL)) {
							return true;
						}
						if(lang::isBuiltIn(cur)) {
							return true;
						}

						for(auto& binding : cur->getDefinition()->getDefinitions()) {
							if(!visitedBindings.insert(binding).second) {
								continue;
							}
							const auto& lambda = binding->getLambda();
							const auto& funType = lambda->getType();
							if (!funType->isMember()) {
								newLine();
								out << "decl " << lambdaNames[binding->getReference()] << " : ";
								visit(NodeAddress(funType));
								out << ";";
							} else if (visitedMemberFunctions.insert(binding->getReference()).second) {
								// branch for free defined function
								if (binding->getReference()->getType().isMemberFunction()) { // free member functions
									auto tagname = getObjectName(cur->getType());
									vector<std::string> splitstring;
									boost::split(splitstring, lambdaNames[binding->getReference()],
												 boost::is_any_of("::"));
									visitedFreeFunctions[binding->getReference()] = std::make_tuple(tagname,
																									splitstring[2]);

									lambdaNames[binding->getReference()] = splitstring[2];
									newLine();
									out << "decl " << splitstring[2] << ":";
									visit(NodeAddress(funType));
									out << ";";
								} else if (binding->getReference()->getType().isConstructor()) { // free constructors
									auto tagname = getObjectName(cur->getType());
									visitedFreeFunctions[binding->getReference()] = std::make_tuple(tagname, lambdaNames[binding->getReference()]);
								}
							}
						}
						return false;

					});

					//third: print all declarations for memberFields, constructors, destructors, memberFunctions...
					for(auto& tag : visitedTagTypes) {
						auto record = tag.getRecord();
						auto& tagName = tag->getName()->getValue();

						if (tagName.compare("")) {
							// print all memberFields declarations
							for (auto field : record->getFields()) {
								newLine();
								out << "decl " << tagName << "::";
								visit(NodeAddress(field->getName()));
								out << ":";
								visit(NodeAddress(field->getType()));
								out << ";";
							}

							// print all constructors declarations
							auto constructors = record->getConstructors();
							for (auto constr : constructors) {
								if (!printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS) &&
									analysis::isaDefaultConstructor(constr)) {
									continue;
								}

								if (auto ctorIn = constr.isa<LambdaExprPtr>()) {
									auto ctor = tag->peel(ctorIn);
									newLine();
									out << "decl ctor:";
									visit(NodeAddress(ctor->getType()));
									out << ";";
								}
							}

							// print destructor declaration
							if(record->hasDestructor() && (printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS) || !analysis::hasDefaultDestructor(tag))) {
								auto destructor = record->getDestructor();
								if(auto dtorIn = destructor.isa<LambdaExprPtr>()) {
									auto dtor = tag->peel(dtorIn);
									newLine();
									out << "decl dtor:";
									visit(NodeAddress(dtor->getType()));
									out << ";";
								}
							}

							// print all memberFunctions declarations
							auto memberFunctions = record->getMemberFunctions();
							for (auto memberFunIn : memberFunctions) {
								auto memberFun = tag->peel(memberFunIn);
								if (!printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS) &&
									analysis::isaDefaultMember(memberFun))
									continue;
								if (auto member = memberFun->getImplementation().isa<LambdaExprPtr>()) {
									newLine();
									out << "decl " << memberFun->getName()->getValue() << ":";
									visit(NodeAddress(member->getType()));
									out << ";";
								}
							}

							// TODO: wait unit pure virtual member funcs are implemented in the nodemanager
						}
					}

					//fourth: print all record definitions
					for(auto& tag : visitedTagTypes) {
						if (auto record = tag.getRecord().isa<RecordPtr>()) {
							//auto& recordName = record->getName()->getValue();
							if (record->getName()->getValue().compare("")) {
								newLine();
								out << "def ";
								visit(NodeAddress(record));
								out << ";";
							}
						}
					}

					//fifth: print all definitions for functions
					visitedBindings.clear();
					visitDepthFirstOncePrunable(node, [&](const LambdaExprPtr& cur) {
						// jump over this part, if lambda is derived, but printer don't have the option to print them
						LambdaExprAddress curAddress{cur};

						if(lang::isDerived(cur) && !printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL)) {
							return true;
						}
						if (lang::isBuiltIn(cur)) {
							return true;
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
								if(entryPoints.find(binding->getReference()) != entryPoints.end()) {
									return false;
								} else {
									newLine();
									out << "def " << lambdaNames[binding->getReference()];

									auto parameters = lambda.getParameterList();
									out << " = function (" <<
									join(", ", parameters, [&](std::ostream& out, const VariablePtr& curVar) {
										visit(NodeAddress(curVar));
										out << " : ";
										visit(NodeAddress(curVar->getType()));
									}) << ")" << (funType->isVirtualMemberFunction() ? " ~> " : " -> ");
									visit(NodeAddress(funType->getReturnType()));
									out << " ";
									visit(bindingAddress->getLambda()->getBody());
									out << ";";
								}
							} else if (visitedFreeFunctions.find(binding->getReference()) != visitedFreeFunctions.end()) {
								// branch for the free member functions
								if (funType->isConstructor()) {
									auto tagname = std::get<0>(visitedFreeFunctions[binding->getReference()]);
									auto funname = std::get<1>(visitedFreeFunctions[binding->getReference()]);
									auto parameters = lambda.getParameterList();

									newLine();
									out << "def " << tagname << " :: ctor " << funname << " = function (" <<
											join(", ", lambda->getParameters().begin() + 1, lambda->getParameters().end(),
												 [&](std::ostream& out, const VariablePtr& curVar) {
													 visit(NodeAddress(curVar));
													 out << " : ";
													 visit(NodeAddress(curVar->getType()));
												 }) << ") ";
									thisStack.push(lambda->getParameters().front());
									visit(bindingAddress->getLambda()->getBody());
									thisStack.pop();
									out << ";";
								} else if (funType->isMemberFunction()) {
									auto tagname = std::get<0>(visitedFreeFunctions[binding->getReference()]);
									auto funname = std::get<1>(visitedFreeFunctions[binding->getReference()]);
									auto parameters = lambda.getParameterList();

									newLine();
									out << "def " << tagname << " :: function " << funname << " = (" <<
									join(", ", lambda->getParameters().begin() + 1, lambda->getParameters().end(),
										 [&](std::ostream& out, const VariablePtr& curVar) {
											 visit(NodeAddress(curVar));
											 out << " : ";
											 visit(NodeAddress(curVar->getType()));
										 }) << ")" << (funType->isVirtualMemberFunction() ? " ~> " : " -> ");
									visit(NodeAddress(funType->getReturnType()));
									out << " ";
									visit(bindingAddress->getLambda()->getBody());
									out << ";";
								}
							}
						}
						return false;
					});
					newLine();
				}

				if(printer.hasOption(PrettyPrinter::JUST_LOCAL_CONTEXT)) { letBindings.erase(node); }

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
			virtual void visit(const NodeAddress& inElement) {
				NodeAddress element = inElement;

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

				// for types, apply reverse aliases
				auto typePtr = element.getAddressedNode().isa<TypePtr>();
				if(typePtr) {
					element = NodeAddress(resolveTypeAliases(typePtr, aliases));
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
					out << " : [ " << join(", ", parent_types, printer) << " ]";
				}

				const auto& types = node->getTypeParameter();

				if(types->empty()) { return; }

				out << "<" << join(",", types, [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); }) << ">";
			}

			PRINT(FunctionType) {

				auto printer = [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); };

				if(node->hasInstantiationTypes()) {
					out << "<" << join(", ", node->getInstantiationTypeList(), printer) << ">";
				}

				if(node->isMember()) {
					auto thisParam = node->getParameterType(0);
					assert_true(lang::isReference(thisParam)) << "This param has to be a reference type";
					lang::ReferenceType refTy(thisParam);
					if (refTy.isConst()) out << "const ";
					if (refTy.isVolatile()) out << "volatile ";
					out << (node->isDestructor() ? "~" : "") << getObjectName(node) << "::";
				}
				if(node->isConstructor()) {
					auto params = node->getParameterTypes();
					out << "(" << join(", ", params.begin() + 1, params.end(), printer) << ")";
				} else if(node->isDestructor()) {
					out << "()";
				} else if(node->isMemberFunction() || node->isVirtualMemberFunction()) {
					auto parameterTypes = node->getParameterTypes();
					out << "(" << join(", ", parameterTypes.begin() + 1, parameterTypes.end(), printer) << ")"
							<< (node->isMemberFunction() ? " -> " : " ~> ");
					VISIT(node->getReturnType());
				} else {
					out << "(" << join(", ", node->getParameterTypes(), printer) << ")";
					out << ((node->isPlain()) ? " -> " : " => ");
					VISIT(node->getReturnType());
				}
			}

			PRINT(TagType) {
				if(node->getName()->getValue().compare("")) {
					VISIT(node->getTag());
				} else {
					VISIT(node.getRecord());
				}
			}

			PRINT(Record) {
					const auto& tagType = node.getFirstParentOfType(NT_TagType).getAddressedNode().as<TagTypePtr>();

					auto paramPrinter = [&](std::ostream &out, const VariableAddress &cur) {
						VISIT(cur);
						out << " : ";
						VISIT(cur->getType());
					};


					auto strct = analysis::isStruct(node);

					out << (strct ? "struct " : "union ");

					auto recordName = node->getName()->getValue();

					if (recordName.compare("")) {
						out << recordName << " ";
					}

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
					for(auto constr : constructors) {
						if (!printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS)
							&& analysis::isaDefaultConstructor(constr.getAddressedNode())) continue;
						if (auto ctor = constr.isa<LambdaExprAddress>()) {
							this->newLine();
							out << "ctor function ";
							auto parameters = ctor->getParameterList();
							thisStack.push(ctor->getParameterList().front());
							out << "(" << join(", ", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
							if(analysis::isaDefaultConstructor(constr.getAddressedNode())) out << "= default;";
							else VISIT(ctor->getBody());
							thisStack.pop();
						}
					}

					// print the destructor definitions
					if(node->hasDestructor()) {
						if(auto dtor = node->getDestructor().isa<LambdaExprPtr>()) {
							if(analysis::isDefaultDestructor(dtor)) {
								if(printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS)) {
									newLine();
									out << "dtor function () = default;";
								}
							} else {
								newLine();
								out << "dtor ";
								if(node->getDestructorVirtual() && node->getDestructorVirtual()->getValue()) { out << "virtual "; }
								out << "function () ";
								thisStack.push(dtor->getParameterList().front());
								visit(NodeAddress(dtor->getBody()));
								thisStack.pop();
							}
						}
					} else {
						newLine();
						out << "dtor function () = delete;";
					}

					// print all member functions
					auto memberfuns = node->getMemberFunctions();
					for (auto memberFun : memberfuns) {
						if (!printer.hasOption(PrettyPrinter::PRINT_DEFAULT_MEMBERS) && analysis::isaDefaultMember(memberFun.getAddressedNode())) continue;
						if (auto impl = memberFun->getImplementation().isa<LambdaExprAddress>()) {
							newLine();
							if (memberFun->isVirtual()) out << "virtual ";

							const auto &params = impl->getParameterList();
							assert_true(params.size() >= 1);

							TypePtr thisParam = params[0].getAddressedNode()->getType();
							assert_true (analysis::isRefType(thisParam));
							if (analysis::isRefType(analysis::getReferencedType(thisParam))) {
								thisParam = analysis::getReferencedType(thisParam);
							}
							const auto thisParamRef = lang::ReferenceType(thisParam);
							if (thisParamRef.isConst()) { out << "const "; }
							if (thisParamRef.isVolatile()) { out << "volatile "; }

							out << "function " << memberFun->getName()->getValue();
							auto parameters = impl->getParameterList();
							thisStack.push(parameters.front().getAddressedNode());

							out << " = (" << join(", ", parameters.begin() + 1, parameters.end(),
												  [&](std::ostream &out, const VariableAddress &curVar) {
													  VISIT(curVar);
													  out << " : ";
													  VISIT(curVar->getType());
												  }) << ") -> ";
							VISIT(impl->getFunctionType()->getReturnType());
							out << " ";
							if(analysis::isaDefaultMember(memberFun.getAddressedNode())) out << "= default;";
							else VISIT(impl->getBody());
							thisStack.pop();
						}
					}
					// TODO: print all pur virtual member functions


					// close record scope
					decreaseIndent();
					newLine();
					out << "}";
			}

			PRINT(TupleType) { out << "(" << join(", ", node->getElementTypes(), [&](std::ostream&, const TypeAddress& cur) { VISIT(cur); }) << ")"; }

			PRINT(Type) {
				if (auto tagtype = node.isa<TagTypeAddress>()) {
					out << *tagtype.peel();
				} else {
					out << *node;
				}
			}

			PRINT(BreakStmt) { out << "break"; }

			PRINT(ContinueStmt) { out << "continue"; }

			PRINT(ReturnStmt) {
				out << "return ";
				auto exp = node->getReturnExpr();
				VISIT(exp);
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
					if(printer.hasOption(PrettyPrinter::USE_VARIABLE_NAME_ANNOTATIONS) && annotations::hasAttachedName(node)) {
						out << annotations::getAttachedName(node);
					} else {
						out << *node;
					}
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
				// decide whether to really do the full print
				bool doFullSyntax = printer.hasOption(PrettyPrinter::FULL_LITERAL_SYNTAX);
				if(doFullSyntax) {
					if(core::lang::isBuiltIn(node) || core::lang::isBuiltIn(node->getType())) doFullSyntax = false;
				}
				if(doFullSyntax) {
					out << "lit(\"";
				}
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

				if(doFullSyntax) {
					out << "\" : ";
					VISIT(node->getType());
					out << ")";
				}
				else if(printer.hasOption(PrettyPrinter::PRINT_LITERAL_TYPES)) {
					out << ":";
					VISIT(node->getType());
				}
			}

			PRINT(LambdaExpr) {

				if(!printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(node)) {
					out << lang::getConstructName(node);
					return;
				}
				out << lambdaNames[node->getReference()];
			}

			PRINT(LambdaReference) {
				out << node->getNameAsString();
			}

			PRINT(LambdaDefinition) {
				auto defs = node->getDefinitions();
				if(defs.empty()) { return; }

				out << "{";
				increaseIndent();
				newLine();
				std::size_t count = 0;
				for_each(defs.begin(), defs.end(), [&](const LambdaBindingAddress& cur) {
					VISIT(cur->getReference());
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
					VISIT(analysis::getObjectType(funType));
					out << " ";
					VISIT(node->getParameters()->getElement(0));
					auto parameters = node->getParameters();
					out << " :: (" << join(", ", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isDestructor()) {
					// print destructor header
					VISIT(analysis::getObjectType(funType));
					auto parameters = node->getParameters();
					out << " :: (" << join(", ", parameters.begin() + 1, parameters.end(), paramPrinter) << ") ";
					if (!node->getParameterList().empty())
						thisStack.push(node->getParameterList().front());
					// .. and body
					VISIT(node->getBody());
					if (!node->getParameterList().empty())
						thisStack.pop();

				} else if(funType->isMemberFunction() || funType->isVirtualMemberFunction()) {
					// print member function header
					out << "function ";
					VISIT(analysis::getObjectType(funType));
					auto parameters = node->getParameters();
					out << "::(" << join(", ", parameters.begin() + 1, parameters.end(), paramPrinter) << ")"
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
					out << "(" << join(", ", node->getParameterList(), paramPrinter) << ") -> ";
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

				bool isMemberFun = false;

				// print name/variable/whatever
				if(printer.hasOption(PrettyPrinter::PRINT_DERIVED_IMPL) && lang::isDerived(function)) {
					out << lang::getConstructName(function);
				} else if (printer.hasOption(PrettyPrinter::PRINT_ATTRIBUTES) && !analysis::getAttributes(node.getAddressedNode()).empty()) {
					out << "attr";
				} else {
					auto functionPtr = function.getAddressedNode();
					if (auto lambdaExpr = functionPtr.isa<LambdaExprPtr>()) {
						if (lambdaExpr->getType().as<FunctionTypePtr>().isMemberFunction()) {
							isMemberFun = true;
							auto arguments = node->getArgumentList();
							VISIT(arguments[0]);
							out << ".";
							//auto lambdaFunType = lambdaExpr->getType().as<FunctionTypePtr>();
							out << lambdaNames[lambdaExpr->getReference()];
						} else if (lang::isDerived(lambdaExpr)) {
							out << lang::getConstructName(lambdaExpr);
						} else if (lambdaExpr->getType().as<FunctionTypePtr>().isConstructor()) {
							auto funType = lambdaExpr->getType().as<FunctionTypePtr>();
							// check if the constructor is a freely defined
							if (visitedFreeFunctions.find(lambdaExpr->getReference()) != visitedFreeFunctions.end()) {
								// indeed, we have a free constructor here
								out << std::get<1>(visitedFreeFunctions[lambdaExpr->getReference()]);
							} else {
								out << getObjectName(lambdaExpr->getType()) << "::";
							}
						} else {
							// standard function call name
							out << lambdaNames[lambdaExpr->getReference()];
						}
					} else if (auto var = functionPtr.isa<LambdaReferencePtr>()) {
						if(var->getType()->isMember()) {
							isMemberFun = true;
						}
						out << lambdaNames[var];
					} else if (auto call = functionPtr.isa<CallExprPtr>()) {
						const auto& refExt = call.getNodeManager().getLangExtension<lang::ReferenceExtension>();
						if (analysis::isCallOf(call, refExt.getRefDeref())) {
							out << "(";
							VISIT(function);
							out << ")";
						} else {
							VISIT(function);
						}
					} else if (functionPtr.isa<CastExprPtr>()) {
						out << "(";
						VISIT(function);
						out << ")";
					} else {
						//Literal Expression
						VISIT(function);
					}
				}

				// print arguments
				auto args = node->getArgumentList();
				if(args.empty()) {
					out << "()";
				} else {
					auto begin = args.begin() + (isMemberFun ? 1 : 0);
					out << "(" << join(", ", begin, args.end(), [&](std::ostream& out, const NodeAddress& curParam) {
						VISIT(curParam);
					}) << ")";
				}

				// print materialize if required
				if(core::analysis::isMaterializingCall(node)) {
					out << " materialize ";
				}
			}

			PRINT(BindExpr) {
				out << "(" << join(",", node->getParameters(),[&](std::ostream& out, const ExpressionAddress& cur) {
					VISIT(cur);
					out << " : ";
					VISIT(cur->getType());
				}) << ")=> ";
				VISIT(node->getCall());
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
				out << "(" << ::join(", ", node->getExpressions(), [&](std::ostream&, const ExpressionAddress& cur) { VISIT(cur); }) << ")";
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

			PRINT(InitExpr) {
				out << "<";
				VISIT(node->getType());
				out << ">(";
				VISIT(node->getMemoryExpr());
				out << ") {" <<	join(", ", node->getInitExprList(), [&](std::ostream& out, const ExpressionAddress& cur) {
					VISIT(cur);
				}) << "}";
			}

			PRINT(TagTypeReference) {
				out << node->getName()->getValue();
			}

			PRINT(Program) {
				out << "// Inspire Program ";
				for(const auto& point : node->getEntryPoints()) {
					const auto& cur = point.isa<LambdaExprAddress>();
					newLine();
					VISIT(cur->getFunctionType()->getReturnType());
					auto parameters = cur->getParameterList();
					out << " function " << lambdaNames[cur->getReference()] << " ("
					<< join(", ", parameters, [&](std::ostream& out, const VariableAddress& curVar) {
						VISIT(curVar);
						out << " : ";
						VISIT(curVar->getType());
					}) << ")";
					VISIT(cur->getBody());
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
			ExpressionAddress argument = call->getArgument(n);
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
			#define ARG(N) call->getArgument(N)
			#define MGR call->getNodeManager()
			#define PRINT_EXPR(E) printer.visit(E)
			#define PRINT_ARG(N) printArgument(printer, call, N)
			#define HAS_OPTION(OPT) printer.getPrettyPrint().hasOption(PrettyPrinter::OPT)
			#define ADD_FORMATTER(Literal)                                                                                                             \
				res[Literal] = [](InspirePrinter & printer, const CallExprAddress& call)

//			if(config.hasOption(PrettyPrinter::PRINT_DEREFS)) {
				ADD_FORMATTER(refExt.getRefDeref()) {
					if (ARG(0).isa<VariableAddress>() && ARG(0).getAddressedNode() == printer.getTopOfThisStack()) {
						PRINT_ARG(0);
					} else {
						OUT("*");
						PRINT_ARG(0);
					}
				};
//			} else {
//				ADD_FORMATTER(refExt.getRefDeref()) { PRINT_ARG(0); };
//			}

			ADD_FORMATTER(refExt.getRefAssign()) {
				PRINT_ARG(0);
				OUT(" = ");
				PRINT_ARG(1);
			};
			ADD_FORMATTER(refExt.getRefTemp()) {
				OUT("ref_temp(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefNew()) {
				OUT("ref_new(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefTempInit()) {
				OUT("ref_temp_init(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefNewInit()) {
				OUT("ref_new_init(");
				PRINT_ARG(0);
				OUT(")");
			};
			ADD_FORMATTER(refExt.getRefDelete()) {
				OUT("ref_delete(");
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

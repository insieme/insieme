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
 *
 */
#include "insieme/core/types/type_variable_deduction.h"

#include <iterator>

#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/types/subtype_constraints.h"
#include "insieme/core/types/type_variable_renamer.h"

#include "insieme/utils/annotation.h"
#include "insieme/utils/difference_constraints.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace types {

	namespace {

		// An enumeration of the constraint directions
		enum Direction { SUB_TYPE = 0, SUPER_TYPE = 1 };

		/**
		 * A utility function to inverse constrain types. This function will turn a sub-type constrain
		 * into a super-type constraint and vica verce.
		 */
		inline static Direction inverse(Direction type) {
			return (Direction)(1 - type);
		}

		/**
		 *  Adds the necessary constraints on the instantiation of the type variables used within the given type parameter.
		 *
		 *  @param constraints the set of constraints to be extended
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 */
		void addEqualityConstraints(SubTypeConstraints& constraints, const TypePtr& typeA, const TypePtr& typeB);

		/**
		 * Expands variadic type parameters in the given params type list to fit the structure of the given argument type. The
		 * necessary un-pack substitution will be returned as a result.
		 *
		 * @param manager the manager to be used for creating entries within the resulting substitution
		 * @param params the list of parameters to be expanded
		 * @param arguments the list of arguments to be matched against
		 * @return the substitution applied on the params type list
		 */
		SubstitutionOpt expandVariadicTypeVariables(NodeManager& manager, const TypeList& params, const TypeList& arguments);


		// -------------------------------------------------------- Implementation ----------------------------------------------

		// the kind of constraints collected for computing the length of parameter expansions
		using PackLengthConstraints = utils::DifferenceConstraints<TypePtr>;

		namespace {

			// A utility class to collect pack-length constraints
			class PackLengthConstraintsCollector : public IRVisitor<void,Pointer,const NodePtr&> {

				using super = IRVisitor<void,Pointer,const NodePtr&>;

				PackLengthConstraints& result;

			public:

				PackLengthConstraintsCollector(PackLengthConstraints& result)
					: super(true), result(result) {}

				void visit(const NodePtr& node, const NodePtr& other) override {

					// check whether result is unsatisfiable
					if (result.isUnsatisfiable()) return;

					// general rules for all pairs
					if (other.isa<TypeVariablePtr>()) return;
					if (other.isa<VariadicTypeVariablePtr>()) return;

					// if one is a reference, the other is not => remove reference (materialization)
					bool isRefA = core::lang::isReference(node);
					bool isRefB = core::lang::isReference(other);
					if (isRefA && !isRefB) return visit(core::analysis::getReferencedType(node),other);
					if (!isRefA && isRefB) return visit(node,core::analysis::getReferencedType(other));

					// dispatch to individual nodes
					super::visit(node,other);
				}

				void visitTypeVariable(const TypeVariablePtr& varType, const NodePtr& other) override {

					// if type variable, everything of length 1 is allowed -> constrain variadic types
					if(other.isa<VariadicTypeVariablePtr>() || other.isa<VariadicGenericTypeVariablePtr>()) {
						result.addConstraint(other.as<TypePtr>(), 1);
					}
				}

				void visitGenericType(const GenericTypePtr& genType, const NodePtr& other) override {

					// if other side is a generic type ...
					if (auto otherGen = other.isa<GenericTypePtr>()) {
						collectFromLists(genType->getTypeParameterList(), otherGen->getTypeParameterList());
					} else if (auto otherGenVar = other.isa<GenericTypeVariablePtr>()) {
						collectFromLists(genType->getTypeParameterList(), otherGenVar->getTypeParameter());
					}
				}

				void visitTupleType(const TupleTypePtr& tuple, const NodePtr& other) override {

					// the other side has to be of the tuple type
					auto otherTuple = other.isa<TupleTypePtr>();
					if (!otherTuple) {
						result.markUnsatisfiable();
						return;
					}

					// check which of the two tuples are ending with a variadic variable
					const auto& listA = tuple->getElementTypes();
					const auto& listB = otherTuple->getElementTypes();
					collectFromLists(listA, listB);
				}

				void visitFunctionType(const FunctionTypePtr& function, const NodePtr& other) override {

					// the other side has to be of the tuple type
					auto otherFunction = other.isa<FunctionTypePtr>();
					if (!otherFunction) {
						result.markUnsatisfiable();
						return;
					}

					// process various sub-lists
					collectFromLists(function->getInstantiationTypeList(), otherFunction->getInstantiationTypeList());
					collectFromLists(function->getParameterTypeList(), otherFunction->getParameterTypeList());
					visit(function->getReturnType(), otherFunction->getReturnType());
				}

				void visitTagType(const TagTypePtr& tag, const NodePtr& other) override {

					auto otherTag = other.isa<TagTypePtr>();
					if (!otherTag) {
						result.markUnsatisfiable();
						return;
					}

					// check argument type
					if (
						tag->getRecord()->getNodeType() != otherTag->getRecord()->getNodeType() ||
						tag->getFields().size() != otherTag->getFields().size()
					) {
						result.markUnsatisfiable();
						return;
					}

					// use peeled version
					auto peeledA = tag->peel();
					auto peeledB = otherTag->peel();

					// check fields
					for (const auto& cur : peeledA->getFields()) {
						auto otherType = peeledB->getFieldType(cur->getName());
						if (!otherType) {
							result.markUnsatisfiable();
							return;
						}
						visit(cur->getType(), otherType);
					}
				}

				void visitNode(const NodePtr& a, const NodePtr&) override {
					assert_not_implemented() << "No support for node type: " << a->getNodeType() << "\n";
				}

			private:

				void collectFromLists(const TypeList& listA, const TypeList& listB) {

					// extract tailing variadic types
					auto varA = endsWithVariadic(listA);
					auto varB = endsWithVariadic(listB);

					// get size without variadic
					auto sizeA = listA.size() - ((varA) ? 1 : 0);
					auto sizeB = listB.size() - ((varB) ? 1 : 0);


					// process non-variadic part
					for(size_t i=0; i<std::min(sizeA,sizeB); ++i) {
						visit(listA[i],listB[i]);
					}


					// if size does not fit => fail
					if (!varA && !varB && sizeA != sizeB) {
						result.markUnsatisfiable();
						return;
					}

					if (varA && !varB) {
						if (sizeA <= sizeB) {
							// fix size of v variables A...
							result.addConstraint(varA,sizeB-sizeA);
						} else {
							// unsatisfiable
							result.markUnsatisfiable();
							return;
						}
					}

					if (!varA && varB) {
						// unsatisfiable
						result.markUnsatisfiable();
						return;
					}

					if (varA && varB) {
						result.addConstraint(varA,varB,sizeB-sizeA+1);
					}

				}

				TypePtr endsWithVariadic(const TypeList& types) {
					// check for an empty list
					if (types.empty()) return TypePtr();

					// check whether the last is a variadic type variable
					if (types.back().template isa<VariadicTypeVariablePtr>()) {
						return types.back();
					}

					// check whether the last is a variadic generic type variable
					if (types.back().template isa<VariadicGenericTypeVariablePtr>()) {
						return types.back();
					}

					// no variadic type
					return TypePtr();
				}

			};

		}

		PackLengthConstraints collectPackLengthConstraints(const TypePtr& a, const TypePtr& b) {

			// collect constraints using visitor
			PackLengthConstraints res;
			PackLengthConstraintsCollector(res).visit(a,b);
			return res;

		}

		SubstitutionOpt expandVariadicTypeVariables(NodeManager& manager, const TypeList& params, const TypeList& arguments) {
			static const bool debug = false;
			static const SubstitutionOpt fail;

			// initialize result
			Substitution res;

			// wrap inputs in a tuple type
			auto paramTuple = TupleType::get(manager, params);
			auto argsTuple = TupleType::get(manager, arguments);
			if (debug) std::cout << "\nParameter: " << *paramTuple << "\n";
			if (debug) std::cout << "Argument:  " << *argsTuple << "\n";


			// 1) collect all variadic type variables
			std::vector<VariadicTypeVariablePtr> vvars;
			std::vector<VariadicGenericTypeVariablePtr> vgvars;
			//visitDepthFirstPrunable(TupleType::get(manager, TypeList{ paramTuple, argsTuple }), [&](const NodePtr& cur) {
			visitDepthFirstPrunable(paramTuple, [&](const NodePtr& cur) {

				// stop descent for expressions and statements
				if (cur.isa<ExpressionPtr>() || cur.isa<StatementPtr>()) return Action::Prune;

				// collect variadic type variables
				if (auto var = cur.isa<VariadicTypeVariablePtr>()) {
					vvars.push_back(var);
					return Action::Prune;
				}

				// collect variadic generic type variables
				if (auto var = cur.isa<VariadicGenericTypeVariablePtr>()) {
					vgvars.push_back(var);
				}

				// all others have to be further investigated
				return Action::Descent;
			}, true);

			if (debug) std::cout << "Variadic Type Parameters: " << vvars << "\n";
			if (debug) std::cout << "Variadic Generic Type Parameters:  " << vgvars << "\n";

			// if there are no variadic parameters => done
			if (vvars.empty() && vgvars.empty()) return res;

			// 2) Determine length of parameter pack expansions using difference constraints

			// extract constraints
			PackLengthConstraints constraints = collectPackLengthConstraints(paramTuple,argsTuple);
			if (debug) std::cout << "Constraints: " << constraints << "\n";

			// solve constraints
			auto solution = constraints.solve();
			if (debug) std::cout << "Solution: " << solution << "\n";
			if (!solution) return fail;

			// 3) expand parameters

			// create expanding substitution
			for(const auto& cur : vvars) {
				TypeVariableList vars;
				if (solution[cur] < 0) return fail;
				for(int i=0; i<solution[cur]; i++) {
					vars.push_back(TypeVariable::get(manager,format("%s#%d",*cur->getVarName(), i+1)));
				}
				res.addMapping(cur,vars);
			}
			for(const auto& cur : vgvars) {
				GenericTypeVariableList vars;
				if (solution[cur] < 0) return fail;
				for(int i=0; i<solution[cur]; i++) {
					vars.push_back(GenericTypeVariable::get(manager,format("%s#%d",*cur->getVarName(), i+1), cur->getTypeParameter()));
				}
				res.addMapping(cur,vars);
			}

			if (debug) std::cout << "Substitution: " << res << "\n";

			// return substitution
			return res;

		}

		void addEqualityConstraints(SubTypeConstraints& constraints, const TypePtr& typeA, const TypePtr& typeB) {
			// check constraint status
			if(!constraints.isSatisfiable()) { return; }

			// extract node types
			NodeType nodeTypeA = typeA->getNodeType();
			NodeType nodeTypeB = typeB->getNodeType();

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// the substitution for the variable has to be equivalent to the argument type
				constraints.addEqualsConstraint(static_pointer_cast<const TypeVariable>(typeA), typeB);
				return;
			}

			if(nodeTypeB == NT_TypeVariable) {
				// the substitution for the variable has to be equivalent to the parameter type
				constraints.addEqualsConstraint(static_pointer_cast<const TypeVariable>(typeB), typeA);
				return;
			}

			// for all other types => the node type has to be the same
			if(nodeTypeA != nodeTypeB) {
				// not satisfiable in any way
				constraints.makeUnsatisfiable();
				return;
			}


			// so, the type is the same => distinguish the node type

			// check node types
			switch(nodeTypeA) {
			// first handle those types equipped with type parameters
			case NT_GenericType: {
				// check name of generic type ... if not matching => wrong
				auto genParamType = static_pointer_cast<const GenericType>(typeA);
				auto genArgType = static_pointer_cast<const GenericType>(typeB);
				if(genParamType->getFamilyName() != genArgType->getFamilyName()) {
					// those types cannot be equivalent if they are part of a different family
					constraints.makeUnsatisfiable();
					return;
				}

				// check parameter types
				for(auto it = make_paired_iterator(genParamType->getTypeParameter().begin(), genArgType->getTypeParameter().begin());
				    it != make_paired_iterator(genParamType->getTypeParameter().end(), genArgType->getTypeParameter().end()); ++it) {
					// filter int-type parameter
					if(it->first->getNodeCategory() == NC_Type) {
						// add equality constraints recursively
						addEqualityConstraints(constraints, static_pointer_cast<const Type>(it->first), static_pointer_cast<const Type>(it->second));
					}
				}

				break;
			}
			case NT_FunctionType: {
				FunctionTypePtr funA = static_pointer_cast<const FunctionType>(typeA);
				FunctionTypePtr funB = static_pointer_cast<const FunctionType>(typeB);

				if(funA->getKind() != funB->getKind()) {
					// unable to satisfy equality constraint
					constraints.makeUnsatisfiable();
					return;
				}

				// ... fall-through to next check (all child-type check)
			}
			case NT_TupleType: {
				// the number of sub-types must match
				auto paramChildren = typeA->getChildList();
				auto argChildren = typeB->getChildList();

				// check number of sub-types
				if(paramChildren.size() != argChildren.size()) {
					constraints.makeUnsatisfiable();
					return;
				}

				// check all child nodes
				for(auto it = make_paired_iterator(paramChildren.begin(), argChildren.begin());
				    it != make_paired_iterator(paramChildren.end(), argChildren.end()); ++it) {
					// filter int-type parameter
					if((*it).first->getNodeCategory() == NC_Type) {
						// add equality constraints recursively
						addEqualityConstraints(constraints, static_pointer_cast<const Type>((*it).first), static_pointer_cast<const Type>((*it).second));
					}
				}

				break;
			}

			case NT_TagType: {

				// check for recursive types
				auto tagTypeA = typeA.as<TagTypePtr>();
				auto tagTypeB = typeB.as<TagTypePtr>();

				// handle recursive type on side A
				if (tagTypeA.isRecursive()) {
					addEqualityConstraints(constraints, tagTypeA->peel(), tagTypeB);
					return;
				}

				// handle recursive type on side B
				if (tagTypeB.isRecursive()) {
					addEqualityConstraints(constraints, tagTypeA, tagTypeB->peel());
					return;
				}

				// has to be the same type
				if (tagTypeA->getRecord()->getNodeType() != tagTypeB->getRecord()->getNodeType()) {
					constraints.makeUnsatisfiable();
					return;
				}

				// names and sub-types have to be checked
				auto entriesA = tagTypeA->getFields();
				auto entriesB = tagTypeB->getFields();

				// check equality of names and types
				// check number of entries
				if(entriesA.size() != entriesB.size()) {
					constraints.makeUnsatisfiable();
					return;
				}

				// check all child nodes
				for(const auto& cur : make_paired_range(entriesA, entriesB)) {
					// ensure identifiers are identical (those cannot be variable)
					auto entryA = cur.first;
					auto entryB = cur.second;
					if(*entryA->getName() != *entryB->getName()) {
						// unsatisfiable
						constraints.makeUnsatisfiable();
						return;
					}

					// add equality constraints recursively
					addEqualityConstraints(constraints, entryA->getType(), entryB->getType());
				}

				// also cover parents
				if (tagTypeA->isStruct()) {
					assert_eq(*tagTypeA->getStruct()->getParents(), *tagTypeB->getStruct()->getParents())
							<< "TODO: implement type variable deduction with parent support!"
									"Note: do not forget to check the parent fields virtual and access spec!";
				}

				break;
			}

			case NT_NumericType: {
				// check for equality
				if (*typeA != *typeB) {
					// if they are different => constraints unsatisfiable
					constraints.makeUnsatisfiable();
				}
				break;
			}

			default: assert_fail() << "Missed a kind of type: " << nodeTypeA;
			}
		}

		void addTypeConstraints(SubTypeConstraints& constraints, Substitution& substitution, const TypePtr& paramType, const TypePtr& argType, Direction direction) {
			// check constraint status
			if(!constraints.isSatisfiable()) { return; }

			// check whether relation is already satisfied
			if((direction == Direction::SUB_TYPE && isSubTypeOf(paramType, argType))
			   || (direction == Direction::SUPER_TYPE && isSubTypeOf(argType, paramType))) {
				return;
			}

			// extract node types
			NodeType nodeTypeA = paramType->getNodeType();
			NodeType nodeTypeB = argType->getNodeType();

			// --------------------------------------------- Type Variables --------------------------------------------

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// add corresponding constraint to this variable
				if(direction == Direction::SUB_TYPE) {
					constraints.addSubtypeConstraint(paramType, argType);
				} else {
					constraints.addSubtypeConstraint(argType, paramType);
				}
				return;
			}

			// ------------------------------------------- Generic Type Variables --------------------------------------

			// handle variables
			if(nodeTypeA == NT_GenericTypeVariable) {

				// check for matching type on the other side
				if (nodeTypeB != NT_GenericTypeVariable && nodeTypeB != NT_GenericType) {
					constraints.makeUnsatisfiable();
					return;
				}

				// add corresponding constraint to this variable
				if(direction == Direction::SUB_TYPE) {
					constraints.addSubtypeConstraint(paramType, argType);
				} else {
					constraints.addSubtypeConstraint(argType, paramType);
				}

				// get parameter types
				const TypeList& paramParams = paramType.as<GenericTypeVariablePtr>()->getTypeParameterList();
				const TypeList& argParams = (nodeTypeB == NT_GenericTypeVariable)
					? argType.as<GenericTypeVariablePtr>()->getTypeParameterList()
					: argType.as<GenericTypePtr>()->getTypeParameterList();

				// check number of type parameters
				if(paramParams.size() != argParams.size()) {
					// different number of arguments => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// process parameters
				for(const auto& cur : make_paired_range(paramParams, argParams)) {
					addTypeConstraints(constraints, substitution, cur.first, cur.second, direction);
				}

				return;
			}

			// --------------------------------------------- Function Type ---------------------------------------------

			// check function type
			if(nodeTypeA == NT_FunctionType) {
				// argument has to be a function as well
				if(nodeTypeB != NT_FunctionType) {
					// => no match possible
					constraints.makeUnsatisfiable();
					return;
				}

				auto funParamType = static_pointer_cast<const FunctionType>(paramType);
				auto funArgType = static_pointer_cast<const FunctionType>(argType);

				// check kind of function parameter
				if(funParamType->isPlain() && !funArgType->isPlain()) {
					// cannot pass a closure function to a plain function parameter
					constraints.makeUnsatisfiable();
				}

				// check number of arguments
				const TypeList& paramParams = funParamType->getParameterTypes()->getTypes();
				const TypeList& argParams = funArgType->getParameterTypes()->getTypes();
				if(paramParams.size() != argParams.size()) {
					// different number of arguments => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// add constraints on arguments
				auto begin = make_paired_iterator(paramParams.begin(), argParams.begin());
				auto end = make_paired_iterator(paramParams.end(), argParams.end());
				for(auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, substitution, it->first, it->second, inverse(direction));
				}

				// ... and the return type
				addTypeConstraints(constraints, substitution, funParamType->getReturnType(), funArgType->getReturnType(), direction);
				return;
			}

			// --------------------------------------------- Tuple Types ---------------------------------------------

			// if both are generic types => add sub-type constraint
			if(nodeTypeA == nodeTypeB && nodeTypeA == NT_TupleType) {
				const TypeList& paramList = static_pointer_cast<const TupleType>(paramType)->getElementTypes();
				const TypeList& argumentList = static_pointer_cast<const TupleType>(argType)->getElementTypes();

				// check length
				if(paramList.size() != argumentList.size()) {
					constraints.makeUnsatisfiable();
					return;
				}

				// ensure sub-type relation
				auto begin = make_paired_iterator(paramList.begin(), argumentList.begin());
				auto end = make_paired_iterator(paramList.end(), argumentList.end());
				for(auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, substitution, it->first, it->second, direction);
				}
				return;
			}

			// --------------------------------------------- Generic Types ---------------------------------------------

			// if both are generic types => add sub-type constraint
			if(nodeTypeA == nodeTypeB && nodeTypeA == NT_GenericType) {
				// add a simple sub-type constraint
				constraints.addSubtypeConstraint(argType, paramType);
				return;
			}

			// --------------------------------------------- Remaining Types ---------------------------------------------

			// check rest => has to be equivalent (e.g. ref, channels, ...)
			addEqualityConstraints(constraints, paramType, argType);
		}


		inline TypeMapping substituteFreeVariablesWithConstants(NodeManager& manager, const TypeList& arguments) {
			TypeMapping argumentMapping;

			// realized using a recursive lambda visitor
			IRVisitor<>* rec;
			auto collector = makeLambdaVisitor([&](const NodePtr& cur) {
				if(cur.isa<ExpressionPtr>()) {
					return;
				}
				NodeType kind = cur->getNodeType();
				switch(kind) {
				case NT_TypeVariable: {
					// check whether already encountered
					const TypeVariablePtr& var = static_pointer_cast<const TypeVariable>(cur);
					if(argumentMapping.containsMappingFor(var)) { break; }
					// add a new constant
					const TypePtr substitute = GenericType::get(manager, string("_const_") + var->getVarName()->toString());
					argumentMapping.addMapping(var, substitute);
					break;
				}
				case NT_FunctionType: {
					// do not consider function types (variables inside are bound)
					break;
				}
				default: {
					// decent recursively
					for_each(cur->getChildList(), [&](const NodePtr& cur) { rec->visit(cur); });
				}
				}
			}, true);
			rec = &collector;

			// finally, collect and substitute variables
			collector.visit(TupleType::get(manager, arguments));

			return argumentMapping;
		}
	}

	namespace {

		using MappingSolution = insieme::utils::map::PointerMap<TypeVariablePtr,TypePtr>;

		bool match(const NodePtr& a, const NodePtr& b, MappingSolution& solution) {

			// get node types
			auto nodeTypeA = a->getNodeType();
			auto nodeTypeB = b->getNodeType();

			// if b is a type variable, the pure matching approach is not valid any more
			if (nodeTypeB == NT_TypeVariable) return false;

			// if both are the same => nothing to do
			if (*a == *b) return true;

			// and the category of B
			auto category = b->getNodeCategory();

			// check whether a is a variable
			if (nodeTypeA == NT_TypeVariable) {
				auto var = a.as<TypeVariablePtr>();

				// b must be a type in this case
				if(category != NC_Type) return false;

				// check whether there is already a mapping for this variable
				auto pos = solution.find(var);
				if (pos != solution.end()) {
					// check whether it is the same
					return *pos->second == *b;
				} else {
					// add a new mapping
					solution[var] = b.as<TypePtr>();
					return true;
				}
			}

			// for everything else, compare recursively

			// the current type needs to be the same
			if (nodeTypeA != nodeTypeB) return false;

			// value nodes must match
			if (category == NC_Value) {
				return *a == *b;
			}

			// and there must be the same number of children
			const auto& childrenA = a->getChildList();
			const auto& childrenB = b->getChildList();
			if (childrenA.size() != childrenB.size()) return false;

			// and those must be matchable
			for(std::size_t i = 0; i<childrenA.size(); i++) {
				if (!match(childrenA[i],childrenB[i],solution)) return false;
			}

			// everything seems fine
			return true;
		}


		SubstitutionOpt getMatchBasedInstantiation(NodeManager& manager, const TypeList& parameters, const TypeList& arguments) {

			// the result to be returned upon failure
			const static SubstitutionOpt fail;

			// this quick solver only works for same-size lists
			if (parameters.size() != arguments.size()) return fail;

			// build up solution
			MappingSolution mapping;

			for(std::size_t i = 0; i<parameters.size(); i++) {

				// get the parameter and the argument
				auto param = parameters[i];
				auto arg = arguments[i];

				// handle necessary materialization
				if (arg.isa<TagTypePtr>()) return fail;

				// check whether it can be matched
				if (!match(param,arg,mapping)) return fail;
			}

			// use result
			Substitution res;
			for(const auto& cur : mapping) {
				res.addMapping(cur.first,cur.second);
			}
			return res;
		}


		SubstitutionOpt getTypeVariableInstantiationInternal(NodeManager& manager, TypeList parameter, TypeList arguments, bool allowMaterialization) {
			const bool debug = false;

			// the result to be returned upon failure
			const static SubstitutionOpt fail;

			// see whether a simple match would be sufficient
			{
				IRBuilder builder(manager);

				auto match = getMatchBasedInstantiation(manager, parameter, arguments);
				if (match) return match;

			}


			// -------------------------------- Invalid Call By Value ---------------------------------------
			//
			// Support variadic type variables in parameter list.
			//
			// ----------------------------------------------------------------------------------------------

			// adapt type parameter list for variadic function signature
			auto expansion = expandVariadicTypeVariables(manager, parameter, arguments);
			if (!expansion) return fail;

			// initialize result with variadic variable expansion
			Substitution res(std::move(*expansion));

			// use private node manager for computing temporary results
			NodeManager internalManager(manager);

			// apply substitution to parameter types
			parameter = res(TupleType::get(internalManager,parameter))->getElementTypes();

			if (debug) { std::cout << "Variadic argument expansion: " << res << "\n"; }
			if (debug) { std::cout << "Expanded Parameters: " << parameter << "\n"; }
			if (debug) { std::cout << "Expanded Arguments: " << arguments << "\n"; }

			// check length of parameter and arguments
			if (parameter.size() != arguments.size()) { return fail; }


			// -------------------------------- Invalid Call By Value ---------------------------------------
			//
			// Non-Trivial types must not be passed by value.
			//
			// ----------------------------------------------------------------------------------------------

			// check all arguments
			for (const auto& cur : arguments) {
				if (!analysis::isTrivial(cur)) return fail;
			}


			// ------------------------------ Implicit Materialization --------------------------------------
			//
			// In C++ it is allowed to pass values to function parameters accepting references. In this case,
			// an implicit temporary object initialized by a copy-constructor is created at the call-site.
			// This is supported for all 'trivial' types.
			//
			// ----------------------------------------------------------------------------------------------

			TypeList materializedArguments = arguments;
			for (size_t i = 0; i < parameter.size(); i++) {

				if(allowMaterialization) {

					bool isRefArg = lang::isReference(arguments[i]);
					bool isRefParam = lang::isReference(parameter[i]);

					// implicit materialization
					if (!isRefArg && isRefParam) {
						lang::ReferenceType refParam(parameter[i]);
						switch (refParam.getKind()) {
						case lang::ReferenceType::Kind::Undefined: return fail;
						case lang::ReferenceType::Kind::Plain: return fail;
						case lang::ReferenceType::Kind::Qualified: return fail;
						case lang::ReferenceType::Kind::CppReference: {
							/* the cpp reference must be const */
							if (!refParam.isConst()) return fail;
							break;
						}
						case lang::ReferenceType::Kind::CppRValueReference: {
							/* all fine */
						}
						}

						// check whether the argument is trivial or not convertible to parameter type
						if (!analysis::isTrivial(arguments[i]) && !analysis::hasConstructorAccepting(parameter[i], arguments[i])) {
							return fail;
						}

						// all checks out => materialize
						materializedArguments[i] = lang::ReferenceType::create(
							arguments[i], refParam.isConst(), refParam.isVolatile(), refParam.getKind()
							);
					}

					// deal with value construction
					if(isRefArg && !isRefParam) {
						if(lang::isCppReference(arguments[i]) || lang::isCppRValueReference(arguments[i])) {
							lang::ReferenceType refArg(arguments[i]);
							materializedArguments[i] = refArg.getElementType();
						}
						// implicit copy construction
						else if(analysis::hasConstructorAccepting(parameter[i], arguments[i])) {
							materializedArguments[i] = parameter[i];
						}
						// directly passing initialized materialized value as plain ref
						else if(transform::materialize(parameter[i]) == materializedArguments[i]) {
							materializedArguments[i] = parameter[i];
						}
					}

					// qualifier promotion and implicit plain-ref casting
					if(isRefArg && isRefParam) {
						lang::ReferenceType argType(arguments[i]);
						lang::ReferenceType paramType(parameter[i]);

						// promote qualifiers
						if(paramType.isConst() && !argType.isConst()) { argType.setConst(true); }
						if(paramType.isVolatile() && !argType.isVolatile()) { argType.setVolatile(true); }

						// convert implicitly to plain reference
						if(paramType.isPlain() && (argType.isCppReference() || argType.isCppRValueReference())) {
							argType.setKind(lang::ReferenceType::Kind::Plain);
						}

						// update argument
						materializedArguments[i] = argType.toType();
					}
				}

				// promote qualifiers on pointers (TODO: better solution to this)
				if(lang::isPointer(arguments[i]) && lang::isPointer(parameter[i])) {
					lang::PointerType argType(arguments[i]);
					lang::PointerType paramType(parameter[i]);
					if(paramType.isConst() && !argType.isConst()) { argType.setConst(true); }
					if(paramType.isVolatile() && !argType.isVolatile()) { argType.setVolatile(true); }
					materializedArguments[i] = argType;
				}
			}

			if (debug) { std::cout << " Substitution: " << res << std::endl; }
			if (debug) { std::cout << "    Arguments: " << arguments << std::endl; }
			if (debug) { std::cout << " Materialized: " << materializedArguments << std::endl; }


			// ---------------------------------- Variable Renaming -----------------------------------------
			//
			// The variables have to be renamed to avoid collisions within type variables used within the
			// function type and variables used within the arguments. Further, variables within function
			// types of the arguments need to be renamed independently (to avoid illegal capturing)
			//
			// ----------------------------------------------------------------------------------------------


			// for the renaming a variable re-namer is used
			VariableRenamer renamer;

			// 1) convert parameters (consistently, all at once)
			TypeMapping parameterMapping = renamer.mapVariables(TupleType::get(internalManager, parameter));
			TypeList renamedParameter = parameterMapping.applyForward(internalManager, parameter);

			if (debug) { std::cout << " Parameter: " << parameter << std::endl; }
			if (debug) { std::cout << "   Renamed: " << renamedParameter << std::endl; }

			// 2) convert arguments (individually)
			//		- fix free variables consistently => replace with constants
			//		- rename unbounded variables - individually per parameter

			// collects the mapping of free variables to constant replacements (to protect them
			// from being substituted during some unification process)
			TypeMapping&& argumentMapping = substituteFreeVariablesWithConstants(internalManager, materializedArguments);

			if (debug) { std::cout << " Arguments: " << materializedArguments << std::endl; }
			if (debug) { std::cout << "   Mapping: " << argumentMapping << std::endl; }

			// apply renaming to arguments
			TypeList renamedArguments = materializedArguments;
			vector<TypeMapping> argumentRenaming;
			for (std::size_t i = 0; i < renamedArguments.size(); ++i) {
				TypePtr& cur = renamedArguments[i];

				// first: apply bound variable substitution
				cur = argumentMapping.applyForward(internalManager, cur);

				// second: apply variable renaming
				TypeMapping mapping = renamer.mapVariables(internalManager, cur);
				if (!mapping.empty()) { argumentRenaming.push_back(mapping); }
				cur = mapping.applyForward(internalManager, cur);
			}

			if (debug) { std::cout << " Renamed Arguments: " << renamedArguments << std::endl; }
			if (debug) { std::cout << " Renamings: " << argumentRenaming << std::endl; }


			// ---------------------------------- Assembling Constraints -----------------------------------------

			// collect constraints on the type variables used within the parameter types
			SubTypeConstraints constraints;

			// collect constraints
			auto begin = make_paired_iterator(renamedParameter.begin(), renamedArguments.begin());
			auto end = make_paired_iterator(renamedParameter.end(), renamedArguments.end());
			for (auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
				// add constraints to ensure current parameter is a super-type of the arguments
				addTypeConstraints(constraints, res, it->first, it->second, Direction::SUPER_TYPE);
			}


			if (debug) { std::cout << " Constraints: " << constraints << std::endl; }

			// ---------------------------------- Solve Constraints -----------------------------------------

			// solve constraints to obtain results
			SubstitutionOpt solution = constraints.solve(internalManager);
			if (!solution) {
				// if unsolvable => return this information
				if (debug) { std::cout << " Terminated with no solution!" << std::endl << std::endl; }
				return fail;
			}

			if (debug) { std::cout << " Solution: " << *solution << std::endl; }

			// ----------------------------------- Revert Renaming ------------------------------------------
			// (and produce a result within the correct node manager - internal manager will be thrown away)

			// check for empty solution (to avoid unnecessary operations
			if (solution->empty()) {
				if (debug) { std::cout << " Terminated before revert-renaming with: " << res << std::endl << std::endl; }
				return res;
			}

			// reverse variables from previously selected constant replacements (and bring back to correct node manager)
			for (const auto& cur : solution->getVariableMapping()) {
				TypeVariablePtr var = static_pointer_cast<const TypeVariable>(parameterMapping.applyBackward(manager, cur.first));
				TypePtr substitute = argumentMapping.applyBackward(manager, cur.second);

				// also apply argument renaming backwards ..
				for(const auto& cur : argumentRenaming) {
					var = cur.applyBackward(manager, var).as<TypeVariablePtr>();
					substitute = cur.applyBackward(manager, substitute);
				}

				res.addMapping(manager.get(var), manager.get(substitute));
			}

			// also for generic type variables
			for (const auto& cur : solution->getGenericVariableMapping()) {
				GenericTypeVariablePtr var = parameterMapping.applyBackward(manager, cur.first).as<GenericTypeVariablePtr>();
				TypePtr substitute = argumentMapping.applyBackward(manager, cur.second);

				// also apply argument renaming backwards ..
				for(const auto& cur : argumentRenaming) {
					var = cur.applyBackward(manager, var).as<GenericTypeVariablePtr>();
					substitute = cur.applyBackward(manager, substitute);
				}

				res.addMapping(manager.get(var), manager.get(substitute));
			}

			if (debug) { std::cout << " Terminated with: " << res << std::endl << std::endl; }
			return res;
		}

	}

	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypeList& parameter, const TypeList& arguments, bool allowMaterialization) {

		using ResultMap = std::map<std::tuple<TypeList, TypeList, bool>, SubstitutionOpt>;

		struct ResultCache {
			mutable ResultMap results;
			bool operator==(const ResultCache& other) const {
				assert_fail() << "Should never be reached!";
				return false;
			};
		};

		// initialize result cache if necessary
		if (!manager.hasAttachedValue<ResultCache>()) {
			manager.attachValue(ResultCache());
		}

		// obtain result cache
		auto& cache = manager.getAttachedValue<ResultCache>().results;

		// bring parameter and arguments into this node manager
		TypeList params;
		for (const auto& cur : parameter) params.push_back(manager.get(cur));

		TypeList args;
		for (const auto& cur : arguments) args.push_back(manager.get(cur));

		// look up cache
		ResultMap::key_type key { params, args, allowMaterialization };
		auto pos = cache.find(key);
		if (pos != cache.end()) return pos->second;

		// resolve internal, cache, and return result
		return cache[key] = getTypeVariableInstantiationInternal(manager, params, args, allowMaterialization);
	}

	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypePtr& parameter, const TypePtr& argument, bool allowMaterialization) {
		return getTypeVariableInstantiation(manager, toVector(parameter), toVector(argument), allowMaterialization);
	}


	namespace {

		// -------------------------------------- Function Type Annotations -----------------------

		/**
		 * The kind of annotations attached to a function type nodes to cache type variable substitutions.
		 */
		class VariableInstantionInfo : public core::NodeAnnotation {
			// The name of this annotation
			const static string NAME;

		  public:
			// The key used to attack instantiation results to call nodes
			const static utils::StringKey<VariableInstantionInfo> KEY;

		  private:
			/**
			 * The type of map used internally to manager
			 */
			typedef boost::unordered_map<TypeList, SubstitutionOpt> SubstitutionMap;

			/**
			 * A container for cached results. Both, the type list and the
			 * substitution has to refere to elements maintained by the same manager
			 * than the annotated function type.
			 */
			SubstitutionMap substitutions;

		  public:
			VariableInstantionInfo() {}

			virtual const utils::AnnotationKeyPtr getKey() const {
				return &KEY;
			}

			virtual const std::string& getAnnotationName() const {
				return NAME;
			}

			const SubstitutionOpt* get(const TypeList& args) const {
				auto pos = substitutions.find(args);
				if(pos != substitutions.end()) { return &(pos->second); }
				return 0;
			}

			void add(const TypeList& args, const SubstitutionOpt& res) {
				substitutions.insert(std::make_pair(args, res));
			}

			virtual void clone(const NodeAnnotationPtr& ptr, const NodePtr& clone) const {
				// copy the nodes reference by this annotation to the new manager
				NodeManager& manager = clone->getNodeManager();

				// attach annotation
				std::shared_ptr<VariableInstantionInfo> copy =
				    (clone->hasAnnotation(KEY)) ? clone->getAnnotation(KEY) : std::make_shared<VariableInstantionInfo>();
				SubstitutionMap& map = copy->substitutions;

				// insert copies of the current map
				for_each(substitutions,
				         [&](const SubstitutionMap::value_type& cur) { map.insert(std::make_pair(manager.getAll(cur.first), copyTo(manager, cur.second))); });

				// finally, add new annotation to new version of node
				clone->addAnnotation(copy);
			}

			static const SubstitutionOpt* getFromAnnotation(const FunctionTypePtr& function, const TypeList& arguments) {
				// try loading annotation
				if(auto res = function->getAnnotation(VariableInstantionInfo::KEY)) { return res->get(arguments); }

				// no such annotation present
				return 0;
			}

			static void addToAnnotation(const FunctionTypePtr& function, const TypeList& arguments, const SubstitutionOpt& substitution) {
				auto res = function->getAnnotation(VariableInstantionInfo::KEY);
				if(!res) {
					// create a new annotation
					res = std::make_shared<VariableInstantionInfo>();
					function->addAnnotation(res);
				}
				res->add(arguments, substitution);
			}
		};

		const string VariableInstantionInfo::NAME = "VariableInstantionInfo";
		const utils::StringKey<VariableInstantionInfo> VariableInstantionInfo::KEY = utils::StringKey<VariableInstantionInfo>("VARIABLE_INSTANTIATION_INFO");
	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const FunctionTypePtr& function, const TypeList& arguments, bool allowMaterialization) {
		// use deduction mechanism
		return getTypeVariableInstantiation(manager, function->getParameterTypes()->getTypes(), arguments, allowMaterialization);
	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const CallExprPtr& call, bool allowMaterialization) {
		// check for null
		if(!call) { return boost::none; }

		// derive substitution

		// get argument types
		TypeList argTypes;
		::transform(call->getArgumentList(), back_inserter(argTypes), [](const ExpressionPtr& cur) { return cur->getType(); });

		// get function parameter types
		const TypePtr& funType = call->getFunctionExpr()->getType();
		if(funType->getNodeType() != NT_FunctionType) { return boost::none; }

		// compute type variable instantiation
		SubstitutionOpt res = getTypeVariableInstantiation(manager, static_pointer_cast<const FunctionType>(funType), argTypes, allowMaterialization);

		// done
		return res;
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

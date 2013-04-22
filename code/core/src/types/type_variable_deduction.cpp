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

#include "insieme/core/types/type_variable_deduction.h"

#include <iterator>

#include "insieme/utils/annotation.h"

#include "insieme/core/types/type_variable_renamer.h"
#include "insieme/core/types/subtype_constraints.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/ir_expressions.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace types {

	namespace {

		// An enumeration of the constraint directions
		enum Direction {
			SUB_TYPE = 0, SUPER_TYPE = 1
		};

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
		 *  @param constraints the set of constraints to be extendeded
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 */
		void addEqualityConstraints(SubTypeConstraints& constraints, const TypePtr& typeA, const TypePtr& typeB);

		/**
		 * Adds additional constraints to the given constraint collection such that the type variables used within the
		 * parameter type are limited to values representing super- / sub-types of the given argument type.
		 *
		 *  @param constraints the set of constraints to be extended
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 *  @param direction the direction to be ensured - sub- or supertype
		 */
		void addTypeConstraints(SubTypeConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction);


		// -------------------------------------------------------- Implementation ----------------------------------------------

		void addEqualityConstraints(SubTypeConstraints& constraints, const IntTypeParamPtr& paramA, const IntTypeParamPtr& paramB) {
			if (paramA->getNodeType() == NT_VariableIntTypeParam) {
				// obtain variable
				VariableIntTypeParamPtr var = static_pointer_cast<const VariableIntTypeParam>(paramA);

				// check if the parameter has already been set
				if (IntTypeParamPtr value = constraints.getIntTypeParamValue(var)) {
					if (*value == *paramB) {
						// everything is fine
						return;
					}
					// int type parameter needs to be instantiated twice, in different ways => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// fix a new value for the parameter
				constraints.fixIntTypeParameter(var, paramB);
			} else {
				// the two parameters have to be the same!
				if (*paramA != *paramB) {
					// unable to satisfy constraints
					constraints.makeUnsatisfiable();
					return;
				}
			}
		}


		void addEqualityConstraints(SubTypeConstraints& constraints, const TypePtr& typeA, const TypePtr& typeB) {

			// check constraint status
			if (!constraints.isSatisfiable()) {
				return;
			}

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
			if (nodeTypeA != nodeTypeB) {
				// not satisfiable in any way
				constraints.makeUnsatisfiable();
				return;
			}


			// so, the type is the same => distinguish the node type

			// check node types
			switch(nodeTypeA) {

				case NT_RefType:
				{
					auto refParamType = static_pointer_cast<RefTypePtr>(typeA);
					auto refArgType = static_pointer_cast<RefTypePtr>(typeB);

					// add equality constraint for element type
					addEqualityConstraints(constraints,
							refParamType->getElementType(),
							refArgType->getElementType()
					);

					break;
				}

				case NT_ArrayType:
				case NT_VectorType:
				case NT_ChannelType:
				{
					auto genParamType = static_pointer_cast<SingleElementTypePtr>(typeA);
					auto genArgType = static_pointer_cast<SingleElementTypePtr>(typeB);

					// add equality constraint for element type
					addEqualityConstraints(constraints,
							genParamType->getElementType(),
							genArgType->getElementType()
					);

					// add constraint for int type parameter
					addEqualityConstraints(constraints,
							genParamType->getIntTypeParameter(),
							genArgType->getIntTypeParameter()
					);

					break;
			    }

				// first handle those types equipped with int type parameters
				case NT_GenericType:
				{
					// check name of generic type ... if not matching => wrong
					auto genParamType = static_pointer_cast<const GenericType>(typeA);
					auto genArgType = static_pointer_cast<const GenericType>(typeB);
					if (genParamType->getFamilyName() != genArgType->getFamilyName()) {
						// those types cannot be equivalent if they are part of a different family
						constraints.makeUnsatisfiable();
						return;
					}

					// check the int-type parameter ...
					auto param = genParamType->getIntTypeParameter();
					auto args = genArgType->getIntTypeParameter();

					// quick-check on size
					if (param.size() != args.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// match int-type parameters individually
					for (auto it = make_paired_iterator(param.begin(), args.begin()); it != make_paired_iterator(param.end(), args.end()); ++it) {
						// check constraints state
						if (!constraints.isSatisfiable()) {
							return;
						}
						auto paramA = (*it).first;
						auto paramB = (*it).second;

						addEqualityConstraints(constraints, paramA, paramB);
					}

					// check parameter types
					for (auto it = make_paired_iterator(genParamType->getTypeParameter().begin(), genArgType->getTypeParameter().begin());
							it != make_paired_iterator(genParamType->getTypeParameter().end(), genArgType->getTypeParameter().end()); ++it) {

						// filter int-type parameter
						if (it->first->getNodeCategory() == NC_Type) {
							// add equality constraints recursively
							addEqualityConstraints(constraints,
									static_pointer_cast<const Type>(it->first),
									static_pointer_cast<const Type>(it->second)
							);
						}
					}

					break;
				}
				case NT_FunctionType: {

					FunctionTypePtr funA = static_pointer_cast<const FunctionType>(typeA);
					FunctionTypePtr funB = static_pointer_cast<const FunctionType>(typeB);

					if (funA->getKind() != funB->getKind()) {
						// unable to satisfy equality constraint
						constraints.makeUnsatisfiable();
						return;
					}

					// ... fall-through to next check (all child-type check)
				}
				case NT_TupleType:
				{
					// the number of sub-types must match
					auto paramChildren = typeA->getChildList();
					auto argChildren = typeB->getChildList();

					// check number of sub-types
					if (paramChildren.size() != argChildren.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// check all child nodes
					for (auto it = make_paired_iterator(paramChildren.begin(), argChildren.begin());
							it != make_paired_iterator(paramChildren.end(), argChildren.end()); ++it) {

						// filter int-type parameter
						if ((*it).first->getNodeCategory() == NC_Type) {
							// add equality constraints recursively
							addEqualityConstraints(constraints,
									static_pointer_cast<const Type>((*it).first),
									static_pointer_cast<const Type>((*it).second)
							);
						}
					}

					break;
				}

				case NT_StructType:
				case NT_UnionType:
				{
					// names and sub-types have to be checked
					auto entriesA = static_pointer_cast<const NamedCompositeType>(typeA)->getEntries();
					auto entriesB = static_pointer_cast<const NamedCompositeType>(typeB)->getEntries();

					// check equality of names and types
					// check number of entries
					if (entriesA.size() != entriesB.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// check all child nodes
					for (auto it = make_paired_iterator(entriesA.begin(), entriesB.begin());
							it != make_paired_iterator(entriesA.end(), entriesB.end()); ++it) {

						// ensure identifiers are identical (those cannot be variable)
						auto entryA = (*it).first;
						auto entryB = (*it).second;
						if (*entryA->getName() != *entryB->getName()) {
							// unsatisfiable
							constraints.makeUnsatisfiable();
							return;
						}

						// add equality constraints recursively
						addEqualityConstraints(constraints, entryA->getType(), entryB->getType());
					}


					break;
				}

				case NT_RecType:
				{
					// TODO: implement RecType pattern matching
					if (*typeA != *typeB) {
						LOG(WARNING) << "Yet unhandled recursive type encountered while resolving subtype constraints:"
									 << " Parameter Type: " << typeA << std::endl
									 << "  Argument Type: " << typeB << std::endl
									 << " => the argument will be considered equal!!!";
//						assert(false && "Sorry - not implemented!");
						constraints.makeUnsatisfiable();
					}
					break;
				}
				default:
					assert(false && "Missed a kind of type!");
			}
		}

		void addTypeConstraints(SubTypeConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction) {

			// check constraint status
			if (!constraints.isSatisfiable()) {
				return;
			}

			// check whether relation is already satisfied
			if ((direction == Direction::SUB_TYPE && isSubTypeOf(paramType, argType)) ||
					(direction == Direction::SUPER_TYPE && isSubTypeOf(argType, paramType))) {
				return;
			}

			// extract node types
			NodeType nodeTypeA = paramType->getNodeType();
			NodeType nodeTypeB = argType->getNodeType();

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// add corresponding constraint to this variable
				if (direction == Direction::SUB_TYPE) {
					constraints.addSubtypeConstraint(paramType, argType);
				} else {
					constraints.addSubtypeConstraint(argType, paramType);
				}
				return;
			}


			// ---------------------------------- Arrays / Vectors Type ---------------------------------------------

			// check direction and array/vector relationship
			if ((direction == Direction::SUB_TYPE && nodeTypeA == NT_VectorType && nodeTypeB == NT_ArrayType) ||
					(direction == Direction::SUPER_TYPE && nodeTypeA == NT_ArrayType && nodeTypeB == NT_VectorType)) {

				bool isSubType = (direction == Direction::SUB_TYPE);
				const VectorTypePtr& vector = static_pointer_cast<const VectorType>((isSubType)?paramType:argType);
				const ArrayTypePtr& array = static_pointer_cast<const ArrayType>((isSubType)?argType:paramType);

				// make sure the dimension of the array is 1
				auto dim = array->getDimension();
				ConcreteIntTypeParamPtr one = ConcreteIntTypeParam::get(array->getNodeManager(), 1);
				switch(dim->getNodeType()) {
					case NT_VariableIntTypeParam:
						// this is fine ... no restrictions required
						if (!isSubType) constraints.fixIntTypeParameter(static_pointer_cast<const VariableIntTypeParam>(dim), one);
						break;
					case NT_ConcreteIntTypeParam:
					case NT_InfiniteIntTypeParam:
						if (*dim != *one) {
							// only dimension 1 is allowed when passing a vector
							constraints.makeUnsatisfiable();
							return;
						}
						break;
					default:
						assert(false && "Unknown int-type parameter encountered!");
				}

				// also make sure element types are equivalent
				addEqualityConstraints(constraints, array->getElementType(), vector->getElementType());
				return;

			}

			// --------------------------------------------- Function Type ---------------------------------------------

			// check function type
			if (nodeTypeA == NT_FunctionType) {
				// argument has to be a function as well
				if (nodeTypeB != NT_FunctionType) {
					// => no match possible
					constraints.makeUnsatisfiable();
					return;
				}

				auto funParamType = static_pointer_cast<const FunctionType>(paramType);
				auto funArgType = static_pointer_cast<const FunctionType>(argType);

				// check kind of function parameter
				if (funParamType->isPlain() && !funArgType->isPlain()) {
					// cannot pass a closure function to a plain function parameter
					constraints.makeUnsatisfiable();
				}

				// check number of arguments
				const TypeList& paramParams = funParamType->getParameterTypes()->getTypes();
				const TypeList& argParams = funArgType->getParameterTypes()->getTypes();
				if (paramParams.size() != argParams.size()) {
					// different number of arguments => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// add constraints on arguments
				auto begin = make_paired_iterator(paramParams.begin(), argParams.begin());
				auto end = make_paired_iterator(paramParams.end(), argParams.end());
				for (auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, it->first, it->second, inverse(direction));
				}

				// ... and the return type
				addTypeConstraints(constraints, funParamType->getReturnType(), funArgType->getReturnType(), direction);
				return;
			}

			// --------------------------------------------- Tuple Types ---------------------------------------------

			// if both are generic types => add sub-type constraint
			if (nodeTypeA == nodeTypeB && nodeTypeA == NT_TupleType) {
				const TypeList& paramList = static_pointer_cast<const TupleType>(paramType)->getElementTypes();
				const TypeList& argumentList = static_pointer_cast<const TupleType>(argType)->getElementTypes();

				// check length
				if (paramList.size() != argumentList.size()) {
					constraints.makeUnsatisfiable();
					return;
				}

				// ensure sub-type relation
				auto begin = make_paired_iterator(paramList.begin(), argumentList.begin());
				auto end = make_paired_iterator(paramList.end(), argumentList.end());
				for (auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, it->first, it->second, direction);
				}
				return;
			}

			// --------------------------------------------- Generic Types ---------------------------------------------

			// if both are generic types => add sub-type constraint
			if (nodeTypeA == nodeTypeB && nodeTypeA == NT_GenericType) {
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
			auto collector = makeLambdaVisitor([&](const NodePtr& cur){
				NodeType kind = cur->getNodeType();
				switch(kind) {
				case NT_TypeVariable: {
					// check whether already encountered
					const TypeVariablePtr& var = static_pointer_cast<const TypeVariable>(cur);
					if (argumentMapping.containsMappingFor(var)) {
						break;
					}
					// add a new constant
					const TypePtr substitute = GenericType::get(manager, string("_const_") + var->getVarName()->toString());
					argumentMapping.addMapping(var, substitute);
					break;
				}
				case NT_VariableIntTypeParam: {
					// check whether already encountered
					const VariableIntTypeParamPtr& var = static_pointer_cast<const VariableIntTypeParam>(cur);
					if (argumentMapping.containsMappingFor(var)) {
						break;
					}
					// add a new constant
					// NOTE: the generation of constants is not safe in all cases - it is just assumed
					// that no constants > 1.000.000.000 are used
					auto substitute = ConcreteIntTypeParam::get(manager, 1000000000 + var->getSymbol());
					argumentMapping.addMapping(var, substitute);
					break;
				}
				case NT_RecType:
				case NT_FunctionType: {
					// do not consider function types and recursive types (variables inside are bound)
					break;
				}
				default: {
					// decent recursively
					for_each(cur->getChildList(), [&](const NodePtr& cur) {
						rec->visit(cur);
					});
				}
				}
			}, true);
			rec = &collector;

			// finally, collect and substitute variables
			collector.visit(TupleType::get(manager, arguments));

			return argumentMapping;
		}
	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypeList& parameter, const TypeList& arguments) {

		const bool debug = false;

		// check length of parameter and arguments
		if (parameter.size() != arguments.size()) {
			return 0;
		}

		// use private node manager for computing results
		NodeManager internalManager;

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

		if (debug) std::cout << " Parameter: " << parameter << std::endl;
		if (debug) std::cout << "   Renamed: " << renamedParameter << std::endl;

		// 2) convert arguments (individually)
		//		- fix free variables consistently => replace with constants
		//		- rename unbounded variables - individually per parameter

		// collects the mapping of free variables to constant replacements (to protect them
		// from being substituted during some unification process)
		TypeMapping&& argumentMapping = substituteFreeVariablesWithConstants(internalManager, arguments);

		if (debug) std::cout << " Arguments: " << arguments << std::endl;
		if (debug) std::cout << "   Mapping: " << argumentMapping << std::endl;

		// apply renaming to arguments
		TypeList renamedArguments = arguments;
		vector<TypeMapping> argumentRenaming;
		for (std::size_t i = 0; i < renamedArguments.size(); ++i) {
			TypePtr& cur = renamedArguments[i];

			// first: apply bound variable substitution
			cur = argumentMapping.applyForward(internalManager, cur);

			// second: apply variable renaming
			TypeMapping mapping = renamer.mapVariables(internalManager, cur);
			if (!mapping.empty()) argumentRenaming.push_back(mapping);
			cur = mapping.applyForward(internalManager, cur);
		}

		if (debug) std::cout << " Renamed Arguments: " << renamedArguments << std::endl;


		// ---------------------------------- Assembling Constraints -----------------------------------------

		// collect constraints on the type variables used within the parameter types
		SubTypeConstraints constraints;

		// collect constraints
		auto begin = make_paired_iterator(renamedParameter.begin(), renamedArguments.begin());
		auto end = make_paired_iterator(renamedParameter.end(), renamedArguments.end());
		for (auto it = begin; constraints.isSatisfiable() && it!=end; ++it) {
			// add constraints to ensure current parameter is a super-type of the arguments
			addTypeConstraints(constraints, it->first, it->second, Direction::SUPER_TYPE);
		}


		if (debug) std::cout << " Constraints: " << constraints << std::endl;

		// ---------------------------------- Solve Constraints -----------------------------------------

		// solve constraints to obtain results
		SubstitutionOpt&& res = constraints.solve(internalManager);
		if (!res) {
			// if unsolvable => return this information
			if (debug) std::cout << " Terminated with no solution!" << std::endl << std::endl;
			return res;
		}

		// ----------------------------------- Revert Renaming ------------------------------------------
		// (and produce a result within the correct node manager - internal manager will be thrown away)

		// check for empty solution (to avoid unnecessary operations
		if (res->empty()) {
			if (debug) std::cout << " Terminated with: " << *res << std::endl << std::endl;
			return res;
		}

		// reverse variables from previously selected constant replacements (and bring back to correct node manager)
		Substitution restored;
		for (auto it = res->getMapping().begin(); it != res->getMapping().end(); ++it) {
			TypeVariablePtr var = static_pointer_cast<const TypeVariable>(parameterMapping.applyBackward(manager, it->first));
			TypePtr substitute = argumentMapping.applyBackward(manager, it->second);

			// also apply argument renaming backwards ..
			for(auto it2 = argumentRenaming.begin(); it2 != argumentRenaming.end(); ++it2) {
				substitute = it2->applyBackward(manager, it->second);
			}

			restored.addMapping(manager.get(var), manager.get(substitute));
		}
		for (auto it = res->getIntTypeParamMapping().begin(); it != res->getIntTypeParamMapping().end(); ++it) {
			VariableIntTypeParamPtr var = static_pointer_cast<const VariableIntTypeParam>(parameterMapping.applyBackward(manager, it->first));
			IntTypeParamPtr substitute = argumentMapping.applyBackward(manager, it->second);

			// also apply argument renaming backwards ..
			for(auto it2 = argumentRenaming.begin(); it2 != argumentRenaming.end(); ++it2) {
				substitute = it2->applyBackward(manager, it->second);
			}

			restored.addMapping(manager.get(var), manager.get(substitute));
		}
		if (debug) std::cout << " Terminated with: " << restored << std::endl << std::endl;
		return restored;
	}

	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypePtr& parameter, const TypePtr& argument) {
		return getTypeVariableInstantiation(manager, toVector(parameter), toVector(argument));
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

		public :

			VariableInstantionInfo() {}

			virtual const utils::AnnotationKeyPtr getKey() const {
				return &KEY;
			}

			virtual const std::string& getAnnotationName() const {
				return NAME;
			}

			const SubstitutionOpt* get(const TypeList& args) const {
				auto pos = substitutions.find(args);
				if (pos != substitutions.end()) {
					return &(pos->second);
				}
				return 0;
			}

			void add(const TypeList& args, const SubstitutionOpt& res) {
				substitutions.insert(std::make_pair(args, res));
			}

			virtual void clone(const NodeAnnotationPtr& ptr, const NodePtr& clone) const {

				assert(!clone->hasAnnotation(KEY) && "Clone should not have this annotation already!");

				// copy the nodes reference by this annotation to the new manager
				NodeManager& manager = clone->getNodeManager();

				// attach annotation
				std::shared_ptr<VariableInstantionInfo> copy = std::make_shared<VariableInstantionInfo>();
				SubstitutionMap& map = copy->substitutions;

				// insert copies of the current map
				for_each(substitutions, [&](const SubstitutionMap::value_type& cur) {
					map.insert(std::make_pair(manager.getAll(cur.first), copyTo(manager, cur.second)));
				});

				// finally, add new annotation to new version of node
				clone->addAnnotation(copy);
			}

			static const SubstitutionOpt* getFromAnnotation(const FunctionTypePtr& function, const TypeList& arguments) {
				// try loading annotation
				if (auto res = function->getAnnotation(VariableInstantionInfo::KEY)) {
					return res->get(arguments);
				}

				// no such annotation present
				return 0;
			}

			static void addToAnnotation(const FunctionTypePtr& function, const TypeList& arguments, const SubstitutionOpt& substitution) {
				auto res = function->getAnnotation(VariableInstantionInfo::KEY);
				if (!res) {
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


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const FunctionTypePtr& function, const TypeList& arguments) {

		// check annotations
		TypeList localArgs = function->getNodeManager().getAll(arguments);
		if (auto res = VariableInstantionInfo::getFromAnnotation(function, localArgs)) {
			return copyTo(manager, *res);
		}

		// use deduction mechanism
		SubstitutionOpt res = getTypeVariableInstantiation(manager, function->getParameterTypes()->getTypes(), arguments);

		// attack substitution
		VariableInstantionInfo::addToAnnotation(function, localArgs, copyTo(function->getNodeManager(), res));
		return res;
	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const CallExprPtr& call) {

		// check for null
		if (!call) {
			return 0;
		}


		// derive substitution

		// get argument types
		TypeList argTypes;
		::transform(call->getArguments(), back_inserter(argTypes), [](const ExpressionPtr& cur) {
			return cur->getType();
		});

		// get function parameter types
		const TypePtr& funType = call->getFunctionExpr()->getType();
		if (funType->getNodeType() != NT_FunctionType) {
			return 0;
		}

		// compute type variable instantiation
		SubstitutionOpt res = getTypeVariableInstantiation(manager, static_pointer_cast<const FunctionType>(funType), argTypes);

		// done
		return res;
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

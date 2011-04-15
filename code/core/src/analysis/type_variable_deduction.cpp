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

#include "insieme/core/analysis/type_variable_deduction.h"

#include <iterator>

#include "insieme/core/annotation.h"
#include "insieme/core/expressions.h"

namespace insieme {
namespace core {
namespace analysis {


	TypePtr TypeVariableConstraints::VariableConstraint::solve() const {
		TypePtr res;

		// start by checking whether there is a equal type
		if (!equalTypes.empty()) {

			// get first type to be considered
			res = *equalTypes.begin();


			// check against all constraints
			bool valid = true;

			// check against equality constraints
			for_each(equalTypes, [&](const TypePtr& cur) {
				// bidirectional ...
				valid = valid && isSubTypeOf(res, cur) && isSubTypeOf(cur, res);
			});

			if (valid) for_each(superTypes, [&](const TypePtr& cur) {
				// check super-type constraint
				valid = valid && isSubTypeOf(cur, res);
			});

			if (valid) for_each(subTypes, [&](const TypePtr& cur) {
				// check super-type constraint
				valid = valid && isSubTypeOf(res, cur);
			});

			// if all constraints are satisfied ...
			if (valid) {
				return res;
			}

			// => unsatisfiable
			return 0;
		}


		// no equality constraints defined
		// => a type has to be inferred

		// try infer type from sub-type constraints
		if (!subTypes.empty()) {
			// compute biggest common sub-type
			res = getBiggestCommonSubType(subTypes);

			// check against super-type constraints
			bool valid = res;
			for_each(superTypes, [&](const TypePtr& cur) {
				// check super-type constraint
				valid = valid && isSubTypeOf(cur, res);
			});

			// if all constraints are satisfied ...
			if (valid) {
				return res;
			}

			// => unsatisfiable
			return 0;
		}

		// infer type from super-type constraints
		if (!superTypes.empty()) {
			// compute smallest common super type
			res = getSmallestCommonSuperType(superTypes);
		}

		// done
		return res;

	}


	std::ostream& TypeVariableConstraints::VariableConstraint::printTo(std::ostream& out) const {
		return out << "[ x < " << superTypes << " && x = " << equalTypes << " && x > " << subTypes << " ]";
	}


	SubstitutionOpt TypeVariableConstraints::solve() const {

		// quick check to exclude unsatisfiability
		if (!isSatisfiable()) {
			return 0;
		}

		// create result
		Substitution res;

		// solve individual constraints ...
		for(auto it = constraints.begin(); it != constraints.end(); ++it) {

			const TypeVariablePtr& var = it->first;
			const TypePtr substitute = it->second.solve();

			if (substitute) {
				res.addMapping(var, substitute);
			} else {
				// => unsatisfiable
				return 0;
			}
		}

		// add int type parameter substitutions
		for (auto it = intTypeParameter.begin(); it != intTypeParameter.end(); ++it) {
			res.addMapping(it->first, it->second);
		}

		// done
		return boost::optional<Substitution>(res);
	}


	std::ostream& TypeVariableConstraints::printTo(std::ostream& out) const {
		return out << "[" << constraints << "/" << intTypeParameter << "]";
	}


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
		void addEqualityConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType);

		/**
		 * Adds additional constraints to the given constraint collection such that the type variables used within the
		 * parameter type are limited to values representing super- / sub-types of the given argument type.
		 *
		 *  @param constraints the set of constraints to be extended
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 *  @param direction the direction to be ensured - sub- or supertype
		 */
		void addTypeConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction);


		// -------------------------------------------------------- Implementation ----------------------------------------------


		void addEqualityConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType) {

			// check constraint status
			if (!constraints.isSatisfiable()) {
				return;
			}

			// extract node types
			NodeType nodeTypeA = paramType->getNodeType();
			NodeType nodeTypeB = argType->getNodeType();

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// the substitution for the variable has to be equivalent to the argument type
				constraints.addEqualsConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
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

				// first handle those types equipped with int type parameters
				case NT_GenericType:
				case NT_RefType:
				case NT_ArrayType:
				case NT_VectorType:
				case NT_ChannelType:
				{
					// check name of generic type ... if not matching => wrong
					auto genParamType = static_pointer_cast<const GenericType>(paramType);
					auto genArgType = static_pointer_cast<const GenericType>(argType);
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
						if (paramA->getNodeType() == NT_VariableIntTypeParam) {
							// obtain variable
							VariableIntTypeParamPtr var = static_pointer_cast<const VariableIntTypeParam>(paramA);

							// check if the parameter has already been set
							if (IntTypeParamPtr value = constraints.getIntTypeParamValue(var)) {
								if (*value == *paramB) {
									// everything is fine
									continue;
								} else {
									// int type parameter needs to be instantiated twice, in different ways => unsatisfiable
									constraints.makeUnsatisfiable();
									return;
								}
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

					// ... and fall-through to check the child types
				}

				case NT_TupleType:
				case NT_FunctionType:
				{
					// the number of sub-types must match
					auto paramChildren = paramType->getChildList();
					auto argChildren = argType->getChildList();

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
					auto entriesA = static_pointer_cast<const NamedCompositeType>(paramType)->getEntries();
					auto entriesB = static_pointer_cast<const NamedCompositeType>(argType)->getEntries();

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
						if (*entryA.first != *entryB.first) {
							// unsatisfiable
							constraints.makeUnsatisfiable();
							return;
						}

						// add equality constraints recursively
						addEqualityConstraints(constraints, entryA.second, entryB.second);
					}


					break;
				}

				case NT_RecType:
				{
					// TODO: implement RecType pattern matching
					assert(false && "Sorry - not implemented!");
					break;
				}
				default:
					assert(false && "Missed a kind of type!");
			}
		}

		void addTypeConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction) {

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
					constraints.addSubtypeConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
				} else {
					constraints.addSupertypeConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
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
					default:
						assert(false && "Unknown int-type parameter encountered!");
				}

				// also make sure element types are equivalent
				if (isSubType) {
					addEqualityConstraints(constraints, array->getElementType(), vector->getElementType());
				} else {
					addEqualityConstraints(constraints, vector->getElementType(), array->getElementType());
				}
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

				// check number of arguments
				auto paramArgs = funParamType->getArgumentTypes();
				auto argArgs = funArgType->getArgumentTypes();
				if (paramArgs.size() != argArgs.size()) {
					// different number of arguments => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// add constraints on arguments
				auto begin = make_paired_iterator(paramArgs.begin(), argArgs.begin());
				auto end = make_paired_iterator(paramArgs.end(), argArgs.end());
				for (auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, it->first, it->second, inverse(direction));
				}

				// ... and the return type
				addTypeConstraints(constraints, funParamType->getReturnType(), funArgType->getReturnType(), direction);
				return;
			}

			// check rest => has to be equivalent (e.g. ref, channels, ...)
			addEqualityConstraints(constraints, paramType, argType);
		}

	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypeList& parameter, const TypeList& arguments) {

		// check length of parameter and arguments
		if (parameter.size() != arguments.size()) {
			return 0;
		}

		// collect constraints on the type variables used within the parameter types
		TypeVariableConstraints constraints;

		// collect constraints
		auto begin = make_paired_iterator(parameter.begin(), arguments.begin());
		auto end = make_paired_iterator(parameter.end(), arguments.end());
		for (auto it = begin; constraints.isSatisfiable() && it!=end; ++it) {
			// add constraints to ensure current parameter is a super-type of the arguments
			addTypeConstraints(constraints, it->first, it->second, Direction::SUPER_TYPE);
		}

		// std::cout << "Constraints: " << constraints << std::endl;

		// solve constraints to obtain results
		return constraints.solve();
	}



	namespace {

		// The kind of annotations attached to call nodes to cache type variable substitutions
		class TypeVariableInstantionInfo : public Annotation {

		public:

			// The key used to attack instantiation results to call nodes
			static StringKey<TypeVariableInstantionInfo> KEY;

		private:

			// the represented value
			SubstitutionOpt substitution;

		public :

			TypeVariableInstantionInfo(const SubstitutionOpt& substitution) : substitution(substitution) {}

			virtual const AnnotationKey* getKey() const {
				return &KEY;
			}

			virtual const std::string getAnnotationName() const {
				return "TypeVariableInstantionInfo";
			}

			const SubstitutionOpt& getSubstitution() const {
				return substitution;
			}

		};

		StringKey<TypeVariableInstantionInfo> TypeVariableInstantionInfo::KEY = StringKey<TypeVariableInstantionInfo>("TYPE_VARIABLE_INSTANTIATION_INFO");
	}

	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const CallExprPtr& call) {

		// check for null
		if (!call) {
			return 0;
		}

		// check annotations
		if (auto data = call->getAnnotation(TypeVariableInstantionInfo::KEY)) {
			return data->getSubstitution();
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
		const TypeList& paramTypes = static_pointer_cast<const FunctionType>(funType)->getArgumentTypes();

		// compute type variable instantiation
		SubstitutionOpt res = getTypeVariableInstantiation(manager, paramTypes, argTypes);

		// attack substitution
		call->addAnnotation(std::make_shared<TypeVariableInstantionInfo>(res));

		// done
		return res;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

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

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace backend {
namespace c_ast {

CNodeManager::~CNodeManager() {
	// free all managed nodes
	for_each(nodes, [](const NodePtr cur) {
		delete &*cur;
	});
	for_each(identMap, [](const std::pair<string, IdentifierPtr>& cur) {
		delete &*cur.second;
	});
}


IdentifierPtr CNodeManager::create(const string& name) {
	auto pos = identMap.find(name);
	if(pos != identMap.end()) {
		return pos->second;
	}
	
	IdentifierPtr res = new Identifier(name);
	res->setManager(this);
	identMap.insert(std::make_pair(name, res));
	return res;
}

/**
 * Defining the equality ID generator.
 */
utils::SimpleIDGenerator<Node::EqualityID> Node::equalityClassIDGenerator;


bool Node::operator==(const Node& other) const {
	// test for identity
	if(this == &other) {
		return true;
	}
	
	// check node type
	if(type != other.type) {
		return false;
	}
	
	// check equality ID
	if(equalityID != 0 && other.equalityID != 0 && equalityID == other.equalityID) {
		// just compare equality IDs (having different IDs does not mean it is different)
		return true;
	}
	
	// use virtual equals method
	bool res = equals(other);
	
	// infect both nodes with a new ID
	if(res) {
		// update equality IDs - both should have the same id
		if(equalityID == 0 && other.equalityID == 0) {
			// non is set yet => pick a new ID and use for both
			equalityID = equalityClassIDGenerator.getNext();
			other.equalityID = equalityID;
		}
		else if(equalityID == 0) {
			// other.equalityID != 0 ... update local ID with other ID
			equalityID = other.equalityID;
		}
		else if(other.equalityID == 0) {
			// equality ID != 0 ... update other ID
			other.equalityID = equalityID;
		}
		else {
			// both are != 0
			assert_true(equalityID != 0 && other.equalityID != 0) << "Equality IDs should be != 0";
			
			// pick smaller ID for both
			if(equalityID < other.equalityID) {
				other.equalityID = equalityID;
			}
			else {
				equalityID = other.equalityID;
			}
		}
	}
	
	// return the comparison result.
	return res;
}

// ----------- Particular Nodes --------------

/**
 * This utility struct definition defines a predicate comparing two pairs
 * of pointers. The pairs are equivalent if the included elements point to equal
 * objects.
 *
 * @tparam T the first pointer type within the pair
 * @tparam P the second pointer type within the pair
 */
template<typename T, typename P>
struct equal_pointer_pair : public std::binary_function<const std::pair<T,P>&, const std::pair<T,P>&, bool> {
	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * pointer types.
	 *
	 * @param x a reference to the first pair
	 * @param y a reference to the second pair
	 */
	bool operator()(const std::pair<T,P>& x, const std::pair<T,P>& y) const {
		return *x.first == *y.first && *x.second == *y.second;
	}
};


bool Identifier::equals(const Node& other) const {
	assert(dynamic_cast<const Identifier*>(&other));
	return name == static_cast<const Identifier&>(other).name;
}

bool Comment::equals(const Node& other) const {
	assert(dynamic_cast<const Comment*>(&other));
	return comment == static_cast<const Comment&>(other).comment;
}

bool OpaqueCode::equals(const Node& other) const {
	assert(dynamic_cast<const OpaqueCode*>(&other));
	return code == static_cast<const OpaqueCode&>(other).code;
}

bool PrimitiveType::equals(const Node& other) const {
	assert(dynamic_cast<const PrimitiveType*>(&other));
	return type == static_cast<const PrimitiveType&>(other).type;
}

bool ModifiedType::equals(const Node& node) const {
	assert(dynamic_cast<const ModifiedType*>(&node));
	auto other = static_cast<const ModifiedType&>(node);
	return mods == other.mods && *type == *other.type;
}

bool NamedType::equals(const Node& node) const {
	assert(dynamic_cast<const NamedType*>(&node));
	auto other = static_cast<const NamedType&>(node);
	return *name == *other.name && ::equals(parameters, other.parameters, equal_target<NodePtr>());
}

bool PointerType::equals(const Node& type) const {
	assert(dynamic_cast<const PointerType*>(&type));
	const auto& other = static_cast<const PointerType&>(type);
	return isConst == other.isConst && *elementType == *other.elementType;
}

bool ReferenceType::equals(const Node& node) const {
	assert(dynamic_cast<const ReferenceType*>(&node));
	auto other = static_cast<const ReferenceType&>(node);
	return isConst == other.isConst && *elementType == *other.elementType;
}

bool VectorType::equals(const Node& node) const {
	assert(dynamic_cast<const VectorType*>(&node));
	auto other = static_cast<const VectorType&>(node);
	return *elementType == *other.elementType && ((!size && !other.size) || (size && other.size && *size == *other.size));
}

bool NamedCompositeType::equals(const Node& node) const {
	assert(dynamic_cast<const NamedCompositeType*>(&node));
	auto other = static_cast<const NamedCompositeType&>(node);
	return *name == *other.name &&
	       ::equals(elements, other.elements, equal_target<VariablePtr>()) &&
	       ::equals(ctors, other.ctors, equal_target<ConstructorPrototypePtr>()) &&
	       ((!dtor && !other.dtor) || (dtor && other.dtor && *dtor == *other.dtor)) &&
	       ::equals(members, other.members, equal_target<MemberFunctionPrototypePtr>());
}

bool Parent::equals(const Node& node) const {
	assert(dynamic_cast<const Parent*>(&node));
	auto other = static_cast<const Parent&>(node);
	return isVirtual == other.isVirtual && *parent == *other.parent;
}

bool ComplexType::equals(const Node& node) const {
	assert(dynamic_cast<const ComplexType*>(&node));
	auto other = static_cast<const ComplexType&>(node);
	return *other.elementType == *elementType;
}

bool StructType::equals(const Node& node) const {
	assert(dynamic_cast<const StructType*>(&node));
	auto other = static_cast<const StructType&>(node);
	return NamedCompositeType::equals(other) &&
	       ::equals(parents, other.parents, equal_target<ParentPtr>());
}

bool FunctionType::equals(const Node& node) const {
	assert(dynamic_cast<const FunctionType*>(&node));
	auto other = static_cast<const FunctionType&>(node);
	return *returnType == *other.returnType && *classType == *other.classType && ::equals(parameterTypes, other.parameterTypes, equal_target<TypePtr>());
}

bool VarArgsType::equals(const Node& node) const {
	assert(dynamic_cast<const VarArgsType*>(&node));
	return true;
}

bool AttributedType::equals(const Node& node) const {
	assert(dynamic_cast<const AttributedType*>(&node));
	auto other = static_cast<const AttributedType&>(node);
	return attribute == other.attribute && *type == *other.type;
}

bool EnumType::equals(const Node& node) const {
	assert(dynamic_cast<const EnumType*>(&node));
	auto other = static_cast<const EnumType&>(node);
	return ((name == other.name) && (annotation == other.annotation));
}

bool MemberFieldPointer::equals(const Node& node) const {
	assert(dynamic_cast<const MemberFieldPointer*>(&node));
	auto other = static_cast<const MemberFieldPointer&>(node);
	return ((*parentType == *other.parentType) && (*type == *other.type));
}

VarDecl::VarDecl(const vector<pair<VariablePtr,ExpressionPtr>>& initList)
	: Statement(NT_VarDecl), isStatic(false), varInit(initList) {
	assert(!varInit.empty() && all(varInit, [&](const pair<VariablePtr, ExpressionPtr>& cur)->bool {
		return cur.first->type == varInit[0].first->type;
	}));
};

bool VarDecl::equals(const Node& node) const {
	assert(dynamic_cast<const VarDecl*>(&node));
	auto other = static_cast<const VarDecl&>(node);
	return isStatic == other.isStatic && ::equals(varInit, other.varInit, equal_pointer_pair<VariablePtr, ExpressionPtr>());
}

bool Compound::equals(const Node& node) const {
	assert(dynamic_cast<const Compound*>(&node));
	auto other = static_cast<const Compound&>(node);
	return ::equals(statements, other.statements, equal_target<NodePtr>());
}

bool If::equals(const Node& node) const {
	assert(dynamic_cast<const If*>(&node));
	auto other = static_cast<const If&>(node);
	return *condition==*other.condition && *thenStmt == *other.thenStmt && *elseStmt == *other.elseStmt;
}

bool Switch::equals(const Node& node) const {
	assert(dynamic_cast<const Switch*>(&node));
	auto other = static_cast<const Switch&>(node);
	return *value == *other.value && *defaultBranch==*other.defaultBranch &&
	       ::equals(cases, other.cases, equal_pointer_pair<ExpressionPtr, StatementPtr>());
}

bool For::equals(const Node& node) const {
	assert(dynamic_cast<const For*>(&node));
	auto other = static_cast<const For&>(node);
	return *init==*other.init && *check == *other.check && *step== *other.step && *body == *other.body;
}

bool While::equals(const Node& node) const {
	assert(dynamic_cast<const While*>(&node));
	auto other = static_cast<const While&>(node);
	return *condition==*other.condition && *body == *other.body;
}

bool TryCatch::Clause::operator==(const TryCatch::Clause& other) const {
	return equalTarget(var, other.var) && equalTarget(body, other.body);
}

bool TryCatch::equals(const Node& node) const {
	assert(dynamic_cast<const TryCatch*>(&node));
	auto other = static_cast<const TryCatch&>(node);
	return *body == *other.body && ::equals(clauses, other.clauses);
}

bool Return::equals(const Node& node) const {
	assert(dynamic_cast<const Return*>(&node));
	auto other = static_cast<const Return&>(node);
	return equalTarget(value, other.value);
}

bool Throw::equals(const Node& node) const {
	assert(dynamic_cast<const Throw*>(&node));
	auto other = static_cast<const Throw&>(node);
	return equalTarget(value, other.value);
}

bool Goto::equals(const Node& node) const {
	assert(dynamic_cast<const Goto*>(&node));
	auto other = static_cast<const Goto&>(node);
	return value==other.value;
}

bool Label::equals(const Node& node) const {
	assert(dynamic_cast<const Label*>(&node));
	auto other = static_cast<const Label&>(node);
	return value==other.value;
}

bool Literal::equals(const Node& node) const {
	assert(dynamic_cast<const Literal*>(&node));
	auto other = static_cast<const Literal&>(node);
	return value==other.value;
}

bool Variable::equals(const Node& node) const {
	assert(dynamic_cast<const Variable*>(&node));
	auto other = static_cast<const Variable&>(node);
	return *type==*other.type && *name == *other.name;
}

bool Initializer::equals(const Node& node) const {
	assert(dynamic_cast<const Initializer*>(&node));
	auto other = static_cast<const Initializer&>(node);
	return explicitType==other.explicitType && *type==*other.type && ::equals(values, other.values, equal_target<NodePtr>());
}

bool DesignatedInitializer::equals(const Node& node) const {
	assert(dynamic_cast<const DesignatedInitializer*>(&node));
	auto other = static_cast<const DesignatedInitializer&>(node);
	return *type==*other.type && *member == *other.member && *value == *other.value;
}

bool ArrayInit::equals(const Node& node) const {
	assert(dynamic_cast<const ArrayInit*>(&node));
	auto other = static_cast<const ArrayInit&>(node);
	return *type==*other.type && ::equals(size, other.size, equal_target<NodePtr>());
}

bool VectorInit::equals(const Node& node) const {
	assert(dynamic_cast<const VectorInit*>(&node));
	auto other = static_cast<const VectorInit&>(node);
	return ::equals(values, other.values, equal_target<NodePtr>());
}

bool OCLVectorInit::equals(const Node& node) const {
	assert(dynamic_cast<const OCLVectorInit*>(&node));
	auto other = static_cast<const OCLVectorInit&>(node);
	return *type==*other.type && ::equals(values, other.values, equal_target<NodePtr>());
}

bool UnaryOperation::equals(const Node& node) const {
	assert(dynamic_cast<const UnaryOperation*>(&node));
	auto other = static_cast<const UnaryOperation&>(node);
	return operation==other.operation && *operand==*other.operand;
}

bool BinaryOperation::equals(const Node& node) const {
	assert(dynamic_cast<const BinaryOperation*>(&node));
	auto other = static_cast<const BinaryOperation&>(node);
	return operation==other.operation && *operandA==*other.operandA && *operandB==*other.operandB;
}

bool TernaryOperation::equals(const Node& node) const {
	assert(dynamic_cast<const TernaryOperation*>(&node));
	auto other = static_cast<const TernaryOperation&>(node);
	return operation==other.operation && *operandA==*other.operandA && *operandB==*other.operandB && *operandC==*other.operandC;
}

bool Call::equals(const Node& node) const {
	assert(dynamic_cast<const Call*>(&node));
	auto other = static_cast<const Call&>(node);
	return *function==*other.function && ::equals(arguments, other.arguments, equal_target<NodePtr>());
}

bool MemberCall::equals(const Node& node) const {
	assert(dynamic_cast<const MemberCall*>(&node));
	auto other = static_cast<const MemberCall&>(node);
	return *memberFun==*other.memberFun && *object == *other.object && ::equals(arguments, other.arguments, equal_target<NodePtr>());
}

bool ConstructorCall::equals(const Node& node) const {
	assert(dynamic_cast<const ConstructorCall*>(&node));
	auto other = static_cast<const ConstructorCall&>(node);
	return *classType == *other.classType &&
	       equalTarget(location, other.location) &&
	       ::equals(arguments, other.arguments, equal_target<NodePtr>());
}

bool DestructorCall::equals(const Node& node) const {
	assert(dynamic_cast<const DestructorCall*>(&node));
	auto other = static_cast<const DestructorCall&>(node);
	return *classType == *other.classType && *location == *other.location && isVirtual == other.isVirtual;
}

bool Parentheses::equals(const Node& node) const {
	assert(dynamic_cast<const Parentheses*>(&node));
	auto other = static_cast<const Parentheses&>(node);
	return *expression==*other.expression;
}

bool OpaqueExpr::equals(const Node& node) const {
	assert(dynamic_cast<const OpaqueExpr*>(&node));
	auto other = static_cast<const OpaqueExpr&>(node);
	return value==other.value;
}

bool StmtExpr::equals(const Node& node) const {
	assert(dynamic_cast<const StmtExpr*>(&node));
	auto other = static_cast<const StmtExpr&>(node);
	return stmt==other.stmt;
}

bool TypeDeclaration::equals(const Node& node) const {
	assert(dynamic_cast<const TypeDeclaration*>(&node));
	auto other = static_cast<const TypeDeclaration&>(node);
	return *type==*other.type;
}

bool FunctionPrototype::equals(const Node& node) const {
	assert(dynamic_cast<const FunctionPrototype*>(&node));
	auto other = static_cast<const FunctionPrototype&>(node);
	return *function==*other.function;
}

bool GlobalVarDecl::equals(const Node& node) const {
	assert(dynamic_cast<const GlobalVarDecl*>(&node));
	auto other = static_cast<const GlobalVarDecl&>(node);
	return *type==*other.type && name==other.name && external == other.external && equalTarget(init, other.init);
}

bool ConstructorPrototype::equals(const Node& node) const {
	assert(dynamic_cast<const ConstructorPrototype*>(&node));
	auto other = static_cast<const ConstructorPrototype&>(node);
	return *ctor == *other.ctor;
}

bool DestructorPrototype::equals(const Node& node) const {
	assert(dynamic_cast<const DestructorPrototype*>(&node));
	auto other = static_cast<const DestructorPrototype&>(node);
	return isVirtual == other.isVirtual && *dtor == *other.dtor;
}

bool MemberFunctionPrototype::equals(const Node& node) const {
	assert(dynamic_cast<const MemberFunctionPrototype*>(&node));
	auto other = static_cast<const MemberFunctionPrototype&>(node);
	return isVirtual == other.isVirtual && pureVirtual == other.pureVirtual && ((!fun && !other.fun) || *fun == *other.fun);
}

bool TypeDefinition::equals(const Node& node) const {
	assert(dynamic_cast<const TypeDefinition*>(&node));
	auto other = static_cast<const TypeDefinition&>(node);
	return *type==*other.type && *name==*other.name;
}

bool Function::equals(const Node& node) const {
	assert(dynamic_cast<const Function*>(&node));
	auto other = static_cast<const Function&>(node);
	return
	    *returnType==*other.returnType &&
	    *name==*other.name &&
	    ::equals(parameter, other.parameter, equal_target<VariablePtr>()) &&
	    *body==*other.body;
}

bool Constructor::equals(const Node& node) const {
	assert(dynamic_cast<const Constructor*>(&node));
	auto other = static_cast<const Constructor&>(node);
	return *className == *other.className && *function == *other.function &&
	::equals(initialization, other.initialization, [](const InitializerListEntry& a, const InitializerListEntry& b) {
		return *a.first == *b.first && ::equals(a.second, b.second, equal_target<NodePtr>());
	});
}

bool Destructor::equals(const Node& node) const {
	assert(dynamic_cast<const Destructor*>(&node));
	auto other = static_cast<const Destructor&>(node);
	return *className == *other.className && *function == *other.function;
}

bool MemberFunction::equals(const Node& node) const {
	assert(dynamic_cast<const MemberFunction*>(&node));
	auto other = static_cast<const MemberFunction&>(node);
	return isConstant == other.isConstant && *className == *other.className && *function == *other.function;
}

bool Namespace::equals(const Node& node) const {
	assert(dynamic_cast<const Namespace*>(&node));
	auto other = static_cast<const Namespace&>(node);
	return name == other.name && *definition == *other.definition;
	
}

bool ExternC::equals(const Node& node) const {
	assert(dynamic_cast<const ExternC*>(&node));
	auto other = static_cast<const ExternC&>(node);
	return ::equals(definitions, other.definitions, equal_target<TopLevelElementPtr>());
}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme


namespace std {
std::ostream& operator<< (std::ostream& o, const insieme::backend::c_ast::Node& node) {
	return o << insieme::backend::c_ast::toC(const_cast<insieme::backend::c_ast::Node*>(&node));
}
}

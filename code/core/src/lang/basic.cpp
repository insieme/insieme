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

#include "insieme/core/lang/basic.h"

#include <map>

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace lang {

using namespace boost;

LiteralNotFoundException::LiteralNotFoundException(const string& lit) throw() : msg(string("Literal not found: ") + lit) { }

// Work around GCC unimplemented features
template<class ...All>
struct GroupChecker;
template<class Head, class ...Tail>
struct GroupChecker<Head, Tail...> {
	bool operator ()(const BasicGenerator& bg, const NodePtr& p) {
		return Head()(bg, p) || GroupChecker<Tail...>()(bg, p);
	}
};
template<>
struct GroupChecker<> {
	bool operator ()(const BasicGenerator&, const NodePtr&) {
		return false;
	}
};

struct BasicGenerator::BasicGeneratorImpl : boost::noncopyable {
	NodeManager &nm;
	parse::IRParser parser;
	ASTBuilder build;

	typedef LiteralPtr (BasicGenerator::*litFunPtr)() const;
	typedef bool (BasicGenerator::*groupCheckFuncPtr)(const NodePtr&) const;
	std::map<std::string, litFunPtr> literalMap;

	typedef std::multimap<BasicGenerator::Operator, std::pair<groupCheckFuncPtr, litFunPtr>> OperationMap;
	OperationMap operationMap;

	#define GROUP(_id, ...) \
	struct _id { bool operator()(const BasicGenerator& bg, const NodePtr& p) const { return bg.is##_id(p); } };
	#define TYPE(_id, _spec) \
	TypePtr ptr##_id; \
	GROUP(_id, _spec)
	#define LITERAL(_id, _name, _spec) \
	LiteralPtr ptr##_id; \
	GROUP(_id, _spec)
	#define OPERATION(_type, _op, _name, _spec) \
	LiteralPtr ptr##_type##_op; \
	GROUP(_type##_op, _spec)
	#include "insieme/core/lang/lang.def"

	BasicGeneratorImpl(NodeManager& nm) : nm(nm), parser(nm), build(nm) {
		#define LITERAL(_id, _name, _spec) \
		literalMap.insert(std::make_pair(_name, &BasicGenerator::get##_id));
		#define OPERATION(_type, _op, _name, _spec) \
		literalMap.insert(std::make_pair(_name, &BasicGenerator::get##_type##_op)); \
		operationMap.insert(std::make_pair(BasicGenerator::_op, std::make_pair(&BasicGenerator::is##_type, &BasicGenerator::get##_type##_op)));
		#include "insieme/core/lang/lang.def"
	}
	
	#define GROUP(_id, ...) \
	bool is##_id(const NodePtr& p) { return GroupChecker<__VA_ARGS__>()(nm.basic, p); };
	#include "insieme/core/lang/lang.def"

	// ----- extra material ---

	StatementPtr ptrNoOp;
};

class BasicGenerator::SubTypeLattice : boost::noncopyable {

	typedef utils::map::PointerMultiMap<TypePtr, TypePtr> RelationMap;
	RelationMap subTypes;
	RelationMap superTypes;

public:

	void addRelation(const TypePtr& subType, const TypePtr& superType) {
		subTypes.insert(std::make_pair(superType, subType));
		superTypes.insert(std::make_pair(subType, superType));
	}

	TypeSet getSubTypesOf(const TypePtr& type) const {
		return getAllOf(type, subTypes);
	}

	TypeSet getSuperTypesOf(const TypePtr& type) const {
		return getAllOf(type, superTypes);
	}
private:
	TypeSet getAllOf(const TypePtr& type, const RelationMap& map) const {
		TypeSet res;
		auto types = map.equal_range(type);
		auto cur = types.first;
		while (cur != types.second) {
			res.insert(cur->second);
			++cur;
		}
		return res;
	}
};

BasicGenerator::BasicGenerator(NodeManager& nm) : nm(nm), pimpl(new BasicGeneratorImpl(nm)), subTypeLattice(0) { }

BasicGenerator::~BasicGenerator() {
	delete pimpl;
	if (subTypeLattice) {
		delete subTypeLattice;
	}
}

#define TYPE(_id, _spec) \
TypePtr BasicGenerator::get##_id() const { \
	if(!pimpl->ptr##_id) pimpl->ptr##_id = pimpl->parser.parseType(_spec); \
	return pimpl->ptr##_id; }; \
bool BasicGenerator::is##_id(const NodePtr& p) const { \
	return *p == *get##_id(); };

#define LITERAL(_id, _name, _spec) \
LiteralPtr BasicGenerator::get##_id() const { \
	if(!pimpl->ptr##_id) pimpl->ptr##_id = pimpl->build.literal(_name, pimpl->parser.parseType(_spec)); \
	return pimpl->ptr##_id; }; \
bool BasicGenerator::is##_id(const NodePtr& p) const { \
	return *p == *get##_id(); };

#define OPERATION(_type, _op, _name, _spec) \
LiteralPtr BasicGenerator::get##_type##_op() const { \
	if(!pimpl->ptr##_type##_op) pimpl->ptr##_type##_op = pimpl->build.literal(_name, pimpl->parser.parseType(_spec)); \
	return pimpl->ptr##_type##_op; }; \
bool BasicGenerator::is##_type##_op(const NodePtr& p) const { \
	return *p == *get##_type##_op(); };

#define GROUP(_id, ...) \
bool BasicGenerator::is##_id(const NodePtr& p) const { \
	return pimpl->is##_id(p); }

#include "insieme/core/lang/lang.def"


bool BasicGenerator::isBuiltIn(const NodePtr& node) const {
	if(auto tN = dynamic_pointer_cast<const Type>(node)) {
		#define TYPE(_id, _spec) \
		if(node == get##_id()) return true;
		#include "insieme/core/lang/lang.def"
	}
	else if(auto lN = dynamic_pointer_cast<const Literal>(node)) {
		#define LITERAL(_id, _name, _spec) \
		if(node == get##_id()) return true;
		#define OPERATION(_type, _op, _name, _spec) \
		if(node == get##_type##_op()) return true;
		#include "insieme/core/lang/lang.def"
	}
	return node == getNoOp();
}

LiteralPtr BasicGenerator::getLiteral(const string& name) const {
	auto lIt = pimpl->literalMap.find(name);
	if(lIt != pimpl->literalMap.end()) {
		return ((*this).*lIt->second)();
	}
	throw LiteralNotFoundException(name);
}

ExpressionPtr BasicGenerator::getOperator(const TypePtr& type, const BasicGenerator::Operator& op) const {
	auto fit = pimpl->operationMap.equal_range(op);
	for(BasicGeneratorImpl::OperationMap::const_iterator it = fit.first, end = fit.second; it != end; ++it) {
		const BasicGeneratorImpl::OperationMap::value_type& curr = *it;
		if(((*this).*curr.second.first)(type))
			return ((*this).*curr.second.second)();
	}

	if(VectorTypePtr vecTy = dynamic_pointer_cast<const VectorType>(type)){
	    core::TypePtr vecElemTy = vecTy->getElementType();
        vecElemTy = (vecElemTy->getNodeType() == core::NT_RefType ?
            core::static_pointer_cast<const core::RefType>(vecElemTy)->getElementType() :
            vecElemTy);

        core::LiteralPtr&& pointwise = (*this).getLiteral(string("vector.pointwise"));
	    return pimpl->build.callExpr(pointwise, (*this).getOperator(vecElemTy, op));
//	    return (*this).getLiteral(string("vector.pointwise"));
	}

	assert(false && "Required combination of operator and type not declared");
	return 0;
}

BasicGenerator::Operator BasicGenerator::getOperator(const LiteralPtr& lit) const {
	// We have to scan the multimap operationMap and find the operation which
	// has this literal as second argument.
	BasicGeneratorImpl::OperationMap& opMap = pimpl->operationMap;
	auto fit = std::find_if(opMap.begin(), opMap.end(), 
		[&](const BasicGeneratorImpl::OperationMap::value_type& cur) { 
			if (*((*this).*cur.second.second)() == *lit) { return true; }
			return false;
		}
	);
	if (fit != opMap.end()) { return fit->first; }
	assert(false && "Literal not found within the OperationMap, therefore not a valid IR literal expression");
}
// ----- extra material ---

StatementPtr BasicGenerator::getNoOp() const {
	if (!pimpl->ptrNoOp) {
		pimpl->ptrNoOp = pimpl->build.compoundStmt();
	}
	return pimpl->ptrNoOp;
}

bool BasicGenerator::isNoOp(const NodePtr& p) const {
	return *p == *getNoOp();
}

LiteralPtr BasicGenerator::getIntTypeParamLiteral(const IntTypeParamPtr& param) const {
	auto type = pimpl->build.genericType("intTypeParam", TypeList(), toVector(param));
	return pimpl->build.literal(type, toString(*param));
}

LiteralPtr BasicGenerator::getTypeLiteral(const TypePtr& type) const {
	auto literalType = pimpl->build.genericType("type", toVector(type));
	return pimpl->build.literal(literalType, toString(*type));
}

LiteralPtr BasicGenerator::getIdentifierLiteral(const core::IdentifierPtr& identifier) const {
	return pimpl->build.literal(getIdentifier(), identifier->getName());
}

ExpressionPtr BasicGenerator::scalarToVector( const TypePtr& type, const ExpressionPtr& subExpr) const {
    // Convert casts form scalars to vectors to vector init exrpessions
    if(core::VectorTypePtr vt = dynamic_pointer_cast<const core::VectorType>(type)) {
        if(pimpl->nm.basic.isScalarType(subExpr->getType())) {
            // get vector element type without ref
            core::TypePtr elementType = vt->getElementType();
            core::TypePtr targetType = elementType;// refs in arrays have been removed! (elementType->getNodeType() != core::NT_RefType) ?  vt->getElementType() :
                    //dynamic_pointer_cast<const core::RefType>(elementType)->getElementType();

            core::ExpressionPtr arg = (subExpr->getType() == targetType) ? subExpr :
                pimpl->build.castExpr(targetType, subExpr); // if the type of the sub expression is not equal the target type we need to cast it

            core::ExpressionPtr&& retExpr = pimpl->build.callExpr(type, pimpl->nm.basic.getVectorInitUniform(),
                (elementType->getNodeType() == core::NT_RefType && arg->getNodeType() != core::NT_RefType)  ? pimpl->build.refVar( arg ) : arg,// if we need a ref type and arg is no ref: add ref
                pimpl->nm.basic.getIntTypeParamLiteral(vt->getSize()));

            return retExpr;
        }
    }


    // check for casts from salar pointers to vector pointers
    if(core::ArrayTypePtr&& array = dynamic_pointer_cast<const core::ArrayType>(type)) {
//        core::RefTypePtr&& refType = dynamic_pointer_cast<const core::RefType>(array->getElementType());
        core::VectorTypePtr&& vt = dynamic_pointer_cast<const core::VectorType>(array->getElementType());
        core::ArrayTypePtr&& castedArray = dynamic_pointer_cast<const core::ArrayType>(subExpr->getType());
        if(castedArray && vt ){
            core::TypePtr elemTy = /*castedArray->getElementType()->getNodeType() == core::NodeType::NT_RefType ?
                    dynamic_pointer_cast<const core::RefType>(castedArray->getElementType())->getElementType() :*/ castedArray->getElementType();

            if(elemTy) {
                // check if they have the same type
                assert(elemTy == vt->getElementType() && "cast from array to array of vectors only allowed within the same type");

                return  pimpl->build.callExpr(pimpl->nm.basic.getArrayElemToVec(), subExpr, pimpl->nm.basic.getIntTypeParamLiteral(vt->getSize()));
            }
        }
    }

    // expression is either already a vector/array type or the type is not a vector type
    return subExpr;
}


const BasicGenerator::SubTypeLattice* BasicGenerator::getSubTypeLattice() const {
	if (!subTypeLattice) {
		SubTypeLattice* res = new SubTypeLattice();

		// initialize lattice with generic relations from lang.def
		#define SUB_TYPE(_typeA, _typeB) \
		res->addRelation(get ## _typeA(), get ## _typeB());
		#include "insieme/core/lang/lang.def"

		subTypeLattice = res;
	}
	return subTypeLattice;
}


TypeSet BasicGenerator::getDirectSuperTypesOf(const TypePtr& type) const {
	return getSubTypeLattice()->getSuperTypesOf(type);
}

TypeSet BasicGenerator::getDirectSubTypesOf(const TypePtr& type) const {
	return getSubTypeLattice()->getSubTypesOf(type);
}

} // namespace lang
} // namespace core
} // namespace insieme

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

namespace insieme {
namespace core {
namespace lang {

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

BasicGenerator::BasicGenerator(NodeManager& nm) : nm(nm), pimpl(new BasicGeneratorImpl(nm)) { }

BasicGenerator::~BasicGenerator() {
	delete pimpl;
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
		#define LITERAL(_id, _name, _spec)\
		if(node == get##_id()) return true;
		#define OPERATION(_type, _op, _name, _spec)\
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
	// DLOG(ERROR) << name;
	// assert(false && "Liternal name not registered");
	return LiteralPtr();
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

LiteralPtr BasicGenerator::getIntTypeParamLiteral(IntTypeParam param) const {
	auto type = pimpl->build.genericType("intTypeParam", TypeList(), toVector(param));
	return pimpl->build.literal(type, toString(param));
}

LiteralPtr BasicGenerator::getTypeLiteral(const TypePtr& type) const {
	auto literalType = pimpl->build.genericType("type", toVector(type));
	return pimpl->build.literal(literalType, toString(*type));
}

} // namespace lang
} // namespace core
} // namespace insieme

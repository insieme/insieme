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

#include "insieme/core/expressions.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/statements.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/transform/node_mapper_utils.h"

using namespace insieme::core;


// ------------------------------------- Expression ---------------------------------

Expression::Expression(NodeType nodeType, const TypePtr& type, const std::size_t& hashCode)
	: Statement(nodeType, hashCode, NC_Expression), type(isolate(type)) { };

bool Expression::equals(const Node& stmt) const {
	// conversion is guaranteed by base Node::operator==
	const Expression& rhs = static_cast<const Expression&>(stmt);
	return (*type == *rhs.type) && equalsExpr(rhs);
}

// ------------------------------------- Literal ---------------------------------

static std::size_t hashLiteral(const TypePtr& type, const string& value) {
	std::size_t seed = 0;
	boost::hash_combine(seed, HS_Literal);
	boost::hash_combine(seed, boost::hash_value(value));
	boost::hash_combine(seed, type->hash());
	return seed;
}

Literal::Literal(const TypePtr& type, const string& value) :
		Expression(NT_Literal, type,::hashLiteral(type, value)), value(value) { }


Literal* Literal::createCopyUsing(NodeMapping& mapper) const {
	return new Literal(mapper.map(0, type), value);
}

bool Literal::equalsExpr(const Expression& expr) const {
	const Literal& rhs = static_cast<const Literal&>(expr);
	return (value == rhs.value);
}

LiteralPtr Literal::get(NodeManager& manager, const string& value, const TypePtr& type) {
	return manager.get(Literal(type, value) );
}

LiteralPtr Literal::get(NodeManager& manager, const TypePtr& type, const string& value) {
	return manager.get(Literal(type, value) );
}

LiteralPtr Literal::parserGet(NodeManager& manager, const TypePtr& type, const string& value) {
	return manager.get(Literal(type, value) );
}



// ------------------------------------- Variable ---------------------------------

namespace {
	std::size_t hashVariable(const TypePtr& type, unsigned int id) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_Variable);
		boost::hash_combine(seed, id);
		boost::hash_combine(seed, type->hash());
		return seed;
	}
}

unsigned int Variable::counter = 0;

Variable::Variable(const TypePtr& type, unsigned int id)
	: Expression(NT_Variable, type, hashVariable(type, id)), id(id) {};

Variable* Variable::createCopyUsing(NodeMapping& mapper) const {
	return new Variable(mapper.map(0, type), id);
}

bool Variable::equalsExpr(const Expression& expr) const {
	const Variable& rhs = static_cast<const Variable&>(expr);
	return (id == rhs.id && *type == *rhs.type);
}

std::ostream& Variable::printTo(std::ostream& out) const {
	return out << "v" << id;
}

VariablePtr Variable::get(NodeManager& manager, const TypePtr& type) {
	return manager.get(Variable(type, ++counter));
}

VariablePtr Variable::get(NodeManager& manager, const TypePtr& type, unsigned int id) {
	return manager.get(Variable(type, id));
}

bool Variable::operator<(const Variable& other) const {
	if (id != other.id) {
		return id < other.id;
	}
	return ::toString(*getType()) < ::toString(*other.getType());
}

// ------------------------------------- TupleExpr ---------------------------------

std::size_t hashTupleExpr(const TypePtr& type, const vector<ExpressionPtr>& expressions) {
	size_t seed = 0;
	boost::hash_combine(seed, HS_TupleExpr);
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, expressions);
	return seed;
}

TupleExpr::TupleExpr(const TupleTypePtr& type, const vector<ExpressionPtr>& expressions)
		: Expression(NT_TupleExpr, type, hashTupleExpr(type, expressions)), expressions(isolate(expressions)) { };

TupleExpr* TupleExpr::createCopyUsing(NodeMapping& mapper) const {
	return new TupleExpr(
			mapper.map(0, static_pointer_cast<const TupleType>(type)),
			mapper.map(1, expressions)
		);
}

bool TupleExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base equals
	const TupleExpr& rhs = static_cast<const TupleExpr&>(expr);
	return ::equals(expressions, rhs.expressions, equal_target<ExpressionPtr>());
}

Node::NodeListOpt TupleExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	std::copy(expressions.cbegin(), expressions.cend(), back_inserter(*res));
	return res;
}

std::ostream& TupleExpr::printTo(std::ostream& out) const {
	return out << "tuple(" << join(",", expressions, print<deref<ExpressionPtr>>()) << ")";
}


TupleExprPtr TupleExpr::get(NodeManager& manager, const vector<ExpressionPtr>& expressions) {
	TypeList elemTypes;
	std::transform(expressions.cbegin(), expressions.cend(), back_inserter(elemTypes), [](const ExpressionPtr& e) { return e->getType(); });
	return get(manager, TupleType::get(manager, elemTypes), expressions);
}

TupleExprPtr TupleExpr::get(NodeManager& manager, const TupleTypePtr& type, const vector<ExpressionPtr>& expressions) {
	return manager.get(TupleExpr(type, expressions));
}

// ------------------------------------- VectorExpr ---------------------------------

std::size_t hashVectorExpr(const TypePtr& type, const vector<ExpressionPtr>& expressions) {
	size_t seed = 0;
	boost::hash_combine(seed, HS_VectorExpr);
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, expressions);
	return seed;
}

VectorExpr::VectorExpr(const VectorTypePtr& type, const vector<ExpressionPtr>& expressions)
		: Expression(NT_VectorExpr, type, hashVectorExpr(type, expressions)), expressions(isolate(expressions)) { };

VectorExpr* VectorExpr::createCopyUsing(NodeMapping& mapper) const {
	return new VectorExpr(
			mapper.map(0, static_pointer_cast<const VectorType>(type)),
			mapper.map(1, expressions)
	);
}

bool VectorExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base equals
	const VectorExpr& rhs = static_cast<const VectorExpr&>(expr);
	return ::equals(expressions, rhs.expressions, equal_target<ExpressionPtr>());
}

Node::NodeListOpt VectorExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	std::copy(expressions.cbegin(), expressions.cend(), back_inserter(*res));
	return res;
}

std::ostream& VectorExpr::printTo(std::ostream& out) const {
	return out << "{" << join(",", expressions, print<deref<ExpressionPtr>>()) << "}";
}


VectorExprPtr VectorExpr::get(NodeManager& manager, const vector<ExpressionPtr>& expressions) {

	TypePtr elementType = TypeVariable::get(manager, string("a"));
	if (!expressions.empty()) {
		elementType = expressions[0]->getType();
	}

	VectorTypePtr resultType = VectorType::get(manager, elementType, ConcreteIntTypeParam::get(manager, expressions.size()));
	return get(manager, resultType, expressions);
}

VectorExprPtr VectorExpr::get(NodeManager& manager, const VectorTypePtr& type, const vector<ExpressionPtr>& expressions) {
	assert (type->getSize()->getNodeType() == NT_ConcreteIntTypeParam
			&& static_pointer_cast<const ConcreteIntTypeParam>(type->getSize())->getValue() == expressions.size()
			&& "Invalid vector type specified!");
	return manager.get(VectorExpr(type, expressions));
}


// ------------------------------------- NamedCompositeExpr ---------------------------------

namespace {

	const StructExpr::Members& isolateMembers(const StructExpr::Members& members) {
		for_each(members, [](const StructExpr::Member& cur) {
			isolate(cur.second);
		});
		return members;
	}

	StructExpr::Members copyMembersUsing(NodeMapping& mapper, unsigned offset, const StructExpr::Members& members) {
		StructExpr::Members res;
		unsigned index = offset;
		std::transform(members.cbegin(), members.cend(), std::back_inserter(res),
				[&mapper, &index](const StructExpr::Member& m) {
					IdentifierPtr name = mapper.map(index++, m.first);
					ExpressionPtr value = mapper.map(index++, m.second);
					return StructExpr::Member(name, value);
		});
		return res;
	}

	NamedCompositeType::Entries getTypeEntries(const StructExpr::Members& member) {
		NamedCompositeType::Entries entries;

		// NOTE: transform would be equivalent but causes an internal error in GCC 4.5.1
	//	std::transform(mem.begin(), mem.end(), back_inserter(entries), [](const Member& m)
	//		{ return NamedCompositeType::Entry(m.first, m.second->getType()); } );

		std::for_each(member.begin(), member.end(), [&entries](const StructExpr::Member& cur) {
			entries.push_back(NamedCompositeType::Entry(cur.first, cur.second->getType()));
		});
		return entries;
	}

	std::size_t hashStructMember(const StructTypePtr& type, const StructExpr::Members& members) {
		std::size_t seed = 0;
		boost::hash_combine(seed, HS_StructExpr);
		boost::hash_combine(seed, *type);
		for_each(members, [&](const StructExpr::Member& m) {
			boost::hash_combine(seed, *m.first);
			boost::hash_combine(seed, *m.second);
		});
		return seed;
	}

}

// ------------------------------------- StructExpr ---------------------------------

StructExpr::StructExpr(const StructTypePtr& type, const Members& members)
	: Expression(NT_StructExpr, type, ::hashStructMember(type, members)), members(isolateMembers(members)) { }

StructExpr* StructExpr::createCopyUsing(NodeMapping& mapper) const {
	return new StructExpr(static_pointer_cast<const StructType>(mapper.map(0, type)), copyMembersUsing(mapper, 1, members));
}

bool StructExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const StructExpr& rhs = static_cast<const StructExpr&>(expr);
	return ::equals(members, rhs.members, [](const Member& l, const Member& r) {
		return *l.first == *r.first && *l.second == *r.second;
	});
}

Node::NodeListOpt StructExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	for_each(members, [&](const Member& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	return res;
}

std::ostream& StructExpr::printTo(std::ostream& out) const {
	// print struct using member - value pairs
	out << "struct{" << join(", ", members, [](std::ostream& out, const Member& cur) {
		out << *cur.first << "=" << *cur.second;
	}) << "}";
	return out;
}

StructExprPtr StructExpr::get(NodeManager& manager, const Members& members) {
	return get(manager, StructType::get(manager, getTypeEntries(members)), members);
}

StructExprPtr StructExpr::get(NodeManager& manager, const StructTypePtr& type, const Members& members) {
	return manager.get(StructExpr(type, members));
}

// ------------------------------------- UnionExpr ---------------------------------

std::size_t hashUnionExpr(size_t seed, const IdentifierPtr& memberName, const ExpressionPtr& member) {
	boost::hash_combine(seed, *memberName);
	boost::hash_combine(seed, *member);
	return seed;
}

UnionExpr::UnionExpr(const TypePtr& type, const IdentifierPtr& memberName, const ExpressionPtr& member)
	: Expression(NT_UnionExpr, type, ::hashUnionExpr(HS_UnionExpr, memberName, member)), memberName(memberName), member(isolate(member)) { }

UnionExpr* UnionExpr::createCopyUsing(NodeMapping& mapper) const {
	return new UnionExpr(static_pointer_cast<const UnionType>(mapper.map(0, type)), memberName, mapper.map(1,member));
}

bool UnionExpr::equalsExpr(const Expression& expr) const {
	// type is guaranteed by super class
	const UnionExpr& other = static_cast<const UnionExpr&>(expr);

	// check element name and expression
	return *memberName == *other.memberName && *member == *other.member;
}

Node::NodeListOpt UnionExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	res->push_back(memberName);
	res->push_back(member);
	return res;
}

std::ostream& UnionExpr::printTo(std::ostream& out) const {
	// TODO fugly
	//out << "union(" << join(", ", members, [](const Member& m) { return format("%s: %s", m.first.getName().c_str(), toString(*m.second).c_str()); }) << ")";
	return out << "<?>I owe you a union print! - peter</?>";
}

UnionExprPtr UnionExpr::get(NodeManager& manager, const TypePtr& type, const IdentifierPtr& memberName, const ExpressionPtr& member) {
	return manager.get(UnionExpr(type, memberName, member));
}

// ------------------------------------- JobExpr ---------------------------------

namespace {

	size_t hashJobExpr(const ExpressionPtr& range, const StatementPtr& defaultStmt, const JobExpr::GuardedStmts& guardedStmts, const JobExpr::LocalDecls& localDecls) {
		size_t seed = 0;
		boost::hash_combine(seed, HS_JobExpr);
		boost::hash_combine(seed, range->hash());
		hashPtrRange(seed, localDecls);
		std::for_each(guardedStmts.cbegin(), guardedStmts.cend(), [&seed](const JobExpr::GuardedStmt& s){
			boost::hash_combine(seed, s.first->hash());
			boost::hash_combine(seed, s.second->hash());
		});
		boost::hash_combine(seed, defaultStmt->hash());
		return seed;
	}

	const JobExpr::GuardedStmts& isolateGuardedStmts(const JobExpr::GuardedStmts& stmts) {
		JobExpr::GuardedStmts localGuardedStmts;
		for_each(stmts, [](const JobExpr::GuardedStmt& stmt) {
				isolate(stmt.first);
				isolate(stmt.second);
		});
		return stmts;
	}

	const JobExpr::GuardedStmts copyGuardedStmtsUsing(NodeMapping& mapper, unsigned offset, const JobExpr::GuardedStmts& stmts) {
		JobExpr::GuardedStmts localGuardedStmts;
		std::transform(stmts.cbegin(), stmts.cend(), back_inserter(localGuardedStmts),
			[&localGuardedStmts, &mapper, &offset](const JobExpr::GuardedStmt& stmt)->JobExpr::GuardedStmt {
				JobExpr::GuardedStmt res = JobExpr::GuardedStmt(
						mapper.map(offset, stmt.first),
						mapper.map(offset+1, stmt.second)
				);
				offset+=2;
				return res;
		});
		return localGuardedStmts;
	}

}

JobExpr::JobExpr(const ExpressionPtr& range, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls)
	: Expression(NT_JobExpr, range->getNodeManager().basic.getJob(), ::hashJobExpr(range, defaultStmt, guardedStmts, localDecls)), threadNumRange(range),
	  localDecls(isolate(localDecls)), guardedStmts(isolateGuardedStmts(guardedStmts)), defaultStmt(isolate(defaultStmt)) {
	// TODO: use ordinary type checks for this section ...
	FunctionTypePtr defaultType = static_pointer_cast<const FunctionType>(defaultStmt->getType());
    assert(defaultType->getParameterTypes().empty() && "Default statement is not allowed to have any arguments");

    TypePtr unitType = type->getNodeManager().basic.getUnit();
    TypePtr boolType = type->getNodeManager().basic.getBool();

    assert(*defaultType->getReturnType() == *unitType && "Return value of default statement must be unit.");

    TypeList guardParams = TypeList(2, type->getNodeManager().basic.getUIntGen());

    std::for_each(guardedStmts.cbegin(), guardedStmts.cend(), [&](const JobExpr::GuardedStmt& s){
        //Check guards
    	FunctionTypePtr guardType = static_pointer_cast<const FunctionType>(s.first->getType());
        assert(::equals(guardType->getParameterTypes(), guardParams, equal_target<TypePtr>()) && "Guard must have two integer arguments");
        assert(*guardType->getReturnType() == *boolType && "Return value of guard must be bool.");

        //Check guarded statements
        FunctionTypePtr stmtType = static_pointer_cast<const FunctionType>(s.second->getType());
        assert(stmtType->getParameterTypes().empty() && "Guarded statement is not allowed to have any arguments");
        assert(*stmtType->getReturnType() == *unitType && "Return value of guarded statement must be void.");
    });

}



JobExpr* JobExpr::createCopyUsing(NodeMapping& mapper) const {
	return new JobExpr(
			mapper.map(0, threadNumRange),
			mapper.map(1 + localDecls.size() + guardedStmts.size()*2,defaultStmt),
			copyGuardedStmtsUsing(mapper, 1 + localDecls.size(), guardedStmts),
			mapper.map(1, localDecls));
}

Node::NodeListOpt JobExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(threadNumRange);
	std::copy(localDecls.cbegin(), localDecls.cend(), back_inserter(*res));
	std::for_each(guardedStmts.cbegin(), guardedStmts.cend(), [&res](const GuardedStmt& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	res->push_back(defaultStmt);
	return res;
}

bool JobExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const JobExpr& rhs = static_cast<const JobExpr&>(expr);
	return
		*defaultStmt == *rhs.defaultStmt &&
		*threadNumRange == *rhs.threadNumRange &&
		::equals(localDecls, rhs.localDecls, equal_target<DeclarationStmtPtr>()) &&
		::equals(guardedStmts, rhs.guardedStmts, [](const GuardedStmt& l, const GuardedStmt& r) {
			return *l.first == *r.first && *l.second == *r.second; } );
}

std::ostream& JobExpr::printTo(std::ostream& out) const {
	out << "job [" << join(", ", localDecls, print<deref<DeclarationStmtPtr>>()) << "] ("
		<< join(", ", guardedStmts, [](std::ostream& out, const GuardedStmt& s) {
	        out << format("(%s, %s)", s.first->toString().c_str(), s.second->toString().c_str());
	    } )
		<< (guardedStmts.empty()?"":", ") << "default: " << *defaultStmt << ")";
	return out;
}

JobExprPtr JobExpr::get(NodeManager& manager, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls) {
	ExpressionPtr one = Literal::get(manager, manager.basic.getUInt8(), "1");
	CallExprPtr range = CallExpr::get(manager, manager.basic.getJobRange(), manager.basic.getCreateMinRange(), toVector(one));
	return get(manager, range, defaultStmt, guardedStmts, localDecls);
}

JobExprPtr JobExpr::get(NodeManager& manager, const ExpressionPtr& threadNumRange, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls) {
	return manager.get(JobExpr(threadNumRange, defaultStmt, guardedStmts, localDecls));
}

// ------------------------------------- CallExpr ---------------------------------

const TypePtr& getReturnType(const ExpressionPtr& functionExpr) {
	const TypePtr& type = functionExpr->getType();
	assert( dynamic_cast<const FunctionType*>(&*type) && "Non-function type expression used as operator within call Expression!");

	const FunctionType& funType = static_cast<const FunctionType&>(*type);
	return funType.getReturnType();
}

std::size_t hashCallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	size_t seed = 0;
	boost::hash_combine(seed, HS_CallExpr);
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, functionExpr->hash());
	hashPtrRange(seed, arguments);
	return seed;
}

CallExpr::CallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments)
		: Expression(NT_CallExpr, type, ::hashCallExpr(type, functionExpr, arguments)), functionExpr(isolate(functionExpr)), arguments(isolate(arguments)) { }

CallExpr::CallExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments)
		: Expression(NT_CallExpr, ::getReturnType(functionExpr), ::hashCallExpr(::getReturnType(functionExpr), functionExpr, arguments)),
		  functionExpr(isolate(functionExpr)), arguments(isolate(arguments)) { }

CallExpr* CallExpr::createCopyUsing(NodeMapping& mapper) const {
	return new CallExpr(
			mapper.map(0, type),
			mapper.map(1, functionExpr),
			mapper.map(2, arguments)
		);
}

bool CallExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CallExpr& rhs = static_cast<const CallExpr&>(expr);
	return (*rhs.functionExpr == *functionExpr) &&
		::equals(arguments, rhs.arguments, equal_target<ExpressionPtr>());
}

Node::NodeListOpt CallExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	res->push_back(functionExpr);
	std::copy(arguments.cbegin(), arguments.cend(), back_inserter(*res));
	return res;
}

std::ostream& CallExpr::printTo(std::ostream& out) const {
	return out << *functionExpr << "(" << join(", ", arguments, print<deref<ExpressionPtr>>()) << ")";
}

const ExpressionPtr& CallExpr::getArgument(size_t pos) const {
	if(pos >= arguments.size()) throw ArgumentOutOfRangeException();
	return arguments[pos];
}

CallExprPtr CallExpr::get(NodeManager& manager, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	return manager.get(CallExpr(resultType, functionExpr, arguments));
}

// ------------------------------------- CastExpr ---------------------------------

std::size_t hashCastExpr(const TypePtr& type, const ExpressionPtr& subExpression) {
	size_t seed = HS_CastExpr;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, subExpression->hash());
	return seed;
}

CastExpr::CastExpr(const TypePtr& type, const ExpressionPtr& subExpression)
		: Expression(NT_CastExpr, type, hashCastExpr(type, subExpression)), subExpression(isolate(subExpression)) { }

CastExpr* CastExpr::createCopyUsing(NodeMapping& mapper) const {
	return new CastExpr(mapper.map(0, type), mapper.map(1, subExpression));
}

bool CastExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CastExpr& rhs = static_cast<const CastExpr&>(expr);
	return (*rhs.subExpression == *subExpression);
}

Node::NodeListOpt CastExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	res->push_back(subExpression);
	return res;
}

std::ostream& CastExpr::printTo(std::ostream& out) const {
	return out << "cast<" << *type << ">" << "(" << *subExpression <<")";
}

CastExprPtr CastExpr::get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& subExpr) {
	return manager.get(CastExpr(type, subExpr));
}

// ------------------------------------ Lambda ------------------------------

namespace {

	std::size_t hashLambda(const FunctionTypePtr& type, const Lambda::ParamList& paramList, const StatementPtr& body) {

		std::size_t hash = 0;
		boost::hash_combine(hash, HS_Lambda);
		boost::hash_combine(hash, type->hash());
		hashPtrRange(hash, paramList);
		boost::hash_combine(hash, body->hash());
		return hash;
	}

}

Lambda::Lambda(const FunctionTypePtr& type, const ParamList& paramList, const CompoundStmtPtr& body)
	: Node(NT_Lambda, NC_Support, hashLambda(type, paramList, body)), type(isolate(type)),
	  paramList(isolate(paramList)), body(isolate(body)) { };

Lambda* Lambda::createCopyUsing(NodeMapping& mapper) const {
	return new Lambda(mapper.map(0, type),
			mapper.map(1, paramList),
			mapper.map(1 + paramList.size(), body)
		);
}

bool Lambda::equals(const Node& other) const {
	// check identity
	if (this == &other) {
		return true;
	}

	// check type
	if (other.getNodeType() != NT_Lambda) {
		return false;
	}

	const Lambda& rhs = static_cast<const Lambda&>(other);
	return 	*type == *rhs.type &&
			::equals(paramList, rhs.paramList, equal_target<VariablePtr>()) &&
			*body == *rhs.body;
}

Node::NodeListOpt Lambda::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(type);
	res->insert(res->end(), paramList.begin(), paramList.end());
	res->push_back(body);
	return res;
}

std::ostream& Lambda::printTo(std::ostream& out) const {
	auto paramPrinter = [](std::ostream& out, const VariablePtr& var) {
		out << *var->getType() << " " << *var;
	};

	out << "fun";
	out << "(" << join(", ", paramList, paramPrinter) << ") " << *body;

	return out;
}

LambdaPtr Lambda::get(NodeManager& manager, const FunctionTypePtr& type, const ParamList& params, const StatementPtr& body) {
	assert(type->isPlain() && "Lambdas should be plain functions!");
	CompoundStmtPtr compound = dynamic_pointer_cast<const CompoundStmt>(body);
	if(!compound) {
		compound = CompoundStmt::get(manager, body);
	}
	return manager.get(Lambda(type, params, compound));
}


// ------------------------------------ Lambda Definition ------------------------------

namespace {

	std::size_t hashLambdaDefinition(const LambdaDefinition::Definitions& definitions) {
		std::size_t hash = 0;
		boost::hash_combine(hash, HS_LambdaDefinition);
		for_each(definitions, [&](const std::pair<VariablePtr, LambdaPtr>& cur) {
			boost::hash_combine(hash, *cur.first);
			boost::hash_combine(hash, *cur.second);
		});
		return hash;
	}

	const LambdaDefinition::Definitions& isolateDefinitions(const LambdaDefinition::Definitions& definitions) {
		for_each(definitions, [](const LambdaDefinition::Definitions::value_type& cur) {
				isolate(cur.first);
				isolate(cur.second);
		});
		return definitions;
	}

	LambdaDefinition::Definitions copyDefinitionsUsing(NodeMapping& mapper, unsigned offset, const LambdaDefinition::Definitions& definitions) {
		typedef LambdaDefinition::Definitions::value_type Value;

		LambdaDefinition::Definitions res;
		std::transform(definitions.begin(), definitions.end(), inserter(res, res.end()),
			[&mapper, &offset](const Value& cur)->Value {
				// apply mapping
				auto res = std::make_pair(
						mapper.map(offset, cur.first),
						mapper.map(offset+1, cur.second));
				offset += 2;
				return res;
		});
		return res;
	}

}

LambdaDefinition::LambdaDefinition(const LambdaDefinition::Definitions& defs)
	: Node(NT_LambdaDefinition, NC_Support, hashLambdaDefinition(defs)), definitions(isolateDefinitions(defs)) { };

LambdaDefinitionPtr LambdaDefinition::get(NodeManager& manager, const LambdaDefinition::Definitions& definitions) {
	return manager.get(LambdaDefinition(definitions));
}

LambdaDefinition* LambdaDefinition::createCopyUsing(NodeMapping& mapper) const {
	return new LambdaDefinition(copyDefinitionsUsing(mapper, 0, definitions));
}

bool LambdaDefinition::equals(const Node& other) const {
	// check identity
	if (this == &other) {
		return true;
	}

	// check type
	if (other.getNodeType() != NT_LambdaDefinition) {
		return false;
	}

	const LambdaDefinition& rhs = static_cast<const LambdaDefinition&>(other);
	return insieme::utils::map::equal(definitions, rhs.definitions, equal_target<LambdaPtr>());
}

Node::NodeListOpt LambdaDefinition::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	std::for_each(definitions.begin(), definitions.end(), [&res](const Definitions::value_type& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	return res;
}

std::ostream& LambdaDefinition::printTo(std::ostream& out) const {
	auto paramPrinter = [](std::ostream& out, const VariablePtr& var) {
		out << *var->getType() << " " << *var;
	};

	return out << "{" << join(", ", definitions, [&paramPrinter](std::ostream& out, const Definitions::value_type& cur) {
		out << *cur.first << "=" << *cur.second;
	}) << "}";
}

const LambdaPtr& LambdaDefinition::getDefinitionOf(const VariablePtr& variable) const {
	auto it = definitions.find(variable);
	assert( it != definitions.end() && "Trying to access undefined Lambda Definition!");
	return it->second;
}

bool LambdaDefinition::isRecursive(const VariablePtr& variable) const {

	// obtain lambda definition
	const LambdaPtr& lambda = getDefinitionOf(variable);

	const Definitions& defs = definitions;

	// a detector which aborts a visiting in cased a recursive function invocation
	// is detected
	auto detector = makeLambdaVisitor([&defs](const NodePtr& node)->bool {
		// check node type
		if (node->getNodeType() != NT_Variable) {
			return false;
		}

		// check whether the variable is a recursive function
		return defs.find(static_pointer_cast<const Variable>(node)) != defs.end();
	}, false);

	// run visitor => if interrupted, the definition is recursive
	return visitDepthFirstOnceInterruptable(lambda, detector);
}

namespace {

	class RecLambdaUnroller : public transform::CachedNodeMapping {

		NodeManager& manager;
		const LambdaDefinition& definition;
		const LambdaDefinition::Definitions& definitions;


	public:

		RecLambdaUnroller(NodeManager& manager, const LambdaDefinition& definition)
			: manager(manager), definition(definition), definitions(definition.getDefinitions()) { }

		virtual const NodePtr resolveElement(const NodePtr& ptr) {
			// check whether it is a known variable
			if (ptr->getNodeType() == NT_Variable) {
				VariablePtr var = static_pointer_cast<const Variable>(ptr);

				auto pos = definitions.find(var);
				if (pos != definitions.end()) {
					return LambdaExpr::get(manager, var, LambdaDefinitionPtr(&definition));
				}
			}

			// cut of types
			if (ptr->getNodeCategory() == NC_Type) {
				return ptr;
			}

			// replace recursively
			return ptr->substitute(manager, *this);
		}

		LambdaPtr apply(const LambdaPtr& node) {
			return static_pointer_cast<const Lambda>(node->substitute(manager, *this));
		}

	};

}

LambdaExprPtr LambdaDefinition::unrollOnce(NodeManager& manager, const VariablePtr& variable) const {
	// unroll recursive lambda
	LambdaPtr lambda = RecLambdaUnroller(manager, *this).apply(getDefinitionOf(variable));
	return LambdaExpr::get(manager, lambda);
}


// ------------------------------------ Lambda Expr ------------------------------

namespace {

	std::size_t hashLambdaExpr(const VariablePtr& variable, const LambdaDefinitionPtr& definition) {
		std::size_t hash = 0;
		boost::hash_combine(hash, HS_LambdaExpr);
		boost::hash_combine(hash, variable->hash());
		boost::hash_combine(hash, definition->hash());
		return hash;
	}

}

LambdaExpr::LambdaExpr(const VariablePtr& variable, const LambdaDefinitionPtr& definition)
	: Expression(NT_LambdaExpr, variable->getType(), ::hashLambdaExpr(variable, definition)),
	  variable(isolate(variable)), definition(isolate(definition)), lambda(definition->getDefinitionOf(variable)),
	  recursive(boost::logic::indeterminate) { }

LambdaExpr* LambdaExpr::createCopyUsing(NodeMapping& mapper) const {
	return new LambdaExpr(mapper.map(0, variable), mapper.map(1, definition));
}

Node::NodeListOpt LambdaExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(variable);
	res->push_back(definition);
	return res;
}

bool LambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const LambdaExpr& rhs = static_cast<const LambdaExpr&>(expr);
	return (*rhs.variable == *variable && *rhs.definition == *definition);
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const LambdaPtr& lambda) {
	return get(manager, lambda->getType(), lambda->getParameterList(), lambda->getBody());
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const VariablePtr& variable, const LambdaDefinitionPtr& definition) {
	return manager.get(LambdaExpr(variable, definition));
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const FunctionTypePtr& type, const Lambda::ParamList& params, const StatementPtr& body) {

	// build definitions
	VariablePtr var = Variable::get(manager, type);
	LambdaDefinition::Definitions defs;
	defs.insert(std::make_pair(var, Lambda::get(manager, type, params, body)));

	return manager.get(LambdaExpr(var, LambdaDefinition::get(manager, defs)));
}

std::ostream& LambdaExpr::printTo(std::ostream& out) const {
	// TODO: special handling for non-recursive functions
	return out << "rec " << *variable << "." << *definition;
}

const Lambda::ParamList& LambdaExpr::getParameterList() const {
	return lambda->getParameterList();
}

const CompoundStmtPtr& LambdaExpr::getBody() const {
	return lambda->getBody();
}


// -------------------------------- Bind Expression -----------------------------------

namespace {

	TypePtr getBindExprType(const vector<VariablePtr>& parameters, const TypePtr& resultType) {

		// create list of arguments
		TypeList paramTypes;
		::transform(parameters, std::back_inserter(paramTypes), [](const VariablePtr& cur) {
			return cur->getType();
		});

		// create resulting type
		return FunctionType::get(resultType->getNodeManager(), paramTypes, resultType, false);
	}

	std::size_t hashBindExpr(const vector<VariablePtr>& parameters, const CallExprPtr& call) {
		std::size_t hash = 0;
		boost::hash_combine(hash, HS_BindExpr);
		hashPtrRange(hash, parameters);
		boost::hash_combine(hash, call->hash());
		return hash;
	}

}

BindExpr::BindExpr(const vector<VariablePtr>& parameters, const CallExprPtr& call)
	: Expression(NT_BindExpr, getBindExprType(parameters, call->getType()), ::hashBindExpr(parameters, call)),
	  parameters(parameters), call(call) {}

BindExpr* BindExpr::createCopyUsing(NodeMapping& mapper) const {
	// just create a clone converted through the given mapper
	return new BindExpr(
			mapper.map(0, parameters),
			mapper.map(parameters.size(), call)
	);
}

Node::NodeListOpt BindExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	copy(parameters, std::back_inserter(*res));
	res->push_back(call);
	return res;
}

bool BindExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const BindExpr& rhs = static_cast<const BindExpr&>(expr);

	// evaluated members
	bool res = true;
	res = res && *call == *(rhs.call);
	res = res && ::equals(parameters, rhs.parameters, equal_target<VariablePtr>());
	return res;
}

BindExprPtr BindExpr::get(NodeManager& manager, const vector<VariablePtr>& parameters, const CallExprPtr& call) {
	return manager.get(BindExpr(parameters, call));
}

std::ostream& BindExpr::printTo(std::ostream& out) const {
	return out << "bind(" << join(",", parameters, print<deref<VariablePtr>>()) << "){" << *call << "}";
}

vector<ExpressionPtr> BindExpr::getBoundExpressions() const {
	vector<ExpressionPtr> res;

	for_each(call->getArguments(), [&](const ExpressionPtr& cur) {
		if (cur->getNodeType() == NT_Variable) {
			const VariablePtr& var = static_pointer_cast<const Variable>(cur);
			if (contains(parameters, var)) {
				return;
			}
		}
		// add to bind expressions
		res.push_back(cur);
	});

	return res;
}



// ------------------------ Member Access Expression ------------------------

namespace {

	TypePtr getMemberType(const ExpressionPtr& subExpression, const IdentifierPtr& member) {
		TypePtr type = subExpression->getType();
		if (type->getNodeType() == NT_RecType) {
			type = static_pointer_cast<const RecType>(type)->unroll(type->getNodeManager());
		}
		assert(type->getNodeType() == NT_StructType && "Accessing member of non-struct type!");
		StructTypePtr structType = static_pointer_cast<const StructType>(type);
		TypePtr res = structType->getTypeOfMember(member);
		assert(res && "Accessing non-existing member!");
		return res;
	}

	std::size_t hashMemberAccess(const ExpressionPtr& subExpression, const IdentifierPtr& member) {
		std::size_t res = 0;
		boost::hash_combine(res, HS_MemberAccessExpr);
		boost::hash_combine(res, subExpression->hash());
		boost::hash_combine(res, member->hash());
		return res;
	}
}

MemberAccessExpr::MemberAccessExpr( const ExpressionPtr& subExpression, const IdentifierPtr& member)
	: Expression(NT_MemberAccessExpr, getMemberType(subExpression, member), hashMemberAccess(subExpression, member)),
	  subExpression(isolate(subExpression)), member(isolate(member)) {}

MemberAccessExpr* MemberAccessExpr::createCopyUsing(NodeMapping& mapper) const {
	return new MemberAccessExpr(mapper.map(0, subExpression), mapper.map(1, member));
}


bool MemberAccessExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const MemberAccessExpr& rhs = static_cast<const MemberAccessExpr&>(expr);
	return (*rhs.subExpression == *subExpression && *rhs.member == *member);
}

Node::NodeListOpt MemberAccessExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(subExpression);
	res->push_back(member);
	return res;
}

std::ostream& MemberAccessExpr::printTo(std::ostream& out) const {
	return out << "(" << *subExpression << "." << *member << ")";
}

MemberAccessExprPtr MemberAccessExpr::get(NodeManager& manager, const ExpressionPtr& subExpression, const IdentifierPtr& member) {
	return manager.get(MemberAccessExpr(subExpression, member));
}

// ------------------------ Tuple Projection Expression ------------------------

namespace {

	TypePtr getElementType(const ExpressionPtr& subExpression, const unsigned index) {
		TypePtr type = subExpression->getType();
		assert(type->getNodeType() == NT_TupleType && "Projection applied to non-tuple type!");
		TupleTypePtr tupleType = static_pointer_cast<const TupleType>(type);
		auto res = tupleType->getElementTypes();
		assert(res.size() > index && "Index out of bound!");
		return res[index];
	}

	std::size_t hashTupleProjection(const ExpressionPtr& subExpression, const unsigned index) {
		std::size_t res = 0;
		boost::hash_combine(res, HS_TupleProjectionExpr);
		boost::hash_combine(res, index);
		boost::hash_combine(res, subExpression->hash());
		return res;
	}
}

TupleProjectionExpr::TupleProjectionExpr( const ExpressionPtr& subExpression, const unsigned index)
	: Expression(NT_TupleProjectionExpr, getElementType(subExpression, index), hashTupleProjection(subExpression, index)),
	  subExpression(isolate(subExpression)), index(index) {}

TupleProjectionExpr* TupleProjectionExpr::createCopyUsing(NodeMapping& mapper) const {
	return new TupleProjectionExpr(mapper.map(0, subExpression), index);
}


bool TupleProjectionExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const TupleProjectionExpr& rhs = static_cast<const TupleProjectionExpr&>(expr);
	return (*rhs.subExpression == *subExpression && rhs.index == index);
}

Node::NodeListOpt TupleProjectionExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(subExpression);
	return res;
}

std::ostream& TupleProjectionExpr::printTo(std::ostream& out) const {
	return out << "(" << *subExpression << "[" << index << "])";
}

TupleProjectionExprPtr TupleProjectionExpr::get(NodeManager& manager, const ExpressionPtr& subExpression, const unsigned index) {
	return manager.get(TupleProjectionExpr(subExpression, index));
}


// ------------------------ The Marker Expression ------------------------

namespace {

	std::size_t hashMarkerExpr(const ExpressionPtr& subExpression, const unsigned id) {
		std::size_t hash = 0;
		boost::hash_combine(hash, HS_MarkerExpr);
		boost::hash_combine(hash, id);
		boost::hash_combine(hash, subExpression->hash());
		return hash;
	}

}


unsigned int MarkerExpr::counter = 0;

MarkerExpr::MarkerExpr(const ExpressionPtr& subExpression, const unsigned id)
	: Expression(NT_MarkerExpr, subExpression->getType(), hashMarkerExpr(subExpression, id)),
	  subExpression(isolate(subExpression)), id(id) { };


MarkerExpr* MarkerExpr::createCopyUsing(NodeMapping& mapper) const {
	return new MarkerExpr(mapper.map(0, subExpression), id);
}

bool MarkerExpr::equalsExpr(const Expression& expr) const {
	// static cast to marker (guaranteed by super implementation)
	const MarkerExpr& rhs = static_cast<const MarkerExpr&>(expr);
	return (rhs.id == id && *rhs.subExpression == *subExpression);
}

Node::NodeListOpt MarkerExpr::getChildNodes() const {
	NodeListOpt res = std::make_shared<NodeList>();
	res->push_back(subExpression);
	return res;
}

std::ostream& MarkerExpr::printTo(std::ostream& out) const {
	return out << "<M id=" << id << ">" << *subExpression << "</M>";
}

MarkerExprPtr MarkerExpr::get(NodeManager& manager, const ExpressionPtr& subExpression) {
	return manager.get(MarkerExpr(subExpression, ++counter));
}

MarkerExprPtr MarkerExpr::get(NodeManager& manager, const ExpressionPtr& subExpression, const unsigned id) {
	return manager.get(MarkerExpr(subExpression, id));
}


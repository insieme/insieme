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

using namespace insieme::core;

enum {
	HASHVAL_LITERAL = 100 /* offset from statements */,
	HASHVAL_VAREXPR, HASHVAL_CALLEXPR, HASHVAL_CASTEXPR, HASHVAL_PARAMEXPR, HASHVAL_CAPTURE_INIT,
	HASHVAL_TUPLEEXPR, HASHVAL_STRUCTEXPR, HASHVAL_UNIONEXPR, HASHVAL_JOBEXPR, HASHVAL_LAMBDA_DEFINITION,
	HASHVAL_LAMBDA, HASHVAL_VECTOREXPR, HASHVAL_VARIABLE, HASHVAL_MEMBER_ACCESS, HASHVAL_TUPLE_PROJECTION,
	HASHVAL_MARKER
};

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
	std::size_t seed = HASHVAL_LITERAL;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, boost::hash_value(value));
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


// ------------------------------------- Variable ---------------------------------

namespace {
	std::size_t hashVariable(const TypePtr& type, unsigned int id) {
		std::size_t seed = HASHVAL_VARIABLE;
		boost::hash_combine(seed, type->hash());
		boost::hash_combine(seed, id);
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
	return (id == rhs.id);
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
	size_t seed = HASHVAL_TUPLEEXPR;
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

Node::OptionChildList TupleExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
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
	size_t seed = HASHVAL_VECTOREXPR;
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

Node::OptionChildList VectorExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
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

	VectorTypePtr resultType = VectorType::get(manager, elementType, IntTypeParam::getConcreteIntParam(expressions.size()));
	return get(manager, resultType, expressions);
}

VectorExprPtr VectorExpr::get(NodeManager& manager, const VectorTypePtr& type, const vector<ExpressionPtr>& expressions) {
	assert (type->getSize().getType() == IntTypeParam::CONCRETE && type->getSize().getValue() == expressions.size() && "Invalid vector type specified!");
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
					return StructExpr::Member(m.first, mapper.map(index++, m.second));
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

	std::size_t hashStructMember(size_t seed, const StructExpr::Members& members) {
		std::for_each(members.cbegin(), members.cend(), [&](const StructExpr::Member &m) { boost::hash_combine(seed, m); });
		return seed;
	}

}

// ------------------------------------- StructExpr ---------------------------------

StructExpr::StructExpr(const StructTypePtr& type, const Members& members)
	: Expression(NT_StructExpr, type, ::hashStructMember(HASHVAL_STRUCTEXPR, members)), members(isolateMembers(members)) { }

StructExpr* StructExpr::createCopyUsing(NodeMapping& mapper) const {
	return new StructExpr(static_pointer_cast<const StructType>(mapper.map(0, type)), copyMembersUsing(mapper, 1, members));
}

bool StructExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const StructExpr& rhs = static_cast<const StructExpr&>(expr);
	return ::equals(members, rhs.members, [](const Member& l, const Member& r) {
		return l.first == r.first && *l.second == *r.second;
	});
}

Node::OptionChildList StructExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	std::transform(members.cbegin(), members.cend(), back_inserter(*res), extractSecond<Member>());
	return res;
}

std::ostream& StructExpr::printTo(std::ostream& out) const {
	// print struct using member - value pairs
	out << "struct{" << join(", ", members, [](std::ostream& out, const Member& cur) {
		out << cur.first << "=" << *cur.second;
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

std::size_t hashUnionExpr(size_t seed, const Identifier& memberName, const ExpressionPtr& member) {
	boost::hash_combine(seed, memberName);
	boost::hash_combine(seed, member);
	return seed;
}

UnionExpr::UnionExpr(const UnionTypePtr& type, const Identifier& memberName, const ExpressionPtr& member)
	: Expression(NT_UnionExpr, type, ::hashUnionExpr(HASHVAL_UNIONEXPR, memberName, member)), memberName(memberName), member(isolate(member)) { }

UnionExpr* UnionExpr::createCopyUsing(NodeMapping& mapper) const {
	return new UnionExpr(static_pointer_cast<const UnionType>(mapper.map(0, type)), memberName, mapper.map(1,member));
}

bool UnionExpr::equalsExpr(const Expression& expr) const {
	// type is guaranteed by super class
	const UnionExpr& other = static_cast<const UnionExpr&>(expr);

	// check element name and expression
	return memberName == other.memberName && *member == *other.member;
}

Node::OptionChildList UnionExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	res->push_back(member);
	return res;
}

std::ostream& UnionExpr::printTo(std::ostream& out) const {
	// TODO fugly
	//out << "union(" << join(", ", members, [](const Member& m) { return format("%s: %s", m.first.getName().c_str(), toString(*m.second).c_str()); }) << ")";
	return out << "<?>I owe you a union print!</?>";
}

UnionExprPtr UnionExpr::get(NodeManager& manager, const UnionTypePtr& type, const Identifier& memberName, const ExpressionPtr& member) {
	return manager.get(UnionExpr(type, memberName, member));
}

// ------------------------------------- JobExpr ---------------------------------

size_t hashJobExpr(const StatementPtr& defaultStmt, const JobExpr::GuardedStmts& guardedStmts, const JobExpr::LocalDecls& localDecls) {
	size_t seed = HASHVAL_JOBEXPR;
	boost::hash_combine(seed, defaultStmt);
	hashPtrRange(seed, localDecls);
	std::for_each(guardedStmts.cbegin(), guardedStmts.cend(), [&seed](const JobExpr::GuardedStmt& s){
		boost::hash_combine(seed, s.first);
		boost::hash_combine(seed, s.second);
	});
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

JobExpr::JobExpr(const TypePtr& type, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls)
	: Expression(NT_JobExpr, type, ::hashJobExpr(defaultStmt, guardedStmts, localDecls)),
	  localDecls(isolate(localDecls)), guardedStmts(isolateGuardedStmts(guardedStmts)), defaultStmt(isolate(defaultStmt)) {

	// TODO: use ordinary type checks for this section ...
	FunctionTypePtr defaultType = static_pointer_cast<const FunctionType>(defaultStmt->getType());
    assert(defaultType->getArgumentTypes().empty() && "Default statement is not allowed to have any arguments");
    assert(defaultType->getReturnType()->getName() == "unit" && "Return value of default statement must be void.");
    TypeList guardParams = TypeList(2, type->getNodeManager().basic.getUIntGen());

    std::for_each(guardedStmts.cbegin(), guardedStmts.cend(), [&](const JobExpr::GuardedStmt& s){
        //Check guards
    	FunctionTypePtr guardType = static_pointer_cast<const FunctionType>(s.first->getType());
    	assert(guardType->getCaptureTypes().empty() && "Guard must not have any capture variables.");
        assert(::equals(guardType->getArgumentTypes(), guardParams, equal_target<TypePtr>()) && "Guard must have two integer arguments");
        assert(guardType->getReturnType()->getName() == "bool" && "Return value of guard must be bool.");

        //Check guarded statements
        FunctionTypePtr stmtType = static_pointer_cast<const FunctionType>(s.second->getType());
        assert(stmtType->getCaptureTypes().empty() && "Guarded statement is not allowed to have any capture variables.");
        assert(stmtType->getArgumentTypes().empty() && "Guarded statement is not allowed to have any arguments");
        assert(stmtType->getReturnType()->getName() == "unit" && "Return value of guarded statement must be void.");
    });

}



JobExpr* JobExpr::createCopyUsing(NodeMapping& mapper) const {
	return new JobExpr(
			mapper.map(0, type),
			mapper.map(2 + localDecls.size() + guardedStmts.size()*2,defaultStmt),
			copyGuardedStmtsUsing(mapper, 2 + localDecls.size(), guardedStmts),
			mapper.map(1, localDecls));
}

Node::OptionChildList JobExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
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
	return *defaultStmt == *rhs.defaultStmt &&
		::equals(localDecls, rhs.localDecls, equal_target<DeclarationStmtPtr>()) &&
		::equals(guardedStmts, rhs.guardedStmts, [](const GuardedStmt& l, const GuardedStmt& r) {
			return *l.first == *r.first && *l.second == *r.second; } );
}

std::ostream& JobExpr::printTo(std::ostream& out) const {
	out << "job [" << join(", ", localDecls, print<deref<DeclarationStmtPtr>>()) << "] ("
		<< join(", ", guardedStmts, [](std::ostream& out, const GuardedStmt& s) {
	        out << format("(%s, %s)", s.first->toString().c_str(), s.second->toString().c_str());
	    } )
		<< ", " << *defaultStmt << ")";
	return out;
}

JobExprPtr JobExpr::get(NodeManager& manager, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls) {
	auto type = manager.basic.getJob();
	return manager.get(JobExpr(type, defaultStmt, guardedStmts, localDecls));
}

// ------------------------------------- CallExpr ---------------------------------

const TypePtr& getReturnType(const ExpressionPtr& functionExpr) {
	const TypePtr& type = functionExpr->getType();
	assert( dynamic_cast<const FunctionType*>(&*type) && "Non-function type expression used as operator within call Expression!");

	const FunctionType& funType = static_cast<const FunctionType&>(*type);
	return funType.getReturnType();
}

std::size_t hashCallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	size_t seed = HASHVAL_CALLEXPR;
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
	NodePtr parent(this);
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

Node::OptionChildList CallExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	res->push_back(functionExpr);
	std::copy(arguments.cbegin(), arguments.cend(), back_inserter(*res));
	return res;
}

std::ostream& CallExpr::printTo(std::ostream& out) const {
	return out << *functionExpr << "(" << join(", ", arguments, print<deref<ExpressionPtr>>()) << ")";
}

// TODO: re-add with proper result type inference (after everybody has switched to alternative)
//CallExprPtr CallExpr::get(NodeManager& manager, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
//	return manager.get(CallExpr(functionExpr, arguments));
//}

CallExprPtr CallExpr::get(NodeManager& manager, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	return manager.get(CallExpr(resultType, functionExpr, arguments));
}

// ------------------------------------- CastExpr ---------------------------------

std::size_t hashCastExpr(const TypePtr& type, const ExpressionPtr& subExpression) {
	size_t seed = HASHVAL_CASTEXPR;
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

Node::OptionChildList CastExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
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

	std::size_t hashLambda(const FunctionTypePtr& type, const Lambda::CaptureList& captureList,
			const Lambda::ParamList& paramList, const StatementPtr& body) {

		std::size_t hash = HASHVAL_LAMBDA;
		boost::hash_combine(hash, type->hash());
		hashPtrRange(hash, captureList);
		hashPtrRange(hash, paramList);
		boost::hash_combine(hash, body->hash());
		return hash;
	}

}

Lambda::Lambda(const FunctionTypePtr& type, const CaptureList& captureList, const ParamList& paramList, const StatementPtr& body)
	: Node(NT_Lambda, NC_Support, hashLambda(type, captureList, paramList, body)), type(isolate(type)),
	  captureList(isolate(captureList)), paramList(isolate(paramList)), body(isolate(body)) { };

Lambda* Lambda::createCopyUsing(NodeMapping& mapper) const {
	return new Lambda(mapper.map(0, type),
			mapper.map(1, captureList),
			mapper.map(1+captureList.size(), paramList),
			mapper.map(1+captureList.size() + paramList.size(), body)
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
			::equals(captureList, rhs.captureList, equal_target<VariablePtr>()) &&
			::equals(paramList, rhs.paramList, equal_target<VariablePtr>()) &&
			*body == *rhs.body;
}

Node::OptionChildList Lambda::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	res->insert(res->end(), captureList.begin(), captureList.end());
	res->insert(res->end(), paramList.begin(), paramList.end());
	res->push_back(body);
	return res;
}

std::ostream& Lambda::printTo(std::ostream& out) const {
	auto paramPrinter = [](std::ostream& out, const VariablePtr& var) {
		out << *var->getType() << " " << *var;
	};

	out << "fun";
	out << "[" << join(", ", captureList, paramPrinter) << "]";
	out << "(" << join(", ", paramList, paramPrinter) << ") " << *body;

	return out;
}

LambdaPtr Lambda::get(NodeManager& manager, const FunctionTypePtr& type, const ParamList& params, const StatementPtr& body) {
	return get(manager, type, CaptureList(), params, body);
}
LambdaPtr Lambda::get(NodeManager& manager, const FunctionTypePtr& type, const CaptureList& captureList, const ParamList& params, const StatementPtr& body) {
	return manager.get(Lambda(type, captureList, params, body));
}


// ------------------------------------ Lambda Definition ------------------------------

namespace {

	std::size_t hashLambdaDefinition(const LambdaDefinition::Definitions& definitions) {
		std::size_t hash = HASHVAL_LAMBDA_DEFINITION;
		boost::hash_combine(hash, insieme::utils::map::computeHash(definitions, hash_target<VariablePtr>(), hash_target<LambdaPtr>()));
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

Node::OptionChildList LambdaDefinition::getChildNodes() const {
	OptionChildList res(new ChildList());
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
	auto detector = makeLambdaPtrVisitor([&defs](const NodePtr& node)->bool {
		// check node type
		if (node->getNodeType() != NT_Variable) {
			return true;
		}

		// check whether the variable is a recursive function
		return defs.find(static_pointer_cast<const Variable>(node)) == defs.end();
	});

	// run visitor => if interrupted, the definition is recursive
	return visitAllInterruptable(lambda, detector);
}

namespace {

	class RecLambdaUnroller : public NodeMapping {

		NodeManager& manager;
		const LambdaDefinition& definition;
		const LambdaDefinition::Definitions& definitions;


	public:

		RecLambdaUnroller(NodeManager& manager, const LambdaDefinition& definition)
			: manager(manager), definition(definition), definitions(definition.getDefinitions()) { }

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// check whether it is a known variable
			if (ptr->getNodeType() == NT_Variable) {
				VariablePtr var = static_pointer_cast<const Variable>(ptr);

				auto pos = definitions.find(var);
				if (pos != definitions.end()) {
					return LambdaExpr::get(manager, var, LambdaDefinitionPtr(&definition));
				}
			}

			// replace recursively
			return ptr->substitute(manager, *this);
		}

		LambdaPtr apply(const LambdaPtr& node) {
			if (!node) {
				return node;
			}
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
		std::size_t hash = HASHVAL_LAMBDA;
		boost::hash_combine(hash, variable->hash());
		boost::hash_combine(hash, definition->hash());
		return hash;
	}

}

LambdaExpr::LambdaExpr(const VariablePtr& variable, const LambdaDefinitionPtr& definition)
	: Expression(NT_LambdaExpr, variable->getType(), ::hashLambdaExpr(variable, definition)),
	  variable(isolate(variable)), definition(isolate(definition)), lambda(definition->getDefinitionOf(variable)),
	  recursive(definition->isRecursive(variable)) { }

LambdaExpr* LambdaExpr::createCopyUsing(NodeMapping& mapper) const {
	return new LambdaExpr(mapper.map(0, variable), mapper.map(1, definition));
}

Node::OptionChildList LambdaExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
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
	return get(manager, lambda->getType(), lambda->getCaptureList(), lambda->getParameterList(), lambda->getBody());
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const VariablePtr& variable, const LambdaDefinitionPtr& definition) {
	return manager.get(LambdaExpr(variable, definition));
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const FunctionTypePtr& type, const Lambda::ParamList& params, const StatementPtr& body) {
	return get(manager, type, toVector<VariablePtr>(), params, body);
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const FunctionTypePtr& type, const Lambda::CaptureList& captureList, const Lambda::ParamList& params, const StatementPtr& body) {

	// build definitions
	VariablePtr var = Variable::get(manager, type);
	LambdaDefinition::Definitions defs;
	defs.insert(std::make_pair(var, Lambda::get(manager, type, captureList, params, body)));

	return manager.get(LambdaExpr(var, LambdaDefinition::get(manager, defs)));
}

std::ostream& LambdaExpr::printTo(std::ostream& out) const {
	// TODO: special handling for non-recursive functions
	return out << "rec " << *variable << "." << *definition;
}

const Lambda::CaptureList& LambdaExpr::getCaptureList() const {
	return lambda->getCaptureList();
}

const Lambda::ParamList& LambdaExpr::getParameterList() const {
	return lambda->getParameterList();
}

const StatementPtr& LambdaExpr::getBody() const {
	return lambda->getBody();
}


// ------------------------ Capture Initialization Expression ------------------------

namespace {

	std::size_t hashCaptureInitExpr(const ExpressionPtr& lambda, const CaptureInitExpr::Values& values) {
		std::size_t hash = HASHVAL_CAPTURE_INIT;
		boost::hash_combine(hash, lambda->hash());
		hashPtrRange(hash, values);
		return hash;
	}



}

CaptureInitExpr::CaptureInitExpr(const FunctionTypePtr& type, const ExpressionPtr& lambda, const Values& values)
	: Expression(NT_CaptureInitExpr, type, ::hashCaptureInitExpr(lambda, values)),
	  lambda(isolate(lambda)), values(isolate(values)) { }

CaptureInitExpr* CaptureInitExpr::createCopyUsing(NodeMapping& mapper) const {
	return new CaptureInitExpr(
			static_pointer_cast<const FunctionType>(mapper.map(0, type)),
			mapper.map(1, lambda), mapper.map(2, values)
		);
}

Node::OptionChildList CaptureInitExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	res->push_back(lambda);
	std::copy(values.begin(), values.end(), std::back_inserter(*res));
	return res;
}

bool CaptureInitExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CaptureInitExpr& rhs = static_cast<const CaptureInitExpr&>(expr);
	return (*rhs.lambda == *lambda && ::equals(values, values, equal_target<ExpressionPtr>()));
}

CaptureInitExprPtr CaptureInitExpr::get(NodeManager& manager, const ExpressionPtr& lambda, const Values& values) {
	const TypePtr& type = lambda->getType();
	assert(type->getNodeType() == NT_FunctionType && "Lambda has to be of a function type!");
	const FunctionTypePtr& funType = static_pointer_cast<const FunctionType>(type);
	FunctionTypePtr initExprType = FunctionType::get(manager, TypeList(), funType->getArgumentTypes(), funType->getReturnType());
	return manager.get(CaptureInitExpr(initExprType, lambda, values));
}

std::ostream& CaptureInitExpr::printTo(std::ostream& out) const {
	return out << "([" << join(", ", values, print<deref<ExpressionPtr>>()) << "]" << *lambda << ")";
}


// ------------------------ Member Access Expression ------------------------

namespace {

	TypePtr getMemberType(const ExpressionPtr& subExpression, const Identifier& member) {
		TypePtr type = subExpression->getType();
		assert(type->getNodeType() == NT_StructType && "Accessing member of non-struct type!");
		StructTypePtr structType = static_pointer_cast<const StructType>(type);
		TypePtr res = structType->getTypeOfMember(member);
		assert(res && "Accessing non-existing member!");
		return res;
	}

	std::size_t hashMemberAccess(const ExpressionPtr& subExpression, const Identifier& member) {
		std::size_t res = HASHVAL_MEMBER_ACCESS;
		boost::hash_combine(res, subExpression);
		boost::hash_combine(res, member);
		return res;
	}
}

MemberAccessExpr::MemberAccessExpr( const ExpressionPtr& subExpression, const Identifier& member)
	: Expression(NT_MemberAccessExpr, getMemberType(subExpression, member), hashMemberAccess(subExpression, member)),
	  subExpression(isolate(subExpression)), member(member) {}

MemberAccessExpr* MemberAccessExpr::createCopyUsing(NodeMapping& mapper) const {
	return new MemberAccessExpr(mapper.map(0, subExpression), member);
}


bool MemberAccessExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const MemberAccessExpr& rhs = static_cast<const MemberAccessExpr&>(expr);
	return (*rhs.subExpression == *subExpression && rhs.member == member);
}

Node::OptionChildList MemberAccessExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(subExpression);
	return res;
}

std::ostream& MemberAccessExpr::printTo(std::ostream& out) const {
	return out << "(" << *subExpression << "." << member << ")";
}

MemberAccessExprPtr MemberAccessExpr::get(NodeManager& manager, const ExpressionPtr& subExpression, const Identifier& member) {
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
		std::size_t res = HASHVAL_TUPLE_PROJECTION;
		boost::hash_combine(res, subExpression);
		boost::hash_combine(res, index);
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

Node::OptionChildList TupleProjectionExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
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
		std::size_t hash = HASHVAL_MARKER;
		boost::hash_combine(hash, subExpression);
		boost::hash_combine(hash, id);
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

Node::OptionChildList MarkerExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(subExpression);
	return res;
}

std::ostream& MarkerExpr::printTo(std::ostream& out) const {
	return out << "<M id=" << id << ">" << subExpression << "</M>";
}

MarkerExprPtr MarkerExpr::get(NodeManager& manager, const ExpressionPtr& subExpression) {
	return manager.get(MarkerExpr(subExpression, ++counter));
}


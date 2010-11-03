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

#include "expressions.h"

#include "statements.h"

#include "container_utils.h"
#include "functional_utils.h"
#include "string_utils.h"
#include "lang_basic.h"
#include "map_utils.h"

using namespace insieme::core;

enum {
	HASHVAL_LITERAL = 100 /* offset from statements */,
	HASHVAL_VAREXPR, HASHVAL_CALLEXPR, HASHVAL_CASTEXPR, HASHVAL_PARAMEXPR, HASHVAL_LAMBDAEXPR,
	HASHVAL_TUPLEEXPR, HASHVAL_STRUCTEXPR, HASHVAL_UNIONEXPR, HASHVAL_JOBEXPR, HASHVAL_REC_LAMBDA_DEFINITION,
	HASHVAL_REC_LAMBDA, HASHVAL_VECTOREXPR, HASHVAL_VARIABLE
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

// ------------------------------------- LambdaExpr ---------------------------------

std::size_t hashLambdaExpr(const TypePtr& type, const LambdaExpr::ParamList& params, const StatementPtr& body) {
	size_t seed = HASHVAL_LAMBDAEXPR;
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, params);
	boost::hash_combine(seed, body->hash());
	return seed;
}

LambdaExpr::LambdaExpr(const TypePtr& type, const CaptureList& captureList, const ParamList& params, const StatementPtr& body)
		: Expression(NT_LambdaExpr, type, ::hashLambdaExpr(type, params, body)), captureList(isolate(captureList)), params(isolate(params)), body(isolate(body)) { };

LambdaExpr* LambdaExpr::createCopyUsing(NodeMapping& mapper) const {
	return new LambdaExpr(
			mapper.map(0, type),
			mapper.map(1, captureList),
			mapper.map(1 + captureList.size(), params),
			mapper.map(1 + captureList.size() + params.size(), body)
	);
}

bool LambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const LambdaExpr& rhs = static_cast<const LambdaExpr&>(expr);
	return (*body == *rhs.body)
			&& ::equals(captureList, rhs.captureList, equal_target<DeclarationStmtPtr>())
			&& ::equals(params, rhs.params, equal_target<VariablePtr>());
}

Node::OptionChildList LambdaExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	std::copy(captureList.begin(), captureList.end(), back_inserter(*res));
	std::copy(params.begin(), params.end(), back_inserter(*res));
	res->push_back(body);
	return res;
}

std::ostream& LambdaExpr::printTo(std::ostream& out) const {
	out << "fun";
	if (!captureList.empty()) {
		out << "[" << join(", ", captureList, print<deref<DeclarationStmtPtr>>()) << "]";
	}

	return out << "("
		<< join(", ", params, [](std::ostream& out, const VariablePtr& cur)->std::ostream& {
				return out << (*cur->getType()) << " " << *cur;
		}) << "){ " << *body << " }";
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const TypePtr& type, const ParamList& params, const StatementPtr& body) {
	return get(manager, type, toVector<DeclarationStmtPtr>(), params, body);
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const TypePtr& type, const CaptureList& captureList, const ParamList& params, const StatementPtr& body) {
	return manager.get(LambdaExpr(type, captureList, params, body));
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
	TupleType::ElementTypeList elemTypes;
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

const StructExpr::Members& isolateMembers(const StructExpr::Members& members) {
	for_each(members, [](const StructExpr::Member& cur) {
		isolate(cur.second);
	});
	return members;
}

bool StructExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const StructExpr& rhs = static_cast<const StructExpr&>(expr);
	return ::equals(members, rhs.members, [](const Member& l, const Member& r) {
		return l.first == r.first && *l.second == *r.second;
	});
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

// ------------------------------------- StructExpr ---------------------------------

StructExpr::StructExpr(const TypePtr& type, const Members& members)
	: Expression(NT_StructExpr, type, ::hashStructMember(HASHVAL_STRUCTEXPR, members)), members(isolateMembers(members)) { }

StructExpr* StructExpr::createCopyUsing(NodeMapping& mapper) const {
	return new StructExpr(mapper.map(0, type), copyMembersUsing(mapper, 1, members));
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

UnionExpr::UnionExpr(const TypePtr& type, const Identifier& memberName, const ExpressionPtr& member)
	: Expression(NT_UnionExpr, type, ::hashUnionExpr(HASHVAL_UNIONEXPR, memberName, member)), memberName(memberName), member(isolate(member)) { }

UnionExpr* UnionExpr::createCopyUsing(NodeMapping& mapper) const {
	return new UnionExpr(mapper.map(0, type), memberName, mapper.map(1,member));
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

JobExpr::JobExpr(const TypePtr& type, const StatementPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls)
	: Expression(NT_JobExpr, type, ::hashJobExpr(defaultStmt, guardedStmts, localDecls)),
	  localDecls(isolate(localDecls)), guardedStmts(isolateGuardedStmts(guardedStmts)), defaultStmt(isolate(defaultStmt)) { }



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
	// TODO
	//out << "job {" << join(", ", localDecls) << "} ("
	//	<< join(", ", guardedStmts, [](const GuardedStmt& s) { return format("(%s, %s)", toString(*s.first).c_str(), toString(*s.second).c_str()); } )
	//	<< ", " << defaultStmt << ")";
	return out;
}

JobExprPtr JobExpr::get(NodeManager& manager, const StatementPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls) {
	auto type = lang::TYPE_JOB_PTR;
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


// ------------------------------------ Recursive Definition ------------------------------


std::size_t hashRecLambdaDefinition(const RecLambdaDefinition::RecFunDefs& definitions) {
	std::size_t hash = HASHVAL_REC_LAMBDA_DEFINITION;
	boost::hash_combine(hash, insieme::utils::map::computeHash(definitions));
	return hash;
}

const RecLambdaDefinition::RecFunDefs& isolateRecFunDef(const RecLambdaDefinition::RecFunDefs& definitions) {
	for_each(definitions, [](const RecLambdaDefinition::RecFunDefs::value_type& cur) {
			isolate(cur.first);
			isolate(cur.second);
	});
	return definitions;
}

RecLambdaDefinition::RecFunDefs copyRecFunDefUsing(NodeMapping& mapper, unsigned offset, const RecLambdaDefinition::RecFunDefs& definitions) {
	RecLambdaDefinition::RecFunDefs res;
	std::transform(definitions.begin(), definitions.end(), inserter(res, res.end()),
		[&mapper, &offset](const RecLambdaDefinition::RecFunDefs::value_type& cur)->RecLambdaDefinition::RecFunDefs::value_type {
			auto res = RecLambdaDefinition::RecFunDefs::value_type(
					mapper.map(offset, cur.first),
					mapper.map(offset+1, cur.second)
			);
			offset +=2;
			return res;
	});
	return res;
}

RecLambdaDefinition::RecLambdaDefinition(const RecLambdaDefinition::RecFunDefs& definitions)
	: Node(NT_RecLambdaDefinition, NC_Support, hashRecLambdaDefinition(definitions)), definitions(isolateRecFunDef(definitions)) { };

RecLambdaDefinitionPtr RecLambdaDefinition::get(NodeManager& manager, const RecLambdaDefinition::RecFunDefs& definitions) {
	return manager.get(RecLambdaDefinition(definitions));
}

RecLambdaDefinition* RecLambdaDefinition::createCopyUsing(NodeMapping& mapper) const {
	return new RecLambdaDefinition(copyRecFunDefUsing(mapper, 0, definitions));
}

bool RecLambdaDefinition::equals(const Node& other) const {
	// check type
	if (typeid(other) != typeid(RecLambdaDefinition)) {
		return false;
	}

	const RecLambdaDefinition& rhs = static_cast<const RecLambdaDefinition&>(other);
	return insieme::utils::map::equal(definitions, rhs.definitions, equal_target<LambdaExprPtr>());
}

Node::OptionChildList RecLambdaDefinition::getChildNodes() const {
	OptionChildList res(new ChildList());
	std::for_each(definitions.begin(), definitions.end(), [&res](const RecFunDefs::value_type& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	return res;
}

std::ostream& RecLambdaDefinition::printTo(std::ostream& out) const {
	return out << "{" << join(", ", definitions, [](std::ostream& out, const RecFunDefs::value_type& cur) {
		out << *cur.first << "=" << *cur.second;
	}) << "}";
}

LambdaExprPtr RecLambdaDefinition::getDefinitionOf(const VariablePtr& variable) const {
	auto it = definitions.find(variable);
	if (it == definitions.end()) {
		return LambdaExprPtr(NULL);
	}
	return (*it).second;
}

namespace {

	class RecLambdaUnroller : public NodeMapping {

		NodeManager& manager;
		const RecLambdaDefinition& definition;


	public:

		RecLambdaUnroller(NodeManager& manager, const RecLambdaDefinition& definition)
			: manager(manager), definition(definition) { }

		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			// check whether it is a known variable
			if (ptr->getNodeType() == NT_Variable) {
				VariablePtr var = static_pointer_cast<const Variable>(ptr);
				LambdaExprPtr def = definition.getDefinitionOf(var);
				if (def) {
					// construct recursive type ...
					return RecLambdaExpr::get(manager, var, RecLambdaDefinitionPtr(&definition));
				}
			}

			// replace recurively
			return ptr->substitute(manager, *this);
		}

		NodePtr apply(const NodePtr& node) {
			if (!node) {
				return node;
			}
			return node->substitute(manager, *this);
		}

	};

}

LambdaExprPtr RecLambdaDefinition::unrollOnce(NodeManager& manager, const VariablePtr& variable) const {
	return static_pointer_cast<const LambdaExpr>(RecLambdaUnroller(manager, *this).apply(getDefinitionOf(variable)));
}


// ------------------------------------ Recursive Lambda Expr ------------------------------

std::size_t hashRecLambdaExpr(const VariablePtr& variable, const RecLambdaDefinitionPtr& definition) {
	std::size_t hash = HASHVAL_REC_LAMBDA;
	boost::hash_combine(hash, variable->hash());
	boost::hash_combine(hash, definition->hash());
	return hash;
}

RecLambdaExpr::RecLambdaExpr(const VariablePtr& variable, const RecLambdaDefinitionPtr& definition)
	: Expression(NT_RecLambdaExpr, variable->getType(), ::hashRecLambdaExpr(variable, definition)),
	  variable(isolate(variable)), definition(isolate(definition)) { }

RecLambdaExpr* RecLambdaExpr::createCopyUsing(NodeMapping& mapper) const {
	return new RecLambdaExpr(mapper.map(0, variable), mapper.map(1, definition));
}

Node::OptionChildList RecLambdaExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(variable);
	res->push_back(definition);
	return res;
}

bool RecLambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const RecLambdaExpr& rhs = static_cast<const RecLambdaExpr&>(expr);
	return (*rhs.variable == *variable && *rhs.definition == *definition);
}

RecLambdaExprPtr RecLambdaExpr::get(NodeManager& manager, const VariablePtr& variable, const RecLambdaDefinitionPtr& definition) {
	return manager.get(RecLambdaExpr(variable, definition));
}

std::ostream& RecLambdaExpr::printTo(std::ostream& out) const {
	return out << "rec " << *variable << "." << *definition;
}

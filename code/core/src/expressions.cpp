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
#include "string_utils.h"
#include "functional_utils.h"
#include "lang_basic.h"
#include "map_utils.h"

using namespace insieme::core;

enum {
	HASHVAL_LITERAL = 100 /* offset from statements */,
	HASHVAL_VAREXPR, HASHVAL_CALLEXPR, HASHVAL_CASTEXPR, HASHVAL_PARAMEXPR, HASHVAL_LAMBDAEXPR,
	HASHVAL_TUPLEEXPR, HASHVAL_STRUCTEXPR, HASHVAL_UNIONEXPR, HASHVAL_JOBEXPR, HASHVAL_REC_LAMBDA_DEFINITION,
	HASHVAL_REC_LAMBDA
};

// ------------------------------------- Expression ---------------------------------

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


Literal* Literal::createCloneUsing(NodeManager& manager) const {
	return new Literal(migratePtr(type, manager), value);
}

LiteralPtr Literal::get(NodeManager& manager, const string& value, const TypePtr& type) {
	return manager.get( Literal(type, value) );
}

// ------------------------------------- VarExpr ---------------------------------

std::size_t hashVarExpr(const TypePtr& type, const Identifier& id) {
	size_t seed = HASHVAL_VAREXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

VarExpr::VarExpr(const TypePtr& type, const Identifier& id)
	: Expression(NT_VarExpr, type, ::hashVarExpr(type, id)), id(id) { };

VarExpr::VarExpr(const TypePtr& type, const Identifier& id, const std::size_t& hashCode)
	: Expression(NT_VarExpr, type, hashCode), id(id) { };

VarExpr* VarExpr::createCloneUsing(NodeManager& manager) const {
	return new VarExpr(migratePtr(type, manager), id);
}

bool VarExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const VarExpr& rhs = static_cast<const VarExpr&>(expr);
	return rhs.id == id;
}

std::ostream& VarExpr::printTo(std::ostream& out) const {
	return out << id;
}

VarExprPtr VarExpr::get(NodeManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(VarExpr(type, id));
}

// ------------------------------------- ParamExpr ---------------------------------


std::size_t hashParamExpr(const TypePtr& type, const Identifier& id) {
	size_t seed = HASHVAL_PARAMEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

ParamExpr::ParamExpr(const TypePtr& type, const Identifier& id) : VarExpr(type, id, ::hashParamExpr(type,id)) { };

ParamExpr* ParamExpr::createCloneUsing(NodeManager& manager) const {
	return new ParamExpr(migratePtr(type, manager), id);
}

std::ostream& ParamExpr::printTo(std::ostream& out) const {
	return out << *type << " " << id;
}

ParamExprPtr ParamExpr::get(NodeManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(ParamExpr(type, id));
}

// ------------------------------------- LambdaExpr ---------------------------------

std::size_t hashLambdaExpr(const TypePtr& type, const LambdaExpr::ParamList& params, const StatementPtr& body) {
	size_t seed = HASHVAL_LAMBDAEXPR;
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, params);
	boost::hash_combine(seed, body->hash());
	return seed;
}

LambdaExpr::LambdaExpr(const TypePtr& type, const ParamList& params, const StatementPtr& body)
		: Expression(NT_LambdaExpr, type, ::hashLambdaExpr(type, params, body)), body(body), params(params) { };

LambdaExpr* LambdaExpr::createCloneUsing(NodeManager& manager) const {
	return new LambdaExpr(
			migratePtr(type, manager),
			migrateAllPtr(params, manager),
			migratePtr(body, manager)
	);
}

bool LambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const LambdaExpr& rhs = static_cast<const LambdaExpr&>(expr);
	return (*body == *rhs.body) && std::equal(params.cbegin(), params.cend(), rhs.params.cbegin(), equal_target<ExpressionPtr>());
}

Node::OptionChildList LambdaExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	std::copy(params.cbegin(), params.cend(), back_inserter(*res));
	res->push_back(body);
	return res;
}

std::ostream& LambdaExpr::printTo(std::ostream& out) const {
	return out << "fun(" << join(", ", params, print<deref<ExpressionPtr>>()) << "){ " << *body << " }";
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const TypePtr& type, const ParamList& params, const StatementPtr& body) {
	return manager.get(LambdaExpr(type, params, body));
}

// ------------------------------------- TupleExpr ---------------------------------

std::size_t hashTupleExpr(const TypePtr& type, const vector<ExpressionPtr>& expressions) {
	size_t seed = HASHVAL_TUPLEEXPR;
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, expressions);
	return seed;
}

TupleExpr::TupleExpr(const TypePtr& type, const vector<ExpressionPtr>& expressions)
		: Expression(NT_TupleExpr, type, hashTupleExpr(type, expressions)), expressions(expressions) { };

TupleExpr* TupleExpr::createCloneUsing(NodeManager& manager) const {
	return new TupleExpr(
			migratePtr(type, manager),
			migrateAllPtr(expressions, manager)
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
	return manager.get(TupleExpr(TupleType::get(manager, elemTypes), expressions));
}

// ------------------------------------- NamedCompositeExpr ---------------------------------

NamedCompositeExpr::NamedCompositeExpr(NodeType nodeType, const TypePtr& type, size_t hashval, const Members& members)
	: Expression(nodeType, type, hashval), members(members) { }

bool NamedCompositeExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const NamedCompositeExpr& rhs = static_cast<const NamedCompositeExpr&>(expr);
	return ::equals(members, rhs.members, [](const Member& l, const Member& r) {
		return l.first == r.first && *l.second == *r.second; });
}

NamedCompositeExpr::Members NamedCompositeExpr::getManagedMembers(NodeManager& manager) const {
	Members managedMembers;
	std::transform(members.cbegin(), members.cend(), std::back_inserter(managedMembers), [&manager, this](const Member& m) {
		return NamedCompositeExpr::Member(m.first, this->migratePtr(m.second, manager)); });
	return managedMembers;
}

NamedCompositeType::Entries NamedCompositeExpr::getTypeEntries(const Members& mem) {
	NamedCompositeType::Entries entries;

	// NOTE: transform would be equivalent but causes an internal error in GCC 4.5.1
//	std::transform(mem.begin(), mem.end(), back_inserter(entries), [](const Member& m)
//		{ return NamedCompositeType::Entry(m.first, m.second->getType()); } );

	std::for_each(mem.begin(), mem.end(), [&entries](const Member& m) {
		entries.push_back(NamedCompositeType::Entry(m.first, m.second->getType()));
	});
	return entries;
}

std::size_t hashStructOrUnionExpr(size_t seed, const StructExpr::Members& members) {
	std::for_each(members.cbegin(), members.cend(), [&](const StructExpr::Member &m) { boost::hash_combine(seed, m); });
	return seed;
}

Node::OptionChildList NamedCompositeExpr::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	std::transform(members.cbegin(), members.cend(), back_inserter(*res), extractSecond<Member>());
	return res;
}

// ------------------------------------- StructExpr ---------------------------------

StructExpr::StructExpr(const TypePtr& type, const Members& members)
	: NamedCompositeExpr(NT_StructExpr, type, ::hashStructOrUnionExpr(HASHVAL_STRUCTEXPR, members), members) { }

StructExpr* StructExpr::createCloneUsing(NodeManager& manager) const {
	return new StructExpr(migratePtr(type, manager), getManagedMembers(manager));
}

std::ostream& StructExpr::printTo(std::ostream& out) const {
	// TODO fugly
	//out << "struct(" << join(", ", members, [](const Member& m) { return format("%s: %s", m.first.getName().c_str(), toString(*m.second).c_str()); }) << ")";
	return out;
}

StructExprPtr StructExpr::get(NodeManager& manager, const Members& members) {
	return manager.get(StructExpr(StructType::get(manager, getTypeEntries(members)), members));
}

// ------------------------------------- UnionExpr ---------------------------------

UnionExpr::UnionExpr(const TypePtr& type, const Members& members)
	: NamedCompositeExpr(NT_UnionExpr, type, ::hashStructOrUnionExpr(HASHVAL_UNIONEXPR, members), members) { }

UnionExpr* UnionExpr::createCloneUsing(NodeManager& manager) const {
	return new UnionExpr(migratePtr(type, manager), getManagedMembers(manager));
}

std::ostream& UnionExpr::printTo(std::ostream& out) const {
	// TODO fugly
	//out << "union(" << join(", ", members, [](const Member& m) { return format("%s: %s", m.first.getName().c_str(), toString(*m.second).c_str()); }) << ")";
	return out;
}

UnionExprPtr UnionExpr::get(NodeManager& manager, const Members& members) {
	return manager.get(UnionExpr(UnionType::get(manager, getTypeEntries(members)), members));
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

JobExpr::JobExpr(const TypePtr& type, const StatementPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecls)
	: Expression(NT_JobExpr, type, ::hashJobExpr(defaultStmt, guardedStmts, localDecls)),
	  localDecls(localDecls), guardedStmts(guardedStmts), defaultStmt(defaultStmt) { }

JobExpr* JobExpr::createCloneUsing(NodeManager& manager) const {
	GuardedStmts localGuardedStmts;
	std::transform(guardedStmts.cbegin(), guardedStmts.cend(), back_inserter(localGuardedStmts),
		[&manager,this](const GuardedStmt& stmt) {
			return JobExpr::GuardedStmt(
					this->migratePtr(stmt.first, manager),
					this->migratePtr(stmt.second, manager));
	} );
	return new JobExpr(migratePtr(type, manager), manager.get(defaultStmt), localGuardedStmts, manager.getAll(localDecls));
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
	auto type = GenericType::get(manager, "Job"); // TODO
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
		: Expression(NT_CallExpr, type, ::hashCallExpr(type, functionExpr, arguments)), functionExpr(functionExpr), arguments(arguments) { }

CallExpr::CallExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments)
		: Expression(NT_CallExpr, ::getReturnType(functionExpr), ::hashCallExpr(::getReturnType(functionExpr), functionExpr, arguments)),
		  functionExpr(functionExpr), arguments(arguments) { }

CallExpr* CallExpr::createCloneUsing(NodeManager& manager) const {
	return new CallExpr(
			migratePtr(type, manager),
			migratePtr(functionExpr, manager),
			migrateAllPtr(arguments, manager));
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

CallExprPtr CallExpr::get(NodeManager& manager, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	return manager.get(CallExpr(functionExpr, arguments));
}

CallExprPtr CallExpr::get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	return manager.get(CallExpr(type, functionExpr, arguments));
}

// ------------------------------------- CastExpr ---------------------------------

std::size_t hashCastExpr(const TypePtr& type, const ExpressionPtr& subExpression) {
	size_t seed = HASHVAL_CASTEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, subExpression->hash());
	return seed;
}

CastExpr::CastExpr(const TypePtr& type, const ExpressionPtr& subExpression)
		: Expression(NT_CastExpr, type, hashCastExpr(type, subExpression)), subExpression(subExpression) { }

CastExpr* CastExpr::createCloneUsing(NodeManager& manager) const {
	return new CastExpr(
			migratePtr(type, manager),
			migratePtr(subExpression, manager)
	);
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

RecLambdaDefinition::RecLambdaDefinition(const RecLambdaDefinition::RecFunDefs& definitions)
	: Node(NT_RecLambdaDefinition, hashRecLambdaDefinition(definitions)), definitions(definitions) { };

RecLambdaDefinitionPtr RecLambdaDefinition::get(NodeManager& manager, const RecLambdaDefinition::RecFunDefs& definitions) {
	return manager.get(RecLambdaDefinition(definitions));
}

RecLambdaDefinition* RecLambdaDefinition::createCloneUsing(NodeManager& manager) const {
	RecFunDefs localDefinitions;
	std::transform(definitions.begin(), definitions.end(), inserter(localDefinitions, localDefinitions.end()),
		[&manager, this](const RecFunDefs::value_type& cur) {
			return RecLambdaDefinition::RecFunDefs::value_type(
					this->migratePtr(cur.first, manager),
					this->migratePtr(cur.second, manager)
			);
	});
	return new RecLambdaDefinition(localDefinitions);
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

const LambdaExprPtr RecLambdaDefinition::getDefinitionOf(const VarExprPtr& variable) const {
	auto it = definitions.find(variable);
	if (it == definitions.end()) {
		return LambdaExprPtr(NULL);
	}
	return (*it).second;
}


// ------------------------------------ Recursive Lambda Expr ------------------------------

std::size_t hashRecLambdaExpr(const VarExprPtr& variable, const RecLambdaDefinitionPtr& definition) {
	std::size_t hash = HASHVAL_REC_LAMBDA;
	boost::hash_combine(hash, variable->hash());
	boost::hash_combine(hash, definition->hash());
	return hash;
}

RecLambdaExpr::RecLambdaExpr(const VarExprPtr& variable, const RecLambdaDefinitionPtr& definition)
	: Expression(NT_RecLambdaExpr, variable->getType(), ::hashRecLambdaExpr(variable, definition)), variable(variable), definition(definition) { }

RecLambdaExpr* RecLambdaExpr::createCloneUsing(NodeManager& manager) const {
	return new RecLambdaExpr(
			migratePtr(variable, manager),
			migratePtr(definition, manager)
	);
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

RecLambdaExprPtr RecLambdaExpr::get(NodeManager& manager, const VarExprPtr& variable, const RecLambdaDefinitionPtr& definition) {
	return manager.get(RecLambdaExpr(variable, definition));
}

std::ostream& RecLambdaExpr::printTo(std::ostream& out) const {
	return out << "rec " << *variable << "." << *definition;
}

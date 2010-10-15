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

#include "statements.h"
#include "expressions.h"

#include "container_utils.h"
#include "iterator_utils.h"

#include "lang_basic.h"

namespace insieme {
namespace core {


enum {
	HASHVAL_NOOP, HASHVAL_BREAK, HASHVAL_CONTINUE, HASHVAL_DECLARATION, HASHVAL_RETURN,
	HASHVAL_COMPOUND, HASHVAL_WHILE, HASHVAL_FOR, HASHVAL_IF, HASHVAL_SWITCH
};


// ------------------------------------- Statement ---------------------------------

bool Statement::equals(const Node& node) const {
	// conversion is guaranteed by base Node::operator==
	const Statement& stmt = static_cast<const Statement&>(node);
	// just check for same specialization and invoke statement comparison
	return (typeid(*this) == typeid(stmt)) && equalsStmt(stmt);
}

std::size_t hash_value(const Statement& stmt) {
	return stmt.hash();
}

// ------------------------------------- BreakStmt ---------------------------------

BreakStmt::BreakStmt(): Statement(NT_BreakStmt, HASHVAL_BREAK) {}

std::ostream& BreakStmt::printTo(std::ostream& out) const {
	return out << "break";
}

bool BreakStmt::equalsStmt(const Statement&) const {
	// type has already been checked => all done
	return true;
}

Node::OptionChildList BreakStmt::getChildNodes() const {
	// does not have any sub-nodes
	return OptionChildList(new ChildList());
}

BreakStmt* BreakStmt::createCopyUsing(NodeMapper&) const {
	return new BreakStmt();
}

BreakStmtPtr BreakStmt::get(NodeManager& manager) {
	return manager.get(BreakStmt());
}

// ------------------------------------- ContinueStmt ---------------------------------

ContinueStmt::ContinueStmt() : Statement(NT_ContinueStmt, HASHVAL_CONTINUE) {};

std::ostream& ContinueStmt::printTo(std::ostream& out) const {
	return out << "continue";
}

bool ContinueStmt::equalsStmt(const Statement&) const {
	// type has already been checked => all done
	return true;
}

Node::OptionChildList ContinueStmt::getChildNodes() const {
	// does not have any sub-nodes
	return OptionChildList(new ChildList());
}

ContinueStmt* ContinueStmt::createCopyUsing(NodeMapper&) const {
	return new ContinueStmt();
}

ContinueStmtPtr ContinueStmt::get(NodeManager& manager) {
	return manager.get(ContinueStmt());
}

// ------------------------------------- ReturnStmt ---------------------------------

std::size_t hashReturnStmt(const ExpressionPtr& returnExpression) {
	std::size_t seed = HASHVAL_RETURN;
    boost::hash_combine(seed, returnExpression->hash());
	return seed;
}

ReturnStmt::ReturnStmt(const ExpressionPtr& returnExpression)
	: Statement(NT_ReturnStmt, hashReturnStmt(returnExpression)), returnExpression(isolate(returnExpression)) {
}

std::ostream& ReturnStmt::printTo(std::ostream& out) const {
	return out << "return " << *returnExpression;
}

bool ReturnStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const ReturnStmt& rhs = static_cast<const ReturnStmt&>(stmt);
	return (*returnExpression == *rhs.returnExpression);
}

Node::OptionChildList ReturnStmt::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(returnExpression);
	return res;
}

ReturnStmt* ReturnStmt::createCopyUsing(NodeMapper& mapper) const {
	return new ReturnStmt(mapper.map(returnExpression));
}

ReturnStmtPtr ReturnStmt::get(NodeManager& manager, const ExpressionPtr& returnExpression) {
	return manager.get(ReturnStmt(returnExpression));
}

// ------------------------------------- DeclarationStmt ---------------------------------

std::size_t hashDeclarationStmt(const VarExprPtr& varExpression, const ExpressionPtr& initExpression) {
	std::size_t seed = HASHVAL_DECLARATION;
    boost::hash_combine(seed, varExpression->hash());
    boost::hash_combine(seed, initExpression->hash());
	return seed;
}

DeclarationStmt::DeclarationStmt(const VarExprPtr& varExpression, const ExpressionPtr& initExpression)
	: Statement(NT_DeclarationStmt, hashDeclarationStmt(varExpression, initExpression)), varExpression(isolate(varExpression)), initExpression(isolate(initExpression)) {
}

std::ostream& DeclarationStmt::printTo(std::ostream& out) const {
	return out << *varExpression->getType() << " " << *varExpression << " = " << *initExpression;
}

bool DeclarationStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const DeclarationStmt& rhs = static_cast<const DeclarationStmt&>(stmt);
	return (*varExpression == *rhs.varExpression) && (*initExpression == *rhs.initExpression);
}

DeclarationStmt* DeclarationStmt::createCopyUsing(NodeMapper& mapper) const {
	return new DeclarationStmt(mapper.map(varExpression), mapper.map(initExpression));
}

Node::OptionChildList DeclarationStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	res->push_back(varExpression);
	res->push_back(initExpression);
	return res;
}

DeclarationStmtPtr DeclarationStmt::get(NodeManager& manager, const TypePtr& type, const Identifier& id, const ExpressionPtr& initExpression) {
	return manager.get(DeclarationStmt(VarExpr::get(manager, type, id), initExpression));
}


// ------------------------------------- CompoundStmt ---------------------------------

std::size_t hashCompoundStmt(const vector<StatementPtr>& stmts) {
	std::size_t seed = HASHVAL_COMPOUND;
	hashPtrRange(seed, stmts);
	return seed;
}

CompoundStmt::CompoundStmt(const vector<StatementPtr>& stmts)
	: Statement(NT_CompoundStmt, hashCompoundStmt(stmts)), statements(isolate(stmts)) { }

std::ostream& CompoundStmt::printTo(std::ostream& out) const {
	if (statements.empty()) {
		return out << "{}";
	}
	return out << "{" << join("; ", statements, print<deref<StatementPtr>>()) << ";}";
}

bool CompoundStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const CompoundStmt& rhs = static_cast<const CompoundStmt&>(stmt);
	return ::equals(statements, rhs.statements, equal_target<StatementPtr>());
}


CompoundStmt* CompoundStmt::createCopyUsing(NodeMapper& mapper) const {
	return new CompoundStmt(mapper.map(statements));
}

Node::OptionChildList CompoundStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	std::copy(statements.cbegin(), statements.cend(), back_inserter(*res));
	return res;
}

const StatementPtr&  CompoundStmt::operator[](unsigned index) const {
	return statements[index];
}

CompoundStmtPtr CompoundStmt::get(NodeManager& manager) {
	return manager.get(CompoundStmt(vector<StatementPtr>()));
}
CompoundStmtPtr CompoundStmt::get(NodeManager& manager, const StatementPtr& stmt) {
	return manager.get(CompoundStmt(toVector(stmt)));
}
CompoundStmtPtr CompoundStmt::get(NodeManager& manager, const vector<StatementPtr>& stmts) {
	return manager.get(CompoundStmt(stmts));
}

// ------------------------------------- WhileStmt ---------------------------------

std::size_t hashWhileStmt(const ExpressionPtr& condition, const StatementPtr& body) {
	std::size_t seed = HASHVAL_WHILE;
	boost::hash_combine(seed, condition->hash());
	boost::hash_combine(seed, body->hash());
	return seed;
}

WhileStmt::WhileStmt(const ExpressionPtr& condition, const StatementPtr& body)
	: Statement(NT_WhileStmt, hashWhileStmt(condition, body)), condition(isolate(condition)), body(isolate(body)) {
}

std::ostream& WhileStmt::printTo(std::ostream& out) const {
	return out << "while(" << *condition << ") " << *body;
}
	
bool WhileStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const WhileStmt& rhs = static_cast<const WhileStmt&>(stmt);
	return (*condition == *rhs.condition) && (*body == *rhs.body);
}

WhileStmt* WhileStmt::createCopyUsing(NodeMapper& mapper) const {
	return new WhileStmt(mapper.map(condition), mapper.map(body));
}

Node::OptionChildList WhileStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	res->push_back(condition);
	res->push_back(body);
	return res;
}

WhileStmtPtr WhileStmt::get(NodeManager& manager, const ExpressionPtr& condition, const StatementPtr& body) {
	return manager.get(WhileStmt(condition, body));
}

// ------------------------------------- ForStmt ---------------------------------

std::size_t hashForStmt(const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step) {
	std::size_t seed = HASHVAL_FOR;
	boost::hash_combine(seed, declaration->hash());
	boost::hash_combine(seed, body->hash());
	boost::hash_combine(seed, end->hash());
	boost::hash_combine(seed, step->hash());
	return seed;
}

ForStmt::ForStmt(const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step)
	: Statement(NT_ForStmt, hashForStmt(declaration, body, end, step)), declaration(isolate(declaration)), body(isolate(body)), end(isolate(end)), step(isolate(step)) {}
	
std::ostream& ForStmt::printTo(std::ostream& out) const {
	return out << "for(" << *declaration << " .. " << *end << " : " << *step << ") " << *body;
}

bool ForStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const ForStmt& rhs = static_cast<const ForStmt&>(stmt);
	return (*declaration == *rhs.declaration) && (*body == *rhs.body) 
		&& (*end == *rhs.end) && (*step == *rhs.step);
}

ForStmt* ForStmt::createCopyUsing(NodeMapper& mapper) const {
	return new ForStmt(
			mapper.map(declaration),
			mapper.map(body),
			mapper.map(end),
			mapper.map(step)
	);
}

Node::OptionChildList ForStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	res->push_back(declaration);
	res->push_back(end);
	res->push_back(step);
	res->push_back(body);
	return res;
}

ForStmtPtr ForStmt::get(NodeManager& manager, const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end,
		const ExpressionPtr& step) {
	return manager.get(ForStmt(declaration, body, end, step));
}

// ------------------------------------- IfStmt ---------------------------------

std::size_t hashIfStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) {
	std::size_t seed = HASHVAL_IF;
	boost::hash_combine(seed, condition->hash());
	boost::hash_combine(seed, thenBody->hash());
	boost::hash_combine(seed, elseBody->hash());
	return seed;
}

IfStmt::IfStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) :
	Statement(NT_IfStmt, hashIfStmt(condition, thenBody, elseBody)), condition(isolate(condition)), thenBody(isolate(thenBody)), elseBody(isolate(elseBody)) {
}

IfStmt* IfStmt::createCopyUsing(NodeMapper& mapper) const {
	return new IfStmt(
			mapper.map(condition),
			mapper.map(thenBody),
			mapper.map(elseBody)
	);
}

std::ostream& IfStmt::printTo(std::ostream& out) const {
	return out << "if(" << *condition << ") " << *thenBody << " else " << *elseBody;
}

bool IfStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const IfStmt& rhs = static_cast<const IfStmt&>(stmt);
	return (*condition == *rhs.condition) && (*thenBody == *rhs.thenBody) && (*elseBody == *rhs.elseBody);
}

Node::OptionChildList IfStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	res->push_back(condition);
	res->push_back(thenBody);
	res->push_back(elseBody);
	return res;
}

IfStmtPtr IfStmt::get(NodeManager& manager, const ExpressionPtr& condition, const StatementPtr& body) {
	// default to empty else block
	return get(manager, condition, body, lang::STMT_NO_OP_PTR);
}

IfStmtPtr IfStmt::get(NodeManager& manager, const ExpressionPtr& condition, const StatementPtr& body, const StatementPtr& elseBody) {
	// default to empty else block
	return manager.get(IfStmt(condition, body, elseBody));
}

// ------------------------------------- SwitchStmt ---------------------------------

namespace { // some anonymous utilities for the switch statement

	std::size_t hashSwitchStmt(const ExpressionPtr& switchExpr, const vector<SwitchStmt::Case>& cases, const StatementPtr& defaultCase) {
		std::size_t seed = HASHVAL_SWITCH;
		boost::hash_combine(seed, switchExpr->hash());
		std::for_each(cases.begin(), cases.end(), [&seed](const SwitchStmt::Case& cur) {
			boost::hash_combine(seed, cur.first->hash());
			boost::hash_combine(seed, cur.second->hash());
		});
		boost::hash_combine(seed, defaultCase->hash());
		return seed;
	}

	const vector<SwitchStmt::Case>& isolateSwitchCases(const vector<SwitchStmt::Case>& cases) {
		for_each(cases, [](const SwitchStmt::Case& cur) {
			isolate(cur.first);
			isolate(cur.second);
		});
		return cases;
	}

	vector<SwitchStmt::Case> copySwitchCasesUsing(NodeMapper& mapper, const vector<SwitchStmt::Case>& cases) {
		vector<SwitchStmt::Case> res;
		std::transform(cases.begin(), cases.end(), back_inserter(res), [&mapper](const SwitchStmt::Case& cur) {
			return SwitchStmt::Case(mapper.map(cur.first), mapper.map(cur.second));
		});
		return res;
	}
}

SwitchStmt::SwitchStmt(const ExpressionPtr& switchExpr, const vector<Case>& cases, const StatementPtr& defaultCase) :
	Statement(NT_SwitchStmt, hashSwitchStmt(switchExpr, cases, defaultCase)), switchExpr(isolate(switchExpr)), cases(isolateSwitchCases(cases)), defaultCase(isolate(defaultCase)) {
}

SwitchStmt* SwitchStmt::createCopyUsing(NodeMapper& mapper) const {
	return new SwitchStmt( mapper.map(switchExpr), copySwitchCasesUsing(mapper, cases), mapper.map(defaultCase) );
}

std::ostream& SwitchStmt::printTo(std::ostream& out) const {
	out << "switch(" << *switchExpr << ") [ ";
	std::for_each(cases.cbegin(), cases.cend(), [&out](const Case& cur) { 
		out << "case " << *(cur.first) << ": " << *(cur.second) << " | ";
	});
	out << "default: " << *defaultCase << " ]";
	return out;
}

bool SwitchStmt::equalsStmt(const Statement& stmt) const {
	// conversion is guaranteed by base equals
	const SwitchStmt& rhs = static_cast<const SwitchStmt&>(stmt);
	return ::equals(cases, rhs.cases,
		[](const Case& l, const Case& r) {
			return *l.first == *r.first && *l.second == *r.second;
		}) && (*defaultCase == *rhs.defaultCase);
}

Node::OptionChildList SwitchStmt::getChildNodes() const {
	// does not have any sub-nodes
	OptionChildList res(new ChildList());
	res->push_back(switchExpr);
	std::for_each(cases.cbegin(), cases.cend(), [&res](const Case& cur) {
		res->push_back(cur.first);
		res->push_back(cur.second);
	});
	res->push_back(defaultCase);
	return res;
}

SwitchStmtPtr SwitchStmt::get(NodeManager& manager, const ExpressionPtr& switchExpr, const vector<Case>& cases) {
	return get(manager, switchExpr, cases, lang::STMT_NO_OP_PTR);
}

SwitchStmtPtr SwitchStmt::get(NodeManager& manager, const ExpressionPtr& switchExpr, const vector<Case>& cases, const StatementPtr& defaultCase) {
	return manager.get(SwitchStmt(switchExpr, cases, defaultCase));
}

} // end namespace core
} // end namespace insieme


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

#include "iterator_utils.h"


// ------------------------------------- Statement ---------------------------------

bool Statement::operator==(const Statement& stmt) const {
	return (typeid(*this) == typeid(stmt)) && (hash() == stmt.hash()) && equals(stmt);
}

bool Statement::operator!=(const Statement& stmt) const {
	return !(*this == stmt);
}

Statement::ChildList Statement::getChildren() const {
	return makeChildList();
}

std::size_t hash_value(const Statement& stmt) {
	return stmt.hash();
}

std::ostream& operator<<(std::ostream& out, const Statement& stmt) {
	stmt.printTo(out);
	return out;
}

std::ostream& operator<<(std::ostream& out, const StmtPtr& stmtPtr) {
	//out << "/* StmtPtr " << &stmtPtr << " */";
	stmtPtr->printTo(out);
	return out;
}


// ------------------------------------- NoOpStmt ---------------------------------

void NoOpStmt::printTo(std::ostream& out) const {
	out << "{ /* NoOp */ };";
}

bool NoOpStmt::equals(const Statement&) const {
	return true;
}

std::size_t NoOpStmt::hash() const {
	return HASHVAL_NOOP;
}

NoOpStmt* NoOpStmt::clone(StatementManager&) const {
	return new NoOpStmt();
}

NoOpStmtPtr NoOpStmt::get(StatementManager& manager) {
	return manager.getStmtPtr(NoOpStmt());
}

// ------------------------------------- BreakStmt ---------------------------------

void BreakStmt::printTo(std::ostream& out) const {
	out << "break;";
}

bool BreakStmt::equals(const Statement&) const {
	return true;
}

std::size_t BreakStmt::hash() const {
	return HASHVAL_BREAK;
}

BreakStmt* BreakStmt::clone(StatementManager&) const {
	return new BreakStmt();
}

BreakStmtPtr BreakStmt::get(StatementManager& manager) {
	return manager.getStmtPtr(BreakStmt());
}

// ------------------------------------- ContinueStmt ---------------------------------

void ContinueStmt::printTo(std::ostream& out) const {
	out << "continue;";
}

bool ContinueStmt::equals(const Statement&) const {
	return true;
}

std::size_t ContinueStmt::hash() const {
	return HASHVAL_CONTINUE;
}

ContinueStmt* ContinueStmt::clone(StatementManager&) const {
	return new ContinueStmt();
}

ContinueStmtPtr ContinueStmt::get(StatementManager& manager) {
	return manager.getStmtPtr(ContinueStmt());
}

// ------------------------------------- DeclarationStmt ---------------------------------

DeclarationStmt::DeclarationStmt(const TypePtr& type, const Identifier& id, const ExprPtr& initExpression) 
	: type(type), id(id), initExpression(initExpression) { 
}

void DeclarationStmt::printTo(std::ostream& out) const {
	out << type << " = " << initExpression << ";";
}

bool DeclarationStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const DeclarationStmt& rhs = dynamic_cast<const DeclarationStmt&>(stmt); 
	return (type == rhs.type) && (initExpression == rhs.initExpression);
}

std::size_t DeclarationStmt::hash() const {
	std::size_t seed = HASHVAL_DECLARATION;
	boost::hash_combine(seed, id.hash());
    boost::hash_combine(seed, type->hash());
    boost::hash_combine(seed, initExpression->hash());
	return seed;
}

DeclarationStmt* DeclarationStmt::clone(StatementManager& manager) const {
	return new DeclarationStmt(manager.getTypeManager().get(type), id, manager.getStmtPtr(*initExpression));
}

DeclarationStmt::ChildList DeclarationStmt::getChildren() const {
	return makeChildList(initExpression);
}

DeclarationStmtPtr DeclarationStmt::get(StatementManager& manager, const TypePtr& type, const Identifier& id, const ExprPtr& initExpression) {
	return manager.getStmtPtr(DeclarationStmt(type, id, initExpression));
}

// ------------------------------------- ReturnStmt ---------------------------------

ReturnStmt::ReturnStmt(const ExprPtr& returnExpression)
	: returnExpression(returnExpression) { 
}

void ReturnStmt::printTo(std::ostream& out) const {
	out << "return " << returnExpression << ";";
}

bool ReturnStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const ReturnStmt& rhs = dynamic_cast<const ReturnStmt&>(stmt); 
	return (returnExpression == rhs.returnExpression);
}

std::size_t ReturnStmt::hash() const {
	std::size_t seed = HASHVAL_RETURN;
    boost::hash_combine(seed, returnExpression->hash());
	return seed;
}

ReturnStmt* ReturnStmt::clone(StatementManager& manager) const {
	return new ReturnStmt(manager.getStmtPtr(*returnExpression));
}

ReturnStmt::ChildList ReturnStmt::getChildren() const {
	return makeChildList(returnExpression);
}

ReturnStmtPtr ReturnStmt::get(StatementManager& manager, const ExprPtr& returnExpression) {
	return manager.getStmtPtr(ReturnStmt(returnExpression));
}

// ------------------------------------- CompoundStmt ---------------------------------

void CompoundStmt::printTo(std::ostream& out) const {
	out << "{\n";
	std::for_each(statements.cbegin(), statements.cend(), [&out](const StmtPtr& cur) { out << cur << "\n";});
	out << "}\n";
}

bool CompoundStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const CompoundStmt& rhs = dynamic_cast<const CompoundStmt&>(stmt);
	//auto start = make_paired_iterator(statements.begin(), rhs.statements.begin());
	//auto end = make_paired_iterator(statements.end(), rhs.statements.end());
	//return all(start, end, [](std::pair<StmtPtr,StmtPtr> elems) { return elems.first == elems.second; } );
	return statements == rhs.statements;
}

std::size_t CompoundStmt::hash() const {
	std::size_t seed = HASHVAL_COMPOUND;
	std::for_each(statements.begin(), statements.end(), [&seed](StmtPtr cur) { 
		boost::hash_combine(seed, cur->hash());
	} );
	return seed;
}

CompoundStmt* CompoundStmt::clone(StatementManager& manager) const {
	return new CompoundStmt(manager.getAll(statements));
}

CompoundStmt::ChildList CompoundStmt::getChildren() const {
	return makeChildList(statements);
}

const StmtPtr&  CompoundStmt::operator[](unsigned index) const {
	return statements[index];
}

CompoundStmtPtr CompoundStmt::get(StatementManager& manager) {
	return manager.getStmtPtr(CompoundStmt());
}
CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const StmtPtr& stmt) {
	return manager.getStmtPtr(CompoundStmt(stmt));
}
CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const vector<StmtPtr>& stmts) {
	return manager.getStmtPtr(CompoundStmt(stmts));
}

// ------------------------------------- WhileStmt ---------------------------------

WhileStmt::WhileStmt(ExprPtr condition, StmtPtr body)
	: condition(condition), body(body) {	
}

void WhileStmt::printTo(std::ostream& out) const {
	out << "while(" << condition << ") " << body << ";";
}
	
bool WhileStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const WhileStmt& rhs = dynamic_cast<const WhileStmt&>(stmt);
	return (condition == rhs.condition) && (body == rhs.body);
}

std::size_t WhileStmt::hash() const {
	std::size_t seed = HASHVAL_WHILE;
	boost::hash_combine(seed, condition->hash());
	boost::hash_combine(seed, body->hash());
	return seed;
}

WhileStmt* WhileStmt::clone(StatementManager& manager) const {
	return new WhileStmt(manager.getStmtPtr(*condition), manager.getStmtPtr(*body));
}

WhileStmt::ChildList WhileStmt::getChildren() const {
	auto ret = makeChildList(condition);
	ret->push_back(body);
	return ret;
}

WhileStmtPtr WhileStmt::get(StatementManager& manager, ExprPtr condition, StmtPtr body) {
	return manager.getStmtPtr(WhileStmt(condition, body));
}

// ------------------------------------- ForStmt ---------------------------------

ForStmt::ForStmt(DeclarationStmtPtr declaration, StmtPtr body, ExprPtr end, ExprPtr step) :
	declaration(declaration), body(body), end(end), step(step) {
}
	
void ForStmt::printTo(std::ostream& out) const {
	out << "for(" << declaration << ".." << end << ":" << step << ") " << body;
}

bool ForStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const ForStmt& rhs = dynamic_cast<const ForStmt&>(stmt);
	return (declaration == rhs.declaration) && (body == rhs.body) 
		&& (end == rhs.end) && (step == rhs.step);
}

std::size_t ForStmt::hash() const {
	std::size_t seed = HASHVAL_FOR;
	boost::hash_combine(seed, declaration->hash());
	boost::hash_combine(seed, body->hash());
	boost::hash_combine(seed, end->hash());
	boost::hash_combine(seed, step->hash());
	return seed;
}

ForStmt* ForStmt::clone(StatementManager& manager) const {
	return new ForStmt(manager.getStmtPtr(*declaration), manager.getStmtPtr(*body), 
		manager.getStmtPtr(*end), manager.getStmtPtr(*step));
}

ForStmt::ChildList ForStmt::getChildren() const {
	auto ret = makeChildList(declaration);
	ret->push_back(end);
	ret->push_back(step);
	ret->push_back(body);
	return ret;
}

ForStmtPtr ForStmt::get(StatementManager& manager, DeclarationStmtPtr declaration, StmtPtr body, ExprPtr end, ExprPtr step) {
	if(!step) /* TODO PT */;
	return manager.getStmtPtr(ForStmt(declaration, body, end, step));
}

// ------------------------------------- IfStmt ---------------------------------

IfStmt::IfStmt(ExprPtr condition, StmtPtr body, StmtPtr elseBody) :
	condition(condition), body(body), elseBody(elseBody) {
}

IfStmt* IfStmt::clone(StatementManager& manager) const {
	return new IfStmt(manager.getStmtPtr(*condition), manager.getStmtPtr(*body), manager.getStmtPtr(*elseBody));
}

void IfStmt::printTo(std::ostream& out) const {
	out << "if(" << condition << ") " << body << "else " << elseBody << ";";
}

bool IfStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const IfStmt& rhs = dynamic_cast<const IfStmt&>(stmt);
	return (condition == rhs.condition) && (body == rhs.body) && (elseBody == rhs.elseBody);
}

std::size_t IfStmt::hash() const {
	std::size_t seed = HASHVAL_IF;
	boost::hash_combine(seed, condition->hash());
	boost::hash_combine(seed, body->hash());
	boost::hash_combine(seed, elseBody->hash());
	return seed;
}

IfStmt::ChildList IfStmt::getChildren() const {
	auto ret = makeChildList(condition);
	ret->push_back(body);
	ret->push_back(elseBody);
	return ret;
}
	
IfStmtPtr IfStmt::get(StatementManager& manager, ExprPtr condition, StmtPtr body, StmtPtr elseBody) {
	// default to empty else block
	if(!elseBody) elseBody = CompoundStmt::get(manager);
	return manager.getStmtPtr(IfStmt(condition, body, elseBody));
}

// ------------------------------------- SwitchStmt ---------------------------------

SwitchStmt::SwitchStmt(ExprPtr switchExpr, const vector<Case>& cases) :
	switchExpr(switchExpr), cases(cases) {
}

SwitchStmt* SwitchStmt::clone(StatementManager& manager) const {
	vector<Case> localCases;
	std::for_each(cases.cbegin(), cases.cend(), [&](const Case& cur) {
		localCases.push_back(SwitchStmt::Case(manager.getStmtPtr(*cur.first), manager.getStmtPtr(*cur.second)));
	});
	return new SwitchStmt(manager.getStmtPtr(*switchExpr), localCases);
}

void SwitchStmt::printTo(std::ostream& out) const {
	out << "switch(" << *switchExpr << ") ";
	std::for_each(cases.cbegin(), cases.cend(), [&out](const Case& cur) { 
		out << *(cur.first) << ": " << *(cur.second) << "\n";
	});
}

bool SwitchStmt::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const SwitchStmt& rhs = dynamic_cast<const SwitchStmt&>(stmt);
	return cases == rhs.cases;
}

std::size_t SwitchStmt::hash() const {
	std::size_t seed = HASHVAL_SWITCH;
	boost::hash_combine(seed, switchExpr->hash());
	std::for_each(cases.begin(), cases.end(), [&seed](Case cur) { 
		boost::hash_combine(seed, cur.first->hash());
		boost::hash_combine(seed, cur.second->hash());
	});
	return seed;
}

SwitchStmt::ChildList SwitchStmt::getChildren() const {
	auto ret = makeChildList(switchExpr);
	std::for_each(cases.begin(), cases.end(), [&ret](Case cur) { 
		ret->push_back(cur.first);
		ret->push_back(cur.second);
	});
	return ret;
}

SwitchStmtPtr SwitchStmt::get(StatementManager& manager, ExprPtr switchExpr, const vector<Case>& cases) {
	return manager.getStmtPtr(SwitchStmt(switchExpr, cases));
}

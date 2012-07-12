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

#include <gtest/gtest.h>

#include "insieme/analysis/cmake_config.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"
#include "insieme/analysis/cfg.h"

#include "insieme/frontend/program.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include <boost/graph/breadth_first_search.hpp>

using namespace insieme::core;
using namespace insieme::analysis;

template <class Tag>
struct vertex_orderer : public boost::base_visitor<vertex_orderer<Tag> >
{
	typedef Tag event_filter;

	vertex_orderer(std::vector<CFG::VertexTy>& bfs) : bfs(bfs) { }

	template <class Vertex, class Graph>
	void operator()(Vertex v, Graph& g) {
		bfs.push_back( v );
	}

private:
	std::vector<CFG::VertexTy>& bfs;
};

template <class Tag>
inline vertex_orderer<Tag> order_blocks(std::vector<CFG::VertexTy>& bfs, Tag) {
	return vertex_orderer<Tag>(bfs);
}

void CHECK_CONNECTED(const CFG::VertexTy& U, const std::vector<CFG::VertexTy>& V, CFGPtr G) {
	for(std::vector<CFG::VertexTy>::const_iterator it = V.begin(), end = V.end(); it != end; ++it)
		EXPECT_TRUE(boost::edge(U, *it, G->getRawGraph()).second);
}

void CHECK_NOT_CONNECTED(const CFG::VertexTy& U, const std::vector<CFG::VertexTy>& V, CFGPtr G) {
	for(std::vector<CFG::VertexTy>::const_iterator it = V.begin(), end = V.end(); it != end; ++it)
		EXPECT_FALSE(boost::edge(U, *it, G->getRawGraph()).second);
}

TEST(CFGBuilder, CompoundStmtMulti) {

	NodeManager manager;

	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	LiteralPtr stmt1 = Literal::get(manager, manager.getLangBasic().getInt4(), "100");
	LiteralPtr stmt2 = Literal::get(manager, manager.getLangBasic().getInt4(), "200");
	LiteralPtr stmt3 = Literal::get(manager, manager.getLangBasic().getInt4(), "300");

	CompoundStmtPtr cs0 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1, stmt2, stmt3));
	CompoundStmtPtr cs1 = CompoundStmt::get(manager, toVector<StatementPtr>(cs0, stmt1));
	CompoundStmtPtr cs2 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt2, cs1));

	CFGPtr cfg = CFG::buildCFG<MultiStmtPerBasicBlock>(cs2);
	EXPECT_EQ(3u, cfg->size());

	CFG::VertexTy entry = cfg->entry();
	const cfg::Block& block = **cfg->successors_begin(entry);

	cfg::Block::const_iterator it = block.stmt_begin(), end = block.stmt_end();
	EXPECT_EQ(stmt2, *(it++));
	EXPECT_TRUE(it != end);

	EXPECT_EQ(stmt1, *(it++));
	EXPECT_TRUE(it != end);

	EXPECT_EQ(stmt2, *(it++));
	EXPECT_TRUE(it != end);

	EXPECT_EQ(stmt3, *(it++));
	EXPECT_TRUE(it != end);

	EXPECT_EQ(stmt1, *(it++));
	EXPECT_TRUE(it == end);
}

TEST(CFGBuilder, CompoundStmtSingle) {

	NodeManager manager;

	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	VariablePtr stmt1 = Variable::get(manager, manager.getLangBasic().getInt4(), 1);
	VariablePtr stmt2 = Variable::get(manager, manager.getLangBasic().getInt4(), 2);
	VariablePtr stmt3 = Variable::get(manager, manager.getLangBasic().getInt4(), 3);

	CompoundStmtPtr cs0 = CompoundStmt::get(manager, toVector<StatementPtr>( stmt1, stmt2, stmt3 ));
	CompoundStmtPtr cs1 = CompoundStmt::get(manager, toVector<StatementPtr>( cs0, stmt1 ));
	CompoundStmtPtr cs2 = CompoundStmt::get(manager, toVector<StatementPtr>( stmt2, cs1 ));

	CFGPtr cfg = CFG::buildCFG(cs2);
	EXPECT_EQ(7u, cfg->size());

	CFG::VertexTy entry = cfg->entry();
	CFG::SuccessorsIterator&& it = cfg->successors_begin(entry), end = cfg->successors_end(entry);

	// visit STMT2
	EXPECT_EQ(1u, it->size());
	EXPECT_EQ(stmt2, (**it)[0]);

	it = cfg->successors_begin(**it); end = cfg->successors_end(**it);
	EXPECT_TRUE(it != end);

	// visit { STMT1, STMT2, STMT3}
	EXPECT_EQ(1u, it->size());
	EXPECT_EQ(stmt1, (**it)[0]);

	it = cfg->successors_begin(**it); end = cfg->successors_end(**it);
	EXPECT_TRUE(it != end);

	EXPECT_EQ(1u, it->size());
	EXPECT_EQ(stmt2, (**it)[0]);

	it = cfg->successors_begin(**it); end = cfg->successors_end(**it);

	EXPECT_TRUE(it != end);

	EXPECT_EQ(1u, it->size());
	EXPECT_EQ(stmt3, (**it)[0]);

	// visit STMT1
	it = cfg->successors_begin(**it); end = cfg->successors_end(**it);

	EXPECT_EQ(1u, it->size());
	EXPECT_EQ(stmt1, (**it)[0]);
}


IfStmtPtr buildIfStmt1(NodeManager& mgr) {
	IRBuilder builder(mgr);
	LiteralPtr literal1 = Literal::get(mgr, mgr.getLangBasic().getInt4(), "12");
	LiteralPtr literal2 = Literal::get(mgr, mgr.getLangBasic().getInt8(), "1222");
	LiteralPtr boolVal = Literal::get(mgr, mgr.getLangBasic().getBool(), "true");

	return builder.ifStmt(boolVal, literal1, literal2);
}

TEST(CFGBuilder, IfStmt1) {

	NodeManager manager;
	IfStmtPtr ifStmt = buildIfStmt1(manager);
	CFGPtr cfg = CFG::buildCFG<MultiStmtPerBasicBlock>(ifStmt);

	// print the graph on standard output
	// std::cout << *cfg;

	EXPECT_EQ(5u, cfg->size());

	enum DFS_ORDER{ ENTRY, IF, THEN, ELSE, EXIT };
	std::vector<CFG::VertexTy> blocks;

	CFG::VertexTy entry = cfg->entry();
	boost::breadth_first_search
	    ( cfg->getRawGraph(), entry,
	    	visitor( boost::make_bfs_visitor( order_blocks(blocks, boost::on_discover_vertex()) ) )
	    );

	// std::copy(blocks.begin(), blocks.end(), std::ostream_iterator<int, char>(std::cout, " "));
	// std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getBlock( blocks[ENTRY] );
	EXPECT_TRUE(entryBlock.empty());
	// ENTRY -> IF
	CHECK_CONNECTED(blocks[ENTRY], { blocks[IF] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ENTRY], { blocks[THEN], blocks[ELSE], blocks[EXIT]}, cfg);

	// IF
	const cfg::Block& ifBlock = cfg->getBlock(blocks[IF]);
	EXPECT_EQ(1u, ifBlock.size());
	EXPECT_TRUE(ifBlock.hasTerminator());
	EXPECT_EQ(*ifBlock.stmt_begin(), ifStmt->getCondition()); // condition
	EXPECT_EQ(ifBlock.terminator(), ifStmt);
	// IF-> THEN && IF -> ELSE
	CHECK_CONNECTED(blocks[IF], { blocks[THEN], blocks[ELSE] }, cfg);
	CHECK_NOT_CONNECTED(blocks[1], { blocks[ENTRY], blocks[EXIT] }, cfg);

	// Then
	const cfg::Block& thenBlock = cfg->getBlock(blocks[THEN]);
	EXPECT_EQ(1u, thenBlock.size());
	EXPECT_FALSE(thenBlock.hasTerminator());
	EXPECT_EQ(*thenBlock.stmt_begin(), ifStmt->getThenBody()->getStatements().front());
	CHECK_CONNECTED(blocks[THEN], { blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[THEN], { blocks[ENTRY], blocks[IF], blocks[ELSE] }, cfg);

	// Else
	const cfg::Block& elseBlock = cfg->getBlock(blocks[ELSE]);
	EXPECT_EQ(1u, elseBlock.size());
	EXPECT_FALSE(elseBlock.hasTerminator());
	EXPECT_EQ(*elseBlock.stmt_begin(), ifStmt->getElseBody()->getStatements().front());
	CHECK_CONNECTED(blocks[ELSE], { blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ELSE], { blocks[ENTRY], blocks[IF], blocks[THEN] }, cfg);

	// Exit
	const cfg::Block& exitBlock = cfg->getBlock(blocks[EXIT]);
	EXPECT_TRUE(exitBlock.empty());
	CHECK_NOT_CONNECTED(blocks[EXIT], { blocks[ENTRY], blocks[IF], blocks[THEN], blocks[ELSE] }, cfg);
}

TEST(CFGBuilder, IfStmt2) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	LiteralPtr stmt1 = Literal::get(manager, manager.getLangBasic().getInt4(), "10");
	LiteralPtr stmt2 = Literal::get(manager, manager.getLangBasic().getInt4(), "20");

	IRBuilder builder(manager);
	IfStmtPtr ifStmt = builder.ifStmt(var, stmt1);
	CompoundStmtPtr stmt = CompoundStmt::get(manager, toVector<StatementPtr>( ifStmt, stmt2 ));
	CFGPtr cfg = CFG::buildCFG(stmt);

	// print the graph on standard output
//	std::cout << *cfg;

	EXPECT_EQ(5u, cfg->size());

	enum DFS_ORDER{ ENTRY, IF, THEN, SINK, EXIT };
	std::vector<CFG::VertexTy> blocks;

	CFG::VertexTy entry = cfg->entry();
	boost::breadth_first_search
	    ( cfg->getRawGraph(), entry,
	    	visitor( boost::make_bfs_visitor( order_blocks(blocks, boost::on_discover_vertex()) ) )
    	);

//	std::copy(blocks.begin(), blocks.end(), std::ostream_iterator<int, char>(std::cout, " "));
//	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getBlock(blocks[ENTRY]);
	EXPECT_TRUE(entryBlock.empty());
	CHECK_CONNECTED(blocks[ENTRY], { blocks[IF] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ENTRY], { blocks[THEN], blocks[SINK], blocks[EXIT] }, cfg);

	// IF
	const cfg::Block& ifBlock = cfg->getBlock(blocks[IF]);
	EXPECT_EQ(1u, ifBlock.size());
	EXPECT_TRUE(ifBlock.hasTerminator());
	EXPECT_EQ(*ifBlock.stmt_begin(), var); // condition
	EXPECT_EQ(ifBlock.terminator(), ifStmt);
	CHECK_CONNECTED(blocks[IF], { blocks[THEN], blocks[SINK] }, cfg);
	CHECK_NOT_CONNECTED(blocks[IF], { blocks[ENTRY], blocks[EXIT] }, cfg);

	// Then
	const cfg::Block& thenBlock = cfg->getBlock(blocks[THEN]);
	EXPECT_EQ(1u, thenBlock.size());
	EXPECT_FALSE(thenBlock.hasTerminator());
	EXPECT_EQ(*thenBlock.stmt_begin(), stmt1);
	CHECK_CONNECTED(blocks[THEN], { blocks[SINK] }, cfg);
	CHECK_NOT_CONNECTED(blocks[THEN], { blocks[ENTRY], blocks[IF], blocks[EXIT] }, cfg);

	// sink
	const cfg::Block& sink = cfg->getBlock(blocks[SINK]);
	EXPECT_EQ(1u, sink.size());
	EXPECT_FALSE(sink.hasTerminator());
	EXPECT_EQ(*sink.stmt_begin(), stmt2);
	CHECK_CONNECTED(blocks[SINK], { blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[SINK], { blocks[ENTRY], blocks[IF], blocks[THEN] }, cfg);

	// Exit
	const cfg::Block& exitBlock = cfg->getBlock(blocks[4]);
	EXPECT_TRUE(boost::edge(blocks[3], blocks[4], cfg->getRawGraph()).second);
	EXPECT_TRUE(exitBlock.empty());
	CHECK_NOT_CONNECTED(blocks[SINK], { blocks[ENTRY], blocks[IF], blocks[THEN], blocks[SINK] }, cfg);
}

TEST(CFGBuilder, ForStmt) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	LiteralPtr step = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	VariablePtr iter = Variable::get(manager, manager.getLangBasic().getInt4(), 1);
	LiteralPtr stmt = Literal::get(manager, manager.getLangBasic().getInt4(), "200");

	IRBuilder builder(manager);
	ForStmtPtr forStmt = builder.forStmt(iter, literal, literal, step, stmt);
	CFGPtr cfg = CFG::buildCFG(forStmt);

	// print the graph on standard output
//	std::cout << *cfg;

	EXPECT_EQ(6u, cfg->size());
	enum DFS_ORDER{ ENTRY, DECL, FOR, BODY, EXIT, INC };
	std::vector<CFG::VertexTy> blocks;

	CFG::VertexTy entry = cfg->entry();
		boost::breadth_first_search
		    ( cfg->getRawGraph(), entry,
		    	visitor( boost::make_bfs_visitor( order_blocks(blocks, boost::on_discover_vertex()) ) )
	    	);
//	std::copy(blocks.begin(), blocks.end(), std::ostream_iterator<int, char>(std::cout, " "));
//	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getBlock(blocks[ENTRY]);
	EXPECT_TRUE(entryBlock.empty());
	CHECK_CONNECTED(blocks[ENTRY], { blocks[DECL] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ENTRY], { blocks[FOR], blocks[BODY], blocks[INC], blocks[EXIT] }, cfg);

	// decl
	const cfg::Block& declBlock = cfg->getBlock(blocks[DECL]);
	EXPECT_EQ(1u, declBlock.size());
	EXPECT_FALSE(declBlock.hasTerminator());
	EXPECT_EQ(*declBlock.stmt_begin(), forStmt);
	EXPECT_EQ(declBlock.stmt_begin()->getType(), cfg::Element::LOOP_INIT);
	CHECK_CONNECTED(blocks[DECL], { blocks[FOR] }, cfg);
	CHECK_NOT_CONNECTED(blocks[DECL], { blocks[ENTRY], blocks[BODY], blocks[INC], blocks[EXIT] }, cfg);

	// for
	const cfg::Block& forBlock = cfg->getBlock(blocks[FOR]);
	EXPECT_EQ(1u, forBlock.size());
	EXPECT_TRUE(forBlock.hasTerminator());
	EXPECT_EQ(*forBlock.stmt_begin(), literal);
	EXPECT_EQ(forBlock.terminator(), forStmt);
	CHECK_CONNECTED(blocks[FOR], { blocks[BODY], blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[FOR], { blocks[ENTRY], blocks[FOR], blocks[INC] }, cfg); 

	// body
	const cfg::Block& bodyBlock = cfg->getBlock(blocks[BODY]);
	EXPECT_EQ(1u, bodyBlock.size());
	EXPECT_FALSE(bodyBlock.hasTerminator());
	EXPECT_EQ(*bodyBlock.stmt_begin(), stmt);
	CHECK_CONNECTED(blocks[BODY], { blocks[INC] }, cfg);
	CHECK_NOT_CONNECTED(blocks[BODY], { blocks[ENTRY], blocks[DECL], blocks[FOR], blocks[EXIT] }, cfg);

	// step
	const cfg::Block& stepBlock = cfg->getBlock(blocks[INC]);
	EXPECT_EQ(1u, stepBlock.size());
	EXPECT_FALSE(stepBlock.hasTerminator());
	EXPECT_EQ(*stepBlock.stmt_begin(), forStmt);
	EXPECT_EQ(stepBlock.stmt_begin()->getType(), cfg::Element::LOOP_INCREMENT);
	CHECK_CONNECTED(blocks[INC], { blocks[FOR] }, cfg);
	CHECK_NOT_CONNECTED(blocks[INC], { blocks[ENTRY], blocks[DECL], blocks[BODY], blocks[EXIT] }, cfg);

	// Exit
	const cfg::Block& exitBlock = cfg->getBlock(blocks[EXIT]);
	EXPECT_TRUE(exitBlock.empty());
	CHECK_NOT_CONNECTED(blocks[EXIT], { blocks[ENTRY], blocks[DECL], blocks[FOR], blocks[BODY], blocks[INC] }, cfg);
}


TEST(CFGBuilder, WhileStmt) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "15");
	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	LiteralPtr stmt = Literal::get(manager, manager.getLangBasic().getInt4(), "100");

	IRBuilder builder(manager);
	WhileStmtPtr whileStmt = builder.whileStmt(var, stmt);
	CFGPtr cfg = CFG::buildCFG(whileStmt);

	// print the graph on standard output
//	std::cout << *cfg;

	EXPECT_EQ(4u, cfg->size());
	enum DFS_ORDER{ ENTRY, WHILE, BODY, EXIT };
	std::vector<CFG::VertexTy> blocks;

	CFG::VertexTy entry = cfg->entry();
		boost::breadth_first_search
		    ( cfg->getRawGraph(), entry,
		    	visitor( boost::make_bfs_visitor( order_blocks(blocks, boost::on_discover_vertex()) ) )
	    	);
//	std::copy(blocks.begin(), blocks.end(), std::ostream_iterator<int, char>(std::cout, " "));
//	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getBlock(blocks[ENTRY]);
	EXPECT_TRUE(entryBlock.empty());
	CHECK_CONNECTED(blocks[ENTRY], { blocks[WHILE] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ENTRY], { blocks[BODY], blocks[EXIT] }, cfg);

	// while
	const cfg::Block& declBlock = cfg->getBlock(blocks[WHILE]);
	EXPECT_EQ(1u, declBlock.size());
	EXPECT_TRUE(declBlock.hasTerminator());
	EXPECT_EQ(*declBlock.stmt_begin(), whileStmt->getCondition());
	CHECK_CONNECTED(blocks[WHILE], { blocks[BODY], blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[WHILE], { blocks[ENTRY] }, cfg);

	// body
	const cfg::Block& bodyBlock = cfg->getBlock(blocks[BODY]);
	EXPECT_EQ(1u, bodyBlock.size());
	EXPECT_FALSE(bodyBlock.hasTerminator());
	EXPECT_EQ(*bodyBlock.stmt_begin(), stmt);
	CHECK_CONNECTED(blocks[BODY], { blocks[WHILE] }, cfg);
	CHECK_NOT_CONNECTED(blocks[BODY], { blocks[ENTRY], blocks[EXIT] }, cfg);

	// Exit
	const cfg::Block& exitBlock = cfg->getBlock(blocks[EXIT]);
	EXPECT_TRUE(exitBlock.empty());
	CHECK_NOT_CONNECTED(blocks[EXIT], toVector<CFG::VertexTy>(blocks[ENTRY], blocks[WHILE], blocks[BODY]), cfg);
}

TEST(CFGBuilder, SwitchStmt) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "15");
	LiteralPtr literal1 = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	LiteralPtr literal2 = Literal::get(manager, manager.getLangBasic().getInt4(), "2");
	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	LiteralPtr stmt1 = Literal::get(manager, manager.getLangBasic().getInt4(), "200");
	LiteralPtr stmt2 = Literal::get(manager, manager.getLangBasic().getInt4(), "300");

	IRBuilder builder(manager);
	SwitchStmtPtr switchStmt = 
		builder.switchStmt(var, toVector(builder.switchCase(literal1, stmt1), builder.switchCase(literal2, stmt2)) );

	CFGPtr cfg = CFG::buildCFG(switchStmt);

	// print the graph on standard output
//	std::cout << *cfg;

	EXPECT_EQ(5u, cfg->size());
	enum DFS_ORDER{ ENTRY, SWITCH, CASE1, CASE2, EXIT };
	std::vector<CFG::VertexTy> blocks;

	CFG::VertexTy entry = cfg->entry();
		boost::breadth_first_search
		    ( cfg->getRawGraph(), entry,
		    	visitor( boost::make_bfs_visitor( order_blocks(blocks, boost::on_discover_vertex()) ) )
	    	);
//	std::copy(blocks.begin(), blocks.end(), std::ostream_iterator<int, char>(std::cout, " "));
//	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getBlock(blocks[ENTRY]);
	EXPECT_TRUE(entryBlock.empty());
	CHECK_CONNECTED(blocks[ENTRY], { blocks[SWITCH] }, cfg);
	CHECK_NOT_CONNECTED(blocks[ENTRY], { blocks[CASE1], blocks[CASE2], blocks[EXIT] }, cfg);

	// switch
	const cfg::Block& switchBlock = cfg->getBlock(blocks[SWITCH]);
	EXPECT_EQ(1u, switchBlock.size());
	EXPECT_TRUE(switchBlock.hasTerminator());
	EXPECT_EQ(*switchBlock.stmt_begin(), switchStmt->getSwitchExpr());
	CHECK_CONNECTED(blocks[SWITCH], { blocks[CASE1], blocks[CASE2], blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[SWITCH], { blocks[ENTRY] }, cfg);

	// case1
	const cfg::Block& case1Block = cfg->getBlock(blocks[CASE1]);
	EXPECT_EQ(1u, case1Block.size());
	EXPECT_FALSE(case1Block.hasTerminator());
	EXPECT_EQ(*case1Block.stmt_begin(), stmt1);
	CHECK_CONNECTED(blocks[CASE1], { blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[CASE1], { blocks[ENTRY], blocks[SWITCH], blocks[CASE2] }, cfg);

	// case2
	const cfg::Block& case2Block = cfg->getBlock(blocks[CASE2]);
	EXPECT_EQ(1u, case2Block.size());
	EXPECT_FALSE(case2Block.hasTerminator());
	EXPECT_EQ(*case2Block.stmt_begin(), stmt2);
	CHECK_CONNECTED(blocks[CASE2], { blocks[EXIT] }, cfg);
	CHECK_NOT_CONNECTED(blocks[CASE2], { blocks[ENTRY], blocks[SWITCH], blocks[CASE1] }, cfg);

	// Exit
	const cfg::Block& exitBlock = cfg->getBlock(blocks[EXIT]);
	EXPECT_TRUE(exitBlock.empty());
	CHECK_NOT_CONNECTED(blocks[EXIT], { blocks[ENTRY], blocks[SWITCH], blocks[CASE1], blocks[CASE2] }, cfg);
}


TEST(CFGBlockIterator, SuccessorsIterator) {
	NodeManager manager;
	IfStmtPtr ifStmt = buildIfStmt1(manager);
	CFGPtr cfg = CFG::buildCFG(ifStmt);

	const cfg::Block& ifBlock = **cfg->successors_begin( cfg->entry() );

	auto succIT = cfg->successors_begin( ifBlock ), end = cfg->successors_end( ifBlock );
	EXPECT_FALSE(succIT == end);

	const cfg::Block& thenBlock = **succIT;
	EXPECT_TRUE(*thenBlock.stmt_begin() == ifStmt->getThenBody()->getStatements().front());
	++succIT;

	const cfg::Block& elseBlock = **succIT;
	EXPECT_FALSE(succIT == end);
	EXPECT_TRUE(*elseBlock.stmt_begin() == ifStmt->getElseBody()->getStatements().front());
	++succIT;
	EXPECT_TRUE(succIT == end);
}

TEST(CFGBlockIterator, PredecessorIterator) {
	NodeManager manager;
	IfStmtPtr ifStmt = buildIfStmt1(manager);
	CFGPtr cfg = CFG::buildCFG(ifStmt);

	auto predIT = cfg->predecessors_begin( cfg->exit() ), end = cfg->predecessors_end( cfg->exit() );

	EXPECT_FALSE(predIT == end);
	const cfg::Block& thenBlock = **predIT;
	EXPECT_TRUE(*thenBlock.stmt_begin() == ifStmt->getThenBody()->getStatements().front());
	++predIT;

	EXPECT_FALSE(predIT == end);
	const cfg::Block& elseBlock = **predIT;
	EXPECT_TRUE(*elseBlock.stmt_begin() == ifStmt->getElseBody()->getStatements().front());
	++predIT;

	EXPECT_TRUE(predIT == end);
}

TEST(CFGBuilder, CallExprSimple) {

	NodeManager manager;

	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	FunctionTypePtr exitFuncTy = 
		FunctionType::get( manager, toVector<TypePtr>(manager.getLangBasic().getInt4()),  manager.getLangBasic().getInt4());

	CallExprPtr callExpr = CallExpr::get(manager, manager.getLangBasic().getInt4(), Literal::get(manager, exitFuncTy, "exit"),
			toVector<ExpressionPtr>(literal));

	CompoundStmtPtr cs = CompoundStmt::get(manager, toVector<StatementPtr>(callExpr));

	// Build the CFG
	CFGPtr cfg = CFG::buildCFG(cs);

	EXPECT_EQ(3u, cfg->size());
}

TEST(CFG, FindNode) {
	
	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "15");
	LiteralPtr literal1 = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	LiteralPtr literal2 = Literal::get(manager, manager.getLangBasic().getInt4(), "2");
	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	LiteralPtr stmt1 = Literal::get(manager, manager.getLangBasic().getInt4(), "200");
	LiteralPtr stmt2 = Literal::get(manager, manager.getLangBasic().getInt4(), "300");

	IRBuilder builder(manager);
	SwitchStmtPtr switchStmt = 
		builder.switchStmt(var, toVector( builder.switchCase(literal1, stmt1), builder.switchCase(literal2, stmt2) ) );

	CFGPtr cfg = CFG::buildCFG(switchStmt);
	NodeAddress stmtAddr(switchStmt);

	//{
		//cfg::BlockPtr b = cfg->find(  );
		//EXPECT_TRUE( !!b );
	//}
	//{
		//LiteralPtr stmt3 = Literal::get(manager, manager.getLangBasic().getInt4(), "90");
		//cfg::BlockPtr b = cfg->find( NodeAddress(stmt3) );
		//EXPECT_FALSE( !!b );
	//}

}

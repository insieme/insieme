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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"
#include "insieme/analysis/cfg.h"

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
inline vertex_orderer<Tag> order_verteces(std::vector<CFG::VertexTy>& bfs, Tag) {
	return vertex_orderer<Tag>(bfs);
}

TEST(CFGBuilder, CompoundStmt) {

	NodeManager manager;

	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);
	DeclarationStmtPtr stmt3 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 3), literal);

	CompoundStmtPtr cs0 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1, stmt2, stmt3));
	CompoundStmtPtr cs1 = CompoundStmt::get(manager, toVector<StatementPtr>(cs0, stmt1));
	CompoundStmtPtr cs2 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt2, cs1));

	CFGPtr cfg = CFG::buildCFG(cs2);
	EXPECT_EQ(static_cast<unsigned>(3), cfg->getSize());

	const cfg::Block& block = cfg->getNode(2);
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

TEST(CFGBuilder, IfStmt1) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	VariablePtr var = Variable::get(manager, manager.basic.getBool(), 1);
	DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);

	IfStmtPtr stmt = IfStmt::get(manager, var, stmt1, stmt2);
	CFGPtr cfg = CFG::buildCFG(stmt);

	// print the graph on standard output
	std::cout << *cfg;

	EXPECT_EQ(static_cast<unsigned>(5), cfg->getSize());

	std::vector<CFG::VertexTy> verteces;

	CFG::VertexTy entry = cfg->getEntry();
	boost::breadth_first_search
	    ( cfg->getGraph(), entry, boost::visitor( boost::make_bfs_visitor( order_verteces(verteces, boost::on_discover_vertex()) ) ) );

	std::copy(verteces.begin(), verteces.end(), std::ostream_iterator<int, char>(std::cout, " "));
	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getNode(verteces[0]);
	EXPECT_TRUE(entryBlock.empty());

	// IF
	const cfg::Block& ifBlock = cfg->getNode(verteces[1]);
	EXPECT_EQ(static_cast<unsigned>(1), ifBlock.size());
	EXPECT_TRUE(ifBlock.hasTerminator());
	EXPECT_EQ(*ifBlock.stmt_begin(), var); // condition
	EXPECT_EQ(ifBlock.getTerminator(), stmt);
	EXPECT_TRUE(boost::edge(verteces[0], verteces[1], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[3], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[4], cfg->getGraph()).second);

	// Then
	const cfg::Block& thenBlock = cfg->getNode(verteces[2]);
	EXPECT_EQ(static_cast<unsigned>(1), thenBlock.size());
	EXPECT_FALSE(thenBlock.hasTerminator());
	EXPECT_EQ(*thenBlock.stmt_begin(), stmt1);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[1], verteces[4], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[2], verteces[3], cfg->getGraph()).second);

	// Else
	const cfg::Block& elseBlock = cfg->getNode(verteces[3]);
	EXPECT_EQ(static_cast<unsigned>(1), elseBlock.size());
	EXPECT_FALSE(elseBlock.hasTerminator());
	EXPECT_EQ(*elseBlock.stmt_begin(), stmt2);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[3], cfg->getGraph()).second);

	// Exit
	const cfg::Block& exitBlock = cfg->getNode(verteces[4]);
	EXPECT_TRUE(exitBlock.empty());
	EXPECT_TRUE(boost::edge(verteces[1], verteces[2], cfg->getGraph()).second);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[3], cfg->getGraph()).second);
}

TEST(CFGBuilder, IfStmt2) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	VariablePtr var = Variable::get(manager, manager.basic.getBool(), 1);
	DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);

	IfStmtPtr ifStmt = IfStmt::get(manager, var, stmt1);
	CompoundStmtPtr stmt = CompoundStmt::get(manager, toVector<StatementPtr>(ifStmt, stmt2));
	CFGPtr cfg = CFG::buildCFG(stmt);

	// print the graph on standard output
	std::cout << *cfg;

	EXPECT_EQ(static_cast<unsigned>(5), cfg->getSize());

	std::vector<CFG::VertexTy> verteces;

	CFG::VertexTy entry = cfg->getEntry();
	boost::breadth_first_search
	    ( cfg->getGraph(), entry, boost::visitor( boost::make_bfs_visitor( order_verteces(verteces, boost::on_discover_vertex()) ) ) );

	std::copy(verteces.begin(), verteces.end(), std::ostream_iterator<int, char>(std::cout, " "));
	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getNode(verteces[0]);
	EXPECT_TRUE(entryBlock.empty());

	// IF
	const cfg::Block& ifBlock = cfg->getNode(verteces[1]);
	EXPECT_EQ(static_cast<unsigned>(1), ifBlock.size());
	EXPECT_TRUE(ifBlock.hasTerminator());
	EXPECT_EQ(*ifBlock.stmt_begin(), var); // condition
	EXPECT_EQ(ifBlock.getTerminator(), ifStmt);
	EXPECT_TRUE(boost::edge(verteces[0], verteces[1], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[3], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[4], cfg->getGraph()).second);

	// Then
	const cfg::Block& thenBlock = cfg->getNode(verteces[2]);
	EXPECT_EQ(static_cast<unsigned>(1), thenBlock.size());
	EXPECT_FALSE(thenBlock.hasTerminator());
	EXPECT_EQ(*thenBlock.stmt_begin(), stmt1);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[1], verteces[4], cfg->getGraph()).second);

	// sink
	const cfg::Block& sink = cfg->getNode(verteces[3]);
	EXPECT_EQ(static_cast<unsigned>(1), sink.size());
	EXPECT_FALSE(sink.hasTerminator());
	EXPECT_EQ(*sink.stmt_begin(), stmt2);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[3], cfg->getGraph()).second);
	EXPECT_TRUE(boost::edge(verteces[2], verteces[3], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[2], verteces[4], cfg->getGraph()).second);

	// Exit
	const cfg::Block& exitBlock = cfg->getNode(verteces[4]);
	EXPECT_TRUE(boost::edge(verteces[3], verteces[4], cfg->getGraph()).second);
	EXPECT_TRUE(exitBlock.empty());
}

TEST(CFGBuilder, ForStmt) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	LiteralPtr step = Literal::get(manager, manager.basic.getInt4(), "1");
	VariablePtr var = Variable::get(manager, manager.basic.getBool(), 1);
	DeclarationStmtPtr decl = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);

	ForStmtPtr forStmt = ForStmt::get(manager, decl, stmt, literal, step);
	CFGPtr cfg = CFG::buildCFG(forStmt);

	// print the graph on standard output
	std::cout << *cfg;

	EXPECT_EQ(static_cast<unsigned>(6), cfg->getSize());

	std::vector<CFG::VertexTy> verteces;

	CFG::VertexTy entry = cfg->getEntry();
	boost::breadth_first_search
	    ( cfg->getGraph(), entry, boost::visitor( boost::make_bfs_visitor( order_verteces(verteces, boost::on_discover_vertex()) ) ) );

	std::copy(verteces.begin(), verteces.end(), std::ostream_iterator<int, char>(std::cout, " "));
	std::cout << std::endl;

	const cfg::Block& entryBlock = cfg->getNode(verteces[0]);
	EXPECT_TRUE(entryBlock.empty());
	EXPECT_TRUE(boost::edge(verteces[0], verteces[1], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[3], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[4], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[5], cfg->getGraph()).second);

	// decl
	const cfg::Block& declBlock = cfg->getNode(verteces[1]);
	EXPECT_EQ(static_cast<unsigned>(1), declBlock.size());
	EXPECT_FALSE(declBlock.hasTerminator());
	EXPECT_EQ(*declBlock.stmt_begin(), decl);
	EXPECT_TRUE(boost::edge(verteces[1], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[1], verteces[3], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[1], verteces[4], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[0], verteces[5], cfg->getGraph()).second);

	// for
	const cfg::Block& forBlock = cfg->getNode(verteces[2]);
	EXPECT_EQ(static_cast<unsigned>(1), forBlock.size());
	EXPECT_TRUE(forBlock.hasTerminator());
	EXPECT_EQ(*forBlock.stmt_begin(), literal);
	EXPECT_EQ(forBlock.getTerminator(), forStmt);
	EXPECT_TRUE(boost::edge(verteces[2], verteces[3], cfg->getGraph()).second);
	EXPECT_TRUE(boost::edge(verteces[2], verteces[4], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[2], verteces[5], cfg->getGraph()).second);

	// body
	const cfg::Block& bodyBlock = cfg->getNode(verteces[3]);
	EXPECT_EQ(static_cast<unsigned>(1), bodyBlock.size());
	EXPECT_FALSE(bodyBlock.hasTerminator());
	EXPECT_EQ(*bodyBlock.stmt_begin(), stmt);
	EXPECT_TRUE(boost::edge(verteces[3], verteces[5], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[3], verteces[4], cfg->getGraph()).second);

	// step
	const cfg::Block& stepBlock = cfg->getNode(verteces[5]);
	EXPECT_EQ(static_cast<unsigned>(1), stepBlock.size());
	EXPECT_FALSE(stepBlock.hasTerminator());
	EXPECT_EQ(*stepBlock.stmt_begin(), step);
	EXPECT_TRUE(boost::edge(verteces[5], verteces[2], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[5], verteces[4], cfg->getGraph()).second);

	// Exit
	const cfg::Block& exitBlock = cfg->getNode(verteces[4]);
	EXPECT_TRUE(boost::edge(verteces[2], verteces[4], cfg->getGraph()).second);
	EXPECT_FALSE(boost::edge(verteces[4], verteces[5], cfg->getGraph()).second);
	EXPECT_TRUE(exitBlock.empty());
}


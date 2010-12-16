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

#include "insieme/analysis/cfg.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

#include <stack>
#include <tuple>

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

namespace {

struct Scope {
	NodePtr  root;
	CFG::VertexTy entry;
	CFG::VertexTy exit;

	Scope(const NodePtr& root, const CFG::VertexTy& entry, const CFG::VertexTy& exit) :
		root(root), entry(entry), exit(exit) { }
};

/**
 * ScopeStack: represent a stack of scopes
 */
struct ScopeStack : std::vector<Scope> {

	ScopeStack() { }

	void push(const Scope& scope) { push_back(scope); }
	const Scope& top() const { return back(); }
	void pop() { pop_back(); }

	const Scope& getEnclosingLoop() const {
		auto it = std::find_if(rbegin(), rend(), [](const Scope& curr){ return curr.root->getNodeType() == NT_ForStmt; });
		assert(it != rend()); // if the IR is well formed a continue statement is only allowed if an enclosing loop exists
		return *it;
	}

	const Scope& getEnclosingFunc() const {
		auto it = std::find_if(rbegin(), rend(), [](const Scope& curr){ return curr.root->getNodeType() == NT_LambdaExpr; });
		assert(it != rend()); // if the IR is well formed a continue statement is only allowed if an enclosing loop exists
		return *it;
	}
};

typedef std::pair<bool, CFG::VertexTy> NodeBound;

struct CFGBuilder: public ASTVisitor< void > {

	CFG& cfg;

	cfg::Block* currBlock;
	bool isPending;

	CFG::VertexTy entry;
	CFG::VertexTy exit;

	CFG::VertexTy succ;

	ScopeStack 	scopeStack;

	CFGBuilder(CFG& cfg) : cfg(cfg), currBlock(NULL), isPending(true),
			entry( cfg.addNode( new cfg::Block(0) ) ), exit( cfg.addNode( new cfg::Block(1) ) ), succ(exit) { }

	void appendPendingBlock() {
		if(isPending && currBlock && !currBlock->empty()) {
			CFG::VertexTy&& node = cfg.addNode(currBlock);
			// set the ID for this block
			currBlock->setBlockID(node);
			cfg.addEdge(node, succ);
			succ = node;
			currBlock = NULL;
		}

		if(currBlock && currBlock->empty()) {
			delete currBlock;
			currBlock = NULL;
		}

		if(!isPending)
			currBlock = NULL;

		isPending = true;
	}

	void visitIfStmt(const IfStmtPtr& ifStmt) {
		cfg::Block* ifBlock = new cfg::Block;
		ifBlock->appendStmt(ifStmt->getCondition());
		ifBlock->setTerminal(ifStmt);
		CFG::VertexTy&& src = cfg.addNode( ifBlock );
		ifBlock->setBlockID(src);

		// the current node needs to be appendend to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		currBlock = new cfg::Block;
		// push scope into the stack for this compound statement
		visit(ifStmt->getThenBody());
		// cfg.addEdge(src, std::get<0>(thenCfg) ? std::get<1>(thenCfg) : head);
		appendPendingBlock();
		cfg.addEdge(src, succ);

		// reset the successor for the thenBody
		succ = sink;

		currBlock = new cfg::Block;
		// push scope into the stack for this compound statement
		visit(ifStmt->getElseBody());
		// cfg.addEdge(src, std::get<0>(elseCfg) ? std::get<1>(elseCfg) : head);
		appendPendingBlock();
		cfg.addEdge(src, succ);

		succ = src;
		currBlock = ifBlock;
		isPending = false;
	}

	void visitContinueStmt(const ContinueStmtPtr& continueStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));
		if(!currBlock)
			currBlock = new cfg::Block;

		currBlock->setTerminal(continueStmt);
		succ = scopeStack.getEnclosingLoop().entry;
	}

	void visitBreakStmt(const BreakStmtPtr& breakStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));
		if(!currBlock)
			currBlock = new cfg::Block;

		currBlock->setTerminal(breakStmt);
		succ = scopeStack.getEnclosingLoop().exit;
	}

	void visitReturnStmt(const ReturnStmtPtr& retStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));
		if(!currBlock)
			currBlock = new cfg::Block;

		currBlock->setTerminal(retStmt);
		currBlock->appendStmt( retStmt->getReturnExpr() );
		succ = scopeStack.getEnclosingFunc().exit;
	}

	void visitMarkerStmt(const MarkerStmtPtr& markerStmt) {
		visit( markerStmt->getSubStatement() );
	}

	void visitForStmt(const ForStmtPtr& forStmt) {
		cfg::Block* forBlock = new cfg::Block;
		forBlock->setTerminal(forStmt);
		forBlock->appendStmt(forStmt->getEnd());
		CFG::VertexTy&& src = cfg.addNode( forBlock );
		forBlock->setBlockID(src);

		appendPendingBlock();
		CFG::VertexTy sink = succ;

		// increment expression
		cfg::Block* incBlock = new cfg::Block;
		incBlock->appendStmt(forStmt->getStep());
		CFG::VertexTy&& inc = cfg.addNode( incBlock );
		incBlock->setBlockID(inc);
		cfg.addEdge(inc, src);

		succ = inc;

		currBlock = new cfg::Block;
		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, sink) );
		visit(forStmt->getBody());
		scopeStack.pop();

		appendPendingBlock();

		cfg.addEdge(src, succ);
		cfg.addEdge(src, sink);

		succ = src;
		// decl stmt of the for loop needs to be part of the incoming block
		currBlock = new cfg::Block;
		currBlock->appendStmt(forStmt->getDeclaration());
	}

	void visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		const std::vector<StatementPtr>& body = compStmt->getStatements();
		if(!currBlock) {
			// create a new block to host this compound stmt
			currBlock = new cfg::Block;
		}

		// we are sure there is at least 1 element in this compound statement
		for_each(body.rbegin(), body.rend(),
			[ this ](const StatementPtr& curr) { this->visit(curr); }
		);
	}

//	BlockBounds visitCallExpr(const CallExprPtr& callExpr) {
//		if(callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
//			cfg::Block callBlock;
//			callBlock.setTerminal(callExpr);
//			CFG::VertexTy&& src = cfg.addNode( callBlock );
//			CFG::VertexTy&& sink = cfg.addNode( cfg::Block() );
//			cfg.addEdge(sink, head);
//			head = sink;
//			visit(callExpr->getFunctionExpr());
//			cfg.addEdge(src, head);
//			head = src;
//			return std::make_tuple(false, head);
//		}
//		return visitStatement(callExpr);
//	}

	void visitLambdaExpr(const LambdaExprPtr& lambda) {
		scopeStack.push( Scope(lambda, CFG::VertexTy(), succ) );
		visit(lambda->getBody());
		scopeStack.pop();

		appendPendingBlock();
	}

	void visitProgram(const ProgramPtr& program) {

		std::for_each(program->getEntryPoints().begin(), program->getEntryPoints().end(),
			[ this ]( const ExpressionPtr& curr ) {
				this->succ = this->exit;
				this->visit(curr);
				// connect the resulting block with the entry point
				this->cfg.addEdge(this->entry, this->succ);
			}
		);

		succ = entry;
	}

	void visitStatement(const StatementPtr& stmt) {
		assert(currBlock);
		currBlock->appendStmt(stmt);
	}

	void completeGraph() {
		if(entry == succ)
			return;

		if(currBlock) {
			appendPendingBlock();
		}
		cfg.addEdge(entry, succ);
		succ = entry;
	}

};

} // end anonymous namespace

namespace insieme {
namespace analysis {

CFGPtr CFG::buildCFG(const NodePtr& rootNode) {
	CFGPtr cfg = std::make_shared<CFG>();
	CFGBuilder builder(*cfg);
	builder.visit(rootNode);
	builder.completeGraph();
	return cfg;
}

CFG::~CFG() {
	boost::graph_traits<ControlFlowGraph>::vertex_iterator vi, vi_end;
	for (boost::tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
		const cfg::Block* block = &getNode(*vi);
		delete block;
	}
}

} // end analysis namespace
} // end insieme namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg) {
	using namespace insieme::analysis;

	CFG::ConstNodePropertyMapTy&& node = get(&CFG::NodeProperty::block, cfg.graph);
	boost::write_graphviz(out, cfg.graph, CFG::LabelWriter<CFG::ConstNodePropertyMapTy>(node));
	return out;
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Block& block) {
	if(!block.empty()) {
		out << "[shape=box,label=\"";
		out << "[B" << block.getBlockID() << "]\\l\\n";
		size_t num = 0;
		std::for_each(block.stmt_begin(), block.stmt_end(), [ &out, &num ](const insieme::analysis::cfg::Element& curr) {
			out << num++ << ": " << printer::PrettyPrinter( static_pointer_cast<const Statement>(curr), 1<<5 ) << "\\l";
		});
		if(!!block.getTerminator())
			out << "T: " << block.getTerminator();

		return out << "\"]";
	}
	assert(block.getBlockID() == 0 || block.getBlockID() == 1); // entry and exit block are the only allowed empty blocks
	return out << "[shape=diamond,label=\""<< (block.getBlockID() == 0 ? "ENTRY" : "EXIT") << "\"]";
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Terminator& term) {
	// assert(type == Element::TERMINAL);
	switch(term->getNodeType()) {
	case NT_IfStmt:
		return out << "IF(...)\\l";
	case NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>(term);
		return out << "FOR( " << "... ; "
				<< printer::PrettyPrinter( forStmt->getDeclaration()->getVariable(), 1<<5 ) << " < "
				<< printer::PrettyPrinter(forStmt->getEnd(), 1<<5 ) << "; ...)\\l";
	}
	case NT_ContinueStmt:
		return out << "CONTINUE\\l";
	case NT_BreakStmt:
		return out << "BREAK\\l";
	case NT_ReturnStmt:
		return out << "RETURN\\l";
	default:
		assert(false);
	}
}

} // end std namespace


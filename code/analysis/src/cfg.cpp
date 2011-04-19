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
#include "insieme/core/ast_builder.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

#include <tuple>

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

namespace {

/**
 * Scope - Store the entry and exit blocks of program scopes. This is used in case of continue or break statements
 * to jump to the corresponding block.
 */
struct Scope {
	NodePtr  root;         // The IR node which defines this scope
	CFG::VertexTy entry;   // The CFG node where the scope begins
	CFG::VertexTy exit;    // The CFG node where the scope ends

	Scope(const NodePtr& root, const CFG::VertexTy& entry, const CFG::VertexTy& exit) :
		root(root), entry(entry), exit(exit) { }
};

/**
 * ScopeStack: represent a stack of scopes
 */
struct ScopeStack : public std::vector<Scope> {

	// Push the current scope on top of the stack
	void push(const Scope& scope) { push_back(scope); }

	// Returns the current top of the stack
	const Scope& top() const { return back(); }

	// Remove the element on top of the stack
	void pop() { pop_back(); }

	const Scope& getEnclosingLambda() const {
		// in the case we have jump outside a function we have to look for the innermost enclosing lambda expression
		auto filter = [](const Scope& curr) -> bool { return curr.root->getNodeType() == NT_LambdaExpr; };
		return getEnclosingBlock(filter);
	}

	// Returns target block of a continue statement placed in the innermost scope
	const Scope& getContinueTarget() const {
		auto filter = [](const Scope& curr) -> bool {
			NodeType&& nt = curr.root->getNodeType();
			return (nt == NT_WhileStmt || nt == NT_ForStmt);
		};
		return getEnclosingBlock(filter);
	}

	// Returns target block of a break statement placed in the innermost scope
	const Scope& getBreakTarget() const {
		auto filter = [](const Scope& curr) -> bool {
			NodeType&& nt = curr.root->getNodeType();
			return (nt == NT_WhileStmt || nt == NT_ForStmt || NT_SwitchStmt);
		};
		return getEnclosingBlock(filter);
	}

private:
	/**
	 * Search for enclosing scope to be returned. The filter class is used to filter out scopes which are not of
	 * interested because neglected by the particular control flow statement (e.g. continue statmente only looks
	 * for surrounding loops, not eventual switch statements)
	 * @param filter Filters the scopes
	 * @return The enclosing scope at which the control statement is referring to
	 */
	template <class Filter>
	const Scope& getEnclosingBlock(const Filter& filter) const {
		auto it = std::find_if(rbegin(), rend(), filter);
		// if the IR is well formed a continue statement is only allowed if an enclosing loop exists
		assert(it != rend());
		return *it;
	}
};

/**
 * Builder of the Control Flow Graph. Traverses the IR and creates blocks appending them to the CFG.
 * The visit is done in reverse order in a way the number of CFG nodes is minimized.
 */
template < CreationPolicy CP >
struct CFGBuilder: public ASTVisitor< void > {

	CFGPtr cfg;

	// A pointer to the block which is currently the head of graph which is built
	// bottom-up visiting the statements in reverse order
	cfg::Block* currBlock, *spawnBlock;

	// Tells us if currBlock has been already inserted in the CFG graph or it is only a temporary element
	bool isPending;

	CFG::VertexTy entry, exit, succ, head;
	ScopeStack 	scopeStack;

	bool hasHead;

	CFGBuilder(CFGPtr cfg, const NodePtr& root) : ASTVisitor<>(false), cfg(cfg), currBlock(NULL), spawnBlock(NULL),
			isPending(false), hasHead(false) {

		assert( !cfg->hasSubGraph(root) && "CFG for this root node already being built");
		CFG::GraphBounds&& bounds = cfg->addSubGraph(root);
		// initialize the entry/exit blocks for this CFG
		entry = bounds.first;
		head = entry;
		exit = bounds.second;
		succ = exit;

		visit(root); 				// Visit the IR

		// Performs the final steps to finalize the CFG
		appendPendingBlock(); 		// if we still have pending node we add them to the CFG

		if ( entry == succ )	return;

		if ( cfg->getBlock(succ).empty() ) {
			// If the first statement of a root element is a function call
			// we end up with an empty statement at the top of the CFG, we
			// want to remove that block and connect the outgoing edges to
			// the entry node
			cfg->replaceNode(succ, entry);
			return;
		}

		cfg->addEdge(entry, succ);	// connect the entry with the top node
	}

	void createBlock() {
		// if we already have a block allocated and the block is empty we return it
		if ( isPending || (currBlock && currBlock->empty()) ) {
			return;
		}

		// we have to make sure the currBlock is not containing elements already
		assert( !isPending && !currBlock && "CFG block lost during CFG creation" );
		currBlock = new cfg::Block;
		isPending = true;
	}

	void appendPendingBlock() {

		// if we already have allocated an empty block and it is not pending
		// if ( !isPending && currBlock && currBlock->empty() )
		//	return;

		// In the case the currBlock is pending and not empty we add it to the Graph and connect
		// it with the successive node in the CFG
		if ( isPending && currBlock && !currBlock->empty() ) {
			CFG::VertexTy&& node = cfg->addBlock(currBlock);
			cfg->addEdge(node, succ);
			succ = node;
			currBlock = NULL;
		}

		if ( isPending && currBlock ) {
			delete currBlock;
			currBlock = NULL;
		}

		if ( !isPending && currBlock ) {
			currBlock = NULL;
		}
		isPending = false;

		// check post conditions
		assert(!isPending && !currBlock && "Failed to satisfy postconditions");
	}

	/**
	 * When a continue statement is encountered we jump to the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitContinueStmt(const ContinueStmtPtr& continueStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->terminator() = cfg::Element(continueStmt);
		succ = scopeStack.getContinueTarget().entry;
	}

	/**
	 * When a break statement is encountered we jump right after the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitBreakStmt(const BreakStmtPtr& breakStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->terminator() = cfg::Element(breakStmt);
		succ = scopeStack.getBreakTarget().exit;
	}

	/**
	 * When a return statement is encountered we jump to the exit block of the closest enclosing function scope
	 * (i.e. lambda expression)
	 * @param retStmt
	 */
	void visitReturnStmt(const ReturnStmtPtr& retStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->terminator() = cfg::Element(retStmt);
		succ = scopeStack.getEnclosingLambda().exit;

		visit( retStmt->getReturnExpr() );
	}

	void visitMarkerStmt(const MarkerStmtPtr& markerStmt) {
		visit( markerStmt->getSubStatement() );
	}

	void visitIfStmt(const IfStmtPtr& ifStmt) {
		cfg::Block* ifBlock = new cfg::Block;
		ifBlock->terminator() = cfg::Element(ifStmt);
		CFG::VertexTy&& src = cfg->addBlock( ifBlock );
		
		appendPendingBlock(); // append any pending block before we fork the CFG for inserting the for stmt
		CFG::VertexTy sink = succ;

		createBlock();
		visit(ifStmt->getThenBody());
		appendPendingBlock();

		ASTBuilder builder(ifStmt->getNodeManager());
		cfg->addEdge(src, succ, cfg::Edge( builder.getBasicGenerator().getTrue() )); 

		// check for empty head block
		if ( cfg->getBlock(succ).empty() ) {
			cfg->replaceNode(succ, src);
		}

		succ = sink; // reset the successor for the thenBody

		createBlock();
		visit(ifStmt->getElseBody());
		appendPendingBlock();
	
		cfg->addEdge(src, succ, cfg::Edge( builder.getBasicGenerator().getFalse() ));

		// check for empty head block
		if ( cfg->getBlock(succ).empty() ) {
			cfg->replaceNode(succ, src);
		}

		succ = src; 		// succ now points to the head of the IF stmt
		currBlock = ifBlock;
		isPending = false;

		visit( ifStmt->getCondition() );
	}

	void visitForStmt(const ForStmtPtr& forStmt) {
		cfg::Block* forBlock = new cfg::Block;
		forBlock->terminator() = cfg::Element(forStmt);
		forBlock->appendElement( cfg::Element(forStmt->getEnd(), cfg::Element::CtrlCond) );
		CFG::VertexTy&& src = cfg->addBlock( forBlock );

		appendPendingBlock();  // append any pending block before we fork the CFG for inserting the for stmt
		currBlock = NULL;

		CFG::VertexTy sink = succ;

		// increment expression
		cfg::Block* incBlock = new cfg::Block;
		incBlock->appendElement( cfg::Element(forStmt, cfg::Element::LoopIncrement) );
		CFG::VertexTy&& inc = cfg->addBlock( incBlock );
		cfg->addEdge(inc, src);

		succ = inc;

		createBlock();
		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, sink) );
		visit( forStmt->getBody() );
		scopeStack.pop();

		appendPendingBlock();

		ASTBuilder builder(forStmt->getNodeManager());
		cfg->addEdge(src, succ, cfg::Edge( builder.getBasicGenerator().getTrue() )); 
		cfg->addEdge(src, sink, cfg::Edge( builder.getBasicGenerator().getFalse() )); 

		// check for empty head block
		//if ( cfg->getBlock(succ).empty() ) {
	///		cfg->replaceNode(succ, src);
//		}

		succ = src;
		// decl stmt of the for loop needs to be part of the incoming block
		createBlock();
		currBlock->appendElement( cfg::Element(forStmt, cfg::Element::LoopInit) );
	}

	void visitWhileStmt(const WhileStmtPtr& whileStmt) {
		cfg::Block* whileBlock = new cfg::Block;
		whileBlock->terminator() = cfg::Element(whileStmt);
		CFG::VertexTy&& src = cfg->addBlock( whileBlock );

		// the current node needs to be append to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		succ = src;
		scopeStack.push( Scope(whileStmt, src, sink) );
		createBlock();
		// push scope into the stack for this compound statement
		visit(whileStmt->getBody());
		scopeStack.pop();

		appendPendingBlock();
		ASTBuilder builder(whileStmt->getNodeManager());
		cfg->addEdge(src, succ, cfg::Edge( builder.getBasicGenerator().getTrue() )); 
		cfg->addEdge(src, sink, cfg::Edge( builder.getBasicGenerator().getFalse() ));

		// check for empty head block
		//if ( cfg->getBlock(succ).empty() ) {
		//	cfg->replaceNode(succ, src);
		//}

		succ = src;
		currBlock = whileBlock;
		isPending = false;

		visit( whileStmt->getCondition() );
	}

	void visitCastExpr(const CastExprPtr& castExpr) {
		return visit(castExpr->getSubExpression());
	}

	void visitSwitchStmt(const SwitchStmtPtr& switchStmt) {
		cfg::Block* switchBlock = new cfg::Block;
		switchBlock->terminator() = cfg::Element(switchStmt);
		CFG::VertexTy&& src = cfg->addBlock( switchBlock );

		// the current node needs to be appent to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		scopeStack.push( Scope(switchStmt, src, sink) );
		const std::vector<SwitchStmt::Case>& cases = switchStmt->getCases();
		for ( auto it = cases.begin(), end = cases.end(); it != end; ++it ) {
			const SwitchStmt::Case& curr = *it;
			succ = sink;
			createBlock();
			// push scope into the stack for this compound statement
			visit(curr.second);

			appendPendingBlock();
			cfg->addEdge(src, succ);

			// check for empty head block
			if ( cfg->getBlock(succ).empty() ) {
				cfg->replaceNode(succ, src);
			}
		}

		succ = sink;
		createBlock();
		// Default case
		visit(switchStmt->getDefaultCase());
		appendPendingBlock();
		cfg->addEdge(src, succ);

		scopeStack.pop();
		succ = src;
		currBlock = switchBlock;
		isPending = false;

		visit( switchStmt->getSwitchExpr() );
	}

	void visitCompoundStmt(const CompoundStmtPtr& compStmt);

	void visitCallExpr(const CallExprPtr& callExpr) {
		// if the call expression is calling a lambda the body of the lambda is processed and the sub graph is built
		if ( callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr ) {
			const LambdaExprPtr& lambdaExpr = static_pointer_cast<const LambdaExpr>(callExpr->getFunctionExpr());

			if ( !cfg->hasSubGraph(lambdaExpr) ) {
				// In the case the body has not been visited yet, proceed with the graph construction
				// TODO: This can be executed in a separate thread (if necessary)
				CFG::buildCFG<CP>(lambdaExpr, cfg);
			}

			appendPendingBlock();

			CFG::GraphBounds&& bounds = cfg->getNodeBounds(lambdaExpr);
			// A call expression creates 2 blocks, 1 spawning the function call and the second one collecting
			// the return value
			cfg::CallBlock* call = new cfg::CallBlock;
			cfg::RetBlock* ret = new cfg::RetBlock;

			// we interconnect the two blocks so that if we want to have intra-procedural analysis we can jump
			// directly to the return block without visiting the body of the function
			call->returnBlock() = ret;
			call->appendElement( cfg::Element(callExpr) );

			ret->callBlock() = call;

			CFG::VertexTy&& callVertex = cfg->addBlock( call );
			cfg->addEdge(callVertex, bounds.first); // CALL -> Function Entry

			CFG::VertexTy&& retVertex = cfg->addBlock( ret );
			cfg->addEdge(bounds.second, retVertex); // Function Exit -> RET

			cfg->addEdge(retVertex, succ);

			succ = callVertex;
			currBlock = NULL;

		} else {
			// we are in the multistmt per block mode we should not append and create a new block here
			appendPendingBlock(); // FIXME
			createBlock();
			currBlock->appendElement( cfg::Element(callExpr) );
			appendPendingBlock();
		}

		bool hasAllocated=false;
		if ( !hasHead ) {
			assert(!spawnBlock);
			spawnBlock = new cfg::Block( cfg::Block::DEFAULT );
			head = cfg->addBlock( spawnBlock );
			hasHead = true;
			hasAllocated=true;
		}

		CFG::VertexTy sink = succ;

		bool hasSpawned = false;
		const vector<ExpressionPtr>& args = callExpr->getArguments();
		std::for_each(args.begin(), args.end(), [this, sink, &hasSpawned](const ExpressionPtr& curr){

			// in the case the argument is a call expression, we need to allocate a separate block in order to
			// perform the inter-procedural function call
			if ( curr->getNodeType() == NT_CallExpr || curr->getNodeType() == NT_CaptureInitExpr || curr->getNodeType() == NT_CastExpr ) {
				this->createBlock();
				this->visit(curr);
				this->appendPendingBlock();

				if(this->succ != sink) {
					hasSpawned = true;
				}

				this->succ = sink;
			}

		});

		if ( !hasSpawned ) {
			cfg->addEdge(head, succ);
		}

		if ( hasAllocated ) {
			succ = head;
			currBlock = spawnBlock;
			isPending = false;
			hasHead = false;
			spawnBlock = NULL;
		}
	}

	void visitLambdaExpr(const LambdaExprPtr& lambda) {
		scopeStack.push( Scope(lambda, CFG::VertexTy(), succ) );
		visit(lambda->getBody());
		scopeStack.pop();
		appendPendingBlock();
	}

	void visitProgram(const ProgramPtr& program) {
		const Program::EntryPointList& entryPoints = program->getEntryPoints();
		std::for_each(entryPoints.begin(), entryPoints.end(),
			[ this ]( const ExpressionPtr& curr ) {
				this->succ = this->exit;
				this->visit(curr);
				// connect the resulting block with the entry point
				this->cfg->addEdge( this->entry, this->succ );
			}
		);
		succ = entry;
	}

	void visitStatement(const StatementPtr& stmt) {
		assert(currBlock);
		currBlock->appendElement(stmt);
	}

};

template <>
void CFGBuilder<OneStmtPerBasicBlock>::visitCompoundStmt(const CompoundStmtPtr& compStmt) {
	const std::vector<StatementPtr>& body = compStmt->getStatements();

	if ( body.empty() ) 	return;

	CFG::VertexTy old = succ;
	appendPendingBlock();

	// we are sure there is at least 1 element in this compound statement
	for_each(body.rbegin(), body.rend(),
		[ this, &old ](const StatementPtr& curr) {
			this->createBlock();
			this->visit(curr);
			this->appendPendingBlock();
		}
	);
}

template <>
void CFGBuilder<MultiStmtPerBasicBlock>::visitCompoundStmt(const CompoundStmtPtr& compStmt) {
	const std::vector<StatementPtr>& body = compStmt->getStatements();

	if ( body.empty() )		return;

	createBlock();
	// we are sure there is at least 1 element in this compound statement
	for_each(body.rbegin(), body.rend(),
		[ this ](const StatementPtr& curr) { this->visit(curr); }
	);
}


// insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr> CFGBuilder::cfgMap = insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr>();
} // end anonymous namespace

namespace insieme {
namespace analysis {

template <>
CFGPtr CFG::buildCFG<OneStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<OneStmtPerBasicBlock> builder(cfg, rootNode);
	cfg->printStats(std::cout);
	return cfg;
}

template <>
CFGPtr CFG::buildCFG<MultiStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<MultiStmtPerBasicBlock> builder(cfg, rootNode);
	cfg->printStats(std::cout);
	return cfg;
}

CFG::VertexTy CFG::addBlock(cfg::Block* block) {
	CFG::VertexTy&& v = boost::add_vertex(graph);
	CFG::NodePropertyMapTy&& block_map = get(&NodeProperty::block, graph);
	block->blockId() = v;
	put(block_map, v, block);

	// Set the index appropriately
	boost::property_map< CFG::ControlFlowGraph, boost::vertex_index_t>::type&& blockID = get(boost::vertex_index, graph);
	put(blockID, v, currId++);
	return v;
}

std::pair<CFG::VertexTy,CFG::VertexTy> CFG::addSubGraph(const NodePtr& root) {
	CFG::VertexTy&& entry = addBlock(new cfg::Block(cfg::Block::ENTRY) );
	CFG::VertexTy&& exit = addBlock(new cfg::Block(cfg::Block::EXIT) );
	if(subGraphs.empty()) {
		setEntry(entry);
		setExit(exit);
	}
	return subGraphs.insert( std::make_pair(root, std::make_pair(entry, exit)) ).first->second;
}

void CFG::replaceNode(const CFG::VertexTy& oldNode, const CFG::VertexTy& newNode) {
	// collect the outgoing edges
	std::vector<VertexTy> dest;
	AdjacencyIterator vi, vi_end;
	tie(vi, vi_end) = adjacent_vertices(oldNode, graph);
	std::copy( vi, vi_end, std::back_inserter(dest) );

	std::vector<cfg::Edge> edges;

	OutEdgeIterator ei, ei_end;
	boost::tie(ei, ei_end) = boost::out_edges(oldNode, graph);

	std::for_each( ei, ei_end, [this, &edges](const EdgeTy& curr) {
		EdgePropertyMapTy&& edgeMap = get(&EdgeProperty::edge, graph);
		edges.push_back( edgeMap[curr] );
	});

	assert(dest.size() == edges.size() && "Number of outgoing edges and children of the node should be the same");

	boost::clear_in_edges(oldNode, graph);
	boost::clear_out_edges(oldNode, graph);
	remove_vertex(oldNode, graph);

	std::vector<cfg::Edge>::const_iterator eit = edges.begin();
	for ( std::vector<VertexTy>::const_iterator vit=dest.begin(), end=dest.end(); vit != end; ++vit, ++eit) {
		this->addEdge(newNode, *vit, *eit);
	}
}

void CFG::printStats(std::ostream& out) {
	out << "***********************************************" << std::endl;
	out << "* Num. of CFGs:        " << subGraphs.size() << std::endl;
	out << "* Num. of total nodes: " << boost::num_vertices(graph) << std::endl;
	out << "* Num. of total edges: " << boost::num_edges(graph) << std::endl;
	out << "***********************************************" << std::endl;
}


CFG::~CFG() {
	boost::graph_traits<ControlFlowGraph>::vertex_iterator vi, vi_end;
	for (boost::tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
		const cfg::Block* block = &getBlock(*vi);
		delete block;
	}
}

} // end analysis namespace
} // end insieme namespace

namespace std {

// Prints the CFG in a DOT fromat
std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg) {
	using namespace insieme::analysis;

	CFG::ConstNodePropertyMapTy&& node = get(&CFG::NodeProperty::block, cfg.graph);
	CFG::ConstEdgePropertyMapTy&& edge = get(&CFG::EdgeProperty::edge, cfg.graph);

	boost::write_graphviz(out, cfg.graph,
			CFG::BlockLabelWriter<CFG::ConstNodePropertyMapTy>(node),
			CFG::EdgeLabelWriter<CFG::ConstEdgePropertyMapTy>(edge)
	);
	return out;
}

namespace {
//~~~~~ DOT FILE PRINTING UTILITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Functions invoked by the boost dot visitor to print the CFG in a dot file 
	
// print IR nodes to a string and removes eventual new lines and leading spaces
std::string getPrettyPrinted(const NodePtr& node) {
	std::ostringstream ss;
	ss << printer::PrettyPrinter(node, printer::PrettyPrinter::OPTIONS_DETAIL | 1 << 5);

	// Remove new lines and leading spaces
	std::vector<char> res;
	std::string prettyPrint = ss.str();
	for(auto it = prettyPrint.begin(), end = prettyPrint.end(); it != end; ++it)
		if(!(*it == '\n' || (it + 1 != end && *it == ' ' && *(it+1) == ' '))) {
			if(*it != '\"')
				res.push_back(*it);
			else {
				res.push_back('\\');
				res.push_back('\"');
			}
		}

	return std::string(res.begin(), res.end());
}
} // end anonymous namespace

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Block& block) {
	switch ( block.type() ) {
	case cfg::Block::DEFAULT:
	{
		out << "[shape=box,label=\"";
		out << "[B" << block.blockId() << "]\\l";
		size_t num = 0;
		std::for_each(block.stmt_begin(), block.stmt_end(), [ &out, &num ](const insieme::analysis::cfg::Element& curr) {
			out << num++ << ": ";
			switch(curr.getType()) {
			case cfg::Element::None:
				out << getPrettyPrinted( curr );
				break;
			case cfg::Element::CtrlCond:
				out << getPrettyPrinted( curr ) << " <CTRL>";
				break;
			case cfg::Element::LoopInit: {
				out << getPrettyPrinted( static_pointer_cast<const ForStmt>(curr)->getDeclaration() ) << " <LOOP_INIT>";
				break;
			}
			case cfg::Element::LoopIncrement: {
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(curr);
				out << printer::PrettyPrinter(forStmt->getDeclaration()->getVariable()) << " += "
				    << printer::PrettyPrinter( forStmt->getStep(), 10001 ) << " <LOOP_INC>";
				break;
			}
			default:
				break;
			}
			out << "\\l";
		});
		if(block.hasTerminator())
			out << "T: " << block.terminator();

		return out << "\"]";
	}
	case cfg::Block::CALL:
		return out << "[shape=box,label=\"CALL\"]";
	case cfg::Block::RET:
		return out << "[shape=box,label=\"RET\"]";
	case cfg::Block::ENTRY:
		return out << "[shape=diamond,label=\"ENTRY\"]";
	case cfg::Block::EXIT:
		return out << "[shape=diamond,label=\"EXIT\"]";
	default:
		return out;
	}
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Terminator& term) {
	// assert(type == Element::TERMINAL);
	switch(term->getNodeType()) {
	case NT_IfStmt:
		return out << "IF(...)\\l";
	case NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>(term);
		return out << "FOR( " << "... ; "
				<< printer::PrettyPrinter( forStmt->getDeclaration()->getVariable(), 10001 ) << " < "
				<< printer::PrettyPrinter(forStmt->getEnd(), 10001 ) << "; ...)\\l";
	}
	case NT_WhileStmt:
		return out << "WHILE(...)\\l";
	case NT_SwitchStmt:
		return out << "SWITCH(...)\\l";
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


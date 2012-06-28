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

#include <boost/graph/strong_components.hpp>
#include <boost/graph/graphviz.hpp>

#include <boost/graph/breadth_first_search.hpp>

#include <boost/graph/depth_first_search.hpp>

#include "insieme/analysis/cfg.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

#include <tuple>
#include <stack>

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

typedef std::vector<ExpressionAddress> ExpressionAddressList;

namespace {

/**
 * Scope - Store the entry and exit blocks of program scopes. This is used in case of continue or
 * break statements to jump to the corresponding block.
 */
struct Scope {
	NodeAddress root;         // The IR node which defines this scope
	CFG::VertexTy entry;   // The CFG node where the scope begins
	CFG::VertexTy exit;    // The CFG node where the scope ends

	Scope(const NodeAddress& root, const CFG::VertexTy& entry, const CFG::VertexTy& exit) :
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
		// in the case we have jump outside a function we have to look for the innermost enclosing
		// lambda expression
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
	 * Search for enclosing scope to be returned. The filter class is used to filter out scopes
	 * which are not of interested because neglected by the particular control flow statement (e.g.
	 * continue statmente only looks for surrounding loops, not eventual switch statements)
	 *
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
struct CFGBuilder: public IRVisitor< void, Address > {

	CFGPtr cfg;
	IRBuilder builder;

	// A pointer to the block which is currently the head of graph which is built
	// bottom-up visiting the statements in reverse order
	cfg::Block* currBlock, *spawnBlock;

	// Tells us if currBlock has been already inserted in the CFG graph or it is only a temporary element
	bool isPending;

	CFG::VertexTy entry, exit, succ, head;
	ScopeStack 	scopeStack;
	
	bool hasHead;

	std::stack<size_t> argNumStack;

	size_t maxSpawnedArg;

	CFGBuilder(CFGPtr cfg, const NodePtr& root) : 
		IRVisitor<void, Address>(false), 
		cfg(cfg), 
		builder(root->getNodeManager()), 
		currBlock(NULL), 
		spawnBlock(NULL), 
		isPending(false), 
		hasHead(false) 
	{
		assert( !cfg->hasSubGraph(root) && "CFG for this root node already being built");
		CFG::GraphBounds&& bounds = cfg->addSubGraph(root);
		// initialize the entry/exit blocks for this CFG
		entry = bounds.first;
		head = entry;
		exit = bounds.second;
		succ = exit;

		visit( NodeAddress(root) ); 				// Visit the IR

		// Performs the final steps to finalize the CFG
		appendPendingBlock(); 		// if we still have pending node we add them to the CFG

		if ( entry == succ )	return;

		if ( cfg->getBlock(succ).empty() ) {
			// If the first statement of a root element is a function call we end up with an empty
			// statement at the top of the CFG, we want to remove that block and connect the
			// outgoing edges to the entry node
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
		currBlock = new cfg::Block(*cfg);
		isPending = true;
	}

	void resetCurrBlock(){ currBlock = NULL; }

	// Appends the block currently referred by currBlock pointer to the graph if it is pending.
	// This requires to add a node to the CFG and connect it to the current head of the CFG which is
	// referred by the succ variable. The pointer to the head (succ) will be then updated to point
	// to the node just inserted. 
	void appendPendingBlock(bool soft=true) {

		// if we already have allocated an empty block and it is not pending
        if ( soft && !isPending && currBlock && currBlock->empty() ) { return; }

		// In the case the currBlock is pending and not empty we add it to the Graph and connect
		// it with the successive node in the CFG
		if ( isPending && currBlock && !currBlock->empty() ) {
			CFG::VertexTy&& node = cfg->addBlock(currBlock);
			
			cfg->addEdge( node, succ, 
					(argNumStack.empty() ? cfg::Edge() : cfg::Edge(builder.intLit( argNumStack.top() ))) 
				);

			succ = node;
			resetCurrBlock();
		}
		
		// The node is pending but the content is empty, in this case we avoid to insert 
		// the node in the CFG and free the memory used to allocate the empty block
		if ( isPending && currBlock ) {
			delete currBlock;
			resetCurrBlock();
		}

		if ( !isPending && currBlock ) { resetCurrBlock(); }
		isPending = false;

		// check post conditions
		assert(!isPending && !currBlock && "Failed to satisfy postconditions");
	}

	/**
	 * When a continue statement is encountered we jump to the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitContinueStmt(const ContinueStmtAddress& continueStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->terminator() = cfg::Terminator(continueStmt);
		succ = scopeStack.getContinueTarget().entry;
	}

	/**
	 * When a break statement is encountered we jump right after the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitBreakStmt(const BreakStmtAddress& breakStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->terminator() = cfg::Terminator( breakStmt );
		succ = scopeStack.getBreakTarget().exit;
	}

	/**
	 * When a return statement is encountered we jump to the exit block of the closest enclosing function scope
	 * (i.e. lambda expression)
	 */
	void visitReturnStmt(const ReturnStmtAddress& retStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));
		
		createBlock();
		currBlock->terminator() = cfg::Terminator(retStmt);
		succ = scopeStack.getEnclosingLambda().exit;

		visit( retStmt->getReturnExpr() );
	}

	void visitMarkerStmt(const MarkerStmtAddress& markerStmt) {
		visit( markerStmt->getSubStatement() );
	}

	void visitMarkerExpr(const MarkerExprAddress& markerExpr) {
		visit( markerExpr->getSubExpression() );
	}

	void visitIfStmt(const IfStmtAddress& ifStmt) {
		cfg::Block* ifBlock = new cfg::Block(*cfg);
		ifBlock->terminator() = cfg::Terminator(ifStmt);
		CFG::VertexTy&& src = cfg->addBlock( ifBlock );
		
		appendPendingBlock(false); // append any pending block before we fork the CFG for inserting the for stmt
		CFG::VertexTy sink = succ;
		
		createBlock();
		visit(ifStmt->getThenBody());
		appendPendingBlock();
		cfg->addEdge(src, succ, cfg::Edge( builder.getLangBasic().getTrue() )); 
		resetCurrBlock();

		succ = sink; // reset the successor for the thenBody

		createBlock();
		visit(ifStmt->getElseBody());
		appendPendingBlock();
	
		cfg->addEdge(src, succ, cfg::Edge( builder.getLangBasic().getFalse() ));

		succ = src; 		// succ now points to the head of the IF stmt
		currBlock = ifBlock;
		isPending = false;

		visit( ifStmt->getCondition() );
	}

	void visitForStmt(const ForStmtAddress& forStmt) {
		cfg::Block* forBlock = new cfg::Block(*cfg);
		forBlock->terminator() = cfg::Terminator(forStmt);
		forBlock->appendElement( cfg::Element(forStmt->getEnd(), cfg::Element::CTRL_COND) );
		CFG::VertexTy&& forHead = cfg->addBlock( forBlock );

		appendPendingBlock(false);  // append any pending block before we fork the CFG for inserting the for stmt
		resetCurrBlock();

		CFG::VertexTy sink = succ;
		CFG::VertexTy src = forHead; 

		const ExpressionAddress& endCond = forStmt->getEnd();
		if ( endCond->getNodeType() == NT_CallExpr || endCond->getNodeType() == NT_CastExpr ) {
			succ = forHead;
			createBlock();
			// Visit expressions in the End conditions 
			visit( forStmt->getEnd() );
			appendPendingBlock(); 
			src = succ;
		} 

		// increment expression
		cfg::Block* incBlock = new cfg::Block(*cfg);
		incBlock->appendElement( cfg::Element(forStmt, cfg::Element::LOOP_INCREMENT) );
		CFG::VertexTy&& inc = cfg->addBlock( incBlock );
		cfg->addEdge(inc, src);

		succ = inc;

		createBlock();
		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, sink) );
		visit( forStmt->getBody() );
		scopeStack.pop();

		appendPendingBlock(false);

		cfg->addEdge(forHead, succ, cfg::Edge( builder.getLangBasic().getTrue() )); 
		cfg->addEdge(forHead, sink, cfg::Edge( builder.getLangBasic().getFalse() )); 

		succ = src;
		// decl stmt of the for loop needs to be part of the incoming block
		createBlock();
		currBlock->appendElement( cfg::Element(forStmt, cfg::Element::LOOP_INIT) );
	}
	
	void visitWhileStmt(const WhileStmtAddress& whileStmt) {
		cfg::Block* whileBlock = new cfg::Block(*cfg);
		whileBlock->terminator() = cfg::Terminator(whileStmt);
		CFG::VertexTy&& src = cfg->addBlock( whileBlock );

		// the current node needs to be append to the graph (if not empty)
		appendPendingBlock(false);
		CFG::VertexTy sink = succ;

		succ = src;
		scopeStack.push( Scope(whileStmt, src, sink) );
		createBlock();
		// push scope into the stack for this compound statement
		visit(whileStmt->getBody());
		scopeStack.pop();

		appendPendingBlock();
		cfg->addEdge(src, succ, cfg::Edge( builder.getLangBasic().getTrue() )); 
		cfg->addEdge(src, sink, cfg::Edge( builder.getLangBasic().getFalse() ));

		succ = src;
		currBlock = whileBlock;
		isPending = false;

		visit( whileStmt->getCondition() );
	}

	void visitCastExpr(const CastExprAddress& castExpr) {
		assert(currBlock);
		currBlock->appendElement( cfg::Element(castExpr) );
		appendPendingBlock(); 
		
		ExpressionAddress subExpr = castExpr->getSubExpression();
		if ( subExpr->getNodeType() == NT_CastExpr || subExpr->getNodeType() == NT_CallExpr ) {
			createBlock();
			visit(subExpr);
			appendPendingBlock();
		} else if ( !argNumStack.empty() ) {
			// it meas this CastExpression was in the middle of callExpr, therefore add a link to
			// the head node 
			assert(hasHead);
			cfg->addEdge( head, succ );
		}
	}

	void visitSwitchStmt(const SwitchStmtAddress& switchStmt) {
		cfg::Block* switchBlock = new cfg::Block(*cfg);
		switchBlock->terminator() = cfg::Terminator(switchStmt);
		CFG::VertexTy&& src = cfg->addBlock( switchBlock );

		// the current node needs to be append to the graph (if not empty)
		appendPendingBlock(false);
		CFG::VertexTy sink = succ;

		scopeStack.push( Scope(switchStmt, src, sink) );
		SwitchCasesAddress cases = switchStmt->getCases();
		for ( auto it = cases.begin(), end = cases.end(); it != end; ++it ) {
			const SwitchCaseAddress& curr = *it;
			succ = sink;
			createBlock();
			// push scope into the stack for this compound statement
			visit(curr->getBody());

			appendPendingBlock();
			cfg->addEdge(src, succ);
			resetCurrBlock();
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

	void visitCompoundStmt(const CompoundStmtAddress& compStmt);

	void visitDeclarationStmt(const DeclarationStmtAddress& declStmt) {
		assert(currBlock);
		currBlock->appendElement( cfg::Element(declStmt) );
		appendPendingBlock();
		
		createBlock();
		visit(declStmt->getInitialization());
		appendPendingBlock(); 
	}

	void visitCallExpr(const CallExprAddress& callExpr) {
		// if the call expression is calling a lambda the body of the lambda is processed and the
		// sub graph is built
		if ( callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr ) {
			const LambdaExprAddress& lambdaExpr = static_address_cast<const LambdaExpr>(callExpr->getFunctionExpr());

			if ( !cfg->hasSubGraph(lambdaExpr.getAddressedNode()) ) {
				// In the case the body has not been visited yet, proceed with the graph construction
				// TODO: This can be executed in a separate thread (if necessary)
				CFG::buildCFG<CP>(lambdaExpr.getAddressedNode(), cfg);
			}

			appendPendingBlock();

			CFG::GraphBounds&& bounds = cfg->getNodeBounds(lambdaExpr.getAddressedNode());
			// A call expression creates 2 blocks, 1 spawning the function call and the second one
			// collecting the return value
			cfg::CallBlock* call = new cfg::CallBlock(*cfg);
			cfg::RetBlock* ret = new cfg::RetBlock(*cfg);

			// we interconnect the two blocks so that if we want to have intra-procedural analysis
			// we can jump directly to the return block without visiting the body of the function
			call->setReturnBlock( *ret );
			// call->appendElement( cfg::Element(callExpr) );

			ret->setCallBlock(*call);

			CFG::VertexTy&& callVertex = cfg->addBlock( call );
			cfg->addEdge(callVertex, bounds.first); // CALL -> Function Entry

			CFG::VertexTy&& retVertex = cfg->addBlock( ret );
			cfg->addEdge(bounds.second, retVertex); // Function Exit -> RET

			cfg->addEdge(retVertex, succ, (argNumStack.empty()?cfg::Edge():cfg::Edge(builder.intLit(argNumStack.top()))) );

			succ = callVertex;
			resetCurrBlock();

		} else {
			// we are in the multistmt per block mode we should not append and create a new block
			// here
			assert(currBlock);
			currBlock->appendElement( cfg::Element(callExpr) );
			appendPendingBlock();
		}

		bool hasAllocated=false;
		if ( !hasHead ) {
			assert(!spawnBlock);
			spawnBlock = new cfg::Block( *cfg, cfg::Block::DEFAULT );
			head = cfg->addBlock( spawnBlock );
			hasHead = true;
			hasAllocated = true;
			maxSpawnedArg = 0;
		}

		CFG::VertexTy sink = succ;

		size_t spawnedArgs = 0;
		const vector<ExpressionAddress>& args = callExpr->getArguments();
		argNumStack.push(0);
		std::for_each(args.begin(), args.end(), [ this, sink, &spawnedArgs ] (const ExpressionAddress& curr) {

			// in the case the argument is a call expression, we need to allocate a separate block
			// in order to perform the inter-procedural function call
			if ( curr->getNodeType() == NT_CallExpr || 
				 curr->getNodeType() == NT_CastExpr || 
				 curr->getNodeType() == NT_MarkerExpr) 
			{
				this->createBlock();
				this->visit(curr);
				this->appendPendingBlock();
				
				if ( this->succ != sink ) {
					++spawnedArgs;
				}

				this->succ = sink;
			}
			this->argNumStack.top()++;

		});
		argNumStack.pop();
	
		if(spawnedArgs > maxSpawnedArg) {
			maxSpawnedArg = spawnedArgs;
		}
		// In the case a spawnblock has been created to capture arguments of the callExpr but no
		// arguments were call expressions, therefore the created spawnblock is not necessary. 
		if ( maxSpawnedArg<2 && hasAllocated ) {
			if (spawnedArgs == 1) {
				succ = **cfg->successors_begin(head);
			}

			// remove the spawned block from the CFG 
			cfg->removeBlock( head );
			// delete spawnBlock;
			spawnBlock = NULL;

			// set the head to false (for next calls to this function)
			hasHead = false;
			isPending = false;
			return;
		}

		if ( spawnedArgs==0 && !hasAllocated ) {
			cfg->addEdge(head, succ);
		}

		if ( hasAllocated ) {
			succ = head;
			currBlock = spawnBlock;
			isPending = false;
			hasHead = false;
			spawnBlock = NULL;
			hasAllocated=false;
		}
	}

	void visitLambdaExpr(const LambdaExprAddress& lambda) {
		scopeStack.push( Scope(lambda, CFG::VertexTy(), succ) );
		visit(lambda->getBody());
		scopeStack.pop();
		appendPendingBlock();
	}

	void visitProgram(const ProgramAddress& program) {
		const ExpressionAddressList& entryPoints = program->getEntryPoints();
		std::for_each(entryPoints.begin(), entryPoints.end(),
			[ this ]( const ExpressionAddress& curr ) {
				this->succ = this->exit;
				this->visit(curr);
				// connect the resulting block with the entry point
				this->cfg->addEdge( this->entry, this->succ );
			}
		);
		succ = entry;
	}

	void visitStatement(const StatementAddress& stmt) {
		assert(currBlock);
		currBlock->appendElement( stmt );
	}

};

template <>
void CFGBuilder<OneStmtPerBasicBlock>::visitCompoundStmt(const CompoundStmtAddress& compStmt) {
	const std::vector<StatementAddress>& body = compStmt->getStatements();

	if ( body.empty() ) {
		return;
	}	

	CFG::VertexTy old = succ;
	appendPendingBlock();

	// we are sure there is at least 1 element in this compound statement
	for_each(body.rbegin(), body.rend(),
		[ this, &old ](const StatementAddress& curr) {
			this->createBlock();
			this->visit(curr);
			this->appendPendingBlock();
		}
	);
}

template <>
void CFGBuilder<MultiStmtPerBasicBlock>::visitCompoundStmt(const CompoundStmtAddress& compStmt) {
	const std::vector<StatementAddress>& body = compStmt->getStatements();

	if ( body.empty() ) {
		return;
	}

	createBlock();
	// we are sure there is at least 1 element in this compound statement
	for_each(body.rbegin(), body.rend(),
		[ this ](const StatementAddress& curr) { this->visit(curr); }
	);
}

} // end anonymous namespace

namespace insieme {
namespace analysis {

template <>
CFGPtr CFG::buildCFG<OneStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<OneStmtPerBasicBlock> builder(cfg, rootNode);
	return cfg;
}

template <>
CFGPtr CFG::buildCFG<MultiStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<MultiStmtPerBasicBlock> builder(cfg, rootNode);
	return cfg;
}

CFG::VertexTy CFG::addBlock(cfg::Block* block) {
	assert(block);
	CFG::VertexTy&& v = boost::add_vertex(graph);
	
	block->setVertexID(v);
	graph[v] = std::shared_ptr<cfg::Block>(block);

	// Set the index appropriately
	BlockIDPropertyMapTy&& blockID = get(boost::vertex_index, graph);
	put(blockID, v, currId++);

	return v;
}

void CFG::removeBlock(const CFG::VertexTy& v) {
	boost::clear_in_edges(v, graph);
	boost::clear_out_edges(v, graph);
	boost::remove_vertex(v, graph);
}

std::pair<CFG::VertexTy,CFG::VertexTy> CFG::addSubGraph(const NodePtr& root) {
	CFG::VertexTy&& entry = addBlock( new cfg::Block(*this, cfg::Block::ENTRY) );
	CFG::VertexTy&& exit = addBlock( new cfg::Block(*this, cfg::Block::EXIT) );
	if (subGraphs.empty()) {
		entry_block = entry; exit_block = exit;
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
		edges.push_back( graph[curr] );
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

bool CFG::isEntry(const cfg::BlockPtr& block) const { 
	return block->getVertexID() == entry_block;
}

bool CFG::isExit(const cfg::BlockPtr& block) const { 
	return block->getVertexID() == exit_block;
}

CFG::EdgeTy CFG::addEdge(const VertexTy& src, const VertexTy& dest, const cfg::Edge& edge) {
	std::pair<EdgeTy, bool>&& edgeDesc = boost::add_edge(src, dest, graph);
	// we don't allow to insert the same edge twice, if this happens something 
	// is wrong in the construction of the CFG graph
	assert( edgeDesc.second && "Tried to insert a duplicated edge, forbidden!");

	graph[edgeDesc.first] = edge;
	return edgeDesc.first;
}

// Returns the Edge object associated to a graph edge connecting src and dest vertices 
const cfg::Edge& CFG::getEdge(const CFG::VertexTy& src, const CFG::VertexTy& dest) const { 
	auto edgeDescriptor = boost::edge(src, dest, graph);
	assert(edgeDescriptor.second && "No edge exists between the two selected vertices");
	return graph[edgeDescriptor.first];
}

void CFG::printStats(std::ostream& out) {
	out << "# of CFGs:        " << subGraphs.size() << std::endl;
	out << "# of total nodes: " << boost::num_vertices(graph) << std::endl;
	out << "# of total edges: " << boost::num_edges(graph) << std::endl;
}

// Prints the CFG in a DOT fromat
std::ostream& CFG::printTo(std::ostream& out) const {
	using namespace insieme::analysis;

	/**
	 * Lambda for the production of a DOT graph from the boost::graph.
	 * This class takes care of printing the content of CFG Blocks to the output stream.
	 */
	auto node_printer = [&](std::ostream& out, const VertexTy& v) {
		out << *graph[v];
	};

	/**
	 * Lambda for the production of a DOT graph from the boost::graph.
	 * This class takes care of printing the content of CFG Edges to the output stream.
	 */
	auto edge_printer = [&](std::ostream& out, const EdgeTy& edge) {
		const cfg::Edge& e = graph[edge];
		out << "[label=\"";
		if (e.getEdgeExpr()) { out << *e.getEdgeExpr(); }
		out << "\"]";
	};

	boost::write_graphviz(out, graph, node_printer, edge_printer);
	return out;
}


int CFG::getStrongComponents() {
	// Map kept to register the connected components,
	// Each vertex is associated to a component which is identified with an intereger number 
	typedef std::map<VertexTy, int> component_type;
	component_type component;
	boost::associative_property_map<component_type> component_map(component);

	typedef std::map<VertexTy, VertexTy> root_type;
	root_type root;
	boost::associative_property_map<root_type> root_map(root);

	typedef std::map<VertexTy, boost::default_color_type> color_type;
	color_type color;
	boost::associative_property_map<color_type> color_map(color);

	component_type discover;
	boost::associative_property_map<component_type> discover_map(discover);

	int num = strong_components(graph, component_map, 
			boost::root_map(root_map).
			color_map(color_map).
			discover_time_map(discover_map));
	

	// FIXME: add Analysis of Connected Components here
	
	return num;
	// std::cout << "Total number of components: " << num << std::endl;
	//
	//BlockIDPropertyMapTy&& blockID = get(boost::vertex_index, graph);
	//for_each ( component.begin(), component.end(), [&] (const component_type::value_type& cur) {
	//	std::cout << "Vertex " << blockID[cur.first] <<" is in component " << cur.second << std::endl;
	//});
}

cfg::BlockPtr CFG::find(const core::NodeAddress& node) const {

	cfg::BlockPtr found;
	
	auto&& block_visitor = [&] (const cfg::BlockPtr& block) -> void {
		for_each(block->stmt_begin(), block->stmt_end(), [&](const cfg::Element& cur) {

			NodeAddress addr = Address<const Node>::find( 
					node.getAddressedNode(), 
					static_pointer_cast<const Node>(cur.getStatementAddress().getAddressedNode()) 
				);

			// if we find the statement inside this block, we return 
			if (addr) { 
				assert(!found && "Another node already matched the requrested node");
				found = block;
				return; 
			}
		});
	};

	// Stop the depth search visitor once we find the node 
	auto terminator = [&] (const CFG::VertexTy& v, const CFG::ControlFlowGraph& g) {
		if (found) { return true; }
		return false;
	};

	typedef std::map<VertexTy, boost::default_color_type> color_type;
	color_type color;
	boost::associative_property_map<color_type> color_map(color);

	boost::depth_first_visit( graph, 
			entry_block, 
			boost::make_dfs_visitor( BlockVisitor(block_visitor) ),
			color_map, 
			terminator
		);
	
	return found;
}

namespace cfg {


namespace {

//~~~~~ DOT FILE PRINTING UTILITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


// FOLLOWING FUNCTION NEEDS REFACTORING!!!! PLEASE REFACTOR ME!
std::ostream& Block::printTo(std::ostream& out) const {

	// Lambda used for printing out Call Expressions 
	auto&& print_call_expr = [&] (const CallExprPtr& callExpr) {
		IRBuilder builder(callExpr->getNodeManager());

		out << *callExpr->getFunctionExpr() << "(";

		// for each argument we have to check if we spawned a block to evaluate it or not in
		// positive case we just write a reference to the block containing the evaluation of the
		// argument 
		const CFG& cfg = getParentCFG();

		const ExpressionList& args = callExpr->getArguments();
		auto predIT = predecessors_begin(), end = predecessors_end();

		size_t argID = 0, argSize = args.size();
		for_each(args, [&] (const ExpressionPtr& curr) {
			bool matched = false;
			if (predIT != end) {
				const cfg::Edge& edge = cfg.getEdge(**predIT, *this);
				if (edge.getEdgeExpr() && *edge.getEdgeExpr() == *builder.intLit(argID)) {
					out << "[B" << cfg.getBlockID(**predIT) << "]." << argID;
					++predIT;
					matched = true;
				}
			}
			if (!matched) {	out << getPrettyPrinted( curr ); }

			if (++argID != argSize) { out << ", "; }
		});

		out << ")";
	};

	auto&& print_cast_expr = [&] (const CastExprPtr& castExpr) {
		
		IRBuilder builder(castExpr->getNodeManager());
		out << "CAST<" << getPrettyPrinted(castExpr->getType()) << ">"; 

		auto predIT = predecessors_begin(), end = predecessors_end();
		if (predIT == end) {
			out << "(" << getPrettyPrinted( castExpr->getSubExpression() ) << ")";
			return;
		}
		const CFG& cfg = getParentCFG();
		const cfg::Edge& edge = cfg.getEdge(**predIT, *this);
		if ( edge.getEdgeExpr() && *edge.getEdgeExpr() == *builder.intLit(0) ) { 
			out << "(...)"; 
		} 
	};

	switch ( type() ) {

	case Block::DEFAULT:
	{
		// CFG Blocks have box shape by default
		out << "[shape=box,label=\"";
		// The first line state the id of this block 
		out << "[B" << getBlockID() << "]\\l";

		size_t num = 0;

		std::for_each(stmt_begin(), stmt_end(), [&](const Element& curr) {
			out << num++ << ": ";
			core::StatementPtr currStmt = curr;
			switch(curr.getType()) {
			case cfg::Element::NONE:
				switch (static_cast<StatementPtr>(curr)->getNodeType()) {
				case core::NT_CallExpr:
					print_call_expr(static_pointer_cast<const CallExpr>(currStmt));
					break;

				case core::NT_CastExpr:
					print_cast_expr(static_pointer_cast<const CastExpr>(currStmt));
					break;

				case core::NT_DeclarationStmt:
					out << "decl " 
						<< getPrettyPrinted( 
							static_pointer_cast<const DeclarationStmt>(currStmt)->getVariable()) 
						<< " = ...";
					break;

				default:
					out << getPrettyPrinted( currStmt );
				}
				break;
			case cfg::Element::CTRL_COND:
				out << getPrettyPrinted( currStmt ) << " <CTRL>";
				break;
			case cfg::Element::LOOP_INIT: {
				out << getPrettyPrinted( 
						static_pointer_cast<const ForStmt>(currStmt)->getStart() 
					) << " <LOOP_INIT>";
				break;
			}
			case cfg::Element::LOOP_INCREMENT: {
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(currStmt);
				out << printer::PrettyPrinter( forStmt->getIterator() ) << " += "
					<< printer::PrettyPrinter( forStmt->getStep(), 10001 ) << " <LOOP_INC>";
				break;
			}
			default: break;
			}
			out << "\\l";
		});

		if(hasTerminator())
			out << "T: " << terminator();

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

std::ostream& Terminator::printTo(std::ostream& out) const {
	// assert(type == Element::TERMINAL);
	core::StatementPtr stmt = *this;
	switch ( stmt->getNodeType() ) {

	case NT_IfStmt: 		return out << "IF(...)\\l";

	case NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>( stmt );
		return out << "FOR( " << "... ; "
				<< printer::PrettyPrinter( forStmt->getIterator(), 10001 ) << " < "
				<< printer::PrettyPrinter( forStmt->getEnd(), 10001 ) << "; ...)\\l";
	}

	case NT_WhileStmt:		return out << "WHILE(...)\\l";

	case NT_SwitchStmt:		return out << "SWITCH(...)\\l";

	case NT_ContinueStmt:	return out << "CONTINUE\\l";

	case NT_BreakStmt:		return out << "BREAK\\l";

	case NT_ReturnStmt: 	return out << "RETURN\\l";

	default:
		assert(false && "Terminator statement is not supported");
	}
}

} // end cfg namespace 
} // end analysis namespace
} // end insieme namespace


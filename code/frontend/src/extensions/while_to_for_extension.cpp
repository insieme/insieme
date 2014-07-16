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

#include <iomanip>
#include <ostream>
#include <string>

#include "insieme/frontend/extensions/while_to_for_extension.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/assert.h"

namespace pattern = insieme::core::pattern;
namespace irp = insieme::core::pattern::irp;

namespace insieme {
namespace frontend {

/// Pretty Printer: allows to print the given node to the output stream
printer::PrettyPrinter WhileToForPlugin::pp(const core::NodePtr& n) {
	return printer::PrettyPrinter(n, printer::PrettyPrinter::NO_LET_BINDINGS);
}

/// Determines the maximum node path depth given node n as the root node. This procedure is most likely be
/// used before or during a PrintNodes invocation.
unsigned int WhileToForPlugin::maxDepth(const insieme::core::Address<const insieme::core::Node> n) {
	unsigned max = 0;
	for (auto c: n.getChildAddresses()) {
		unsigned now=maxDepth(c);
		if (now>max) max=now;
	}
	return max+1;
}

/// Print the nodes to std::cout starting from root n, one by one, displaying the node path
/// and the visual representation. The output can be prefixed by a string, and for visually unifying several
/// PrintNodes invocations the maximum (see MaxDepth) and current depth may also be given.
void WhileToForPlugin::printNodes(const insieme::core::Address<const insieme::core::Node> n,
								  std::string prefix="        @\t", int max=-1, unsigned int depth=0) {
	if (max<0) max=maxDepth(n);
	int depth0=n.getDepth()-depth;
	std::cout << prefix << n << std::setw(2*(max-depth+1)) << "+"
			  << std::setw(1+2*(depth0+depth-1)) << "" << *n << std::endl;
	for (auto c: n.getChildAddresses())
		if (depth<(unsigned int)max) printNodes(c, prefix, max, depth+1);
}

/// From the given set of condition variables, return a set of VariablePtr for further consumption.
insieme::utils::set::PointerSet<core::VariablePtr> WhileToForPlugin::extractCondVars(std::vector<core::NodeAddress> cvars) {
	insieme::utils::set::PointerSet<core::VariablePtr> cvarSet;

	// iterate over all condition variables
	for(core::Address<const core::Node> cvar: cvars) {
		core::Pointer<const core::Variable> varptr=cvar.getAddressedNode().as<core::VariablePtr>();

		if (!cvarSet.contains(varptr) && varptr->getType()->getNodeType()==core::NT_RefType)
			cvarSet.insert(varptr);
	}
	return cvarSet;
}

/// For a given variable, find all assignments in the loop body.
std::vector<core::NodeAddress> WhileToForPlugin::getAssignmentsForVar(core::NodeAddress body, core::VariablePtr var) {
	std::vector<core::Address<const core::Node> > assignments;

	// match all assignments where "var" occurs on the LHS
	// do not consider the rhs in the pattern, as the rhs not necessarily includes the variable in question
	// and/or an addition/subtraction expression
	auto assignpat=irp::assignment(irp::atom(var), pattern::any);                  // one single assignment
	auto assignall=pattern::aT(pattern::all(pattern::var("assignment", assignpat)));     // all assignments
	pattern::AddressMatchOpt nodes=assignall->matchAddress(body);

	// if the variable matched the pattern in the loop body, save the assignment
	if (nodes && nodes.get().isVarBound("assignment")) {
		auto myassignments=nodes.get()["assignment"].getFlattened();
		assignments.insert(assignments.end(), myassignments.begin(), myassignments.end());
	}
	return assignments;
}

/// Given a node "a", verify that it is a self-assignment to the variable with an added constant value,
/// and then extract the integer value (the step size in a for loop), returning it. As an example,
/// given the assignment "x = x - 5", this function would return the integer "-5".
NodeBookmark WhileToForPlugin::extractStepFromAssignment(core::Address<const core::Node> a) {
	NodeBookmark step;

	// set up the patterns and do the matching
	auto operatorpat=pattern::single(irp::literal("int.sub")|irp::literal("int.add"));
	auto assignpat=irp::assignment
			(pattern::var("lhs"), pattern::node(
				 pattern::any									// any type will do (could be integer)
				 << pattern::listVar("addsub", operatorpat)		// operation plus or minus, determines step
				 << pattern::listVar("ops", *pattern::any)));	// operation arguments
	pattern::AddressMatchOpt m=assignpat->matchAddress(a);

	// check whether we have a match, and can assign the nodes to the variables for further investigation
	// consider for now only IRs in the form: v1 = v1 +/- ...
	core::Address<const core::Node> lhs, addsub, op1, op2;
	if (m && m.get().isVarBound("lhs") && m.get().isVarBound("addsub") && m.get().isVarBound("ops")) {
		std::vector<core::Address<const core::Node> >
				addsubvec=m.get()["addsub"].getFlattened(),
				opsvec=m.get()["ops"].getFlattened();
		if (addsubvec.size()==1 && opsvec.size()==2) {
			lhs=m.get()["lhs"].getValue();
			addsub=addsubvec[0];
			op1=opsvec[0];
			op2=opsvec[1];
		} else
			step.err("more than one assignment to loop variable found");
	} else
		step.err("only integer addition and subtraction supported to deduce step size");

	// now we need to set up some IR to compare our nodes against
	core::Address<core::Literal>   intsub, intadd;
	core::Address<core::Statement> stmt;
	core::Pointer<const core::Node> op2ptr=op2.getAddressedNode();
	if (step.ok()) {
		// get the node manager and create int.sub, int.add literals for comparison
		core::NodeManager& mgr=addsub->getNodeManager();
		auto& basic=mgr.getLangBasic();
		intsub=core::LiteralAddress(basic.getSignedIntSub());
		intadd=core::LiteralAddress(basic.getSignedIntAdd());

		// create an IR builder and create an expression from the LHS variable to compare to
		core::IRBuilder builder(mgr);
		stmt=core::Address<core::Statement>(builder.deref(lhs.as<core::ExpressionPtr>()));

		// our first argument should be something like int.add(int<4> ref.deref v1)
		if (*stmt!=*op1) step.err("step size expression is not in the format x = x +- n");

		// our second argument should be a literal
		if (op2ptr.getNodeType()!=core::NT_Literal) step.err("second argument to self-assignment should be a literal");
	}

	// check whether our node satisfies our requirements
	if (step.ok()) {
		step.nodes.push_back(a);
		step.value=(*addsub==*intsub?-1:1)*op2ptr.as<core::Pointer<const core::Literal> >().getValueAs<int>();
	}

	return step;
}

/// For a given variable, try to extract the step size from a loop body, and return the relevant
/// information as a NodeBookmark, if successful.
NodeBookmark WhileToForPlugin::extractStepForVar(core::NodeAddress body, core::VariablePtr var) {
	std::vector<core::Address<const core::Node> > assignments=getAssignmentsForVar(body, var);
	NodeBookmark steps;
	for (auto a: assignments) {
		// std::cout << "assignment: " << pp(a) << " @[" << a << "]" << std::endl;
		steps.merge(extractStepFromAssignment(a));
	}
	return steps;
}

/// For a given variable, try to find its initialization value outside of the loop body.
NodeBookmark WhileToForPlugin::extractInitialValForVar(core::NodeAddress loop, core::VariablePtr var) {
	NodeBookmark initial;

	// visitor who will collect all instances (node addresses) from a given variable into a vector
	std::vector<core::VariableAddress> allvars;
	core::visitDepthFirst(loop.getRootAddress(), [&](const core::VariableAddress &x) {
		// add the variable to the vector only if it occurrs outside of the loop
		if (x.getAddressedNode()==var && !core::isChildOf(loop, x) &&
			x.getParentAddress().getChildAddresses().size()>=2) allvars.push_back(x);
	});

	// there are eventually more references to this variable; we need to check that there is only one single
	// assignment - other constructs may be added later
	for (core::VariableAddress x: allvars) {
		core::NodeAddress mynode=x.getParentAddress(), literal=mynode.getAddressOfChild(1);

		// allow only declarations
		if (x.getParentNode()->getNodeType()==core::NT_DeclarationStmt &&
			literal.getAddressedNode()->getNodeType()==core::NT_Literal) {
			initial.nodes.push_back(mynode);
			initial.value=literal.as<core::Pointer<const core::Literal> >().getValueAs<int>();
		} else
			initial.err("non-declaration loop variable access outside the loop");
	}

	// For now, make sure that no value will be assigned to the loop variable except the initial declaration
	// and the assignment in the while loop. Otherwise, we would have to check whether assignments happen
	// only after the loop has been executed (easy), or which effect the assignment (in front of the loop)
	// to the variable has (anything else than a single literal -> hard: have to evaluate expressions in this case)
	if (allvars.size()!=1) initial.err("multiple variable occurrences outside the loop");

	// the node to replace/remove is stored in "node"
	/*std::cout << "found decl of variable " << *var << " in: " << pp(node)
			  << " (#assignments = " << allvars.size() << ")" << std::endl; */
	return initial;
}

/// For a given variable, try to find its target value which should be given in the loop condition.
NodeBookmark WhileToForPlugin::extractTargetValForVar(core::NodeAddress cond, core::VariablePtr var) {
	NodeBookmark target;

	// visitor who will collect all accesses (node addresses) to a given variable and will try to find the target value
	core::visitDepthFirst(cond, [&](const core::VariableAddress &x) {
		auto& basic=x->getNodeManager().getLangBasic();

		// the variable is only considered if it is contained within a ref.deref and a logic operator
		core::CallExprPtr refderef;
		if (x.getAddressedNode()==var &&
			x.getDepth()>2 &&
			x.getParentNode()->getNodeType() == core::NT_CallExpr &&
			x.getParentAddress().getParentNode()->getNodeType() == core::NT_CallExpr &&
			basic.isRefDeref((refderef=x.getParentNode().as<core::CallExprPtr>()).getFunctionExpr())) {

			// now we need to extract the logic operator and its argument, a literal
			core::CallExprAddress comperator=x.getParentAddress().getParentAddress().as<core::CallExprAddress>();
			core::NodeAddress arg;
			if (comperator.getChildAddresses().size()!=4 ||
				(arg=comperator.getAddressOfChild(3)).getNodeType()!=core::NT_Literal)
				target.err("unsupported comparison operator, or second operand is not a literal");
			if (target.ok()) {
				target.value=arg.as<core::Address<const core::Literal> >().getValueAs<int>();
				/* std::cout << "comparison " << comperator << ":" << *comperator
						  << " is function " << *comperator.getFunctionExpr()
						  << " with argument " << target.value
						  << std::endl; */

				// adjust target value depending on the comparison operator
				/**/ if (basic.isSignedIntLe(comperator.getFunctionExpr())) target.value+=1;
				else if (basic.isSignedIntGe(comperator.getFunctionExpr())) target.value-=1;
				else if (basic.isSignedIntLt(comperator.getFunctionExpr()) ||
						 basic.isSignedIntGt(comperator.getFunctionExpr()) ||
						 basic.isSignedIntNe(comperator.getFunctionExpr()));
				else target.err("unsupported comparison operator"); // unhandled for now

				// We cannot handle while loops with multiple conditions right now, as we would have to replace
				// the initial while-condition with a for loop and an if-statement. A while-loop with only one
				// condition is much simpler, as we can simply drop the if-statement. Hence, indicate this
				// deficiency for now by returning false in case we have more than one condition.
				if (cond!=comperator) target.err("more than one condition in while loop; currently unsupported");
				else target.nodes.push_back(comperator);
			}
		}
	});

	return target;
}

/// Rewriting of the given while loop based on the information in the NodeBookmarks.
core::ProgramPtr WhileToForPlugin::replaceWhileByFor(core::NodeAddress whileaddr,
										 NodeBookmark initial, NodeBookmark target, NodeBookmark step) {
	core::NodeManager& mgr=step.nodes[0]->getNodeManager();
	core::IRBuilder builder(mgr);
	core::StatementPtr noop=builder.getNoOp();

	// convert from NodeAddress to StatementAddress
	std::vector<core::StatementAddress> stepstmt;
	for (core::NodeAddress n: step.nodes) stepstmt.push_back(n.isa<core::StatementAddress>());

	// print some debug info
	/* std::cout << "initial: \t" << *initial.nodes[0] << std::endl
			  << "target:  \t" << *target.nodes[0]  << std::endl
			  << "step:    \t" << *step.nodes[0]    << std::endl
			  << std::endl; */

	// first, remove the node statement that is performing the step size change
	// replace it with a noop, otherwise the loop could become empty which leads to trouble
	core::NodePtr root=whileaddr.getRootNode();
	core::NodeAddress modBody;
	for (core::NodeAddress n: step.nodes) {
		modBody=core::transform::replaceAddress(mgr, n.switchRoot(root), noop);
		root=modBody.getRootNode();
	}
	whileaddr=whileaddr.switchRoot(root);

	// second, generate the component stubs for the new for-loop, and put the for-loop into place
	core::VariablePtr   var=core::Variable::get(mgr, mgr.getLangBasic().getInt4());
	core::LiteralPtr   start=builder.intLit(initial.value),
						 end=builder.intLit(target.value),
						incr=builder.intLit(step.value);
	core::ForStmtPtr forstmt=builder.forStmt(var, start, end, incr, whileaddr.getAddressOfChild(1).as<core::StatementPtr>());
	core::NodeAddress modLoop=core::transform::replaceAddress(mgr, whileaddr, forstmt);
	root=modLoop.getRootNode();

	// third and last, replace the initial assignment with a noop as well
	core::NodeAddress modAll=core::transform::replaceAddress(mgr, initial.nodes[0].switchRoot(root), noop);
	root=modAll.getRootNode();

	return root.as<core::ProgramPtr>();
}

/// while statements can be for statements iff only one variable used in the condition is
/// altered within the statement, and this alteration satisfies certain conditions
insieme::core::ProgramPtr WhileToForPlugin::IRVisit(insieme::core::ProgramPtr& prog) {
		auto whilepat = irp::whileStmt(
					pattern::var("condition", pattern::all(pattern::var("cvar", irp::variable()))),
					pattern::var("body"));

		// match all while statements while making sure that we have access to their superior nodes -> ...Pairs
		irp::matchAllPairsReverse(whilepat, core::NodeAddress(prog),
						   [&](core::NodeAddress whileaddr, pattern::AddressMatch match) {
			std::cout << "Program now:" << std::endl << pp(prog);
			whileaddr=whileaddr.switchRoot(prog);
			auto condition=whileaddr.getAddressOfChild(0),
					  body=whileaddr.getAddressOfChild(1);

			std::cout << "Working on loop:" << std::endl << pp(whileaddr.getAddressedNode()) << std::endl << std::endl;

			// collect all variables from the loop condition, and store them in a PointerSet
			// TODO: do pattern matching of cvar based upon the new whileaddr (with switched roots)
			insieme::utils::set::PointerSet<core::VariablePtr> cvarSet=extractCondVars(match["cvar"].getFlattened());

			// for each condition variable, find its assignments in the loop body, and derive a step size
			for (core::VariablePtr var: cvarSet) {
				NodeBookmark initial=extractInitialValForVar(whileaddr, var),
						target=extractTargetValForVar(condition, var),
						step=extractStepForVar(body, var);

				if (initial.ok() && target.ok() && step.ok()) {
					prog=replaceWhileByFor(whileaddr, initial, target, step);
				} else {
					if (initial.ok()) std::cout << "initial value is " << initial.value << std::endl;
					else              std::cout << "no initial value found: " << initial.msg << std::endl;
					if (target.ok())  std::cout << "target value is " << target.value << std::endl;
					else              std::cout << "target size cannot be determined: " << target.msg << std::endl;
					if (step.ok())    std::cout << "step size is " << step.value << std::endl;
					else              std::cout << "step size cannot be determined: " << step.msg << std::endl;
					std::cout << std::endl;
				}
			}
		} );
		
		std::cout << "Final program with replaced loops:" << std::endl << pp(prog);
		return prog;
}

} // frontend
} // insieme

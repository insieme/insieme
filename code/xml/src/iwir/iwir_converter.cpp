/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/iwir/iwir_converter.h"

#include "insieme/iwir/iwir_condition_converter.h"

namespace iwir {

using namespace iwir::ast;
using namespace insieme;
using namespace std;

#define CONVERTER(name) void IWIRConverter::convert ## name(name* node, IWIRConverter::ConversionContext& context)

#define CONDITION_CONVERTER(name) core::ExpressionPtr IWIRConverter::convert ## name(name* node, IWIRConverter::ConversionContext& context)
#define CONVERT_CONDITION(condition, context) convertCondition(condition, context);

#define LOOPCOUNTER_CONVERTER(name) core::ExpressionPtr IWIRConverter::convert ## name( name* node, IWIRConverter::ConversionContext& context)
#define CONVERT_LOOPCOUNTER(loopcounter, context) convertLoopCounter(loopcounter, context);

#define TYPE_CONVERTER(name) core::TypePtr IWIRConverter::convert ## name( name* node, IWIRConverter::ConversionContext& context)
#define CONVERT_TYPE(type, context) convertType(type, context);

CONVERTER(Links) {
	VLOG(2) << "links ";
	for(Link* n : node->elements) {
		convert(n, context);
	}
}

CONVERTER(Link) { 
	VLOG(2) << *node;
			
	if(node->isDataLink) {
		//lookup port-variable
		auto vFrom = varMap.find( {node->fromTask, node->from} );
		auto vTo = varMap.find( {node->toTask, node->to} );
		assert(vFrom != varMap.end());
		assert(vTo != varMap.end());

		convert(node->from, context);
		convert(node->to, context);

		bool isUnion = (node->to->kind == PK_UnionPort);
		bool isLoopElement = (node->from->kind == PK_LoopElement);

		//TODO control flow links
		
		//TODO implicit casts
		//bool->string, int->string, double->string, int->double
		//A -> collection/A (one entry)
		//file -> string (URI to the file)

		//generate linking statment in IR
		//link(from,to) link(from:fromTy, to:toTy);
		core::StatementPtr linkStmt;
		map<string, core::NodePtr> symbols;
		
		core::ExpressionPtr var1 = vFrom->second;
		core::ExpressionPtr var2 = vTo->second;
		//besides loopCounter variables are all variables ref
		symbols["from"] = irBuilder.tryDeref(var1);
		symbols["to"] = var2;

		if(isUnion) {
			//linkUnion(from,to)
			symbols["link"] = irMgr.getLangExtension<core::lang::CollectionTypeExtension>().getLinkUnion();
			linkStmt = irBuilder.parseStmt("link(from, to);", symbols);
		} else if(isLoopElement) {
			//TODO get iterator to acccess correct element of LoopElements-collection
			symbols["iterator"] = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
			symbols["link"] = irMgr.getLangExtension<core::lang::CollectionTypeExtension>().getLinkLoopElement();
			linkStmt = irBuilder.parseStmt("link(from, iterator, to);", symbols);
		} else {
			//TODO find better solution... output links from parallelFor/ForEach are like
			//unionlinks...
			//FUGLY HACK!!! -- move to AST?
			bool isOutputOfParallelFor = (node->to->parentTask->getNodeType() == NT_ParallelForTask);
			bool isOutputOfParallelForEach = (node->to->parentTask->getNodeType() == NT_ParallelForEachTask);
			if(isOutputOfParallelFor || isOutputOfParallelForEach) {
				symbols["iterator"] = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
				symbols["link"] = irMgr.getLangExtension<core::lang::CollectionTypeExtension>().getLinkParallelOutput();
				linkStmt = irBuilder.parseStmt("link(from, to, iterator);", symbols);
			} else {
				symbols["link"] = irMgr.getLangExtension<core::lang::CollectionTypeExtension>().getLinkBasic();
				linkStmt = irBuilder.parseStmt("link(from,to);", symbols);
			}
		}
		VLOG(2) << linkStmt;
		
		assert(linkStmt);
		context.linkStmtMap[node] = linkStmt;
	} else {
		//control flow link -- link[task1->task2] -- no ports
		//assert(false && "control flow not implemented");
		//only needed for block scope -- handled in blockscope converter
	}
}

CONVERTER(Ports) {
	VLOG(2) << "Ports";
	switch(node->portsKind) {
		case PK_InputPorts:
			VLOG(2) << "\tInputPorts";
			break;

		case PK_OutputPorts:
			VLOG(2) << "\tOutputPorts";
			break;

		case PK_LoopElements:
			VLOG(2) << "\tLoopElements";
			break;

		case PK_LoopPorts:
			VLOG(2) << "\tLoopPorts";
			break;

		case PK_UnionPorts:
			VLOG(2) << "\tUnionPorts";
			break;

		default: assert(false && "Wrong PortsKind");

	}
	for(Port* n : node->elements) {
		convert(n, context);
	}
}

CONVERTER(Port) {
	VLOG(2) << "port: " << node->parentTask->name << "\\" << node->name;

	switch(node->kind) {
		case PK_Basic:
			VLOG(2) << "basic";
			break;
		case PK_LoopPort:
			VLOG(2) << "loopPort";
			break;
		case PK_LoopElement:
			VLOG(2) << "loopElement";
			break;
		case PK_UnionPort:
			VLOG(2) << "unionPort";
			break;
		case PK_LoopCounter:
			VLOG(2) << "loopCounter";
			break;
		default: 
			assert(false && "Wrong PortKind");
	}

	VLOG(2) << "isInput: " << (node->isInput ? "true" : "false" );
	VLOG(2) << "isOutput: " << (node->isOutput ? "true" : "false" );

	core::TypePtr varType = nullptr;
	core::TypePtr portType = CONVERT_TYPE(node->type, context);
	switch(node->kind) {
		case PK_Basic:
			varType = irBuilder.refType(portType);
			break;
		case PK_LoopPort:
			varType = irBuilder.refType(portType);
			break;
		case PK_LoopElement:
			varType = irBuilder.refType(portType);
			break;
		case PK_UnionPort:
			{
				// the variable for the UnionPort 
				varType = irBuilder.refType(portType);
				break;
			}
		case PK_LoopCounter:
			//NOTE: loopcounter is in "inputPorts" group but acts like a outputport
			//		this is the port used as the iterVar in the IR::forStmt
			if(node->isInput) {
				varType = irBuilder.refType(portType);
			} else {
				varType = portType;
			}
			break;
		default: 
			assert(false && "Wrong PortKind");
	}
	assert(varType);
	core::VariablePtr var = irBuilder.variable(varType);

	//declare variables for every portkind EXCEPT OUTPUT LOOPCOUNTER --> is declared in
	//for-loop; INPUT of loopcounter (to, from, step) are "normal" declared variable
	core::DeclarationStmtPtr decl = nullptr;

	//create IR variable for port
	if(varMap.find({node->parentTask, node}) == varMap.end()) {
		pair<Task*,Port*> key = { node->parentTask, node };
		string value = "port_" + node->parentTask->name + "\\" + node->name;
		assert(varType);
		
		//IRVariable for Task/Port
		varMap.insert( {key, var} ) ;
		VLOG(2) << "port: " << node->parentTask->name << "\\" << node->name << " mapped to " << var;
	
		switch(node->kind) {
			case PK_Basic:
				decl = irBuilder.declarationStmt(var, irBuilder.refVar(irBuilder.undefined(portType)));
				break;
			case PK_LoopPort:
				decl = irBuilder.declarationStmt(var, irBuilder.refVar(irBuilder.undefined(portType)));
				break;
			case PK_LoopElement:
				decl = irBuilder.declarationStmt(var, irBuilder.refVar(irBuilder.undefined(portType)));
				break;
			case PK_UnionPort:
				{
					//declare unionport
					decl = irBuilder.declarationStmt(var, irBuilder.refVar(irBuilder.undefined(portType)));
					break;
				}
			case PK_LoopCounter:
				if(node->isInput) {
				//loopcounter variable for "to"/"from"/"step" act as input
					decl = irBuilder.declarationStmt(var, irBuilder.refVar(irBuilder.undefined(portType)));
				}				
				//the loopcounter itself is the iterator variable in the IR::for --> declared in the forstmt
				break;
			default: 
				assert(false && "Wrong PortKind");
		}
	}

	if(decl) {
		VLOG(2) << decl;
		//DeclStmt for IRVariable per Port
		context.declMap[node->parentTask].push_back(decl);
	}
}

LOOPCOUNTER_CONVERTER(LoopCounter) { 
	core::ExpressionPtr ret;
	VLOG(2) << "loopCounter " ;
	VLOG(2) << "hasvalue: " << node->hasValue;
	if(node->hasValue) {
		//from/to/step
		VLOG(2) << "value: " << node->value;
		ret = irBuilder.intLit(node->value); 
	} else {
		//from/to/step and the actual loopCounter
		convert(node->port, context);

		auto c = varMap.find( {node->parentTask, node->port} );
		assert(c != varMap.end());
		if(c != varMap.end()) {
			ret = c->second;
		}
		VLOG(2) << "port " << *(node->port) << " -- " << ret;
	}
	return ret;
}

TYPE_CONVERTER(TaskType) { 
	VLOG(2) << "TaskType : " << node->type;
	//TODO generic type? annotation?
	return nullptr;
}

TYPE_CONVERTER(Type) { 
	VLOG(2) << "Type " << *node;
	core::TypePtr retTy = nullptr;	
	switch(node->getNodeType()) {
		case NT_SimpleType: 
			retTy = convertSimpleType( static_cast<SimpleType*>(node), context); 
			break;
		case NT_CollectionType: 
			retTy = convertCollectionType( static_cast<CollectionType*>(node), context); 
			break;
		default:
			assert(false && "how did you get here?");
	}
	assert(retTy);
	VLOG(2) << retTy;
	return retTy;
}

TYPE_CONVERTER(SimpleType) {
	VLOG(2) << "SimpleType " << *node;
	core::TypePtr retTy = nullptr;

	switch(node->simpleType) {
		case SimpleType::String:
			//TODO proper implementation
			//TODO put into IRExtensions or use stock implementation
			retTy = irBuilder.getLangBasic().getString();
			break;
		case SimpleType::Double: 
			retTy = irBuilder.getLangBasic().getDouble();
			break;
		case SimpleType::Integer: 
			retTy = irBuilder.getLangBasic().getInt4();
			break;
		case SimpleType::File:
			//TODO proper implementation
			//TODO put into IRExtensions
			retTy = irBuilder.genericType("fileDummy");
			break;
		case SimpleType::Bool:
			retTy = irBuilder.getLangBasic().getBool();
			break;
		default:
			assert(false && "how did you get here?");
			retTy = core::TypePtr();
	}
	return retTy;
}

TYPE_CONVERTER(CollectionType) {
	VLOG(2) << "CollectionType " << *node;

	core::TypePtr elemTy = CONVERT_TYPE(node->elementType, context);
	core::TypePtr collectionTy;
	for(int i=0;i<node->nesting;i++) {
		collectionTy =  irMgr.getLangExtension<core::lang::CollectionTypeExtension>().getCollectionType(elemTy);
		//for nesting of collections
		elemTy = collectionTy;
	}

	return collectionTy;
}

CONDITION_CONVERTER(Condition) {
//	VLOG(2) << "Condition : " << node->condition;
	core::ExpressionPtr condExpr = condition_ast::convert_condition_ast_to_inspire(node, varMap, irBuilder);
	dumpPretty(condExpr);
	return condExpr;
}

CONVERTER(Properties) {
	//TODO turn into annotations
	VLOG(2) << "Properties";
	for(Property* n : node->elements) {
		convert(n, context);
	}
}
CONVERTER(Constraints) {
	//TODO turn into annotations
	VLOG(2) << "Constraints";
	for(Constraint* n : node->elements) {
		convert(n, context);
	}
}
CONVERTER(Property) { 
	//TODO turn into annotations
	VLOG(2) << "Property: " << node->name << ":" << node->value;
}
CONVERTER(Constraint) { 
	//TODO turn into annotations
	VLOG(2) << "Constraint : " << node->name << ":" << node->value;
}
#undef CONVERTER

} // namespace iwir end

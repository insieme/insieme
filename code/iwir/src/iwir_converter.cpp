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

#include "insieme/iwir/annotations/property_annotation.h"
#include "insieme/iwir/annotations/constraint_annotation.h"

namespace insieme {
namespace iwir {

using namespace iwir::ast;
using namespace insieme;
using namespace std;

#define CONVERTER(name) void IWIRConverter::convert ## name(name* node, IWIRConverter::ConversionContext& context)

#define CONDITION_CONVERTER(name) core::ExpressionPtr IWIRConverter::convert ## name(name* node, IWIRConverter::ConversionContext& context)

#define LOOPCOUNTER_CONVERTER(name) core::ExpressionPtr IWIRConverter::convert ## name( name* node, IWIRConverter::ConversionContext& context)

#define TYPE_CONVERTER(name) core::TypePtr IWIRConverter::convert ## name( name* node, IWIRConverter::ConversionContext& context)
	
#define CONSTRAINTS_CONVERTER(name) map<string, string> IWIRConverter::convert ## name( name* node, ConversionContext& context)
#define CONSTRAINT_CONVERTER(name) pair<string, string> IWIRConverter::convert ## name( name* node, ConversionContext& context)

#define PROPERTIES_CONVERTER(name) map<string, string> IWIRConverter::convert ## name( name* node, ConversionContext& context)
#define PROPERTY_CONVERTER(name) pair<string, string> IWIRConverter::convert ## name( name* node, ConversionContext& context)

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

		//apply implict casting rules of the link construct
		auto implicit_cast = [&](core::ExpressionPtr from, core::ExpressionPtr to) {
			const core::lang::BasicGenerator& gen = irBuilder.getLangBasic();
			core::TypePtr fromTy = from->getType();
			core::TypePtr toTy = to->getType();

			VLOG(2) << fromTy;
			VLOG(2) << toTy;

			//the target port is a reference, but we need the element type for the cast
			assert(toTy.isa<core::RefTypePtr>()); 
			toTy = toTy.as<core::RefTypePtr>()->getElementType();

			VLOG(2) << fromTy;
			VLOG(2) << toTy;

			if(*fromTy == *toTy) {
				return std::make_tuple(from, to);
			}

			//bool->string, int->string, double->string, int->double
			if(gen.isBool(fromTy) && gen.isString(toTy)) { 
				//bool->string
				auto op = irBuilder.getNodeManager().getLangExtension<iwir::extension::IWIRExtension>().getBoolToString();
				from = irBuilder.callExpr(op, from);
			} else if(gen.isInt(fromTy) && gen.isString(toTy)) { 
				//int->string
				auto op = irBuilder.getNodeManager().getLangExtension<iwir::extension::IWIRExtension>().getIntToString();
				from = irBuilder.callExpr(op,from);
			} else if(gen.isDouble(fromTy) && gen.isString(toTy)) { 
				//double->string
				auto op = irBuilder.getNodeManager().getLangExtension<iwir::extension::IWIRExtension>().getDoubleToString();
				from = irBuilder.callExpr(op,from);
			} else if(gen.isInt(fromTy) && gen.isDouble(toTy)) { 
				// int->double
				from = irBuilder.callExpr(gen.getSignedToReal(),from, irBuilder.getIntParamLiteral(8));
			}

			//A -> collection/A (collection with only one entry) 
			//see further down in handling of BasicLinks as we need
			//to use a different link construct to emplace the given data into the collection
			/* IMPLEMENTED IN BASE LINK HANDLING if(	irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().isCollectionType(toTy) 
			*  IMPLEMENTED IN BASE LINK HANDLING 	&& (*fromTy ==  *(irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getElementType(toTy)))
			*  IMPLEMENTED IN BASE LINK HANDLING ) { }
			*/

			//file -> string (URI to the file)
			if( irBuilder.getNodeManager().getLangExtension<iwir::extension::IWIRExtension>().isFileType(fromTy)
				&& gen.isString(toTy)) {
				auto op = irBuilder.getNodeManager().getLangExtension<iwir::extension::IWIRExtension>().getFilePath();
				from = irBuilder.callExpr(op,from);
			}
			
			return std::make_tuple(from, to);
		};

		//generate linking statment in IR
		//link(from,to) link(from:fromTy, to:toTy);
		core::StatementPtr linkStmt;
		map<string, core::NodePtr> symbols;
		
		core::ExpressionPtr from = irBuilder.tryDeref(vFrom->second);
		core::ExpressionPtr to = vTo->second;
			
		std::tie(from, to) = implicit_cast(from,to);

		VLOG(2) << from;
		VLOG(2) << to;

		//besides loopCounter variables are all variables ref
		symbols["from"] = from;
		symbols["to"] = to;

		if(isUnion) {
			//linkUnion(from,to)
			symbols["link"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getLinkUnion();
			linkStmt = irBuilder.parseStmt("link(from, to);", symbols);
		} else if(isLoopElement) {
			//we create a "leIterator" variable to use as iterator to acccess correct element of LoopElements-collection
			//in the parallelForEach/forEachTask we replace the "leIterator" literal with the actual
			//iterator variable
			symbols["iterator"] = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
			symbols["link"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getLinkLoopElement();
			linkStmt = irBuilder.parseStmt("link(from, iterator, to);", symbols);
		} else {
			//TODO find better solution... output links from parallelFor/ForEach are like
			//unionlinks...
			//FUGLY HACK!!! -- move to AST?
			bool isOutputOfParallelFor = (node->to->parentTask->getNodeType() == NT_ParallelForTask);
			bool isOutputOfParallelForEach = (node->to->parentTask->getNodeType() == NT_ParallelForEachTask);
			if(isOutputOfParallelFor || isOutputOfParallelForEach) {
				symbols["iterator"] = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
				symbols["link"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getLinkParallelOutput();
				linkStmt = irBuilder.parseStmt("link(from, to, iterator);", symbols);
			} else {
				//BASIC LINK!
				
				//to should be a reference
				core::TypePtr toTy = to->getType();
				assert(toTy.isa<core::RefTypePtr>());
				toTy = 	toTy.as<core::RefTypePtr>()->getElementType();
				core::TypePtr fromTy = from->getType();

				bool isImplicitCastToCollection = 
					(	irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().isCollectionType(toTy) 
						&& (*fromTy ==  *(irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getElementType(toTy))));

				if(isImplicitCastToCollection) {
					//a link with a implicit cast from type A to collection/A we use similar ir
					//constructas for parallel output but with iteratro fixed to 0
					symbols["iterator"] = irBuilder.intLit(0);
					symbols["link"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getLinkParallelOutput();
					linkStmt = irBuilder.parseStmt("link(from, to, iterator);", symbols);
				} else {
					symbols["link"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getLinkBasic();
					linkStmt = irBuilder.parseStmt("link(from,to);", symbols);
				}
			}
		}
		VLOG(2) << linkStmt;
		
		assert_true(linkStmt);
		context.linkStmtMap[node] = linkStmt;
	} else {
		//control flow link -- link[task1->task2] -- no ports
		//only needed for block scope -- handled in blockscope converter
		//assert_not_implemented();
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

		default: assert_fail() << "Wrong PortsKind";

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
			assert_fail() << "Wrong PortKind";
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
			assert_fail() << "Wrong PortKind";
	}
	assert_true(varType);
	core::VariablePtr var = irBuilder.variable(varType);

	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context)
	auto properties = CONVERT_PROPERTIES(node->properties, context)

	if(!constraints.empty()) {
		annotations::iwir::attachConstraintMap(var, constraints);
	}
	if(!properties.empty()) {
		annotations::iwir::attachPropertyMap(var, properties);
	}

	//declare variables for every portkind EXCEPT OUTPUT LOOPCOUNTER --> is declared in
	//for-loop; INPUT of loopcounter (to, from, step) are "normal" declared variable
	core::DeclarationStmtPtr decl = nullptr;

	//create IR variable for port
	if(varMap.find({node->parentTask, node}) == varMap.end()) {
		pair<Task*,Port*> key = { node->parentTask, node };
		string value = "port_" + node->parentTask->name + "\\" + node->name;
		assert_true(varType);
		
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
				assert_fail() << "Wrong PortKind";
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
	return irBuilder.genericType(node->type, core::TypeList(), core::IntParamList());
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
			assert_fail() << "how did you get here?";
	}
	assert_true(retTy);
	VLOG(2) << retTy;
	return retTy;
}

TYPE_CONVERTER(SimpleType) {
	VLOG(2) << "SimpleType " << *node;
	core::TypePtr retTy = nullptr;

	switch(node->simpleType) {
		case SimpleType::String:
			retTy = irBuilder.getLangBasic().getString();
			break;
		case SimpleType::Double: 
			retTy = irBuilder.getLangBasic().getDouble();
			break;
		case SimpleType::Integer: 
			retTy = irBuilder.getLangBasic().getInt4();
			break;
		case SimpleType::File:
			retTy = irMgr.getLangExtension<iwir::extension::IWIRExtension>().getFile();
			break;
		case SimpleType::Bool:
			retTy = irBuilder.getLangBasic().getBool();
			break;
		default:
			assert_fail() << "how did you get here?";
			retTy = core::TypePtr();
	}
	return retTy;
}

TYPE_CONVERTER(CollectionType) {
	VLOG(2) << "CollectionType " << *node;

	core::TypePtr elemTy = CONVERT_TYPE(node->elementType, context);
	core::TypePtr collectionTy;
	for(int i=0;i<node->nesting;i++) {
		collectionTy =  irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getCollectionType(elemTy);
		//for nesting of collections
		elemTy = collectionTy;
	}

	return collectionTy;
}

CONDITION_CONVERTER(Condition) {
	VLOG(2) << "Condition : " << node->condition;
	core::ExpressionPtr condExpr = condition_ast::convert_condition_ast_to_inspire(node, varMap, irBuilder);
	VLOG(2) << condExpr;
	return condExpr;
}

PROPERTIES_CONVERTER(Properties) {
	// are attached as annotations in Port, Task* converter
	std::map<string, string> propertyMap;
	if(node) {
		for(Property* n : node->elements) {
			auto property = CONVERT_PROPERTY(n, context);
			propertyMap.insert( property );
		}
	}
	VLOG(2) << "Properties " << propertyMap;
	return propertyMap;
}

PROPERTY_CONVERTER(Property) { 
	// attached to DataPorts, atomic tasks, composite tasks
	// are attached as annotations in Port, Task* converter
	VLOG(2) << "\tProperty: " << node->name << ":" << node->value;
	return std::make_pair(node->name, node->value);
}

CONSTRAINTS_CONVERTER(Constraints) {
	// are attached as annotations in Port, Task* converter
	std::map<string, string> constraintMap;
	if(node) {
		for(Constraint* n : node->elements) {
			auto constraint = CONVERT_CONSTRAINT(n, context);
			constraintMap.insert( constraint );
		}
		VLOG(2) << "Constraints " << constraintMap;
	}
	return constraintMap;
}

CONSTRAINT_CONVERTER(Constraint) { 
	// attached to DataPorts, atomic tasks, composite tasks
	// are attached as annotations in Port, Task* converter
	VLOG(2) << "\tConstraint : " << node->name << ":" << node->value;
	return std::make_pair(node->name, node->value);
}
#undef CONVERTER

} // namespace insieme end
} // namespace iwir end

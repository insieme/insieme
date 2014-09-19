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

#pragma once

#include "insieme/frontend/extensions/frontend_plugin.h"
#include "insieme/core/transform/node_replacer.h"

 namespace insieme {
 namespace frontend {
namespace extensions {

using namespace insieme;

/**
 *
 * This is the frontend cleanup tool.
 * it is a NOT OPTIONAL pass which removes artifacts the frontend might generate.
 * frontend might generate suff in an "correct" but not optimal way just because is the straight forward aproach.
 * instead of trying to fix this everywhere, is much more convinient to clean up afterwars, reduces complexity of code
 */
class AnonymousRename : public insieme::frontend::extensions::FrontendPlugin {

	core::NodeMap renamedTypeDefinitions;
	core::NodeMap renamedTypeDeclarations;

    std::map<core::NodePtr, std::string> nameMap;

    core::TypePtr TypeDeclPostVisit(const clang::TypeDecl* decl, insieme::core::TypePtr res,
                                                insieme::frontend::conversion::Converter& convFact){
		core::IRBuilder builder (res->getNodeManager());

		// NOTE:
		// it might be that a the typedef encloses an anonymous struct declaration, in that case
		// we forward the name to the inner type. this is fuzzy but this is the last time we can do it
		if (const clang::TypedefDecl* typedefDecl = llvm::dyn_cast<clang::TypedefDecl>(decl)){
			if (core::GenericTypePtr symb = res.isa<core::GenericTypePtr>()){

				VLOG(2) << "typedef of an anonymous type, forward the name: ";
				VLOG(2) << "    -" << res;

				core::TypePtr trgTy =  convFact.lookupTypeDetails(symb);
				// a new generic type will point to the previous translation unit decl
				if (core::NamedCompositeTypePtr namedType = trgTy.isa<core::NamedCompositeTypePtr>()){

					clang::QualType typedefType = typedefDecl->getTypeForDecl()->getCanonicalTypeInternal ();

					// build a name for the thing
					std::string name = utils::getNameForRecord(typedefDecl, typedefType, convFact.getSourceManager());
					core::GenericTypePtr gen = builder.genericType(name);

					core::TypePtr definition = symb;
					// if target is an annonymous type, we create a new type with the name of the typedef
					//if (namedType->getName()->getValue().substr(0,5) == "_anon"){
					if (namedType->getName()->getValue() == ""){
                        //guarantee that the second typedef does not change the name again
                        if (nameMap.find(namedType) != nameMap.end()) {
                            return namedType;
                        }
                        nameMap[namedType] = name;

						if (auto structTy = namedType.isa<core::StructTypePtr>()){
							definition = builder.structType (builder.stringValue(name), structTy->getParents(), structTy->getEntries());
						}
						else if (auto unionTy = namedType.isa<core::UnionTypePtr>()){
							definition = builder.unionType (builder.stringValue(name), unionTy->getEntries());
						}
						else{
							assert_true(false) << "this might be pretty malformed:\n" << dumpPretty(namedType);
						}

						core::transform::utils::migrateAnnotations(namedType.as<core::TypePtr>(), definition);
						core::annotations::attachName(definition,name);
						convFact.getHeaderTagger().addHeaderForDecl(definition, typedefDecl);
						core::annotations::attachName(gen,name);
						convFact.getHeaderTagger().addHeaderForDecl(gen, typedefDecl);

						// just before end, solve nested anonymous issues
						definition = core::transform::replaceAllGen (definition->getNodeManager(), definition, renamedTypeDefinitions, false);
						definition = core::transform::replaceAllGen (definition->getNodeManager(), definition, renamedTypeDeclarations, false);

						// replace all recursive usages
						definition = core::transform::replaceAllGen (definition->getNodeManager(), definition, trgTy, gen, false);

						// store for a later solver
						renamedTypeDeclarations[symb] = gen;
						renamedTypeDefinitions[trgTy] = definition;
						convFact.getIRTranslationUnit().addType(gen, definition);

						return gen;
					}
				}
			}
		}

        return nullptr;
    }



	frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu){

		// correct functions
		std::map<core::LiteralPtr, std::pair<core::LiteralPtr, core::LambdaExprPtr>> tuFuncCorrections;
		for (auto& pair : tu.getFunctions()) {
			core::LiteralPtr lit = pair.first.as<core::LiteralPtr>();
			core::LambdaExprPtr func = pair.second;

			func = core::transform::replaceAllGen (lit->getNodeManager(), func, renamedTypeDefinitions, false);
			func = core::transform::replaceAllGen (lit->getNodeManager(), func, renamedTypeDeclarations, false);

			auto newlit  = core::transform::replaceAllGen (lit->getNodeManager(), lit, renamedTypeDefinitions, false);
			newlit  = core::transform::replaceAllGen (lit->getNodeManager(), newlit, renamedTypeDeclarations, false);

			tuFuncCorrections[lit] = {newlit, func};
		}
		for (const auto& pair : tuFuncCorrections){
			tu.substituteFunction(pair.first, pair.second.first, pair.second.second);
		}

		// correct globals
		for (auto& g : tu.getGlobals()) {

			core::LiteralPtr symbol = g.first;
			core::ExpressionPtr init = g.second;

			symbol  = core::transform::replaceAllGen (symbol->getNodeManager(), symbol, renamedTypeDefinitions, false);
			symbol  = core::transform::replaceAllGen (symbol->getNodeManager(), symbol, renamedTypeDeclarations, false);

			init    = core::transform::replaceAllGen (symbol->getNodeManager(), init, renamedTypeDefinitions, false);
			init    = core::transform::replaceAllGen (symbol->getNodeManager(), init, renamedTypeDeclarations, false);
			auto global =  std::make_pair(symbol, init);
			tu.replaceGlobal(g,global);
		}	

		// correct types
		std::map<core::GenericTypePtr, std::pair<core::GenericTypePtr, core::TypePtr>> tuTypeCorrections;
		for (auto& pair : tu.getTypes()) {
			core::GenericTypePtr lit = pair.first;
			core::TypePtr definition = pair.second;

			definition = core::transform::replaceAllGen (lit->getNodeManager(), definition, renamedTypeDefinitions, false);
			definition = core::transform::replaceAllGen (lit->getNodeManager(), definition, renamedTypeDeclarations, false);
			core::GenericTypePtr newType   = core::transform::replaceAllGen (lit->getNodeManager(), lit, renamedTypeDefinitions, false);
			newType                        = core::transform::replaceAllGen (lit->getNodeManager(), lit, renamedTypeDeclarations, false);

			tuTypeCorrections[lit] = {newType, definition};
		}
		for (const auto& pair : tuTypeCorrections){
			tu.substituteType(pair.first, pair.second.first, pair.second.second);
		}


		return tu;
	}

};


} // extensions
} // frontend
} // insieme

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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang/AST/DeclGroup.h>

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/DeclTemplate.h>
#include <llvm/Support/Casting.h>

#pragma GCC diagnostic pop

#include <boost/algorithm/string.hpp>

namespace insieme {
namespace frontend {
namespace utils {

using namespace llvm;

#define REMOVE_SYMBOLS(str) \
{ \
		boost::replace_all(str, "<", "_"); \
		boost::replace_all(str, ">", "_"); \
		boost::replace_all(str, "::", "_"); \
		boost::replace_all(str, " ", "_"); \
}



/* we build a complete name for the class,
 * qualified name does not have the specific types of the spetialization
 */
std::string getNameForRecord(const clang::RecordDecl* decl, const clang::Type* type){

	std::string fullName =  decl->getQualifiedNameAsString();
	if (llvm::isa<clang::ClassTemplateSpecializationDecl>(decl)) {

		std::string name = decl->getNameAsString();
		std::string typeName = type->getCanonicalTypeInternal ().getAsString();

		//fullname has the namespaces and owners, just scope
		//type name has the class name and typing
		//
		//     namespace::owner::myClass                 <= qualname
		//                class  myClass<int>            <= typename
		//                       myClass                 just the name, the key to happines
		//      ---------------------------------
		//     namespace::owner::myClass<int>            <= final name

		unsigned pos = typeName.find(name);
		boost::replace_last(fullName, name, std::string(typeName.begin()+pos, typeName.end()));
	}
	REMOVE_SYMBOLS(fullName);
	return fullName;
}


std::string buildNameForFunction (const clang::FunctionDecl* funcDecl){

	std::string name = funcDecl->getQualifiedNameAsString();
	if(const clang::CXXMethodDecl* method = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl))
		if (method->isVirtual())
			name = funcDecl->getNameAsString();

    //if we have non type template specialization args,
    //we have to modify the name (e.g. template <bool VAR>)
    if(funcDecl->getTemplateSpecializationArgs()) {
        for(unsigned int i=0; i<funcDecl->getTemplateSpecializationArgs()->size(); i++) {
            if(funcDecl->getTemplateSpecializationArgs()->get(i).getKind() == clang::TemplateArgument::ArgKind::Integral) {
                name.append(funcDecl->getTemplateSpecializationArgs()->get(i).getAsIntegral().toString(10));
            }
        }
    }

    //we need to replace right and left shift operators with a dummy
    //to avoid wrong renaming and double occurence when both operators
    //have been overloaded
	boost::algorithm::replace_last(name, "operator<<","dummyss");
	boost::algorithm::replace_last(name, "operator>>","dummygg");
	//if no shift operators found lets check for less, greater
	//and less equals, greater equals (handled in one case)
	boost::algorithm::replace_last(name, "operator<","sdummy");
	boost::algorithm::replace_last(name, "operator>","gdummy");

	// beware of spetialized functions, the type does not show off
	// check if we have template spec args otherwise seg faults may occur
	if (funcDecl->isFunctionTemplateSpecialization () && funcDecl->getTemplateSpecializationArgs()){
		for(unsigned i =0; i<funcDecl->getTemplateSpecializationArgs()->size(); ++i){
            //only add something if we have a type
            switch(funcDecl->getTemplateSpecializationArgs()->get(i).getKind()) {
                case clang::TemplateArgument::Expression: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsExpr()->getType().getAsString());
                    break;
                }
                case clang::TemplateArgument::Type: {
                    name.append ("_" + funcDecl->getTemplateSpecializationArgs()->get(i).getAsType().getAsString());
                    break;
                }
                case clang::TemplateArgument::Null: {
                    name.append ("_null");
                    break;
                }
                case clang::TemplateArgument::Declaration: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsDecl()->getType().getAsString());
                        break;
                }
                case clang::TemplateArgument::NullPtr: {
                    name.append ("_nullptr");
                    break;
                }
                case clang::TemplateArgument::Integral:  {
                    name.append ("_" + funcDecl->getTemplateSpecializationArgs()->get(i).getAsIntegral().toString(10));
                    break;
                }
                case clang::TemplateArgument::Template: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
                }
                case clang::TemplateArgument::TemplateExpansion: {
                    //I don't know what to do here
                    break;
                }
                case clang::TemplateArgument::Pack: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsTemplateOrTemplatePattern().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
                    break;
                }
            }
		}
	}

	REMOVE_SYMBOLS(name);

	if (llvm::isa<clang::CXXMethodDecl>(funcDecl) && llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst())
		name.append("_c");

    //check for dummyss or dummygg and replace it with the original name
	boost::algorithm::replace_last(name, "dummyss", "operator<<");
	boost::algorithm::replace_last(name, "dummygg", "operator>>");
	//if nothing was found check for the other ones
	boost::algorithm::replace_last(name, "sdummy", "operator<");
	boost::algorithm::replace_last(name, "gdummy", "operator>");

	return name;
}

std::string buildNameForVariable (const clang::VarDecl* varDecl){
	std::string name = varDecl->getQualifiedNameAsString();
	REMOVE_SYMBOLS(name);
	return name;
}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace

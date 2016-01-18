/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/clang.h"
#include "insieme/utils/name_mangling.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sstream>
#include <iostream>

namespace insieme {
namespace frontend {
namespace utils {

	using namespace llvm;
	using std::string;

	std::string removeSymbols(std::string str) {
		boost::replace_all(str, "<", "_lt_");
		boost::replace_all(str, ">", "_gt_");
		boost::replace_all(str, ":", "_colon_");
		boost::replace_all(str, " ", "_space_");
		boost::replace_all(str, "(", "_lparen_");
		boost::replace_all(str, ")", "_rparen_");
		boost::replace_all(str, ",", "_comma_");
		boost::replace_all(str, "*", "_star_");
		boost::replace_all(str, "&", "_ampersand_");
		boost::replace_all(str, ".", "_dot_");
		boost::replace_all(str, "+", "_plus_");
		boost::replace_all(str, "/", "_slash_");
		boost::replace_all(str, "-", "_minus_");
		return str;
	}

	std::string createNameForAnon(const std::string& prefix, const clang::Decl* decl, const clang::SourceManager& sm) {
		std::stringstream ss;
		ss << prefix;

		// canonicalize filename in case we refer to it from different relative locations
		std::string filename = sm.getFilename(decl->getLocStart()).str();
		boost::filesystem::path path(filename);
		path = boost::filesystem::canonical(path);

		ss << path.string();
		ss << "_" << sm.getExpansionLineNumber(decl->getLocStart());
		ss << "_" << sm.getExpansionColumnNumber(decl->getLocStart());

		std::string name = removeSymbols(ss.str());
		return name;
	}

	/* we build a complete name for the class,
	 * qualified name does not have the specific types of the specialization
	 */
	std::string getNameForRecord(const clang::NamedDecl* decl, const clang::Type* type, const clang::SourceManager& sm) {
		if(decl->getNameAsString().empty()) { return createNameForAnon("_anonRecord", decl, sm); }
		std::string fullName = decl->getQualifiedNameAsString();

		if(llvm::isa<clang::ClassTemplateSpecializationDecl>(decl) && !llvm::isa<clang::TypedefNameDecl>(decl)) {
			std::string name = decl->getNameAsString();
			std::string typeName = type->getCanonicalTypeInternal().getAsString();

			// fullname has the namespaces and owners, just scope
			// type name has the class name and typing
			//
			//     namespace::owner::myClass                 <= qualname
			//                class  myClass<int, type>      <= typename
			//                       myClass                 just the name, the key to happiness
			//      ---------------------------------
			//     namespace::owner::myClass<int, type>      <= final name

			unsigned pos = typeName.find(name);
			boost::replace_last(fullName, name, std::string(typeName.begin() + pos, typeName.end()));
		}

		return removeSymbols(fullName);
	}

	namespace {
		string getTypeString(clang::QualType t) {
			string s = t.getAsString();
			boost::replace_all(s, " ", "_");
			return removeSymbols(s);
		}
	}

	std::string buildNameForFunction(const clang::FunctionDecl* funcDecl) {
		std::string name = funcDecl->getQualifiedNameAsString();
		if(const clang::CXXMethodDecl* method = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
			if(method->isVirtual()) {
				name = funcDecl->getNameAsString();
			} else if(method->getParent()) {
				if(method->getParent()->isLambda()) { name = createNameForAnon("lambda", method->getParent(), funcDecl->getASTContext().getSourceManager()); }
			}
		}

		// mangle name
		name = insieme::utils::mangle(name);

		// build a suffix for template instantiations
		std::stringstream suffix;

		if(funcDecl->isFunctionTemplateSpecialization() && funcDecl->getTemplateSpecializationArgs()) {
			for(unsigned i = 0; i < funcDecl->getTemplateSpecializationArgs()->size(); ++i) {
				auto arg = funcDecl->getTemplateSpecializationArgs()->get(i);
				switch(arg.getKind()) {
				case clang::TemplateArgument::Expression: {
					suffix << "_" << arg.getAsExpr()->getType().getAsString();
					break;
				}
				case clang::TemplateArgument::Type: {
					// check if the type is a lambda, this one needs special handling
					std::string typeName;
					if(arg.getAsType().getTypePtr()->getAsCXXRecordDecl() && arg.getAsType().getTypePtr()->getAsCXXRecordDecl()->isLambda()) {
						typeName =
							createNameForAnon("lambda", arg.getAsType().getTypePtr()->getAsCXXRecordDecl(), funcDecl->getASTContext().getSourceManager());
					} else {
						typeName = getTypeString(arg.getAsType());
					}
					suffix << "_" << typeName;
					break;
				}
				case clang::TemplateArgument::Null: {
					suffix << "_null";
					break;
				}
				case clang::TemplateArgument::Declaration: {
					suffix << "_" << getTypeString(arg.getAsDecl()->getType());
					break;
				}
				case clang::TemplateArgument::NullPtr: {
					suffix << "_nullptr";
					break;
				}
				case clang::TemplateArgument::Integral: {
					suffix << "_" << arg.getAsIntegral().toString(10);
					break;
				}
				case clang::TemplateArgument::Template: {
					suffix << "_" << removeSymbols(arg.getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
					break;
				}
				case clang::TemplateArgument::TemplateExpansion: {
					// I don't know what to do here
					break;
				}
				case clang::TemplateArgument::Pack: {
					suffix << "_pack_begin";
					for(clang::TemplateArgument::pack_iterator it = arg.pack_begin(), end = arg.pack_end(); it != end; it++) {
						const clang::QualType& argType = (*it).getAsType();
						suffix << "_" << getTypeString(argType);
					}
					suffix << "_pack_end";
					break;
				}
				}
			}
		}

		if(funcDecl->isTemplateInstantiation()) {
			std::string returnType = getTypeString(funcDecl->getReturnType());
			suffix << "_returns_" << returnType;
		}

		if(llvm::isa<clang::CXXMethodDecl>(funcDecl) && llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst()) { suffix << "_c"; }

		string suffixStr = suffix.str();
		boost::algorithm::replace_all(suffixStr, " ", "_");
		
		// all done
		return name + suffixStr;
	}

	std::string getNameForGlobal(const clang::VarDecl* varDecl, const clang::SourceManager& sm) {
		string s = varDecl->getNameAsString();
		if(varDecl->isStaticLocal()) {
			return createNameForAnon(s + "_static_local", varDecl, sm);
		}
		if(!s.empty()) { return s; }
		return createNameForAnon("_global", varDecl, sm);
	}

	std::string getNameForEnum(const clang::EnumDecl* enumDecl, const clang::SourceManager& sm) {
		if(enumDecl->getTypedefNameForAnonDecl()) {
			string qName = enumDecl->getTypedefNameForAnonDecl()->getQualifiedNameAsString();
			if(!qName.empty()) return qName;
		}

		return createNameForAnon("_enum", enumDecl, sm);
	}

	std::string getNameForField(const clang::FieldDecl* fieldDecl, const clang::SourceManager& sm) {
        string fieldName = fieldDecl->getNameAsString();
		if(fieldName.empty() || fieldDecl->isAnonymousStructOrUnion()) {
			auto fileName = sm.getFilename(fieldDecl->getLocStart()).str();
			auto line = sm.getExpansionLineNumber(fieldDecl->getLocStart());
			auto column = sm.getExpansionColumnNumber(fieldDecl->getLocStart());
			// in this case we must mangle the name as "" is not allowed
			return insieme::utils::mangle(fileName, line, column);
		}

		// in this case we return the original name itself
		return fieldName;
	}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace

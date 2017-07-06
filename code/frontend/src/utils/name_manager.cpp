/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/utils/assert.h"
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
		boost::replace_all(str, " ", "_");
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

	std::string getLocationAsString(const clang::SourceLocation sl, const clang::SourceManager& sm, bool mangled) {
		std::stringstream ss;

		std::string filename = sm.getFilename(sl).str();
		// canonicalize filename in case we refer to it from different relative locations
		boost::filesystem::path path(filename);
		path = boost::filesystem::canonical(path);

		ss << path.string();
		ss << "_" << sm.getExpansionLineNumber(sl);
		ss << "_" << sm.getExpansionColumnNumber(sl);

		return mangled ? removeSymbols(ss.str()) : ss.str();
	}

	std::string createNameForAnon(const std::string& prefix, const clang::Decl* decl, const clang::SourceManager& sm) {
		std::stringstream ss;
		ss << prefix;
		ss << getLocationAsString(decl->getLocStart(), sm);
		return removeSymbols(ss.str());
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
		string getTypeString(clang::QualType t, bool cStyleName = false) {
			string s = t.getAsString();
			boost::replace_all(s, "class ", "");    // special handling for clang's translation of template class arguments
			boost::replace_all(s, "_Bool", "bool"); // special handling for clang's translation of bool types
			if (cStyleName) {
				return s;
			}
			return removeSymbols(s);
		}
	}


	std::string buildNameSuffixForTemplateInternal(const llvm::ArrayRef<clang::TemplateArgument>& tempArgs, clang::ASTContext& astContext, bool cStyleName) {
		std::string separator = cStyleName ? "," : "_";
		std::stringstream suffix;
		for(auto arg : tempArgs) {
			switch(arg.getKind()) {
			case clang::TemplateArgument::Expression: {
				suffix << separator << arg.getAsExpr()->getType().getAsString();
				break;
			}
			case clang::TemplateArgument::Type: {
				// check if the type is a lambda, this one needs special handling
				std::string typeName;
				if(arg.getAsType().getTypePtr()->getAsCXXRecordDecl() && arg.getAsType().getTypePtr()->getAsCXXRecordDecl()->isLambda()) {
					typeName = createNameForAnon("lambda", arg.getAsType().getTypePtr()->getAsCXXRecordDecl(), astContext.getSourceManager());
				} else {
					typeName = getTypeString(arg.getAsType(), cStyleName);
				}
				suffix << separator << typeName;
				break;
			}
			case clang::TemplateArgument::Null: {
				suffix << separator << "null";
				break;
			}
			case clang::TemplateArgument::Declaration: {
				suffix << separator << getTypeString(arg.getAsDecl()->getType(), cStyleName);
				break;
			}
			case clang::TemplateArgument::NullPtr: {
				suffix << separator << "nullptr";
				break;
			}
			case clang::TemplateArgument::Integral: {
				suffix << separator << arg.getAsIntegral().toString(10);
				break;
			}
			case clang::TemplateArgument::Template: {
				std::string templateName = arg.getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getQualifiedNameAsString();
				suffix << separator << (cStyleName ? templateName : removeSymbols(templateName));
				break;
			}
			case clang::TemplateArgument::TemplateExpansion: {
				// I don't know what to do here
				assert_not_implemented();
				break;
			}
			case clang::TemplateArgument::Pack: {
				if(!cStyleName) {
					suffix << separator << "pack_begin";
				}
				suffix << buildNameSuffixForTemplateInternal(arg.getPackAsArray(), astContext, cStyleName);
				if(!cStyleName) {
					suffix << separator << "pack_end";
				}
				break;
			}
			}
		}

		//if we should build a c-style name then the first separator isn't correct. also the whole suffix should be packed inside '<' and '>'
		if (cStyleName) {
			std::string res = suffix.str();
			//append another space in case it is needed to avoid the substring ">>"
			if (res.size() > 0 && res[res.size() - 1] == '>') {
				res += " >";
			} else {
				res += ">";
			}
			if (res.length() == 1) {
				return "";
			}
			res[0] = '<';
			return res;
		}

		return suffix.str();
	}

	std::string buildNameSuffixForTemplate(const clang::TemplateArgumentList& tempArgs, clang::ASTContext& astContext, bool cStyleName) {
		return buildNameSuffixForTemplateInternal(tempArgs.asArray(), astContext, cStyleName);
	}

	std::string buildNameForFunction(const clang::FunctionDecl* funcDecl, const conversion::Converter& converter, bool cStyleName) {
		// operator= should not have silly suffixes for templates
		if(boost::starts_with(funcDecl->getNameAsString(), "operator")) cStyleName = true;

		std::string name = funcDecl->getQualifiedNameAsString();
		if(const clang::CXXMethodDecl* method = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
			if(!method->isStatic()) {
				// no need to qualify method name
				name = funcDecl->getNameAsString();
			}
		}

		// adjust name for things in anonymous namespaces
		if(boost::contains(name, "(anonymous") || name.empty()) {
			name = createNameForAnon(name, funcDecl, converter.getSourceManager());
		}

		// build a suffix for template instantiations
		std::stringstream suffix;

		if(!cStyleName && funcDecl->isFunctionTemplateSpecialization() && funcDecl->getTemplateSpecializationArgs()) {
			suffix << buildNameSuffixForTemplate(*funcDecl->getTemplateSpecializationArgs(), funcDecl->getASTContext());
		}

		if(!cStyleName && funcDecl->isTemplateInstantiation()) {
			suffix << "_returns_" << getTypeString(funcDecl->getReturnType());
		}

		string suffixStr = suffix.str();
		if(!cStyleName) {
			boost::algorithm::replace_all(suffixStr, " ", "_");
		}

		// all done
		return insieme::utils::mangle(name + suffixStr);
	}

	std::string getNameForGlobal(const clang::VarDecl* varDecl, const clang::SourceManager& sm) {
		string s = varDecl->getNameAsString();
		if(varDecl->isStaticLocal()) {
			return createNameForAnon(s + "_static_local", varDecl, sm);
		}
		if(varDecl->isStaticDataMember()) {
			return insieme::utils::mangle(varDecl->getQualifiedNameAsString());
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
		if(fieldDecl->isImplicit() && !fieldDecl->isAnonymousStructOrUnion()) {
			return format("capture_%u", fieldDecl->getFieldIndex());
		}
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

	std::pair<std::string,bool> getNameForTagDecl(const conversion::Converter& converter, const clang::TagDecl* tagDecl, bool cStyleName) {
		static std::map<std::string, std::vector<const clang::TagDecl*>> instantiationNameMapping;

		auto canon = tagDecl->getCanonicalDecl();
		// try to use name, if not available try to use typedef name, otherwise no name
		string name = utils::createNameForAnon("__anon_tagtype_", tagDecl, converter.getSourceManager());

		if(canon->getDeclName() && !canon->getDeclName().isEmpty()) name = canon->getQualifiedNameAsString();
		else if(canon->hasNameForLinkage()) name = canon->getTypedefNameForAnonDecl()->getQualifiedNameAsString();
		else {
			// for anonymous structs created to implement lambdas, encode capture type as well as call operator type in name
			auto rec = llvm::dyn_cast<clang::CXXRecordDecl>(tagDecl);
			if(rec && rec->isLambda()) {
				for(auto capture : rec->fields()) {
					name = name + "_" + getTypeString(capture->getType().getCanonicalType(), false);
				}
				if(auto lambdaOperator = rec->getLambdaCallOperator()) {
					if(auto lambdaOperatorType = llvm::dyn_cast<clang::FunctionProtoType>(lambdaOperator->getType())) {
						name = name + "_" + getTypeString(lambdaOperatorType->getReturnType().getCanonicalType(), false);
						for(auto param : lambdaOperatorType->getParamTypes()) {
							name = name + "_" + getTypeString(param.getCanonicalType(), false);
						}
					}
				}

				// now we check that the generated name is unique, as we need to generate unique names for different instantiations.
				// we look up the name in instantiationNameMapping.
				auto instantiationNameMappingSearch = instantiationNameMapping.find(name);
				// If we don't find anything there, we add the mapping and use the name we have.
				if(instantiationNameMappingSearch == instantiationNameMapping.end()) {
					instantiationNameMapping[name] = { tagDecl };

					// If we find an entry there
				} else {
					// we look whether we already generated a name for that entry (i.e. the decl is in the vector)
					auto& mappedDecls = instantiationNameMappingSearch->second;
					auto mappedDeclsSearch = std::find(mappedDecls.cbegin(), mappedDecls.cend(), tagDecl);
					// if not, we add it and modify the returned name
					if(mappedDeclsSearch == mappedDecls.cend()) {
						mappedDecls.push_back(tagDecl);
						name = format("%s_instance%d", name, mappedDecls.size() - 1);

						//if we do find it in the list of already mapped names
					} else {
						// The entry at location 0 gets the unchanged name, all other entries get their index in the vector appended
						if(mappedDeclsSearch != mappedDecls.cbegin()) {
							name = format("%s_instance%d", name, mappedDeclsSearch - mappedDecls.cbegin());
						}
					}
				}
			}
		}

		// encode template parameters in name
		auto tempSpec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(tagDecl);
		if(tempSpec && !cStyleName) {
			name = name + utils::buildNameSuffixForTemplate(tempSpec->getTemplateInstantiationArgs(), tempSpec->getASTContext(), cStyleName);
		}

		// if externally visible, build mangled name based on canonical decl without location
		if(tagDecl->isExternallyVisible()) return std::make_pair(insieme::utils::mangle(name), true);

		// not externally visible: build mangled name with location
		// canonicalize filename in case we refer to it from different relative locations
		auto& sm = converter.getSourceManager();
		std::string filename = sm.getFilename(canon->getLocStart()).str();
		boost::filesystem::path path(filename);
		path = boost::filesystem::canonical(path);
		auto line = sm.getExpansionLineNumber(canon->getLocStart());
		auto column = sm.getExpansionColumnNumber(canon->getLocStart());

		return std::make_pair(insieme::utils::mangle(name, path.string(), line, column), canon->hasNameForLinkage());
	}

	std::string stripLeadingGlobalNamespace(const std::string& name) {
		if(boost::starts_with(name, "::")) {
			return name.substr(2);
		}
		return name;
	}

	std::string getNameForDependentNameType(const clang::DependentNameType* depName) {
		string name;
		llvm::raw_string_ostream strstr(name);
		strstr << clang::TypeWithKeyword::getKeywordName(depName->getKeyword());
		if (depName->getKeyword() != clang::ETK_None) strstr << " ";
		depName->getQualifier()->print(strstr, clang::PrintingPolicy(clang::LangOptions()));
		strstr << depName->getIdentifier()->getName().str();
		return strstr.str();
	}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace

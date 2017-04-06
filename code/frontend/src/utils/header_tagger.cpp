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
#include "insieme/frontend/utils/header_tagger.h"

#include <algorithm>
#include <regex>

#include <boost/optional.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/utils/logging.h"
#include "insieme/core/ir_node.h"
#include "insieme/annotations/c/include.h"

namespace insieme {
namespace frontend {
namespace utils {

	namespace detail {
		boost::optional<fs::path> getInterceptedLibHeader(const std::vector<fs::path>& userIncludeDirs, const std::vector<fs::path>& interceptedHeaderDirs,
		                                                  const fs::path& path) {
			// first we find the (longest) intercepted path matching this path
			const auto canonicalInterceptedPathString = fs::canonical(path).string();
			fs::path longestInterceptedPrefix;
			for(const auto& cur : interceptedHeaderDirs) {
				// if the current dir is a parent of the given path
				if(canonicalInterceptedPathString.find(cur.string()) == 0) {
					// and the current dir is 'longer' than the previous one
					if(cur.string().length() > longestInterceptedPrefix.string().length()) {
						longestInterceptedPrefix = cur;
					}
				}
			}

			// return if given path is not below an intercepted directory
			if(longestInterceptedPrefix.empty()) {
				return {};
			}

			// now we find the matching user include - again the 'longest' one
			const auto canonicalIncludePathString = fs::canonical(path).string();
			fs::path longestIncludePrefix;
			for(const auto& cur : userIncludeDirs) {
				// if the current dir is a parent of the given path
				if(canonicalIncludePathString.find(cur.string()) == 0) {
					// and the current dir is 'longer' than the previous one
					if(cur.string().length() > longestIncludePrefix.string().length()) {
						longestIncludePrefix = cur;
					}
				}
			}

			// return if we couldn't find a user include path as the parent of the intercepted path
			if(longestIncludePrefix.empty()) {
				return {};
			}

			// finally we compute the difference between the include path and the given path
			auto parent = path.parent_path();
			auto res = path.filename();
			while(!parent.empty() && (longestIncludePrefix / res) != path) {
				res = parent.filename() / res;
				parent = parent.parent_path();
			}

			return res;
		}
	}

	namespace {
		// removes duplicate entries from the given vector
		std::vector<fs::path> removeDuplicatesAndCanonicalize(const vector<fs::path>& input) {
			std::vector<fs::path> ret;
			for(const auto& p : input) {
				try {
					auto canonicalPath = fs::canonical(p);
					if(std::find(ret.begin(), ret.end(), canonicalPath) == ret.end()) {
						ret.push_back(canonicalPath);
					}
				} catch(const fs::filesystem_error& error) {
					LOG(ERROR) << "Header Tagger: could not canonicalize path " << p << "\n" << error.what();
				}
			}
			return ret;
		}
	}

	namespace fs = boost::filesystem;
	namespace ba = boost::algorithm;

	HeaderTagger::HeaderTagger(const vector<fs::path>& stdLibDirs,
	                           const vector<fs::path>& interceptedHeaderDirs,
	                           const vector<fs::path>& userIncludeDirs,
	                           const clang::SourceManager& srcMgr)
		: stdLibDirs(removeDuplicatesAndCanonicalize(stdLibDirs)),
		  interceptedHeaderDirs(removeDuplicatesAndCanonicalize(interceptedHeaderDirs)),
		  userIncludeDirs(removeDuplicatesAndCanonicalize(userIncludeDirs)),
		  sm(srcMgr) {

		// we need to ensure that each directory which is intercepted (or any parent of that) is also an include directory
		assert_decl({
		for(const auto& interceptedPath : this->interceptedHeaderDirs) {
			auto path = interceptedPath;
			// move up the path to the root
			bool found = false;
			while (!path.empty()) {
				// if we found the current path component in the user includes, we are fine
				if(std::find(this->userIncludeDirs.begin(), this->userIncludeDirs.end(), path) != this->userIncludeDirs.end()) {
					found = true;
					break;
				}
				// otherwise we move to the parent folder
				path = path.parent_path();
			}
			// if we didn't find the intercepted path all the way up to root without finding a matching user include, we fail
			assert_true(found) << "Intercepted path \"" << interceptedPath << "\" (or any of it's parents) has not been added as user include.";
		}
		});

		VLOG(2) << "stdLibDirs: \n\t" << this->stdLibDirs;
		VLOG(2) << "interceptedHeaderDirs: \n\t" << this->interceptedHeaderDirs;
		VLOG(2) << "userIncludeDirs: \n\t" << this->userIncludeDirs;
	}

	/**
	 * A utility function cutting down std-lib header files.
	 */
	boost::optional<fs::path> HeaderTagger::toStdLibHeader(const fs::path& path) const {
		static const boost::optional<fs::path> fail;

		if(stdLibDirs.empty()) { return fail; }

		if(contains(stdLibDirs, fs::canonical(path))) { // very expensive, keep low use
			return fs::path();
		}

		if(!path.has_parent_path()) { return fail; }

		// if it is within the std-lib directory, build relative path
		auto res = toStdLibHeader(path.parent_path());
		return (res) ? (*res / path.filename()) : fail;
	}

	bool HeaderTagger::isStdLibHeader(const clang::SourceLocation& loc) const {
		if(!loc.isValid()) { return false; }
		auto fit = isStdCache.find(sm.getFileID(loc));
		if(fit != isStdCache.end()) { return fit->second.second; }

		std::string filename = sm.getPresumedLoc(loc).getFilename();
		if(!isHeaderFile(filename)) return false;
		bool isSys = isStdLibHeader(filename);
		isStdCache[sm.getFileID(loc)] = {filename, isSys};
		return isSys;
	}

	bool HeaderTagger::isStdLibHeader(const fs::path& path) const {
		return toStdLibHeader(fs::canonical(path)); // expensive, dont go crazy with this
	}

	bool HeaderTagger::isInterceptedLibHeader(const clang::SourceLocation& loc) const {
		if(!loc.isValid()) { return false; }
		auto fit = isInterceptedCache.find(sm.getFileID(loc));
		if(fit != isInterceptedCache.end()) { return !fit->second.second; }

		std::string filename = sm.getPresumedLoc(loc).getFilename();
		if(!isHeaderFile(filename)) return false;
		bool isIntercepted = isInterceptedLibHeader(filename);
		isInterceptedCache[sm.getFileID(loc)] = {filename, !isIntercepted};
		return isIntercepted;
	}

	bool HeaderTagger::isInterceptedLibHeader(const fs::path& path) const {
		return toInterceptedLibHeader(path);
	}

	boost::optional<fs::path> HeaderTagger::toInterceptedLibHeader(const fs::path& path) const {
		return detail::getInterceptedLibHeader(userIncludeDirs, interceptedHeaderDirs, path);
	}

	bool HeaderTagger::isUserLibHeader(const clang::SourceLocation& loc) const {
		if(!loc.isValid()) { return false; }
		auto fit = isUserCache.find(sm.getFileID(loc));
		if(fit != isUserCache.end()) { return !fit->second.second; }

		std::string filename = sm.getPresumedLoc(loc).getFilename();
		if(!isHeaderFile(filename)) return false;
		bool isUser = isUserLibHeader(filename);
		isUserCache[sm.getFileID(loc)] = {filename, !isUser};
		return isUser;
	}

	bool HeaderTagger::isUserLibHeader(const fs::path& path) const {
		return toUserLibHeader(path);
	}

	boost::optional<fs::path> HeaderTagger::toUserLibHeader(const fs::path& path) const {
		static const boost::optional<fs::path> fail;

		if(userIncludeDirs.empty()) { return fail; }

		if(contains(userIncludeDirs, fs::canonical(path))) { return fs::path(); }

		if(!path.has_parent_path()) { return fail; }

		// if it is within the user-added-include directory, build relative path
		auto res = toUserLibHeader(path.parent_path());
		return (res) ? (*res / path.filename()) : fail;
	}


	bool HeaderTagger::isHeaderFile(const string& name) const {
		// everything ending with .h or .hpp or nothing (e.g. vector) => so check for not being c,cpp,...
		VLOG(2) << "isHeaderFile? " << name;
		return !name.empty()
			   && !(ba::ends_with(name, ".c") || ba::ends_with(name, ".cc") || ba::ends_with(name, ".cpp") || ba::ends_with(name, ".cxx")
					|| ba::ends_with(name, ".C"));
	}

	string HeaderTagger::getTopLevelInclude(const clang::SourceLocation& includeLocation) const {
		// if it is a dead end
		if(!includeLocation.isValid()) { return ""; }

		auto toFilename = [&](const clang::SourceLocation& loc) { return sm.getPresumedLoc(loc).getFilename(); };

		// get the presumed location (whatever this is, ask clang) ...
		// ~ ploc represents the file were includeLocation is located
		clang::PresumedLoc ploc = sm.getPresumedLoc(includeLocation);

		// .. and retrieve the associated include
		// ~ from where the file ploc represents was included
		clang::SourceLocation includingLocation = ploc.getIncludeLoc();

		// check whether the stack can be continued
		if(!includingLocation.isValid()) {
			return ""; // this happens when element is declared in c / cpp file => no header
		}

		// ~ pIncludeLoc represents the file were includLoc is located
		clang::PresumedLoc pIncludeLoc = sm.getPresumedLoc(includingLocation);

		if(isInjectedHeader(pIncludeLoc)) {
			// the header was injected -- has no valid filename ("<command line">)
			return "";
		}

		//*******************
		//
		// travel down until we are at the sourcefile then work up again
		// if userProvided header, we need to go further
		// if userSearchPath/systemSearch path we need to include the header
		//
		//*******************

		// descent further as long as we have a header file as presumed include includeLocation
		if(isHeaderFile(toFilename(includingLocation))) {
			// check if last include was in the search path and next is not,
			// this case is a system header included inside of a programmer include chain
			// BUT if both are still in the search path, continue cleaning the include
			if(isStdLibHeader(includeLocation) && !isStdLibHeader(includingLocation)) {
				if(!isIntrinsicHeader(toFilename(includingLocation))) { return ploc.getFilename(); }
			}

			if(isInterceptedLibHeader(includeLocation) && !isInterceptedLibHeader(includingLocation)) {
				if(!isIntrinsicHeader(toFilename(includingLocation))) { return ploc.getFilename(); }
			}

			if(isUserLibHeader(includeLocation) && !isUserLibHeader(includingLocation)) {
				if(!isIntrinsicHeader(toFilename(includingLocation))) { return ploc.getFilename(); }
			}

			return getTopLevelInclude(includingLocation);
		}

		// we already visited all the headers and we are in the .c/.cpp file
		if(isHeaderFile(ploc.getFilename())) {
			if(isIntrinsicHeader(ploc.getFilename())) { return ploc.getFilename(); }

			if(isStdLibHeader(ploc.getFilename())) {
				return ploc.getFilename(); // this happens when header file is included straight in the code
			}

			if(isInterceptedLibHeader(ploc.getFilename())) {
				return ploc.getFilename(); // this happens when header file is included straight in the code
			}

			if(isUserLibHeader(ploc.getFilename())) { return ploc.getFilename(); }

			return ""; // this happens when is declared in a header which is not system header
		}
		return "";
	}

	bool HeaderTagger::isIntrinsicHeader(const string& name) const {
		return toIntrinsicHeader(fs::path(name));
	}
	bool HeaderTagger::isInjectedHeader(const clang::PresumedLoc& ploc) const {
		// NOTE: the "-include" of clang is what we call injectedHeaders
		// injected headers are "included" from a file called "<command line>" (by clang)
		return std::strcmp("<command line>", ploc.getFilename()) == 0;
	}

	boost::optional<fs::path> HeaderTagger::toIntrinsicHeader(const fs::path& path) const {
		static const boost::optional<fs::path> fail;
		fs::path filename = path.filename();
		return (!filename.empty() && ba::ends_with(filename.string(), "intrin.h")) ? (filename) : fail;
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 			header tagging interface

	bool HeaderTagger::isDefinedInSystemHeader(const clang::Decl* decl) const {
		return isStdLibHeader(decl->getLocation());
	}

	void HeaderTagger::addHeaderForDecl(const core::NodePtr& node, const clang::Decl* decl, bool attachUserDefined) const {
		// check whether there is a declaration at all
		if(!decl) { return; }

		// the node was already annotated, what is the point of doint it again?
		if(insieme::annotations::c::hasIncludeAttached(node)) { return; }

		if(VLOG_IS_ON(2)) {
			std::string name("UNNAMED");
			if(const clang::NamedDecl* nmd = llvm::dyn_cast<clang::NamedDecl>(decl)) { name = nmd->getQualifiedNameAsString(); }
			VLOG(2) << "Searching header for: " << node << " of type " << node->getNodeType() << " [clang: " << name << "]";
		}

		string fileName = getTopLevelInclude(decl->getLocation());

		// file must be a header file
		if(!isHeaderFile(fileName)) {
			VLOG(2) << "'" << fileName << "' not a headerfile";
			return; // not to be attached
		}

		// do not add headers for external declarations unless those are within the std-library
		if(const clang::FunctionDecl* funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
			// TODO: this is just based on integration tests - to make them work, no real foundation :(
			if(funDecl->isExternC() && !(isStdLibHeader(fileName) || isIntrinsicHeader(fileName))) { return; }
		}

		// get absolute path of header file
		fs::path header = fs::canonical(fileName);

		if(auto stdLibHeader = toStdLibHeader(header)) {
			header = *stdLibHeader;
		} else if(auto interceptedLibHeader = toInterceptedLibHeader(header)) {
			header = *interceptedLibHeader;
		} else if(auto intrinsicHeader = toIntrinsicHeader(header)) {
			header = *intrinsicHeader;
		} else if(auto userLibHeader = toUserLibHeader(header)) {
			if(attachUserDefined) {
				header = *userLibHeader;
			} else {
				return;
			}
		}

		VLOG(2) << "		header to be attached: " << header.string();

		// use resulting header
		insieme::annotations::c::attachInclude(node, header.string());
	}

	bool HeaderTagger::isIntercepted(const clang::Decl* decl) const	{
		auto location = decl->getLocation();
		// we do not want to intercept initializer lists
		if(auto namedDecl = llvm::dyn_cast<clang::NamedDecl>(decl)) {
			if(boost::starts_with(namedDecl->getQualifiedNameAsString(), "std::initializer_list")) return false;
		}
		// check whether we should intercept this decl
		return isStdLibHeader(location) || isInterceptedLibHeader(location);
	}

} // end namespace utils
} // end namespace frontend
} // end namespace utils

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

#pragma once
#include "insieme/core/lang/extension.h"

namespace insieme {
namespace iwir {
namespace extension {

class IWIRExtension : public core::lang::Extension {

	/**
	* Allow the node manager to create instances of this class.
	*/
	friend class core::NodeManager;

	/**
	* Creates a new instance based on the given node manager.
	*/
	IWIRExtension(core::NodeManager& manager)
		: core::lang::Extension(manager) {}

public:

	LANG_EXT_LITERAL(StringToInt, "string_to_int", "(ref<array<char,1>> ,intTypeParam<#b>) -> int<#b>");
	LANG_EXT_LITERAL(StringToDouble, "string_to_double", "(ref<array<char,1>> ,intTypeParam<#b>) -> real<#b>");
	LANG_EXT_LITERAL(StringToBool, "string_to_bool", "(ref<array<char,1>>) -> bool");

	LANG_EXT_LITERAL(StringLt, "string_lt", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");
	LANG_EXT_LITERAL(StringLe, "string_le", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");
	LANG_EXT_LITERAL(StringGt, "string_gt", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");
	LANG_EXT_LITERAL(StringGe, "string_ge", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");
	LANG_EXT_LITERAL(StringEq, "string_eq", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");
	LANG_EXT_LITERAL(StringNe, "string_ne", "(ref<array<char,1>>, ref<array<char,1>>) -> bool");

	//file<string> to differentiate string and a filepath-string
	LANG_EXT_TYPE(File, "file<ref<array<char,1>>>");
	LANG_EXT_LITERAL(FilePath, "file_path", "(file<ref<array<char,1>>>) -> ref<array<char,1>>");

	bool isFileType(const core::TypePtr type) const {
		assert_true(type);
		core::GenericTypePtr gt = type.isa<core::GenericTypePtr>();
		if(!gt) return false;

		return (gt->getName()->getValue() == "file" && 
				gt->getTypeParameter().size() == 1u &&
				gt->getIntTypeParameter().empty()
			   ); 
	}
	LANG_EXT_LITERAL(BoolToString, "bool_to_string", "(bool) -> ref<array<char,1>>");
	LANG_EXT_LITERAL(IntToString, "int_to_string", "(int<4>) -> ref<array<char,1>>");
	LANG_EXT_LITERAL(DoubleToString, "double_to_string", "(real<8>) -> ref<array<char,1>>");
};

class CollectionTypeExtension : public core::lang::Extension {

	/**
	* Allow the node manager to create instances of this class.
	*/
	friend class core::NodeManager;

	/**
	* Creates a new instance based on the given node manager.
	*/
	CollectionTypeExtension (core::NodeManager& manager)
		: core::lang::Extension(manager) {}

public:
	LANG_EXT_TYPE   (Collection,            "collection<'elemTy>");
	LANG_EXT_LITERAL(CollectionSize,	    "collection_size", "(collection<'elemTy>) -> int<4>");
	LANG_EXT_LITERAL(ShortestCollection,	"shortest_collection", "(list<ref<collection<'elemTy>>>) -> int<4>");
	//LANG_EXT_LITERAL(ShortestCollection,	"shortest_collection", "(vector<collection<'elemTy>, #l>) -> int<4>");
	LANG_EXT_LITERAL(RefCollectionSize,     "ref_collection_size", "(ref<collection<'elemTy>>) -> int<4>");
	LANG_EXT_LITERAL(RefCollectionAppend,   "ref_collection_append", "(ref<collection<'elemTy>>, 'elemTy) -> ref<collection<'elemTy>>");
	LANG_EXT_LITERAL(CollectionAt,		    "collection_at", "(collection<'elemTy>, int<4>) -> 'elemTy");
	LANG_EXT_LITERAL(RefCollectionAt,	    "ref_collection_at", "(ref<collection<'elemTy>>, int<4>) -> ref<'elemTy>");

	LANG_EXT_DERIVED(RefLinkLoopElement,
			"lambda (ref<collection<'elemTy>> from, int<4> it, ref<'elemTy> to)->unit {"
			"	let at = expr lit(\"ref_collection_at\":(ref<collection<'elemTy>>, int<4>) -> ref<'elemTy>);"
			"	to = *( at(from, it));"
			"}");

	LANG_EXT_DERIVED(LinkLoopElement,
			"lambda (collection<'elemTy> from, int<4> it, ref<'elemTy> to)->unit {"
			"	let at = expr lit(\"collection_at\":(collection<'elemTy>, int<4>) -> 'elemTy);"
			"	to = at(from, it);"
			"}");


	LANG_EXT_DERIVED(RefLinkUnion, 
			"lambda (ref<'elemTy> from, ref<collection<'elemTy>> to)->unit {"
			"	let append = expr lit(\"ref.collection_append\":(ref<collection<'elemTy>>, 'elemTy) -> ref<collection<'elemTy>>);"
			"	to = * append(to, *from);"
			"}");

	LANG_EXT_DERIVED(LinkUnion, 
			"lambda ('elemTy from, ref<collection<'elemTy>> to)->unit {"
			"	let append = expr lit(\"ref.collection_append\":(ref<collection<'elemTy>>, 'elemTy) -> ref<collection<'elemTy>>);"
			"	to = *(append(to, from));"
			"}");


	LANG_EXT_DERIVED(RefLinkBasic, 
			"lambda (ref<'a> from, ref<'a> to)->unit {"
			"	to = *from;"
			"}");

	LANG_EXT_DERIVED(LinkBasic, 
			"lambda ('a from, ref<'a> to)->unit {"
			"	to = from;"
			"}");

	LANG_EXT_DERIVED(RefLinkParallelOutput,
			"lambda (ref<'elemTy> from, ref<collection<'elemTy>> to, int<4> it)->unit {"
			"	let at = expr lit(\"ref.collection_at\":(ref<collection<'elemTy>>, int<4>) -> ref<'elemTy>);"
			"	at(to, it) = *from;"
			"}");

	LANG_EXT_DERIVED(LinkParallelOutput,
			"lambda ('elemTy from, ref<collection<'elemTy>> to, int<4> it)->unit {"
			"	let at = expr lit(\"ref.collection_at\":(ref<collection<'elemTy>>, int<4>) -> ref<'elemTy>);"
			"	at(to, it) = from;"
			"}");



	core::TypePtr getCollectionType(const core::TypePtr elemType) const {
		assert_true(elemType);
		core::IRBuilder builder(elemType.getNodeManager());
		return builder.genericType("collection", { elemType }, core::IntParamList());
	}

	bool isCollectionType(const core::TypePtr type) const {
		assert_true(type);
		core::GenericTypePtr gt = type.isa<core::GenericTypePtr>();
		if(!gt) return false;

		return (gt->getName()->getValue() == "collection" && 
				gt->getTypeParameter().size() == 1u &&
				gt->getIntTypeParameter().empty()
			   ); 
	}

	core::TypePtr getElementType(const core::TypePtr collectionType) const {
		assert_true(isCollectionType(collectionType));
		return collectionType.as<core::GenericTypePtr>()->getTypeParameter()[0];
	}
};

} // namespace extension end
} // namespace iwir end
} // namespace insieme end

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

#include <vector>

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/tuples.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_class_info.h"


namespace insieme {
namespace core {
namespace encoder {

	using std::vector;

	// -- encoder support for IR class meta infos --

	namespace detail {

		/**
		 * The tuple type utilized for encoding a member function.
		 */
		typedef std::tuple<string, ExpressionPtr, bool, bool> encoded_member_fun_type;

		/**
		 * The tuple type utilized for encoding a complete meta info object.
		 */
		typedef std::tuple<vector<ExpressionPtr>, ExpressionPtr, bool, vector<encoded_member_fun_type>> encoded_class_info_type;

		/**
		 * A generic functor creating the IR type of an encoded meta info.
		 */
		struct create_class_info_type {
			core::TypePtr operator()(NodeManager& manager) const {
				const static type_factory<encoded_class_info_type> factory;
				return factory(manager);
			}
		};

		/**
		 * A generic functor testing whether a given expression is a valid
		 * encoding of a meta info object.
		 */
		struct is_class_info {
			bool operator()(const core::ExpressionPtr& expr) const {
				const static is_encoding_of<encoded_class_info_type> check;
				return check(expr);
			}
		};

		/**
		 * A generic functor encoding a meta info object into an IR expression.
		 */
		struct encode_class_info {
			core::ExpressionPtr operator()(NodeManager& manager, const ClassMetaInfo& info) const {

				// convert member functions
				auto encodedMemberFuns = ::transform(info.getMemberFunctions(), [](const MemberFunction& cur)->encoded_member_fun_type {
					return encoded_member_fun_type(
							cur.getName(),
							cur.getImplementation(),
							cur.isVirtual(),
							cur.isConst()
					);
				});

				// encode class meta-info object
				return encoder::toIR(
					manager,
					encoded_class_info_type (
							info.getConstructors(),
							info.getDestructor(),
							info.isDestructorVirtual(),
							encodedMemberFuns
					)
				);
			}
		};

		/**
		 * A generic functor decoding a meta info object into an IR expression.
		 */
		struct decode_class_info {
			ClassMetaInfo operator()(const core::ExpressionPtr& info) const {
				assert(encoder::isEncodingOf<ClassMetaInfo>(info) && "Invalid encoding encountered!");

				// decode the class object
				auto tuple = encoder::toValue<encoded_class_info_type>(info);

				// restore info
				ClassMetaInfo res;

				res.setConstructors(std::get<0>(tuple));
				res.setDestructor(std::get<1>(tuple));
				res.setDestructorVirtual(std::get<2>(tuple));
				res.setMemberFunctions(::transform(std::get<3>(tuple), [](const encoded_member_fun_type& cur) {
					return MemberFunction(std::get<0>(cur), std::get<1>(cur), std::get<2>(cur), std::get<3>(cur));
				}));

				// done
				return res;
			}
		};

	} // end namespace detail

	// define encoder / decoder according to the encoder framework

	/**
	 * Defines a class-meta-info-converter functor allowing to customize the encoding of the element type.
	 */
	struct ClassMetaInfoConverter : public Converter<ClassMetaInfo, detail::create_class_info_type, detail::encode_class_info, detail::decode_class_info, detail::is_class_info> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of meta info objects.
	 */
	template<>
	struct type_factory<ClassMetaInfo> : public detail::create_class_info_type {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of meta info objects.
	 */
	template<>
	struct value_to_ir_converter<ClassMetaInfo> : public detail::encode_class_info {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of meta info objects.
	 */
	template<>
	struct ir_to_value_converter<ClassMetaInfo> : public detail::decode_class_info {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of meta info objects.
	 */
	template<>
	struct is_encoding_of<ClassMetaInfo> : public detail::is_class_info { };



} // end namespace encoder
} // end namespace core
} // end namespace insieme

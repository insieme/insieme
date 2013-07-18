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

#include <inttypes.h>

#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>

#include "insieme/utils/numeric_cast.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"

namespace insieme {
namespace core {
namespace encoder {

	/**
	 * Within this header file the basic infrastructure for encoding C++ values into
	 * IR values and the other way around is defined. For each convertible value
	 * type three functors have to be defined.
	 *
	 * 		- type_factory<T> ... a functor of type TypePtr(NodeManager)
	 * 							  returning the IR equivalent of T.
	 *
	 * 		- value_to_ir_converter<T> ... a functor of type
	 * 							  ExpressionPtr(NodeManager&, const T&) encoding
	 * 							  the given value into an IR expression using the
	 * 							  given node manager.
	 *
	 * 		- ir_to_value_converter<T> ... a functor of type T(const ExpressionPtr&)
	 * 							  trying to decode the given expression and producing
	 * 							  a value of the given type T. If the conversion can
	 * 							  not be supported, an exception is raised.
	 *
	 *		- is_encoding_of<T> ... a functor of type bool(const ExpressionPtr&)
	 *							  testing whether the given expression is a valid encoding
	 *							  of a value of type T. If this functor is returning
	 *							  true for some expression e, than the decoding of
	 *							  e using ir_to_value_converter<T> must not fail.
	 *
	 * The three generic methods toIR(..), toValue(..) and isEncodingOf(..) provide
	 * generic wrappers for the conversion process.
	 */


	/**
	 * A generic functor realizing a type factory for a given value type T. The
	 * template is only defined by its partial specializations to cause a compile
	 * error whenever a given type is not supported.
	 */
	template<typename T>
	struct type_factory;

	/**
	 * A generic functor realizing a value converter for a given value type T. The
	 * template is only defined by its partial specializations to cause a compile
	 * error whenever a given type is not supported.
	 */
	template<typename T>
	struct value_to_ir_converter;

	/**
	 * A generic functor realizing a value converter for a given value type T. The
	 * template is only defined by its partial specializations to cause a compile
	 * error whenever a given type is not supported.
	 */
	template<typename T>
	struct ir_to_value_converter;

	/**
	 * A generic functor testing whether a given expression is encoding a value of
	 * the given generic parameter type.
	 */
	template<typename T>
	struct is_encoding_of;

	/**
	 * A struct bundling together a collection of encoders / decoders for a given
	 * value type. This class can be used to determine the actual encoding within
	 * the following wrapper functions.
	 */
	template<
		typename T,
		typename TypeFactory = type_factory<T>,
		typename Value2IR    = value_to_ir_converter<T>,
		typename IR2Value    = ir_to_value_converter<T>,
		typename Tester      = is_encoding_of<T>
	>
	struct Converter {
		typedef T 				value_type;
		typedef TypeFactory 	type_factory;
		typedef Value2IR 		value_to_ir_converter;
		typedef IR2Value 		ir_to_value_converter;
		typedef Tester 			is_encoding_of;
	};

	/**
	 * A generic utility function determining the IR type to be used to encode
	 * information of the given value type T.
	 *
	 * @tparam T the type of value to be encoded
	 * @tparam C the converter to be used
	 * @param manager the manager to be used to obtain the resulting type
	 * @return the IR type used to encode information of type T
	 */
	template<typename T, typename C = Converter<T>>
	core::TypePtr getTypeFor(core::NodeManager& manager) {
		return typename C::type_factory()(manager);
	}

	/**
	 * A generic utility function supporting the encoding of a given value into an
	 * equivalent IR representation.
	 *
	 * @tparam C the converter to be used
	 * @tparam T the type of value to be encoded
	 * @param manager the manager to be used to create the resulting IR node
	 * @param value the value to be encoded
	 * @return the encoded equivalent IR structure
	 */
	template<typename T, typename C = Converter<T>>
	core::ExpressionPtr toIR(core::NodeManager& manager, T value) {
		return typename C::value_to_ir_converter()(manager, value);
	}

	/**
	 * A generic utility function supporting the decoding a given IR representation
	 * into a C++ value.
	 *
	 * @tparam T the type of value to be decoded
	 * @tparam C the converter to be used
	 * @param expr the expression to be decoded
	 * @return the decoded equivalent value
	 */
	template<typename T, typename C = Converter<T>>
	T toValue(const core::ExpressionPtr& expr) {
		return typename C::ir_to_value_converter()(expr);
	}

	/**
	 * A generic utility function allowing to test whether the given IR expression
	 * is a encoding of the given value type.
	 *
	 * @tparam T the type of value to be tested for
	 * @tparam C the converter to be used
	 * @param expr the expression to be tested
	 * @return true if it is a valid encoding, false otherwise
	 */
	template<typename T, typename C = Converter<T>>
	bool isEncodingOf(const core::ExpressionPtr& expr) {
		return typename C::is_encoding_of()(expr);
	}

	/**
	 * An exception which will be raised if a expression not representing
	 * a formula should be converted into one.
	 */
	class InvalidExpression : public std::exception {

		/**
		 * A message describing the cause of the problem.
		 */
		std::string msg;

	public:
		InvalidExpression(const string& cause) : msg(cause) {};
		InvalidExpression(const ExpressionPtr& expr);
		InvalidExpression(const TypePtr& should, const TypePtr& is);
		virtual ~InvalidExpression() throw() { }
		virtual const char* what() const throw();
	};


	// ------------------------------------------------------------------------
	//  Definition of basic type conversion functors
	// ------------------------------------------------------------------------

	namespace detail {

		/**
		 * Defines some generic converters helpers implementing basic encoding/decoding
		 * capabilities for primitive data types.
		 */

		/**
		 * A simple value converter converting values into literals representing the corresponding
		 * value.
		 */
		template<typename T>
		struct simple_value_converter {
			core::ExpressionPtr operator()(core::NodeManager& manager, const T& value) const {
				type_factory<T> factory;
				return Literal::get(manager, factory(manager), utils::numeric_cast<string>(value));
			}
		};

		/**
		 * A simple generic utility class enabling the decoding of an expression for literal based
		 * encodings.
		 */
		template<typename T>
		struct simple_ir_converter {
			T operator()(const core::ExpressionPtr& expr) const {

				// check type
				TypePtr should = type_factory<T>()(expr->getNodeManager());
				TypePtr is = expr->getType();
				if (!types::isSubTypeOf(is, should))
				if (should != is) {
					throw InvalidExpression(should, is);
				}

				// handle casts
				if (expr->getNodeType() == core::NT_CastExpr) {
					return (*this)(static_pointer_cast<const core::CastExpr>(expr)->getSubExpression());
				}

				// check node-type
				if (expr->getNodeType() != core::NT_Literal) {
					throw InvalidExpression(expr);
				}

				// check values again ...
				assert(types::isSubTypeOf(expr->getType(), type_factory<T>()(expr->getNodeManager())) && "Cannot convert non-related type!");
				assert(expr->getNodeType() == core::NT_Literal && "Simple conversion only works for literals!");

				// convert
				return utils::numeric_cast<T>(static_pointer_cast<const core::Literal>(expr)->getStringValue());
			}
		};

		/**
		 * A simple test whether the a given expression is an encoding of a given type.
		 */
		template<typename T>
		struct simple_is_encoding_of_test {
			bool operator()(const core::ExpressionPtr& expr) const {
				return expr->getNodeType() == NT_Literal && types::isSubTypeOf(expr->getType(), type_factory<T>()(expr->getNodeManager()));
			}
		};
	}

	/**
	 * A macro simplifying the definition of converts for primitive types.
	 */
	#define ADD_CONVERTER(type,irtype) \
		template<> \
		struct type_factory<type> { \
			core::TypePtr operator()(core::NodeManager& manager) const { \
				return manager.getLangBasic().get ## irtype(); \
			} \
		}; \
		\
		template<> struct value_to_ir_converter<type> : public detail::simple_value_converter<type> {}; \
		template<> struct ir_to_value_converter<type> : public detail::simple_ir_converter<type> {}; \
		template<> struct is_encoding_of<type> : public detail::simple_is_encoding_of_test<type> {};

		// -- define converter for primitive types --------------

		ADD_CONVERTER(int8_t,  Int1);
		ADD_CONVERTER(int16_t, Int2);
		ADD_CONVERTER(int32_t, Int4);
		ADD_CONVERTER(int64_t, Int8);

		ADD_CONVERTER(uint8_t,  UInt1);
		ADD_CONVERTER(uint16_t, UInt2);
		ADD_CONVERTER(uint32_t, UInt4);
		ADD_CONVERTER(uint64_t, UInt8);

		ADD_CONVERTER(float,  Float);
		ADD_CONVERTER(double, Double);

		ADD_CONVERTER(string, String);

	#undef ADD_CONVERTER

	// --------------------------------------------------------------------
	//   Add support for encoding boolean values
	// --------------------------------------------------------------------

	template<> struct type_factory<bool> {
		core::TypePtr operator()(core::NodeManager& manager) const {
			return manager.getLangBasic().getBool();
		}
	};

	template<> struct is_encoding_of<bool> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& basic = expr->getNodeManager().getLangBasic();
			return basic.isTrue(expr) || basic.isFalse(expr);
		}
	};

	template<> struct value_to_ir_converter<bool> {
		core::ExpressionPtr operator()(core::NodeManager& manager, bool value) const {
			return IRBuilder(manager).boolLit(value);
		}
	};

	template<> struct ir_to_value_converter<bool> {
		bool operator()(const core::ExpressionPtr& expr) const {
			auto& basic = expr->getNodeManager().getLangBasic();
			assert(is_encoding_of<bool>()(expr));
			return basic.isTrue(expr);
		}
	};

	// --------------------------------------------------------------------
	//   Add support for encoding expressions directly into expressions
	// --------------------------------------------------------------------

	namespace detail {

		struct create_expr_type {
			core::TypePtr operator()(core::NodeManager& manager) const {
				assert(false && "Not applicable in the general case!");
				throw InvalidExpression("Cannot define generic type for all expressions!");
			}
		};

		struct is_expr {
			bool operator()(const core::ExpressionPtr& expr) const {
				return true;	// every expression is a direct encoding of itself
			}
		};

		struct encode_expr {
			core::ExpressionPtr operator()(core::NodeManager& manager, const core::ExpressionPtr& value) const {
				return manager.get(value);
			}
		};

		struct decode_expr {
			core::ExpressionPtr operator()(const core::ExpressionPtr& expr) const {
				return expr;
			}
		};

	}

	/**
	 * Defines a converter for IR expressions mapping expressions 1:1 to themselves.
	 *
	 * @tparam E the element type within the list
	 * @tparam C the converter to be used for encoding element types
	 */
	struct DirectExprConverter : public Converter<ExpressionPtr, detail::create_expr_type, detail::encode_expr, detail::decode_expr, detail::is_expr> {};


	// ------------ Also support general and derived expression types -----------------

	#define ADD_EXPRESSION_CONVERTER(_TYPE) \
		template<> \
		struct type_factory<_TYPE> { \
			core::TypePtr operator()(core::NodeManager& manager) const { \
				return GenericType::get(manager, "encoded_" #_TYPE); \
			} \
		}; \
		\
		template<> \
		struct is_encoding_of<_TYPE> { \
			bool operator()(const core::ExpressionPtr& expr) const { \
				IRBuilder builder(expr->getNodeManager()); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				auto alpha = builder.typeVariable("a"); \
				auto wrapFun = builder.literal("wrap_" #_TYPE, builder.functionType(alpha, resType)); \
				auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
				return core::analysis::isCallOf(expr, wrapFun) || core::analysis::isCallOf(expr, nullFun); \
			} \
		}; \
		\
		template<> \
		struct value_to_ir_converter<_TYPE> { \
			core::ExpressionPtr operator()(core::NodeManager& manager, const _TYPE& value) const { \
				IRBuilder builder(manager); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				if (!value) { \
					auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
					return builder.callExpr(resType, nullFun); \
				} \
				auto alpha = builder.typeVariable("a"); \
				auto wrapFun = builder.literal("wrap_" #_TYPE, builder.functionType(alpha, resType)); \
				return builder.callExpr(resType, wrapFun, value); \
			} \
		}; \
		\
		template<> \
		struct ir_to_value_converter<_TYPE> { \
			_TYPE operator()(const core::ExpressionPtr& expr) const { \
				assert(is_encoding_of<_TYPE>()(expr) && "Invalid encoding!"); \
				IRBuilder builder(expr->getNodeManager()); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
				if (core::analysis::isCallOf(expr, nullFun)) { \
					return _TYPE(); \
				} \
				return expr.as<CallExprPtr>().getArgument(0).as<_TYPE>(); \
			} \
		}

	ADD_EXPRESSION_CONVERTER(ExpressionPtr);
	ADD_EXPRESSION_CONVERTER(LambdaExprPtr);
	ADD_EXPRESSION_CONVERTER(LiteralPtr);

	// --------------------------------------------------------------------
	//       Add support for encoding of types within expressions
	// --------------------------------------------------------------------

	#define ADD_TYPE_CONVERTER(_TYPE) \
		template<> \
		struct type_factory<_TYPE> { \
			core::TypePtr operator()(core::NodeManager& manager) const { \
				return GenericType::get(manager, "encoded_" #_TYPE); \
			} \
		}; \
		\
		template<> \
		struct is_encoding_of<_TYPE> { \
			bool operator()(const core::ExpressionPtr& expr) const { \
				IRBuilder builder(expr->getNodeManager()); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				auto alpha = builder.typeVariable("a"); \
				auto wrapFun = builder.literal("wrap_" #_TYPE, builder.functionType(alpha, resType)); \
				auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
				if (core::analysis::isCallOf(expr, nullFun)) return true; \
				if (!core::analysis::isCallOf(expr, wrapFun)) return false; \
				\
				auto arg = expr.as<CallExprPtr>()[0]; \
				if (arg->getNodeType() != core::NT_Literal) { \
					return false; \
				} \
				 \
				const core::TypePtr& type = arg->getType(); \
				if (type->getNodeType() != core::NT_GenericType) { \
					return false; \
				} \
				 \
				const core::GenericTypePtr& genType = type.as<GenericTypePtr>(); \
				return genType->getName()->getValue() == "type" && \
						genType->getTypeParameter().size() == static_cast<std::size_t>(1) && \
						genType->getTypeParameter()[0].isa<_TYPE>() && \
						genType->getIntTypeParameter().empty(); \
			} \
		}; \
		\
		template<> \
		struct value_to_ir_converter<_TYPE> { \
			core::ExpressionPtr operator()(core::NodeManager& manager, const _TYPE& value) const { \
				IRBuilder builder(manager); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				if (!value) { \
					auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
					return builder.callExpr(resType, nullFun); \
				} \
				auto alpha = builder.typeVariable("a"); \
				auto wrapFun = builder.literal("wrap_" #_TYPE, builder.functionType(alpha, resType)); \
				return builder.callExpr(resType, wrapFun, builder.getTypeLiteral(value)); \
			} \
		}; \
		\
		template<> \
		struct ir_to_value_converter<_TYPE> { \
			_TYPE operator()(const core::ExpressionPtr& expr) const { \
				assert(is_encoding_of<_TYPE>()(expr) && "Invalid encoding!"); \
				IRBuilder builder(expr->getNodeManager()); \
				auto resType = builder.genericType("encoded_" #_TYPE); \
				auto nullFun = builder.literal("null_" # _TYPE, builder.functionType(TypeList(), resType)); \
				if (core::analysis::isCallOf(expr, nullFun)) { \
					return _TYPE(); \
				} \
				return analysis::getRepresentedType(expr.as<CallExprPtr>().getArgument(0)).as<_TYPE>(); \
			} \
		}

	ADD_TYPE_CONVERTER(TypePtr);
	ADD_TYPE_CONVERTER(GenericTypePtr);

} // end namespace lists
} // end namespace core
} // end namespace insieme

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
 */

#pragma once

#include <algorithm>
#include <ostream>

#include <array>
#include <vector>
#include <set>

namespace insieme {
namespace utils {


	// ------------------------------------------------------------------------
	//							     Interfaces
	// ------------------------------------------------------------------------

	template<char Ctor, typename ... Fields>
	struct Variant;

	template<typename ... Variants>
	class ADT;



	// ------------------------------------------------------------------------
	//								Implementation
	// ------------------------------------------------------------------------

	namespace detail {

		template<typename ... Variants>
		struct adt_arity;

		template<>
		struct adt_arity<> {
			enum { value = 0 };
		};

		template<
			char Ctor,
			typename ... Fields,
			typename ... Rest
		>
		struct adt_arity<Variant<Ctor,Fields...>,Rest...> {
			enum { value = (sizeof...(Fields) > (unsigned)(adt_arity<Rest...>::value))
				? sizeof...(Fields)
				: (unsigned)(adt_arity<Rest...>::value)
			};
		};

	}

	namespace detail {

		enum class ValueType {
			// atomic values
			Bool,
			Char,
			Integer,
			Float,
			Double,

			// everything more complex than atomic values
			Composed
		};


		struct value_wrapper_base {

			virtual ~value_wrapper_base() {};

			virtual bool operator==(const value_wrapper_base& other) const =0;

			bool operator!=(const value_wrapper_base& other) const {
				return !(*this == other);
			}

			virtual bool operator<(const value_wrapper_base& other) const =0;

			virtual std::ostream& printTo(std::ostream& out) const =0;

		};

		template<typename T>
		struct value_wrapper : public value_wrapper_base {

			T value;

			value_wrapper(const T& value) : value(value) {}

			value_wrapper(T&& value) : value(std::move(value)) {}

			~value_wrapper() override {}

			bool operator==(const value_wrapper_base& other) const override {
				return value == static_cast<const value_wrapper<T>&>(other).value;
			}
			bool operator<(const value_wrapper_base& other) const override {
				return value < static_cast<const value_wrapper<T>&>(other).value;
			}

			std::ostream& printTo(std::ostream& out) const override {
				return out << value;
			}
		};


		struct adt_value {
			ValueType type;
			union {
				bool b;
				char c;
				int i;
				float f;
				double d;
				value_wrapper_base* v; 	// < a wrapper for non-primitive types
			};

			friend std::ostream& operator<<(std::ostream& out, const adt_value& value);
		};

		template<std::size_t arity>
		struct adt_data {
			char kind;
			char size;
			int ref_count;
			std::array<adt_value,arity> data;

			adt_data(char kind, char size, const std::array<adt_value,arity>& data)
				: kind(kind), size(size), ref_count(0), data(data) {};

			void incRefCounter() {
				ref_count++;
			}

		private:

			void reduceRefCounter(const adt_value& value) {
				switch(value.type) {
					case ValueType::Bool:
					case ValueType::Char:
					case ValueType::Integer:
					case ValueType::Float:
					case ValueType::Double:
						break;
					case ValueType::Composed:
						delete value.v;
						break;
				}
			}

		public:

			void decRefCounter() {
				ref_count--;
				if (ref_count != 0) return;

				// cleanup this node
				for(int i=0; i<size; i++) {
					reduceRefCounter(data[i]);
				}
				delete this;
			}

			bool operator==(const adt_data& other) const {
				if (kind != other.kind || size != other.size) return false;
				for(int i=0; i<size; i++) {
					assert(data[i].type == other.data[i].type);
					switch(data[i].type) {
					case ValueType::Bool: {
						if (data[i].b != other.data[i].b) return false;
						break;
					}
					case ValueType::Char: {
						if (data[i].c != other.data[i].c) return false;
						break;
					}
					case ValueType::Integer: {
						if (data[i].i != other.data[i].i) return false;
						break;
					}
					case ValueType::Float: {
						if (data[i].f != other.data[i].f) return false;
						break;
					}
					case ValueType::Double: {
						if (data[i].d != other.data[i].d) return false;
						break;
					}
					case ValueType::Composed: {
						auto a = data[i].v;
						auto b = other.data[i].v;
						return (a == b || *a == *b);
					}
					}
				}
				return true;
			}

			bool operator<(const adt_data& other) const {
				// quick exit
				if (this == &other) return false;

				// first check kind
				if (kind < other.kind) return true;
				if (kind != other.kind) return false;

				// compare content
				assert(size == other.size);
				for(int i=0; i<size; i++) {
					assert(data[i].type == other.data[i].type);
					switch(data[i].type) {
					case ValueType::Bool: {
						if (data[i].b < other.data[i].b) return true;
						if (data[i].b != other.data[i].b) return false;
						break;
					}
					case ValueType::Char: {
						if (data[i].c < other.data[i].c) return true;
						if (data[i].c != other.data[i].c) return false;
						break;
					}
					case ValueType::Integer: {
						if (data[i].i < other.data[i].i) return true;
						if (data[i].i != other.data[i].i) return false;
						break;
					}
					case ValueType::Float: {
						if (data[i].f < other.data[i].f) return true;
						if (data[i].f != other.data[i].f) return false;
						break;
					}
					case ValueType::Double: {
						if (data[i].d < other.data[i].d) return true;
						if (data[i].d != other.data[i].d) return false;
						break;
					}
					case ValueType::Composed: {
						if (*data[i].v < *other.data[i].v) return true;
						if (*data[i].v != *other.data[i].v) return false;
						break;
					}
					}
				}

				// they are equivalent
				return false;
			}

			friend std::ostream& operator<<(std::ostream& out, const adt_data& value) {
				if (value.size == 0) {
					return out << value.kind;
				}

				out << "(" << value.kind;
				for(int i=0; i<value.size; i++) {
					out << " " << value.data[i];
				}
				return out << ")";
			}
		};

		inline std::ostream& operator<<(std::ostream& out, const adt_value& value) {
			switch(value.type) {
			case ValueType::Bool: {
				return out << ( value.b ? "true" : "false" );
			}
			case ValueType::Char: {
				return out << value.c;
			}
			case ValueType::Integer: {
				return out << value.i;
			}
			case ValueType::Float: {
				return out << value.f;
			}
			case ValueType::Double: {
				return out << value.d;
			}
			case ValueType::Composed: {
				return value.v->printTo(out);
			}
			}
			return out << "-unknown-";
		}

		template<typename UserType>
		struct adt_value_encoder {
			adt_value pack(const UserType& cur) const {
				adt_value res;
				res.type = ValueType::Composed;
				res.v = new value_wrapper<UserType>(cur);
				return res;
			}

			UserType unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Composed);
				return static_cast<const value_wrapper<UserType>&>(*cur.v).value;
			}

		};

		template<>
		struct adt_value_encoder<bool> {
			adt_value pack(bool cur) const {
				adt_value res;
				res.type = ValueType::Bool;
				res.b = cur;
				return res;
			}

			bool unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Bool);
				return cur.b;
			}
		};

		template<>
		struct adt_value_encoder<char> {
			adt_value pack(char cur) const {
				adt_value res;
				res.type = ValueType::Char;
				res.c = cur;
				return res;
			}

			char unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Char);
				return cur.c;
			}
		};

		template<>
		struct adt_value_encoder<int> {
			adt_value pack(int cur) const {
				adt_value res;
				res.type = ValueType::Integer;
				res.i = cur;
				return res;
			}

			int unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Integer);
				return cur.i;
			}
		};

		template<>
		struct adt_value_encoder<float> {
			adt_value pack(float cur) const {
				adt_value res;
				res.type = ValueType::Float;
				res.f = cur;
				return res;
			}

			float unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Float);
				return cur.f;
			}
		};

		template<>
		struct adt_value_encoder<double> {
			adt_value pack(double cur) const {
				adt_value res;
				res.type = ValueType::Double;
				res.d = cur;
				return res;
			}

			double unpack(const adt_value& cur) const {
				assert(cur.type == ValueType::Double);
				return cur.d;
			}
		};

		template<std::size_t s>
		struct adt_value_encoder<const char[s]>
			: public adt_value_encoder<std::string> {};

		template<std::size_t s>
		struct adt_value_encoder<char[s]>
			: public adt_value_encoder<std::string> {};


		template<typename Lambda>
		struct result_type_of : public result_type_of<decltype(&Lambda::operator())> {};

		template<typename ... Params, typename R>
		struct result_type_of<R(Params...)> {
			using type = R;
		};

		template<typename C, typename ... Params, typename R>
		struct result_type_of<R(C::*)(Params...)> {
			using type = R;
		};

		template<typename C, typename ... Params, typename R>
		struct result_type_of<R(C::*)(Params...) const> {
			using type = R;
		};

	}

	template<
		char C, bool all, typename Op,
		typename ResType = typename detail::result_type_of<Op>::type
	>
	struct match_case {
		const Op& op;
		match_case(const Op& op) : op(op) {}

		template<typename ... Args>
		ResType operator()(const Args& ... args) const {
			return op(args...);
		}
	};

	template<
		char C, typename Op,
		typename ResType = typename detail::result_type_of<Op>::type
	>
	match_case<C,false,Op,ResType> on_case(const Op& op) {
		return match_case<C,false,Op,ResType>(op);
	}

	template<
		typename Op,
		typename ResType = typename detail::result_type_of<Op>::type
	>
	match_case<'*',true,Op,ResType> other(const Op& op) {
		return match_case<'*',true,Op,ResType>(op);
	}

	namespace detail {

		template<std::size_t i, typename Res, typename ... Fields>
		struct field_forwarder;

		template<std::size_t i, typename Res, typename First, typename ... Rest>
		struct field_forwarder<i,Res,First,Rest...> {
			template<std::size_t arity, typename Op, typename ... Args>
			Res operator()(const std::array<adt_value,arity>& values, const Op& op, const Args& ... args) {
				static_assert(i < arity, "To long field list!");
				return field_forwarder<i+1,Res,Rest...>()(values,op,args...,adt_value_encoder<First>().unpack(values[i]));
			}
		};

		template<std::size_t i, typename Res>
		struct field_forwarder<i,Res> {
			template<std::size_t arity, typename Op, typename ... Args>
			Res operator()(const std::array<adt_value,arity>&, const Op& op, const Args& ... args) {
				return op(args...);
			}
		};

		template<typename Res, typename MatchCase, typename ... Variants>
		struct variant_match;

		// the fall-through case
		template<typename Res, typename MatchCase, typename First, typename ... Rest>
		struct variant_match<Res,MatchCase,First,Rest...> {
			template<std::size_t arity>
			Res operator()(const std::array<adt_value,arity>& value, const MatchCase& cur) const {
				return variant_match<Res,MatchCase,Rest...>()(value, cur);
			}
		};

		// the default case
		template<typename Res, char C, typename Op, typename First, typename ... Rest>
		struct variant_match<Res,match_case<C,true,Op,Res>,First,Rest...> {
			template<std::size_t arity>
			Res operator()(const std::array<adt_value,arity>&, const match_case<C,true,Op,Res>& cur) const {
				return cur();		// -> no arguments passed to default case
			}
		};

		// the matching case
		template<typename Res, char C, typename Op, typename ... Fields, typename ... Rest>
		struct variant_match<Res,match_case<C,false,Op,Res>,Variant<C,Fields...>,Rest...> {
			template<std::size_t arity>
			Res operator()(const std::array<adt_value,arity>& values, const match_case<C,false,Op,Res>& cur) const {
				// unpack and forward values
				return field_forwarder<0,Res,Fields...>()(values, cur);
			}
		};


	}

	template<typename ... Variants>
	class ADT {

		template<typename T>
		friend struct detail::adt_value_encoder;

	public:

		enum { arity = detail::adt_arity<Variants...>::value };

	private:

		detail::adt_data<arity>* data;

	public:

		ADT(const ADT& other) : data(other.data) {
			if (data) data->incRefCounter();
		}

		ADT(ADT&& other) : data(other.data) {
			other.data = nullptr;
		}

		// destructor
		~ADT() {
			if (data) data->decRefCounter();
		}

		ADT& operator=(const ADT& other) {
			if (data == other.data) return *this;
			if (data) data->decRefCounter();
			data = other.data;
			if (data) data->incRefCounter();
			return *this;
		}

		ADT& operator=(ADT&& other) {
			if (data == other.data) return *this;
			if (data) data->decRefCounter();
			data = other.data;
			other.data = nullptr;
			return *this;
		}

		// --- inspection ---

		bool isa(char kind) const {
			return data && data->kind == kind;
		}

		// --- matching ---

		template<typename Res, char C, bool all, typename Op, typename ... Rest>
		Res match(const match_case<C,all,Op,Res>& first, const Rest& ... rest) const {
			if (all || data->kind == C) {
				return detail::variant_match<Res,match_case<C,all,Op,Res>,Variants...>()(data->data, first);
			}
			return match<Res>(rest...);
		}

		template<typename Res>
		Res match() const {
			throw std::logic_error("Unmatched variant!");
		}


		// --- comparison operators ---

		bool operator==(const ADT& other) const {
			return data == other.data || (*data == *other.data);
		}

		bool operator!=(const ADT& other) const {
			return !(*this == other);
		}

		bool operator<(const ADT& other) const {
			if (data == other.data) return false;
			return *data < *other.data;
		}

		bool operator<=(const ADT& other) const {
			return *this == other || *this < other;
		}

		bool operator>(const ADT& other) const {
			return !(*this <= other);
		}

		bool operator>=(const ADT& other) const {
			return !(*this < other);
		}

		// --- printing ---

		friend std::ostream& operator<<(std::ostream& out, const ADT& value) {
			return out << *value.data;
		}

	};

	template<typename ADT, typename ... Fields>
	ADT create(char kind, const Fields& ... fields) {

		#pragma GCC diagnostic push
		#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
		auto res = new detail::adt_data<ADT::arity>(
			kind, sizeof...(Fields),
			{
				{detail::adt_value_encoder<Fields>().pack(fields)...}
			}
		);
		#pragma GCC diagnostic pop

		return reinterpret_cast<ADT&>(res);
	}


} // end namespace utils
} // end namespace insieme

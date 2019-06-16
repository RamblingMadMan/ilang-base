#ifndef ILANG_EVAL_HPP
#define ILANG_EVAL_HPP 1

#include <exception>
#include <variant>

#include "AInt.hpp"
#include "ARatio.hpp"
#include "AReal.hpp"
#include "Expr.hpp"

#include "EvalDetail.hpp"

//! \file

namespace ilang{
	//! Error thrown by evaluation functions
	class EvalError: public std::exception{
		public:
			EvalError(std::string msg, ExprHandle expr_ = nullptr)
				: m_msg(std::move(msg)), m_expr(expr_){}
			
			const char *what() const noexcept override{ return m_msg.c_str(); }
			ExprHandle expr() const noexcept{ return m_expr; }
			
		private:
			std::string m_msg;
			ExprHandle m_expr;
	};
	
	struct Result;

	using ResultHandle = const Result*;
	using ResultPtr = std::unique_ptr<const Result>;
	
	//! Data required for evaluating expressions
	struct EvalData{
		TypeData typeData;
		
		using FunctionGen = std::function<ResultPtr(EvalData&, std::vector<ResultPtr>)>;
		std::map<std::string, std::map<TypeHandle, FunctionGen>> registeredFunctions;
		std::map<std::string, ResultPtr> boundNames;
	};

	//! Data type for the result of an evaluation
	struct Result{
		//! get the type of the result
		virtual TypeHandle resolveType(const TypeData &typeData) const noexcept = 0;
		
		//! get the result as a string
		virtual std::string toString() const = 0;

		/**
		 * \defgroup ResultOperators Perform calculations using evaluation results
		 * \{
		 **/
		virtual ResultPtr add(ResultHandle rhs) const;
		virtual ResultPtr sub(ResultHandle rhs) const;
		virtual ResultPtr mul(ResultHandle rhs) const;
		virtual ResultPtr div(ResultHandle rhs) const;
		/** \} */
		
		//! Call a callable object
		virtual ResultPtr call(EvalData &data, std::vector<ResultPtr>) const;
	};
	
	struct UnitResult: Result{
		TypeHandle resolveType(const TypeData &typeData) const noexcept override{
			return findUnitType(typeData);
		}
		
		std::string toString() const noexcept override{
			return "()";
		}
	};
	
	struct TypeResult: Result{
		TypeResult(TypeHandle value_): value(value_){}
		
		TypeHandle resolveType(const TypeData &typeData) const noexcept override{
			return findTypeType(typeData);
		}
		
		std::string toString() const noexcept override{
			return value->str;
		}
		
		TypeHandle value;
	};

	struct NumberResult: Result{
		using Value = std::variant<AInt, AReal, ARatio>;

		template<typename T>
		explicit NumberResult(T &&value_): value(std::forward<T>(value_)){}

		explicit NumberResult(float f): NumberResult(AReal(f)){}
		explicit NumberResult(double d): NumberResult(AReal(d)){}
		explicit NumberResult(std::int64_t i): NumberResult(AInt(i)){}
		
		TypeHandle resolveType(const TypeData &typeData) const noexcept override;
		
		std::string toString() const noexcept override{
			return std::visit([](auto &&v){ return v.toString(); }, value);
		}

		ResultPtr add(ResultHandle rhs) const override;
		ResultPtr sub(ResultHandle rhs) const override;
		ResultPtr mul(ResultHandle rhs) const override;
		ResultPtr div(ResultHandle rhs) const override;
		
		Value value;
	};
	
	struct RefResult: Result{
		explicit RefResult(ResultHandle other): value(other){}
		
		TypeHandle resolveType(const TypeData &typeData) const noexcept override{
			return value->resolveType(typeData);
		}
		
		std::string toString() const override{ return value->toString(); }
		
		ResultPtr add(ResultHandle rhs) const override{ return value->add(rhs); }
		ResultPtr sub(ResultHandle rhs) const override{ return value->sub(rhs); }
		ResultPtr mul(ResultHandle rhs) const override{ return value->mul(rhs); }
		ResultPtr div(ResultHandle rhs) const override{ return value->div(rhs); }
		
		ResultPtr call(EvalData &data, std::vector<ResultPtr> args) const override{ return value->call(data, std::move(args)); }
		
		ResultHandle value;
	};
	
	struct StringResult: Result{
		explicit StringResult(std::string value_): value(std::move(value_)){}
		
		TypeHandle resolveType(const TypeData &typeData) const noexcept override;
		
		std::string toString() const noexcept override{ return value; }

		ResultPtr add(ResultHandle rhs) const override;
		
		std::string value;
	};
	
	struct CallableResult: Result{
		using Fn = std::function<ResultPtr(EvalData&, std::vector<ResultPtr>)>;
		
		explicit CallableResult(Fn fn): value(fn){}
		
		TypeHandle resolveType(const TypeData &typeData) const noexcept override;
		
		std::string toString() const noexcept override;
		
		ResultPtr call(EvalData &data, std::vector<ResultPtr> args) const override;
		
		Fn value;
	};

	struct ObjectResult: Result{
		virtual std::size_t numMembers() const noexcept = 0;
		virtual ResultHandle *members() const noexcept = 0;
	};

	struct ListResult: Result{
		TypeHandle resolveType(const TypeData &typeData) const noexcept override;
		
		std::string toString() const noexcept override;
		
		std::vector<ResultPtr> values;
	};
	
	struct ProductResult: Result{
		TypeHandle resolveType(const TypeData & typeData) const noexcept override;
		
		std::string toString() const noexcept override;
		
		std::vector<ResultPtr> values;
	};

	//! Result of a call to eval
	struct EvalResult{
		ResultPtr result;
		ExprIterator rest, end;
	};

	std::pair<EvalResult, EvalData> eval(ExprIterator begin, ExprIterator end, EvalData data = EvalData{});
	
	inline auto eval(const std::vector<ExprHandle> &exprs, EvalData data = EvalData()){
		return eval(cbegin(exprs), cend(exprs), std::move(data));
	}

	inline auto eval(EvalResult remainder, EvalData data = EvalData{}){
		return eval(remainder.rest, remainder.end, std::move(data));
	}

	namespace detail{
		template<typename Value, typename Enable = void>
		struct ValueWrapper;
		
		struct Unit{};
		
		template<>
		struct ValueWrapper<void, void>{
			static auto get(ResultHandle res){
				auto unit = dynamic_cast<const UnitResult*>(res);
				if(!unit)
					throw EvalError("invalid result for value type");
				
				return Unit{};
			}
			
			static ResultPtr wrap(void){
				return std::make_unique<UnitResult>();
			}
		};
		
		template<typename Int>
		struct ValueWrapper<Int, std::enable_if_t<std::is_integral_v<Int>>>{
			static auto get(ResultHandle res){
				auto num = dynamic_cast<const NumberResult*>(res);
				if(!num)
					throw EvalError("invalid result for value type");
				
				return detail::CppTypeGetter<Int>::getValue(num->value);
			}
			
			static ResultPtr wrap(Int i){
				return std::make_unique<NumberResult>(i);
			}
		};
		
		template<typename Real>
		struct ValueWrapper<Real, std::enable_if_t<std::is_floating_point_v<Real>>>{
			static auto get(ResultHandle res){
				auto num = dynamic_cast<const NumberResult*>(res);
				if(!num)
					throw EvalError("invalid result for value type");
				
				return detail::CppTypeGetter<Real>::getValue(num->value);
			}
			
			static ResultPtr wrap(Real r){
				return std::make_unique<NumberResult>(r);
			}
		};
		
		template<>
		struct ValueWrapper<std::string, void>{
			static auto get(ResultHandle res){
				auto str = dynamic_cast<const StringResult*>(res);
				if(!str)
					throw EvalError("invalid result for value type");
				
				return str->value;
			}
			
			static ResultPtr wrap(std::string s){
				return std::make_unique<StringResult>(std::move(s));
			}
		};
		
		template<typename...>
		struct FnWrapper;
		
		template<typename Ret, typename ... Params>
		struct FnWrapper<Ret(Params...)>{
			enum {
				numParams = sizeof...(Params)
			};
			
			template<std::size_t Idx>
			static void setParam(std::tuple<Params...> &tup, std::vector<ResultPtr> &args){
				auto &&param = std::get<Idx>(tup);
				using ParamType = std::decay_t<decltype(param)>;
				param = ValueWrapper<ParamType>::get(args[Idx].get());
			}
			
			template<std::size_t ... Indices>
			static void setParamsImpl(std::tuple<Params...> &tup, std::vector<ResultPtr> &args, std::index_sequence<Indices...>){
				(setParam<Indices>(tup, args), ...);
			}
			
			static void setParams(std::tuple<Params...> &tup, std::vector<ResultPtr> &args){
				setParamsImpl(tup, args, std::make_index_sequence<numParams>{});
			}
			
			static auto wrap(TypeData &data, std::function<Ret(Params...)> fn) -> std::function<ResultPtr(EvalData&, std::vector<ResultPtr>)>{
				auto retType = CppTypeGetter<Ret>::get(data);
				auto paramTypes = std::vector{CppTypeGetter<Params>::get()...};
				
				auto wrapped = [fn{std::move(fn)}, retType, paramTypes{std::move(paramTypes)}](EvalData &data, std::vector<ResultPtr> args){
					std::tuple<Params...> paramValues;
					
					if(args.size() != numParams)
						throw EvalError("wrong number of arguments");
					
					setParams(paramValues, args);
					
					return ValueWrapper<Ret>::wrap(std::apply(fn, paramValues));
				};
				
				return wrapped;
			}
		};
		
		template<>
		struct FnWrapper<void()>{
			static auto wrap(TypeData &data, std::function<void()> fn) -> std::function<ResultPtr(EvalData&, std::vector<ResultPtr>)>{
				auto retType = findUnitType(data);
				
				auto wrapped = [fn{std::move(fn)}, retType](EvalData &data, std::vector<ResultPtr> args){					
					if(args.size() != 0)
						throw EvalError("wrong number of arguments");
					
					fn();
					
					return std::make_unique<UnitResult>();
				};
				
				return wrapped;
			}
		};
	}
	
	template<typename FnType>
	inline std::pair<ResultPtr, EvalData> registerEvalFn(const std::string &name, std::function<FnType> fn, EvalData data){
		auto fnType = detail::CppTypeGetter<FnType>::get(data.typeData);
		auto wrappedFn = detail::FnWrapper<FnType>::wrap(data.typeData, std::move(fn));
		
		auto &&functionMap = data.registeredFunctions[name];
		
		functionMap[fnType] = std::move(wrappedFn);
		return {std::make_unique<CallableResult>(functionMap[fnType]), std::move(data)};
	}
	
	template<typename T>
	inline std::pair<ResultHandle, EvalData> bindEvalName(const std::string &name, T value, EvalData data){
		auto wrapped = detail::ValueWrapper<T>::wrap(value);
		auto ptr = wrapped.get();
		data.boundNames[name] = std::move(wrapped);
		return {ptr, std::move(data)};
	}
}

#endif // !ILANG_EVAL_HPP 

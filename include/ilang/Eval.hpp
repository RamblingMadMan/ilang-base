#ifndef ILANG_EVAL_HPP
#define ILANG_EVAL_HPP 1

#include <exception>
#include <variant>

#include "AInt.hpp"
#include "ARatio.hpp"
#include "AReal.hpp"
#include "Expr.hpp"

#include "Result.hpp"
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

	//! Data required for evaluating expressions
	struct EvalData{
		TypeData typeData;
		
		ResultScope root;
		//using FunctionGen = std::function<ResultPtr(EvalData&, std::vector<ResultPtr>)>;
		std::map<std::string, ResultPtr> registeredFunctions;
		std::map<std::string, ResultPtr> boundNames;
		std::map<const Exprs::FnDecl*, ResultPtr> fns;
	};

	//! Result of a call to eval
	struct EvalResult{
		ResultPtr result;
		ExprIterator nextIt, endIt;
	};

	EvalResult eval(ExprIterator begin, ExprIterator end, EvalData &data);
	
	inline auto eval(const std::vector<ExprHandle> &exprs, EvalData &data){
		return eval(cbegin(exprs), cend(exprs), data);
	}

	inline auto eval(EvalResult remainder, EvalData &data){
		return eval(remainder.nextIt, remainder.endIt, data);
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
					if(args.size() != 1)
						throw EvalError("wrong number of arguments");
					else if(!dynamic_cast<const UnitResult*>(args[0].get()))
						throw EvalError("expected single unit argument");
					
					fn();
					
					return std::make_unique<UnitResult>();
				};
				
				return wrapped;
			}
		};
	}
	
	template<typename FnType>
	inline ResultPtr registerEvalFn(const std::string &name, std::function<FnType> fn, EvalData &data){
		auto fnType = detail::CppTypeGetter<FnType>::get(data.typeData);
		auto wrappedFn = detail::FnWrapper<FnType>::wrap(data.typeData, std::move(fn));
		
		data.registeredFunctions[name] = std::make_unique<CallableResult>(std::move(wrappedFn));
		
		return std::make_unique<RefResult>(data.registeredFunctions[name].get());
	}
	
	template<typename T>
	inline ResultPtr bindEvalName(const std::string &name, T value, EvalData &data){
		auto wrapped = detail::ValueWrapper<T>::wrap(value);
		auto ptr = wrapped.get();
		data.boundNames[name] = std::move(wrapped);
		return std::make_unique<RefResult>(ptr);
	}
}

#endif // !ILANG_EVAL_HPP 

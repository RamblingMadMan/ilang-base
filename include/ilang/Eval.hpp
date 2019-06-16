#ifndef ILANG_EVAL_HPP
#define ILANG_EVAL_HPP 1

#include <exception>
#include <variant>

#include "gsl/span"

#include "ilang/Expr.hpp"
#include "ilang/AInt.hpp"
#include "ilang/ARatio.hpp"
#include "ilang/AReal.hpp"

//! \file

namespace ilang{
	//! Error thrown by evaluation functions
	class EvalError: public std::exception{
		public:
			EvalError(std::string msg, ExprHandle expr_)
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

	//! Data type for the result of an evaluation
	struct Result{
		//! get the evaluated expression
		virtual ExprHandle expr() const noexcept = 0;

		//! get the type of the result
		virtual TypeHandle type() const noexcept = 0;
		
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
	};

	struct NumberResult: Result{
		using Value = std::variant<AInt, AReal, ARatio>;

		template<typename T>
		explicit NumberResult(T &&value_, ExprHandle expr_): value(std::forward<T>(value_)), exprHandle(expr_){}

		explicit NumberResult(float f, ExprHandle expr_): NumberResult(AReal(f), expr_){}
		explicit NumberResult(double d, ExprHandle expr_): NumberResult(AReal(d), expr_){}
		explicit NumberResult(std::int64_t i, ExprHandle expr_): NumberResult(AInt(i), expr_){}
		
		ExprHandle expr() const noexcept override{ return exprHandle; }
		TypeHandle type() const noexcept override{ return exprHandle->type(); }
		
		std::string toString() const noexcept override{
			auto valueStr = std::visit([](auto &&v){ return v.toString(); }, value);
			return valueStr + " : " + type()->str;
		}

		ResultPtr add(ResultHandle rhs) const override;
		ResultPtr sub(ResultHandle rhs) const override;
		ResultPtr mul(ResultHandle rhs) const override;
		ResultPtr div(ResultHandle rhs) const override;
		
		Value value;
		ExprHandle exprHandle;
	};
	
	struct StringResult: Result{
		explicit StringResult(std::string value_): value(std::move(value_)){}
		
		ExprHandle expr() const noexcept override{ return exprHandle; }
		TypeHandle type() const noexcept override{ return exprHandle->type(); }
		
		std::string toString() const noexcept override{ return value; }

		ResultPtr add(ResultHandle rhs) const override;
		
		std::string value;
		ExprHandle exprHandle;
	};

	struct ObjectResult: Result{
		virtual std::size_t numMembers() const noexcept = 0;
		virtual ResultHandle *members() const noexcept = 0;
	};

	struct ProductResult: Result{
		virtual std::size_t numMembers() const noexcept = 0;
		virtual ResultHandle *members() const noexcept = 0;
	};

	//! Data required for evaluating expressions
	struct EvalData{
		TypeData typeData;
		//std::map<ExprHandle, ResultHandle> memoized;
	};

	//! Result of a call to eval
	struct EvalResult{
		ResultPtr result;
		gsl::span<ExprHandle> rest;
	};

	std::pair<EvalResult, EvalData> eval(gsl::span<ExprHandle> exprs, EvalData data = EvalData{});

	inline auto eval(EvalResult remainder, EvalData data = EvalData{}){
		return eval(remainder.rest, std::move(data));
	}
}

#endif // !ILANG_EVAL_HPP 

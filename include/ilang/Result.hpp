#ifndef ILANG_RESULT_HPP
#define ILANG_RESULT_HPP

#include <memory>
#include <variant>
#include <functional>

#include "AReal.hpp"
#include "Expr.hpp"
#include "Type.hpp"

namespace ilang{
	class ResultError: public std::exception{
		public:
			ResultError(std::string msg): m_msg(std::move(msg)){}

			const char *what() const noexcept override{ return m_msg.c_str(); }

		private:
			std::string m_msg;
	};

	struct EvalData;
	struct Result;
	using ResultHandle = const Result*;
	using ResultPtr = std::unique_ptr<const Result>;

	struct ResultScope{
		explicit ResultScope(const ResultScope *parent_ = nullptr): parent(parent_){}

		ResultPtr resolveName(const std::string &name) const;

		const ResultScope *parent;
		std::map<std::string, ResultPtr> registeredFunctions;
		std::map<std::string, ResultPtr> boundNames;
		std::map<const Exprs::FnDecl*, ResultPtr> fns;
	};

	//! Data type for the result of an evaluation
	struct Result{
		virtual ~Result() = default;

		//! get the type of the result
		virtual TypeHandle resolveType(TypeData &typeData) const noexcept = 0;

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
		virtual ResultPtr call(TypeData &typeData, std::vector<ResultPtr>) const;
	};

	struct UnitResult: Result{
		TypeHandle resolveType(TypeData &typeData) const noexcept override{
			return findUnitType(typeData);
		}

		std::string toString() const noexcept override{
			return "()";
		}
	};

	struct TypeResult: Result{
		TypeResult(TypeHandle value_): value(value_){}

		TypeHandle resolveType(TypeData &typeData) const noexcept override{
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

		TypeHandle resolveType(TypeData &typeData) const noexcept override;

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

		TypeHandle resolveType(TypeData &typeData) const noexcept override{
			return value->resolveType(typeData);
		}

		std::string toString() const override{ return value->toString(); }

		ResultPtr add(ResultHandle rhs) const override{ return value->add(rhs); }
		ResultPtr sub(ResultHandle rhs) const override{ return value->sub(rhs); }
		ResultPtr mul(ResultHandle rhs) const override{ return value->mul(rhs); }
		ResultPtr div(ResultHandle rhs) const override{ return value->div(rhs); }

		ResultPtr call(TypeData &data, std::vector<ResultPtr> args) const override{ return value->call(data, std::move(args)); }

		ResultHandle value;
	};

	struct StringResult: Result{
		explicit StringResult(std::string value_): value(std::move(value_)){}

		TypeHandle resolveType(TypeData &typeData) const noexcept override;

		std::string toString() const noexcept override{ return value; }

		ResultPtr add(ResultHandle rhs) const override;

		std::string value;
	};

	struct CallableResult: Result{
		using CallbackFn = std::function<ResultPtr(TypeData&, std::vector<ResultPtr>)>;

		explicit CallableResult(CallbackFn fn): value(fn){}

		TypeHandle resolveType(TypeData &typeData) const noexcept override;

		std::string toString() const noexcept override;

		ResultPtr call(TypeData &data, std::vector<ResultPtr> args) const override{
			return value(data, std::move(args));
		}

		CallbackFn value;
		TypeHandle fnType;
	};

	struct ObjectResult: Result{
		virtual std::size_t numMembers() const noexcept = 0;
		virtual ResultHandle *members() const noexcept = 0;
	};

	struct ListResult: Result{
		TypeHandle resolveType(TypeData &typeData) const noexcept override;

		std::string toString() const noexcept override;

		std::vector<ResultPtr> values;
	};

	struct ProductResult: Result{
		TypeHandle resolveType(TypeData & typeData) const noexcept override;

		std::string toString() const noexcept override;

		std::vector<ResultPtr> values;
	};
}

#endif // ILANG_RESULT_HPP

#include <functional>
#include <string_view>

#include "ilang/Eval.hpp"

using namespace std::string_view_literals;
using namespace ilang;

template<typename ... Ts>
struct Overloaded: Ts...{ using Ts::operator()...; };

template<typename ... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template<typename T, std::size_t N, typename Fn>
auto map(Fn &&fn, gsl::span<T, N> vals){
	std::array<T, N> results;
	for(std::size_t i = 0; i < N; i++)
		results[i] = std::forward<Fn>(fn)(vals[i]);
	return results;
}

template<typename T, typename Fn>
auto map(Fn &&fn, gsl::span<T, -1> vals){
	std::vector<T> results;
	results.reserve(vals.size());
	
	for(auto &&v : vals)
		results.emplace_back(std::forward<Fn>(fn)(v));
	
	return results;
}

ResultPtr Result::add(ResultHandle rhs) const{
	throw EvalError("Addition not implemented for result of type '" + type()->str + "'", expr());
	return nullptr;
}

ResultPtr Result::sub(ResultHandle rhs) const{
	throw EvalError("Subtraction not implemented for result of type '" + type()->str + "'", expr());
	return nullptr;
}

ResultPtr Result::mul(ResultHandle rhs) const{
	throw EvalError("Multiplication not implemented for result of type '" + type()->str + "'", expr());
	return nullptr;
}

ResultPtr Result::div(ResultHandle rhs) const{
	throw EvalError("Division not implemented for result of type '" + type()->str + "'", expr());
	return nullptr;
}

auto numberAddFn(){ return [](auto a, auto b){ return a + b; }; }
auto numberSubFn(){ return [](auto a, auto b){ return a - b; }; }
auto numberMulFn(){ return [](auto a, auto b){ return a * b; }; }

auto numberDivFn(){
	auto divGenFn = [](auto a, auto b){ return a / b; };
	auto divIntFn = [](const AInt &a, const AInt &b){ return ARatio(a, b); };

	auto divFn = Overloaded{
		divIntFn,
		divGenFn
	};

	return [divFn](auto a, auto b){ return divFn(a, b); };
}

using NumberValue = typename NumberResult::Value;

template<typename Fn>
auto doNumberBinop(const NumberValue &lhs, const NumberValue &rhs, Fn fn){
	return std::visit(
		Overloaded{
			[&fn, &rhs](const AReal &aReal){
				return std::visit(
					Overloaded{
						[&fn, &aReal](const AReal &other){ return fn(aReal, other); },
						[&fn, &aReal](const auto &other){ return fn(aReal, AReal(other)); }
					},
					rhs
				);
			},
			[&fn, &rhs](const ARatio &aRatio){
				return std::visit(
					Overloaded{
						[&fn, &aRatio](const AReal &other){ return fn(AReal(aRatio), other); },
						[&fn, &aRatio](const ARatio &other){ return fn(aRatio, other); },
						[&fn, &aRatio](const AInt &other){ return fn(aRatio, ARatio(other)); }
					},
					rhs
				);
			},
			[&fn, &rhs](const AInt &aInt){
				return std::visit(
					Overloaded{
						[&fn, &aInt](const AReal &other){ return fn(AReal(aInt), other); },
						[&fn, &aInt](const ARatio &other){ return fn(ARatio(aInt), other); },
						[&fn, &aInt](const AInt &other){ return fn(aInt, other); }
					},
					rhs
				);
			},
		},
		lhs
	);
}

template<typename Fn>
ResultPtr doNumOp(const NumberValue &lhs, const NumberValue &rhs, Fn op){
	auto packed = [op](auto a, auto b){
		auto res = op(a, b);
		return std::make_unique<NumberResult>(res);
	};

	return doNumberBinop(lhs, rhs, packed);
}

auto getNumberBinopFn(std::string_view op) -> std::function<ResultPtr(const NumberResult*, const NumberResult*)>{
	auto pack = [](auto fn){
		return [fn](const NumberResult *lhs, const NumberResult *rhs){
			return doNumberBinop(lhs->value, rhs->value, [fn](auto a, auto b){
				auto res = fn(a, b);
				return std::make_unique<const NumberResult>(res);
			});
		};
	};

	if(op == "+")      return pack(numberAddFn());
	else if(op == "-") return pack(numberSubFn());
	else if(op == "*") return pack(numberMulFn());
	else if(op == "/") return pack(numberDivFn());
	else throw EvalError("Unexpected operator", nullptr);
}

auto getBinopFn(std::string_view op) -> std::function<ResultPtr(ResultHandle, ResultHandle)>{
	auto numBinopFn = getNumberBinopFn(op);

	return [numBinopFn](ResultHandle lhs, ResultHandle rhs){
		auto lhsNum = dynamic_cast<const NumberResult*>(lhs);
		auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
		if(lhsNum && rhsNum) return numBinopFn(lhsNum, rhsNum);

		/*
		auto lhsStr = dynamic_cast<const StringResult*>(lhs);
		auto rhsStr = dynamic_cast<const StringResult*>(rhs);
		if(lhsStr && rhsStr) return strBinopFn(lhsStr, rhsStr);
		*/
		throw EvalError(
			"Binary operators not implemented for type '" + lhs->type()->str + "'",
			nullptr
		);
	};
}

ResultPtr NumberResult::add(ResultHandle rhs) const{
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Addition not implemented between types '" + type()->str + "' and '" + rhs->type()->str + "'",
			nullptr
		);

	return doNumOp(value, rhsNum->value, numberAddFn());
}

ResultPtr NumberResult::sub(ResultHandle rhs) const{
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Subtraction not implemented between types '" + type()->str + "' and '" + rhs->type()->str + "'",
			nullptr
		);

	return doNumOp(value, rhsNum->value, numberSubFn());
}

ResultPtr NumberResult::mul(ResultHandle rhs) const{
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Multiplication not implemented between types '" + type()->str + "' and '" + rhs->type()->str + "'",
			nullptr
		);

	return doNumOp(value, rhsNum->value, numberMulFn());
}

ResultPtr NumberResult::div(ResultHandle rhs) const{
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Division not implemented between types '" + type()->str + "' and '" + rhs->type()->str + "'",
			nullptr
		);

	return doNumOp(value, rhsNum->value, numberDivFn());
}

ResultPtr StringResult::add(ResultHandle rhs) const{
	auto rhsStr = dynamic_cast<const StringResult*>(rhs);
	if(!rhsStr)
		throw EvalError(
			"Addition not implemented between types '" + type()->str + "' and '" + rhs->type()->str + "'",
			nullptr
		);

	return std::make_unique<StringResult>(value + rhsStr->value);
}

std::pair<EvalResult, EvalData> eval_result(ResultPtr &result, EvalData &data){
	return {EvalResult{std::move(result)}, std::move(data)};
}

ResultPtr evalExpr(ExprHandle expr, EvalData &data);

ResultPtr evalLiteral(const Exprs::Literal *lit, EvalData &data){
	if(isNaturalType(lit->type(), data.typeData) || isIntegerType(lit->type(), data.typeData))
		return std::make_unique<NumberResult>(AInt(lit->toString()), lit);
	else if(isRationalType(lit->type(), data.typeData))
		return std::make_unique<NumberResult>(ARatio(lit->toString()), lit);
	else if(isRealType(lit->type(), data.typeData))
		return std::make_unique<NumberResult>(AReal(lit->toString()), lit);
	else if(isStringType(lit->type(), data.typeData))
		return std::make_unique<StringResult>(lit->toString());
	else
		throw EvalError("Unknown literal type", lit);
}

ResultPtr evalBinop(const Exprs::BinOp *binop, EvalData &data){
	ResultHandle res;

	auto lhsRes = evalExpr(binop->lhs, data);
	auto rhsRes = evalExpr(binop->rhs, data);
	auto binopFn = getBinopFn(binop->op);

	return binopFn(lhsRes.get(), rhsRes.get());
}

ResultPtr evalExpr(ExprHandle expr, EvalData &data){
	if(auto lit = dynamic_cast<const Exprs::Literal*>(expr))
		return evalLiteral(lit, data);
	else
		throw EvalError("Unexpected expression", expr);
}

std::pair<EvalResult, EvalData> ilang::eval(gsl::span<ExprHandle> exprs, EvalData data){
	EvalResult res;

	res.rest = exprs.subspan(1);
	res.result = evalExpr(exprs[0], data);

	return std::make_pair(std::move(res), std::move(data));
}

#include <functional>
#include <string_view>

#include "ilang/Eval.hpp"

using namespace std::string_view_literals;
using namespace ilang;

ResultPtr Result::add(ResultHandle rhs) const{
	throw EvalError("Addition not implemented for result type", nullptr);
	return nullptr;
}

ResultPtr Result::sub(ResultHandle rhs) const{
	throw EvalError("Subtraction not implemented for result type", nullptr);
	return nullptr;
}

ResultPtr Result::mul(ResultHandle rhs) const{
	throw EvalError("Multiplication not implemented for result type", nullptr);
	return nullptr;
}

ResultPtr Result::div(ResultHandle rhs) const{
	throw EvalError("Division not implemented for result type", nullptr);
	return nullptr;
}

ResultPtr Result::call(EvalData&, std::vector<ResultPtr> args) const{
	throw EvalError("Result is not callable");
	return nullptr;
}

TypeHandle CallableResult::resolveType(const TypeData &typeData) const noexcept{
	std::fprintf(stderr, "warning: finding the type of callable results is not currently supported\n");
	return nullptr;
}

std::string CallableResult::toString() const noexcept{
	return "!CALLABLE!";
}

ResultPtr CallableResult::call(EvalData& data, std::vector<ResultPtr> args) const{
	return value(data, std::move(args));
}

std::string ListResult::toString() const noexcept{
	std::string res = "[";
	
	if(!values.empty()){
		res += values[0]->toString();
		for(std::size_t i = 1; i < values.size(); i++)
			res += ", " + values[i]->toString();
	}
	
	res += "]";
	return res;
}

std::string ProductResult::toString() const noexcept{
	std::string res = "(" + values[0]->toString();
	for(std::size_t i = 1; i < values.size(); i++)
		res += ", " + values[i]->toString();
	
	res += ")";
	return res;
}

TypeHandle ListResult::resolveType(const TypeData &typeData) const noexcept{
	std::vector<TypeHandle> innerTypes;
	innerTypes.resize(values.size());
	
	std::transform(
		begin(values), end(values),
		begin(innerTypes),
		[&typeData](auto &&res){ return res->resolveType(typeData); }
	);
	
	if(innerTypes.empty())
		return findStaticArrayType(typeData, findUnitType(typeData), 0);
	
	auto commonType = innerTypes[0];
	for(std::size_t i = 1; i < innerTypes.size(); i++)
		commonType = findCommonType(commonType, innerTypes[i]);
	
	return findStaticArrayType(typeData, commonType, values.size());
}

TypeHandle ProductResult::resolveType(const TypeData &typeData) const noexcept{
	std::vector<TypeHandle> innerTypes;
	innerTypes.resize(values.size());
	
	std::transform(
		begin(values), end(values),
		begin(innerTypes),
		[&typeData](auto &&res){ return res->resolveType(typeData); }
	);
	
	return findProductType(typeData, innerTypes);
}

auto numberAddFn(){ return [](auto a, auto b){ return a + b; }; }
auto numberSubFn(){ return [](auto a, auto b){ return a - b; }; }
auto numberMulFn(){ return [](auto a, auto b){ return a * b; }; }
auto numberDivFn(){ return [](auto a, auto b){ return a / b; }; }

auto numberPowFn(){
	auto powGenFn = [](auto a, auto b){ return a.pow(b); };
	
	auto powIntRealFn = [](const AInt &a, const AReal &b){
		AReal realA(a);
		return realA.pow(b);
	};
	
	auto powIntRatioFn = [](const AInt &a, const ARatio &b){
		AReal realA(a);
		return realA.pow(b);
	};
	
	auto powRatioRatioFn = [](const ARatio &a, const ARatio &b){
		AReal realA(a);
		return realA.pow(b);
	};
	
	auto powRatioRealFn = [](const ARatio &a, const AReal &b){
		AReal realA(a);
		return realA.pow(b);
	};
	
	return detail::Overloaded{
		powIntRealFn,
		powIntRatioFn,
		powRatioRatioFn,
		powRatioRealFn,
		powGenFn
	};
}

using NumberValue = typename NumberResult::Value;

template<typename Fn>
auto doNumberBinop(const NumberValue &lhs, const NumberValue &rhs, Fn fn){
	return std::visit(
		detail::Overloaded{
			[&fn, &rhs](const AReal &aReal){
				return std::visit(
					detail::Overloaded{
						[&fn, &aReal](const AReal &other){ return fn(aReal, other); },
						[&fn, &aReal](const auto &other){ return fn(aReal, AReal(other)); }
					},
					rhs
				);
			},
			[&fn, &rhs](const ARatio &aRatio){
				return std::visit(
					detail::Overloaded{
						[&fn, &aRatio](const AReal &other){ return fn(AReal(aRatio), other); },
						[&fn, &aRatio](const ARatio &other){
							if(other.denominator() == AInt(1L)){
								if(aRatio.denominator() == AInt(1L))
									return fn(aRatio.numerator(), other.numerator());
								else
									return fn(aRatio, other.numerator());
							}
							else
								return fn(aRatio, other);
						},
						[&fn, &aRatio](const AInt &other){ return fn(aRatio, other); }
					},
					rhs
				);
			},
			[&fn, &rhs](const AInt &aInt){
				return std::visit(
					detail::Overloaded{
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
ResultPtr doNumOp(const NumberResult *lhs, const NumberResult *rhs, Fn op){
	auto packed = [op](auto a, auto b){
		auto res = op(a, b);
		
		if constexpr(std::is_same_v<ARatio, std::decay_t<decltype(res)>>){
			if(res.denominator() == AInt(1L))
				return std::make_unique<const NumberResult>(res.numerator());
		}
		
		return std::make_unique<const NumberResult>(res);
	};

	return doNumberBinop(lhs->value, rhs->value, packed);
}

auto getNumberBinopFn(std::string_view op) -> std::function<ResultPtr(const NumberResult*, const NumberResult*)>{
	auto pack = [](auto fn){
		return [fn](const NumberResult *lhs, const NumberResult *rhs){
			return doNumberBinop(lhs->value, rhs->value, [fn](auto a, auto b){
				auto res = fn(a, b);
				if constexpr(std::is_same_v<ARatio, std::decay_t<decltype(res)>>){
					if(res.denominator() == AInt(1L))
						return std::make_unique<const NumberResult>(res.numerator());
				}
				
				return std::make_unique<const NumberResult>(res);
			});
		};
	};

	if(op == "+")      return pack(numberAddFn());
	else if(op == "-") return pack(numberSubFn());
	else if(op == "*") return pack(numberMulFn());
	else if(op == "/") return pack(numberDivFn());
	else if(op == "^") return pack(numberPowFn());
	else return [](auto, auto){ throw EvalError("Unsupported number operator"); return nullptr; };
}

auto getStrBinopFn(std::string_view op) -> std::function<ResultPtr(const StringResult*, const StringResult*)>{
	if(op == "+") return [](const StringResult *lhs, const StringResult *rhs){
		return std::make_unique<StringResult>(lhs->value + rhs->value);
	};
	else return [](auto, auto){
		throw EvalError("Unsupported string operator");
		return nullptr;
	};
};

auto getBinopFn(std::string_view op) -> std::function<ResultPtr(ResultHandle, ResultHandle)>{
	auto numBinopFn = getNumberBinopFn(op);
	auto strBinopFn = getStrBinopFn(op);

	return [numBinopFn, strBinopFn](ResultHandle lhs, ResultHandle rhs){
		while(auto lhsRef = dynamic_cast<const RefResult*>(lhs))
			lhs = lhsRef->value;
		
		while(auto rhsRef = dynamic_cast<const RefResult*>(rhs))
			rhs = rhsRef->value;
		
		auto lhsNum = dynamic_cast<const NumberResult*>(lhs);
		auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
		if(lhsNum && rhsNum) return numBinopFn(lhsNum, rhsNum);

		auto lhsStr = dynamic_cast<const StringResult*>(lhs);
		auto rhsStr = dynamic_cast<const StringResult*>(rhs);
		if(lhsStr && rhsStr) return strBinopFn(lhsStr, rhsStr);
		
		throw EvalError(
			"Binary operators not implemented for result types",
			nullptr
		);
	};
}

TypeHandle NumberResult::resolveType(const TypeData &data) const noexcept{
	return std::visit(
		detail::Overloaded{
			[&data](const AInt&){ return findIntegerType(data); },
			[&data](const ARatio&){ return findRationalType(data); },
			[&data](const AReal&){ return findRealType(data); }
		},
		value
	);
}

TypeHandle StringResult::resolveType(const TypeData &data) const noexcept{
	return findStringType(data);
}

ResultPtr NumberResult::add(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;
	
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Addition not implemented between types result types",
			nullptr
		);

	return doNumOp(this, rhsNum, numberAddFn());
}

ResultPtr NumberResult::sub(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;
	
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Subtraction not implemented between result types",
			nullptr
		);

	return doNumOp(this, rhsNum, numberSubFn());
}

ResultPtr NumberResult::mul(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;
	
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Multiplication not implemented between result types",
			nullptr
		);

	return doNumOp(this, rhsNum, numberMulFn());
}

ResultPtr NumberResult::div(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;
	
	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw EvalError(
			"Division not implemented between result types",
			nullptr
		);

	return doNumOp(this, rhsNum, numberDivFn());
}

ResultPtr StringResult::add(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;
	
	auto rhsStr = dynamic_cast<const StringResult*>(rhs);
	if(!rhsStr)
		throw EvalError(
			"Addition not implemented between result types",
			nullptr
		);

	return std::make_unique<StringResult>(value + rhsStr->value);
}

std::pair<EvalResult, EvalData> eval_result(ResultPtr &result, EvalData &data){
	return {EvalResult{std::move(result)}, std::move(data)};
}

ResultPtr evalExpr(ExprHandle expr, EvalData &data);

ResultPtr evalListLiteral(const Exprs::ListLiteral *list, EvalData &data){
	std::vector<ResultPtr> results;
	results.reserve(list->elements.size());
	for(auto &&expr : list->elements)
		results.emplace_back(evalExpr(expr.get(), data));
	
	auto result = std::make_unique<ListResult>();
	result->values = std::move(results);
	
	return result;
}

ResultPtr evalProductLiteral(const Exprs::ProductLiteral *product, EvalData &data){
	std::vector<ResultPtr> results;
	results.reserve(product->elements.size());
	for(auto &&expr : product->elements)
		results.emplace_back(evalExpr(expr.get(), data));
	
	if(results.size() == 1)
		return std::move(results[0]);
	
	auto result = std::make_unique<ProductResult>();
	result->values = std::move(results);
	
	return result;
}

ResultPtr evalLiteral(const Exprs::Literal *lit, EvalData &data){
	auto type = lit->typeExpr()->resolve(data.typeData, [](TypeData&, TypeHandle partial){ throw EvalError("Partial typing currently unimplemented"); return partial; });
	
	if(auto list = dynamic_cast<const Exprs::ListLiteral*>(lit))
		return evalListLiteral(list, data);
	else if(auto product = dynamic_cast<const Exprs::ProductLiteral*>(lit))
		return evalProductLiteral(product, data);
	else if(isUnitType(type, data.typeData))
		return std::make_unique<UnitResult>();
	else if(auto typeLit = dynamic_cast<const Exprs::TypeLiteral*>(lit))
		return std::make_unique<TypeResult>(typeLit->value);
	else if(isNaturalType(type, data.typeData) || isIntegerType(type, data.typeData))
		return std::make_unique<NumberResult>(AInt(lit->toString()));
	else if(isRationalType(type, data.typeData))
		return std::make_unique<NumberResult>(ARatio(lit->toString()));
	else if(isRealType(type, data.typeData))
		return std::make_unique<NumberResult>(AReal(lit->toString()));
	else if(isStringType(type, data.typeData)){
		auto str = lit->toString();
		return std::make_unique<StringResult>(str.substr(1, str.size()-2));
	}
	else
		throw EvalError("Unknown literal type", lit);
}

ResultPtr evalBinop(const Exprs::BinOp *binop, EvalData &data){
	ResultHandle res;
	
	auto lstr = binop->lhs->toString();
	auto rstr = binop->rhs->toString();
	auto opstr = binop->op;

	auto lhsRes = evalExpr(binop->lhs.get(), data);
	auto rhsRes = evalExpr(binop->rhs.get(), data);
	auto binopFn = getBinopFn(binop->op);

	return binopFn(lhsRes.get(), rhsRes.get());
}

ResultPtr evalRef(const Exprs::Ref *ref, EvalData &data){
	if(auto unresolved = dynamic_cast<const Exprs::UnresolvedRef*>(ref)){
		auto boundRes = data.boundNames.find(unresolved->name);
		if(boundRes != end(data.boundNames))
			return std::make_unique<RefResult>(boundRes->second.get());
		
		throw EvalError("Could not resolve reference", ref);
	}
	else
		throw EvalError("internal error: unrecognized reference type", ref);
}

ResultPtr evalTypeExpr(TypeExprHandle expr, EvalData &data){
	auto type = expr->resolve(
		data.typeData, 
		[](TypeData&, TypeHandle partial){
			throw EvalError("Partial typing currently unimplemented");
			return partial;
		}
	);
	
	return std::make_unique<TypeResult>(type);
}

ResultPtr evalExpr(ExprHandle expr, EvalData &data){
	if(auto type = dynamic_cast<TypeExprHandle>(expr))
		return evalTypeExpr(type, data);
	else if(auto lit = dynamic_cast<const Exprs::Literal*>(expr))
		return evalLiteral(lit, data);
	else if(auto binop = dynamic_cast<const Exprs::BinOp*>(expr))
		return evalBinop(binop, data);
	else if(auto ref = dynamic_cast<const Exprs::Ref*>(expr))
		return evalRef(ref, data);
	else if(!expr)
		throw EvalError("internal error: nullptr given as expression", nullptr);
	else
		throw EvalError("Unexpected expression '" + expr->toString() + "'", expr);
}

std::pair<EvalResult, EvalData> ilang::eval(ExprIterator begin, ExprIterator end, EvalData data){
	EvalResult res;

	res.rest = begin + 1;
	res.end = end;
	res.result = evalExpr(*begin, data);

	return std::make_pair(std::move(res), std::move(data));
}

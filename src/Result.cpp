#include <iostream>

#include "ilang/Result.hpp"
#include "ilang/Util.hpp"

using namespace ilang;

ResultPtr ResultScope::resolveName(const std::string &name) const{
	auto fnRes = registeredFunctions.find(name);
	if(fnRes != registeredFunctions.cend())
		return std::make_unique<RefResult>(fnRes->second.get());

	auto boundRes = boundNames.find(name);
	if(boundRes != boundNames.cend())
		return std::make_unique<RefResult>(boundRes->second.get());

	std::cerr << "ERROR RESOLVING " << name << '\n';

	if(parent) return parent->resolveName(name);

	return nullptr;
}

ResultPtr Result::add(ResultHandle rhs) const{
	throw ResultError("Addition not implemented for result type");
	return nullptr;
}

ResultPtr Result::sub(ResultHandle rhs) const{
	throw ResultError("Subtraction not implemented for result type");
	return nullptr;
}

ResultPtr Result::mul(ResultHandle rhs) const{
	throw ResultError("Multiplication not implemented for result type");
	return nullptr;
}

ResultPtr Result::div(ResultHandle rhs) const{
	throw ResultError("Division not implemented for result type");
	return nullptr;
}

ResultPtr Result::call(TypeData&, std::vector<ResultPtr> args) const{
	throw ResultError("Result is not callable");
	return nullptr;
}

TypeHandle CallableResult::resolveType(TypeData &typeData) const noexcept{
	return fnType;
}

std::string CallableResult::toString() const noexcept{
	return "!CALLABLE!";
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

TypeHandle ListResult::resolveType(TypeData &typeData) const noexcept{
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

TypeHandle ProductResult::resolveType(TypeData &typeData) const noexcept{
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

	return overload(
		powIntRealFn,
		powIntRatioFn,
		powRatioRatioFn,
		powRatioRealFn,
		powGenFn
	);
}

using NumberValue = typename NumberResult::Value;

template<typename Fn>
auto doNumberBinop(const NumberValue &lhs, const NumberValue &rhs, Fn fn){
	return std::visit(
		overload(
			[&fn, &rhs](const AReal &aReal){
				return std::visit(
					overload(
						[&fn, &aReal](const AReal &other){ return fn(aReal, other); },
						[&fn, &aReal](const auto &other){ return fn(aReal, AReal(other)); }
					),
					rhs
				);
			},
			[&fn, &rhs](const ARatio &aRatio){
				return std::visit(
					overload(
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
					),
					rhs
				);
			},
			[&fn, &rhs](const AInt &aInt){
				return std::visit(
					overload(
						[&fn, &aInt](const AReal &other){ return fn(AReal(aInt), other); },
						[&fn, &aInt](const ARatio &other){ return fn(ARatio(aInt), other); },
						[&fn, &aInt](const AInt &other){ return fn(aInt, other); }
					),
					rhs
				);
			}
		),
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
	else return [](auto, auto){ throw ResultError("Unsupported number operator"); return nullptr; };
}

auto getStrBinopFn(std::string_view op) -> std::function<ResultPtr(const StringResult*, const StringResult*)>{
	if(op == "+") return [](const StringResult *lhs, const StringResult *rhs){
		return std::make_unique<StringResult>(lhs->value + rhs->value);
	};
	else return [](auto, auto){
		throw ResultError("Unsupported string operator");
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

		throw ResultError("Binary operators not implemented for result types");
	};
}

std::function<ResultPtr(ResultHandle, ResultHandle)> getResultBinOpFn(std::string_view op){
	return getBinopFn(op);
}

TypeHandle NumberResult::resolveType(TypeData &data) const noexcept{
	return std::visit(
		overload(
			[&data](const AInt&){ return findIntegerType(data); },
			[&data](const ARatio&){ return findRationalType(data); },
			[&data](const AReal&){ return findRealType(data); }
		),
		value
	);
}

TypeHandle StringResult::resolveType(TypeData &data) const noexcept{
	return findStringType(data);
}

ResultPtr NumberResult::add(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;

	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw ResultError("Addition not implemented between types result types");

	return doNumOp(this, rhsNum, numberAddFn());
}

ResultPtr NumberResult::sub(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;

	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw ResultError("Subtraction not implemented between result types");

	return doNumOp(this, rhsNum, numberSubFn());
}

ResultPtr NumberResult::mul(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;

	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw ResultError("Multiplication not implemented between result types");

	return doNumOp(this, rhsNum, numberMulFn());
}

ResultPtr NumberResult::div(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;

	auto rhsNum = dynamic_cast<const NumberResult*>(rhs);
	if(!rhsNum)
		throw ResultError("Division not implemented between result types");

	return doNumOp(this, rhsNum, numberDivFn());
}

ResultPtr StringResult::add(ResultHandle rhs) const{
	while(auto ref = dynamic_cast<const RefResult*>(rhs))
		rhs = ref->value;

	auto rhsStr = dynamic_cast<const StringResult*>(rhs);
	if(!rhsStr)
		throw ResultError("Addition not implemented between result types");

	return std::make_unique<StringResult>(value + rhsStr->value);
}

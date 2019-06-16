#include "ilang/Expr.hpp"
#include "ilang/Parser.hpp"

using namespace ilang;

TypeHandle Exprs::ListType::resolve(TypeData &typeData, typename TypeExpr::ResolveFn resolver) const{
	std::vector<TypeHandle> types;
	types.resize(elementTypes.size());
	std::transform(
		begin(elementTypes), end(elementTypes),
		begin(types),
		[&](TypeExprHandle typeExpr){ return typeExpr->resolve(typeData, resolver); }
	);
	
	if(!types.empty()){
		auto commonType = types[0];
		for(std::size_t i = 1; i < types.size(); i++)
			commonType = findCommonType(commonType, types[i]);
	
		return getStaticArrayType(typeData, commonType, elementTypes.size());
	}
	else{
		auto unitType = findUnitType(typeData);
		return getStaticArrayType(typeData, unitType, 0);
	}
}

std::string Exprs::ListType::toString() const noexcept{
	std::string ret = "[" + elementTypes[0]->toString();
	for(std::size_t i = 1; i < elementTypes.size(); i++)
		ret += " | " + elementTypes[i]->toString();
	
	ret += "]";
	
	return ret;
}

std::string Exprs::ProductType::toString() const noexcept{
	std::string ret = innerTypes[0]->toString();
	for(std::size_t i = 1; i < innerTypes.size(); i++)
		ret += " * " + innerTypes[i]->toString();
	
	return ret;
}

TypeHandle Exprs::ProductType::resolve(TypeData &typeData, typename TypeExpr::ResolveFn resolver) const{
	std::vector<TypeHandle> types;
	types.resize(innerTypes.size());
	std::transform(
		begin(innerTypes), end(innerTypes),
		begin(types),
		[&](TypeExprHandle typeExpr){ return typeExpr->resolve(typeData, resolver); }
	);
	
	return getProductType(typeData, std::move(types));
}

TypeHandle Exprs::BinOpType::resolve(TypeData &typeData, typename TypeExpr::ResolveFn resolver) const{
	auto lhsType = lhsTypeExpr->resolve(typeData, resolver);
	auto rhsType = rhsTypeExpr->resolve(typeData, resolver);
	
	if(op == "/"){
		if(isIntegerType(lhsType, typeData) && isIntegerType(rhsType, typeData))
			return findRationalType(typeData);
	}
	
	return findCommonType(lhsType, rhsType);
}

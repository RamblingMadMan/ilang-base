#include "ilang/Expr.hpp"
#include "ilang/Parser.hpp"

using namespace ilang;

TypeHandle Exprs::FnType::resolve(TypeData &typeData, TypeExpr::ResolveFn resolver) const{
	std::vector<TypeHandle> paramTypeHandles;
	paramTypeHandles.resize(paramTypes.size());
	std::transform(
		cbegin(paramTypes), cend(paramTypes),
		begin(paramTypeHandles),
		[&](const TypeExprPtr &ptr){ return ptr->resolve(typeData, resolver); }
	);
	
	auto resultTypeHandle = resultType->resolve(typeData, resolver);
	
	return getFunctionType(typeData, std::move(paramTypeHandles), resultTypeHandle);
}

TypeHandle Exprs::ResultTypes::resolve(TypeData &typeData, TypeExpr::ResolveFn resolver) const{
	std::vector<TypeHandle> typeHandles;
	typeHandles.resize(types.size());
	std::transform(
		cbegin(types), cend(types),
		begin(typeHandles),
		[&](const TypeExprPtr &ptr){ return ptr->resolve(typeData, resolver); }
	);
	
	std::sort(begin(typeHandles), end(typeHandles));
	typeHandles.erase(std::unique(begin(typeHandles), end(typeHandles)), end(typeHandles));
	
	return getSumType(typeData, std::move(typeHandles));
}

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
	
	if(types.size() == 1)
		return types[0];
	else
		return getProductType(typeData, std::move(types));
}

TypeHandle Exprs::ApplicationType::resolve(TypeData &typeData, TypeExpr::ResolveFn resolved) const{
	auto functorType = appExpr->functor->typeExpr()->resolve(typeData, resolved);
	if(!isFunctionType(functorType, typeData))
		return nullptr;
	
	if(functorType->types.size() != (appExpr->args.size() + 1))
		return nullptr;
	
	std::vector<TypeHandle> paramTypes, argTypes;
	std::transform(
		std::begin(appExpr->args), std::end(appExpr->args),
		std::back_inserter(argTypes),
		[&](auto &&p){ return p->typeExpr()->resolve(typeData, resolved); }
	);
	
	std::map<TypeHandle, TypeHandle> partialTypeMap;
	
	auto newResolver = [&partialTypeMap, &resolved](TypeData &td, TypeHandle type){
		auto res = partialTypeMap.find(type);
		if(res != end(partialTypeMap))
			return res->second;
		
		return resolved(td, type);
	};
	
	for(std::size_t i = 0; i < argTypes.size(); i++){
		auto paramType = functorType->types[i];
		auto argType = argTypes[i];
		
		if(isPartialType(paramType, typeData))
			paramType = newResolver(typeData, paramType);
		
		if(isPartialType(argType, typeData))
			argType = newResolver(typeData, argType);
		
		if(isPartialType(argType, typeData))
			throw ParseError("Tried to resolve function application with partially typed argument");
		else if(isPartialType(paramType, typeData))
			partialTypeMap[paramType] = argType;
		else if(!hasBaseType(argType, paramType))
			throw ParseError("Tried to resolve function application with incorrectly typed argument");
	}
	
	functorType = appExpr->functor->typeExpr()->resolve(typeData, newResolver);
	return functorType->types.back();
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

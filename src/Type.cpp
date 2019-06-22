#include <algorithm>

#include "ilang/Type.hpp"

using namespace ilang;

TypeHandle createEncodedStringType(TypeData &data, StringEncoding encoding) noexcept{
	auto type = std::make_unique<Type>();
	type->base = data.stringType;

	switch(encoding){
		case StringEncoding::ascii: type->str = "AsciiString"; type->mangled = "sa8"; break;
		case StringEncoding::utf8:  type->str = "Utf8String"; type->mangled = "su8"; break;
		default: return nullptr;
	}

	return data.storage.emplace_back(std::move(type)).get();
}

TypeHandle createSizedNumberType(
	TypeData &data, TypeHandle base,
	const std::string &name, const std::string &mangledName,
	std::uint32_t numBits
) noexcept
{
	if(numBits == 0) return base;

	auto bitsStr = std::to_string(numBits);
	auto type = std::make_unique<Type>();

	type->base = base;
	type->str = name + bitsStr;
	type->mangled = mangledName + bitsStr;
	type->bits = numBits;
	
	return data.storage.emplace_back(std::move(type)).get();
}

TypeHandle createFunctionType(
	TypeData &data,
	const std::vector<TypeHandle> &params, TypeHandle ret
) noexcept
{
	auto type = std::make_unique<Type>();

	type->base = data.functionType;
	type->str = params[0]->str;
	type->mangled = "f" + std::to_string(params.size()) + ret->mangled + params[0]->mangled;

	for(std::size_t i = 1; i < params.size(); i++){
		type->str += " -> " + params[i]->str;
		type->mangled += params[i]->mangled;
	}
	
	type->str += " -> " + ret->str;

	type->types.reserve(params.size() + 1);
	type->types.insert(begin(type->types), begin(params), end(params));
	type->types.emplace_back(ret);

	return data.storage.emplace_back(std::move(type)).get();
}

template<typename Container, typename Key>
TypeHandle findInnerType(const TypeData &data, TypeHandle base, const Container &cont, std::optional<Key> key) noexcept{
	if(key){
		auto res = cont.find(*key);
		if(res != end(cont))
			return res->second;

		return nullptr;
	}
	else
		return base;
}

template<typename Container>
TypeHandle findInnerNumberType(const TypeData &data, TypeHandle base, const Container &cont, std::uint32_t numBits) noexcept{
	return findInnerType(data, base, cont, numBits ? std::make_optional(numBits) : std::nullopt);
}

template<typename Container, typename Key, typename Create>
TypeHandle getInnerType(
	TypeData &data, TypeHandle base,
	Container &&container, std::optional<Key> key,
	Create &&create
){
	auto res = findInnerType(data, base, container, key);
	return res ? res : create(data, *key);
}

template<typename Container, typename Create>
TypeHandle getInnerNumberType(
	TypeData &data, TypeHandle base,
	Container &&container, std::uint32_t numBits,
	Create &&create
){
	return getInnerType(
		data, base, container,
		numBits ? std::make_optional(numBits) : std::nullopt,
		std::forward<Create>(create)
	);
}

bool impl_isInfinityType(TypeHandle type) noexcept{
	return type->mangled == "??";
}

bool ilang::hasBaseType(TypeHandle type, TypeHandle baseType) noexcept{
	if(type == baseType || impl_isInfinityType(baseType))
		return true;
	
	while(1){
		if(type->base == baseType)
			return true;
		else if(impl_isInfinityType(type->base))
			return false;
		else
			type = type->base;
	}
}

bool ilang::isRootType(TypeHandle type) noexcept{ return impl_isInfinityType(type->base) && !impl_isInfinityType(type); }

bool ilang::isRefinedType(TypeHandle type) noexcept{
	if(isRootType(type->base))
		return true;
	else if(impl_isInfinityType(type->base))
		return false;
	
	return isRefinedType(type->base);
}

bool ilang::isCompoundType(TypeHandle type) noexcept;

bool ilang::isListType(TypeHandle type, const TypeData &data) noexcept{
	if(type->types.size() != 1)
		return false;
	
	auto listBase = findListType(data, type->types[0]);
	if(!listBase)
		return false;
	
	return hasBaseType(type, listBase);
}

bool ilang::isArrayType(TypeHandle type, const TypeData& data) noexcept{
	if(type->types.size() != 1)
		return false;
	
	auto arrayBase = findArrayType(data, type->types[0]);
	if(!arrayBase)
		return false;
	
	return hasBaseType(type, arrayBase);
}


#define REFINED_TYPE_CHECK(type, typeLower)\
bool ilang::is##type##Type(TypeHandle type, const TypeData &data) noexcept{\
	auto baseType = data.typeLower##Type;\
	return type == baseType || hasBaseType(type, baseType);\
}

REFINED_TYPE_CHECK(Unit, unit)
REFINED_TYPE_CHECK(Type, type)
REFINED_TYPE_CHECK(Partial, partial)
REFINED_TYPE_CHECK(Function, function)
REFINED_TYPE_CHECK(Number, number)
REFINED_TYPE_CHECK(Complex, complex)
REFINED_TYPE_CHECK(Imaginary, imaginary)
REFINED_TYPE_CHECK(Real, real)
REFINED_TYPE_CHECK(Rational, rational)
REFINED_TYPE_CHECK(Integer, integer)
REFINED_TYPE_CHECK(Natural, natural)
REFINED_TYPE_CHECK(Boolean, boolean)
REFINED_TYPE_CHECK(String, string)

#define NUMBER_VALUE_TYPE(T, t, mangledSig)\
TypeHandle create##T##Type(TypeData &data, std::uint32_t numBits){\
	return createSizedNumberType(data, data.t##Type, #T, mangledSig, numBits);\
}\
TypeHandle ilang::find##T##Type(const TypeData &data, std::uint32_t numBits) noexcept{\
	return findInnerNumberType(data, data.t##Type, data.sized##T##Types, numBits);\
}\
TypeHandle ilang::get##T##Type(TypeData &data, std::uint32_t numBits){\
	return getInnerNumberType(data, data.t##Type, data.sized##T##Types, numBits, create##T##Type);\
}

#define ROOT_TYPE(T, t)\
TypeHandle ilang::find##T##Type(const TypeData &data) noexcept{ return data.t##Type; }\
TypeHandle ilang::get##T##Type(TypeData &data){ return data.t##Type; }

ROOT_TYPE(Infinity, infinity)
ROOT_TYPE(Type, type)
ROOT_TYPE(Unit, unit)
ROOT_TYPE(Number, number)

NUMBER_VALUE_TYPE(Boolean, boolean, "b")
NUMBER_VALUE_TYPE(Natural, natural, "n")
NUMBER_VALUE_TYPE(Integer, integer, "z")
NUMBER_VALUE_TYPE(Rational, rational, "q")
NUMBER_VALUE_TYPE(Real, real, "r")
NUMBER_VALUE_TYPE(Imaginary, imaginary, "i")
NUMBER_VALUE_TYPE(Complex, complex, "c")

template<typename Comp>
auto getSortedTypes(const TypeData &data, Comp &&comp = std::less<void>{}){
	std::vector<TypeHandle> types;
	types.reserve(data.storage.size());
	
	std::transform(
		begin(data.storage), end(data.storage),
		std::back_inserter(types),
		[](auto &&ptr){ return ptr.get(); }
	);
	
	std::sort(begin(types), end(types), std::forward<Comp>(comp));
	return types;
}

TypeHandle ilang::findTypeByString(const TypeData &data, std::string_view str){
	auto strS = std::string(str);
	
	auto aliased = data.typeAliases.find(strS);
	if(aliased != end(data.typeAliases))
		return aliased->second;
	
	auto types = getSortedTypes(data, [](auto lhs, auto rhs){ return lhs->str < rhs->str; });
	auto res = std::lower_bound(begin(types), end(types), str, [](TypeHandle lhs, std::string_view rhs){ return lhs->str < std::string(rhs); });
	
	if((res != end(types)) && (str < (*res)->str))
		res = end(types);
	
	if(res != end(types))
		return *res;
	
	return nullptr;
}

TypeHandle ilang::findTypeByMangled(const TypeData &data, std::string_view mangled){
	auto types = getSortedTypes(data, [](auto lhs, auto rhs){ return lhs->mangled < rhs->mangled; });
	auto res = std::lower_bound(begin(types), end(types), mangled, [](TypeHandle lhs, std::string_view rhs){ return lhs->mangled < std::string(rhs); });
	
	if((res != end(types)) && (mangled < (*res)->mangled))
		res = end(types);
	
	if(res != end(types))
		return *res;
	
	return nullptr;
}

TypeHandle ilang::findCommonType(TypeHandle type0, TypeHandle type1) noexcept{
	if(type0 == type1 || hasBaseType(type1, type0)) return type0;
	else if(hasBaseType(type0, type1)) return type1;
	else return findCommonType(type0->base, type1->base);
}

TypeHandle ilang::findPartialType(const TypeData &data, std::optional<std::uint32_t> id) noexcept{
	if(!id)
		return data.partialType;

	auto num = *id;

	if(data.partialTypes.size() >= num)
		return nullptr;
	else
		return data.partialTypes[num];
}

TypeHandle ilang::findStringType(const TypeData &data, std::optional<StringEncoding> encoding) noexcept{
	return findInnerType(data, data.stringType, data.encodedStringTypes, encoding);
}

TypeHandle ilang::findTreeType(const TypeData &data, TypeHandle t) noexcept{
	return findInnerType(data, nullptr, data.treeTypes, std::make_optional(t));
}

TypeHandle ilang::findListType(const TypeData &data, TypeHandle t) noexcept{
	return findInnerType(data, nullptr, data.listTypes, std::make_optional(t));
}

TypeHandle ilang::findArrayType(const TypeData &data, TypeHandle t) noexcept{
	return findInnerType(data, nullptr, data.arrayTypes, std::make_optional(t));
}

TypeHandle ilang::findDynamicArrayType(const TypeData &data, TypeHandle t) noexcept{
	return findInnerType(data, nullptr, data.listTypes, std::make_optional(t));
}

TypeHandle ilang::findStaticArrayType(const TypeData &data, TypeHandle t, std::size_t n) noexcept{
	auto res = data.staticArrayTypes.find(t);
	if(res != end(data.staticArrayTypes))
		return findInnerType(data, nullptr, res->second, std::make_optional(n));
	
	return nullptr;
}

TypeHandle findSumTypeInner(const TypeData &data, const std::vector<TypeHandle> &uniqueSortedInnerTypes) noexcept{
	return findInnerType(data, nullptr, data.sumTypes, std::make_optional(std::ref(uniqueSortedInnerTypes)));	
}

TypeHandle ilang::findSumType(const TypeData &data, std::vector<TypeHandle> innerTypes) noexcept{
	std::sort(begin(innerTypes), end(innerTypes));
	innerTypes.erase(std::unique(begin(innerTypes), end(innerTypes)), end(innerTypes));
	return findSumTypeInner(data, innerTypes);
}

TypeHandle ilang::findProductType(const TypeData &data, const std::vector<TypeHandle> &innerTypes) noexcept{
	return findInnerType(data, nullptr, data.productTypes, std::make_optional(std::ref(innerTypes)));
}

TypeHandle ilang::findFunctionType(const TypeData &data, const std::vector<TypeHandle> &params, TypeHandle result) noexcept{
	auto paramsRes = data.functionTypes.find(params);
	if(paramsRes != end(data.functionTypes)){
		auto resultRes = paramsRes->second.find(result);
		if(resultRes != end(paramsRes->second))
			return resultRes->second;
	}

	return nullptr;
}

TypeHandle ilang::findFunctionType(const TypeData &data) noexcept{ return data.functionType; }

TypeHandle ilang::getStringType(TypeData &data, std::optional<StringEncoding> encoding){
	return getInnerType(data, data.stringType, data.encodedStringTypes, encoding, createEncodedStringType);
}

TypeHandle ilang::getTreeType(TypeData &data, TypeHandle t){
	if(auto res = findTreeType(data, t))
		return res;
	
	auto ptr = data.storage.emplace_back(std::make_unique<Type>()).get();
	
	ptr->base = data.infinityType;
	ptr->str = "(Tree " + t->str + ")";
	ptr->mangled = "ot0" + t->mangled;
	ptr->types = {t};
	
	data.treeTypes[t] = ptr;
	
	return ptr;
}

TypeHandle ilang::getListType(TypeData &data, TypeHandle t){
	if(auto res = findListType(data, t))
		return res;
	
	auto newType = std::make_unique<Type>();
	
	newType->base = getTreeType(data, t);
	newType->str = "(List " + t->str + ")";
	newType->mangled = "ol0" + t->mangled;
	newType->types = {t};
	
	auto ptr = data.storage.emplace_back(std::move(newType)).get();
	
	data.listTypes[t] = ptr;
	
	return ptr;
}

TypeHandle ilang::getArrayType(TypeData &data, TypeHandle t){
	if(auto res = findArrayType(data, t))
		return res;
	
	auto newType = std::make_unique<Type>();
	
	newType->base = getListType(data, t);
	newType->str = "(Array " + t->str + ")";
	newType->mangled = "oa0" + t->mangled;
	newType->types = {t};
	
	auto ptr = data.storage.emplace_back(std::move(newType)).get();
	
	data.arrayTypes[t] = ptr;
	
	return ptr;
}

TypeHandle ilang::getDynamicArrayType(TypeData &data, TypeHandle t){
	if(auto res = findDynamicArrayType(data, t))
		return res;
	
	auto newType = std::make_unique<Type>();
	
	newType->base = getArrayType(data, t);
	newType->str = "(DynamicArray " + t->str + ")";
	newType->mangled = "a0" + t->mangled;
	newType->types = {t};
	
	auto ptr = data.storage.emplace_back(std::move(newType)).get();
	
	data.dynamicArrayTypes[t] = ptr;
	
	return ptr;
}

TypeHandle ilang::getStaticArrayType(TypeData &data, TypeHandle t, std::size_t n){
	if(auto res = findStaticArrayType(data, t, n))
		return res;
	
	auto newType = std::make_unique<Type>();
	
	auto nStr = std::to_string(n);
	
	newType->base = getArrayType(data, t);
	newType->str = "(StaticArray " + t->str + " " + nStr + ")";
	newType->mangled = "a" + nStr + t->mangled;
	newType->types = {t};
	newType->bits = n;
	
	auto ptr = data.storage.emplace_back(std::move(newType)).get();
	
	data.staticArrayTypes[t][n] = ptr;
	
	return ptr;
}

TypeHandle ilang::getPartialType(TypeData &data){
	auto type = std::make_unique<Type>();
	auto id = std::to_string(data.partialTypes.size());
	type->base = data.partialType;
	type->str = "Partial" + id;
	type->mangled = "_" + id;
	auto &&typePtr = data.storage.emplace_back(std::move(type));
	return typePtr.get();
}

TypeHandle ilang::getSumType(TypeData &data, std::vector<TypeHandle> innerTypes){
	std::sort(begin(innerTypes), end(innerTypes));
	innerTypes.erase(std::unique(begin(innerTypes), end(innerTypes)), end(innerTypes));
	
	if(auto res = findSumTypeInner(data, innerTypes))
		return res;
	
	auto &&newType = data.storage.emplace_back(std::make_unique<Type>());
	
	newType->base = findInfinityType(data);
	newType->types = std::move(innerTypes);
	
	newType->mangled = "u" + std::to_string(innerTypes.size());
	newType->mangled += innerTypes[0]->mangled;
	
	newType->str = innerTypes[0]->str;

	
	for(std::size_t i = 1; i < innerTypes.size(); i++){
		newType->mangled += innerTypes[i]->mangled;
		newType->str += " | " + innerTypes[i]->str;
	}

	newType->bits = innerTypes.size();
	
	auto[it, good] = data.sumTypes.try_emplace(newType->types, newType.get());
	
	if(!good){
		// TODO: throw TypeError
	}
	
	return newType.get();
}

TypeHandle ilang::getProductType(TypeData &data, std::vector<TypeHandle> innerTypes){
	if(innerTypes.size() < 2){
		// TODO: throw TypeError
		throw std::runtime_error("product type can not have less than 2 inner types");
	}
	
	if(auto res = findProductType(data, innerTypes))
		return res;
	
	auto &&newType = data.storage.emplace_back(std::make_unique<Type>());
	
	newType->base = findInfinityType(data);
	
	newType->mangled = "p" + std::to_string(innerTypes.size()) + innerTypes[0]->mangled;
	newType->str = innerTypes[0]->str;
	
	for(std::size_t i = 1; i < innerTypes.size(); i++){
		newType->mangled += innerTypes[i]->mangled;
		newType->str += " * " + innerTypes[i]->str;
	}

	newType->bits = innerTypes.size();
	
	newType->types = std::move(innerTypes);
		
	auto[it, good] = data.productTypes.try_emplace(newType->types, newType.get());
	
	if(!good){
		// TODO: throw TypeError
	}
	
	return newType.get();
}

TypeHandle ilang::getFunctionType(TypeData &data, std::vector<TypeHandle> params, TypeHandle result){
	auto &&retMap = data.functionTypes[params];

	auto res = retMap.find(result);
	if(res != end(retMap))
		return res->second;

	return retMap[result] = createFunctionType(data, params, result);
}

TypeData::TypeData(){
	auto newInfinityType = [this](){
		auto &&ptr = storage.emplace_back(std::make_unique<Type>());
		ptr->base = ptr.get();
		ptr->str = "Infinity";
		ptr->mangled = "??";
		return ptr.get();
	};

	auto newType = [this](std::string str, std::string mangled, auto base){
		auto &&ptr = storage.emplace_back(std::make_unique<Type>());
		ptr->base = base;
		ptr->str = std::move(str);
		ptr->mangled = std::move(mangled);
		return ptr.get();
	};

	infinityType = newInfinityType();

	auto newRootType = [&newType, this](auto str, auto mangled){
		return newType(str, mangled, infinityType);
	};
	
	partialType = newRootType("Partial", "_?");
	typeType = newRootType("Type", "t?");
	unitType = newRootType("Unit", "u0");
	stringType = newRootType("String", "s?");
	numberType = newRootType("Number", "w?");
	functionType = newRootType("Function", "f?");

	complexType = newType("Complex", "c?", numberType);
	imaginaryType = newType("Imaginary", "i?", complexType);
	realType = newType("Real", "r?", complexType);
	rationalType = newType("Rational", "q?", realType);
	integerType = newType("Integer", "z?", rationalType);
	naturalType = newType("Natural", "n?", integerType);
	booleanType = newType("Boolean", "b?", naturalType);
	
	typeAliases["Ratio"] = rationalType;
	typeAliases["Int"] = integerType;
	typeAliases["Nat"] = naturalType;
	typeAliases["Bool"] = booleanType;
	
	auto real64Type = createSizedNumberType(*this, realType, "Real", "r", 64);
	auto real32Type = createSizedNumberType(*this, real64Type, "Real", "r", 32);
	auto real16Type = createSizedNumberType(*this, real32Type, "Real", "r", 16);
	
	sizedRealTypes[64] = real64Type;
	sizedRealTypes[32] = real32Type;
	sizedRealTypes[16] = real16Type;
	
	auto rational128Type = createSizedNumberType(*this, realType, "Rational", "q", 128);
	auto rational64Type = createSizedNumberType(*this, rational128Type, "Rational", "q", 64);
	auto rational32Type = createSizedNumberType(*this, rational64Type, "Rational", "q", 32);
	auto rational16Type = createSizedNumberType(*this, rational32Type, "Rational", "q", 16);
	
	sizedRationalTypes[128] = rational128Type;
	sizedRationalTypes[64] = rational64Type;
	sizedRationalTypes[32] = rational32Type;
	sizedRationalTypes[16] = rational16Type;
	
	typeAliases["Ratio128"] = rational128Type;
	typeAliases["Ratio64"] = rational64Type;
	typeAliases["Ratio32"] = rational32Type;
	typeAliases["Ratio16"] = rational16Type;
	
	auto int64Type = createSizedNumberType(*this, integerType, "Integer", "i", 64);
	auto int32Type = createSizedNumberType(*this, int64Type, "Integer", "i", 32);
	auto int16Type = createSizedNumberType(*this, int32Type, "Integer", "i", 16);
	auto int8Type = createSizedNumberType(*this, int16Type, "Integer", "i", 8);
	
	sizedIntegerTypes[64] = int64Type;
	sizedIntegerTypes[32] = int32Type;
	sizedIntegerTypes[16] = int16Type;
	sizedIntegerTypes[8]  = int8Type;
	
	typeAliases["Int64"] = int64Type;
	typeAliases["Int32"] = int32Type;
	typeAliases["Int16"] = int16Type;
	typeAliases["Int8"] = int8Type;
	
	auto nat64Type = createSizedNumberType(*this, naturalType, "Natural", "n", 64);
	auto nat32Type = createSizedNumberType(*this, nat64Type, "Natural", "n", 32);
	auto nat16Type = createSizedNumberType(*this, nat32Type, "Natural", "n", 16);
	auto nat8Type = createSizedNumberType(*this, nat16Type, "Natural", "n", 8);
	
	sizedNaturalTypes[64] = nat64Type;
	sizedNaturalTypes[32] = nat32Type;
	sizedNaturalTypes[16] = nat16Type;
	sizedNaturalTypes[8]  = nat8Type;
	
	typeAliases["Nat64"] = nat64Type;
	typeAliases["Nat32"] = nat32Type;
	typeAliases["Nat16"] = nat16Type;
	typeAliases["Nat8"] = nat8Type;
}

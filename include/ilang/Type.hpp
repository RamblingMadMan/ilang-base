#ifndef ILANG_TYPE_HPP
#define ILANG_TYPE_HPP 1

#include <string>
#include <string_view>
#include <memory>
#include <vector>
#include <optional>
#include <map>

#include "ilang/fwd.hpp"

/** \file */

namespace ilang{
	//! String encoding type
	enum class StringEncoding{
		ascii, utf8
	};

	//! Data type for type values
	struct Type{
		//! Base type of the type.
		const Type *base = nullptr;

		//! The type name as it would appear in code.
		std::string str;

		//! The type name as it would appear in binaries.
		std::string mangled;
		
		/**
		 * \brief Information stored about type
		 * 
		 * This is usually just 0
		 * 
		 * For sized types this will be the bit-width
		 * For static list types this will be the number of elements
		 * For product and sum types this will be the number of elements
		 **/
		std::uint64_t bits = 0;

		/**
		 * \brief Inner types of the type.
		 *
		 * This is empty for most types, but has special meaning otherwise.
		 * 
		 * For a function type, this is the list of parameter types (in-order)
		 * followed by the result type.
		 *
		 * For a sum type, this is the list of inner types.
		 * 
		 * For a product type, this is the list of inner types.
		 *
		 * For a list type, this will have a single element representing the
		 * list element type.
		 *
		 **/
		std::vector<const Type*> types;
		
		/**
		 * \brief Names associated with inner types.
		 * 
		 * This will be empty or types.size() elements long
		 * 
		 * Names are used for compound object types to store their member information
		 **/
		std::vector<std::string> names;
	};
	
	//! \brief Refinement data for a type
	struct TypeRefinement{
		ExprHandle typeRef;
		std::vector<ExprHandle> conditions;
	};

	/**
	 * \brief Data required for type calculations
	 *
	 * This should be treated as an opaque data type and
	 * only ever be used with the accompanying find and get functions
	 **/
	struct TypeData{
		TypeData();
		
		TypeData(TypeData&&) = default;
		TypeData(const TypeData&) = delete;
		
		TypeData &operator=(TypeData&&) noexcept = default;

		TypeHandle infinityType;
		TypeHandle partialType;
		TypeHandle typeType;
		TypeHandle unitType;
		TypeHandle stringType;
		TypeHandle numberType, complexType, imaginaryType, realType, rationalType, integerType, naturalType, booleanType;
		TypeHandle functionType;
		std::map<std::uint32_t, TypeHandle> sizedBooleanTypes;
		std::map<std::uint32_t, TypeHandle> sizedNaturalTypes;
		std::map<std::uint32_t, TypeHandle> sizedIntegerTypes;
		std::map<std::uint32_t, TypeHandle> sizedRationalTypes;
		std::map<std::uint32_t, TypeHandle> sizedImaginaryTypes;
		std::map<std::uint32_t, TypeHandle> sizedRealTypes;
		std::map<std::uint32_t, TypeHandle> sizedComplexTypes;
		std::map<StringEncoding, TypeHandle> encodedStringTypes;
		std::map<std::vector<TypeHandle>, std::map<TypeHandle, TypeHandle>> functionTypes;
		std::map<std::vector<TypeHandle>, TypeHandle> sumTypes;
		std::map<std::vector<TypeHandle>, TypeHandle> productTypes;
		std::map<TypeHandle, TypeHandle> treeTypes;
		std::map<TypeHandle, std::map<TypeHandle, TypeHandle>> mapTypes;
		std::map<TypeHandle, TypeHandle> listTypes, arrayTypes, dynamicArrayTypes;
		std::map<TypeHandle, std::map<std::size_t, TypeHandle>> staticArrayTypes;
		std::vector<TypeHandle> partialTypes;
		std::vector<std::unique_ptr<Type>> storage;
		
		std::map<std::string, TypeHandle> typeAliases;
	};

	/**
	 * \defgroup RefinementCheckers Type refinement checking
	 * \brief Functions for checking refinement of types
	 * \{
	 **/

	bool hasBaseType(TypeHandle type, TypeHandle baseType) noexcept;

	bool isRootType(TypeHandle type) noexcept;
	bool isRefinedType(TypeHandle type) noexcept;
	bool isValueType(TypeHandle type) noexcept;
	bool isCompoundType(TypeHandle type) noexcept;

	/** \} */

	/**
	 * \defgroup RootTypeCheckers Root type checking
	 * \brief Functions for checking root types
	 * \{
	 **/

	bool isUnitType(TypeHandle type, const TypeData &data) noexcept;
	bool isTypeType(TypeHandle type, const TypeData &data) noexcept;
	bool isPartialType(TypeHandle type, const TypeData &data) noexcept;
	bool isFunctionType(TypeHandle type, const TypeData &data) noexcept;
	bool isNumberType(TypeHandle type, const TypeData &data) noexcept;
	bool isStringType(TypeHandle type, const TypeData &data) noexcept;
	bool isTreeType(TypeHandle type, const TypeData &data) noexcept;

	/** \} */
	
	/**
	 * \defgroup TreeTypeCheckers Tree type checking
	 * \brief Functions for checking tree types
	 * \{
	 **/
	
	bool isListType(TypeHandle type, const TypeData &data) noexcept;
	bool isArrayType(TypeHandle type, const TypeData &data) noexcept;
	
	/** \} */

	/**
	 * \defgroup NumberTypeCheckers Number type checking
	 * \brief Functions for checking numeric types
	 * \{
	 **/

	bool isComplexType(TypeHandle type, const TypeData &data) noexcept;
	bool isImaginaryType(TypeHandle type, const TypeData &data) noexcept;
	bool isRealType(TypeHandle type, const TypeData &data) noexcept;
	bool isRationalType(TypeHandle type, const TypeData &data) noexcept;
	bool isIntegerType(TypeHandle type, const TypeData &data) noexcept;
	bool isNaturalType(TypeHandle type, const TypeData &data) noexcept;
	bool isBooleanType(TypeHandle type, const TypeData &data) noexcept;

	/** \} */

	/**
	 * \defgroup TypeFinders Type finding functions
	 * \brief Functions for finding a type without modifying any state.
	 * \returns The \ref TypeHandle or nullptr if it could not be found.
	 * \{
	 **/

	//! Find a type by name
	TypeHandle findTypeByString(const TypeData &data, std::string_view str);
	
	//! Find a type by mangled name
	TypeHandle findTypeByMangled(const TypeData &data, std::string_view mangled);

	//! Find the most-refined common type
	TypeHandle findCommonType(TypeHandle type0, TypeHandle type1) noexcept;

	TypeHandle findInfinityType(const TypeData &data) noexcept;
	
	TypeHandle findPartialType(const TypeData &data, std::optional<std::uint32_t> id = std::nullopt) noexcept;
	TypeHandle findTypeType(const TypeData &data) noexcept;
	TypeHandle findUnitType(const TypeData &data) noexcept;
	TypeHandle findStringType(const TypeData &data, std::optional<StringEncoding> encoding = std::nullopt) noexcept;
	
	TypeHandle findTreeType(const TypeData &data, TypeHandle t) noexcept;
	TypeHandle findListType(const TypeData &data, TypeHandle t) noexcept;
	TypeHandle findArrayType(const TypeData &data, TypeHandle t) noexcept;
	TypeHandle findDynamicArrayType(const TypeData &data, TypeHandle t) noexcept;
	TypeHandle findStaticArrayType(const TypeData &data, TypeHandle t, std::size_t n) noexcept;
	
	TypeHandle findNumberType(const TypeData &data) noexcept;
	TypeHandle findComplexType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findImaginaryType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findRealType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findRationalType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findIntegerType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findNaturalType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	TypeHandle findBooleanType(const TypeData &data, std::uint32_t numBits = 0) noexcept;
	
	TypeHandle findSumType(const TypeData &data, std::vector<TypeHandle> innerTypes) noexcept;
	TypeHandle findProductType(const TypeData &data, const std::vector<TypeHandle> &innerTypes) noexcept;
	
	TypeHandle findFunctionType(const TypeData &data) noexcept;
	TypeHandle findFunctionType(const TypeData &data, const std::vector<TypeHandle> &params, TypeHandle result) noexcept;
	
	/** \} */


	/**
	 * \defgroup TypeGetters Type getting functions
	 * \brief Functions for getting a type, otherwise creating it.
	 * \returns Pair of the \ref TypeData and resulting \ref TypeHandle (in that order)
	 * \{
	 **/

	TypeHandle getInfinityType(TypeData &data);
	
	TypeHandle getPartialType(TypeData &data);
	TypeHandle getTypeType(TypeData &data);
	TypeHandle getUnitType(TypeData &data);
	TypeHandle getStringType(TypeData &data, std::optional<StringEncoding> encoding = std::nullopt);
	
	TypeHandle getTreeType(TypeData &data, TypeHandle t);
	TypeHandle getListType(TypeData &data, TypeHandle t);
	TypeHandle getArrayType(TypeData &data, TypeHandle t);
	TypeHandle getDynamicArrayType(TypeData &data, TypeHandle t);
	TypeHandle getStaticArrayType(TypeData &data, TypeHandle t, std::size_t n);

	TypeHandle getNumberType(TypeData &data);
	TypeHandle getComplexType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getImaginaryType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getRealType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getRationalType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getIntegerType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getNaturalType(TypeData &data, std::uint32_t numBits = 0);
	TypeHandle getBooleanType(TypeData &data, std::uint32_t numBits = 0);
		
	TypeHandle getFunctionType(TypeData &data, std::vector<TypeHandle> args, TypeHandle ret);
	
	TypeHandle getSumType(TypeData &data, std::vector<TypeHandle> innerTypes = {});
	TypeHandle getProductType(TypeData &data, std::vector<TypeHandle> innerTypes = {});
	
	/** \} */
}

#endif // !ILANG_TYPE_HPP

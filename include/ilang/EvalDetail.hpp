#ifndef ILANG_EVAL_DETAIL_HPP
#define ILANG_EVAL_DETAIL_HPP 1

#include <cstdint>
#include <climits>
#include <variant>

#include "ilang/Type.hpp"
#include "ilang/AInt.hpp"
#include "ilang/AReal.hpp"

namespace ilang{
	namespace detail{
		template<typename ... Ts>
		struct Overloaded: Ts...{ using Ts::operator()...; };
		
		template<typename ... Ts>
		Overloaded(Ts...) -> Overloaded<Ts...>;
		
		template<typename...>
		struct CppTypeGetter;
		
		template<typename...>
		struct CppValueGetter;
		
		using NumberValue = std::variant<AReal, ARatio, AInt>;
		
		// what is this shit
		template<typename Void>
		struct CppTypeGetter<Void, std::enable_if_t<std::is_void_v<Void>>>{
			static TypeHandle get(TypeData &data){ return findUnitType(data); }
		};
		
		template<typename Nat>
		struct CppTypeGetter<Nat, std::enable_if_t<std::is_unsigned_v<Nat>>>{
			enum {
				bitSize = CHAR_BIT * sizeof(Nat)
			};
			
			static TypeHandle get(TypeData &data){
				TypeHandle res;
				std::tie(res, data) = getNaturalType(std::move(data), bitSize);
				return res;
			}
			
			static auto getValue(const NumberValue &v){
				auto val = std::visit(
					Overloaded{
						[](const AInt &i){
							if(i.bitsRequired() > bitSize)
								throw std::runtime_error("argument value too large");
							
							return std::stoull(i.toString());
						},
						[](auto){
							throw std::runtime_error("invalid value for integer type");
							return 0;
						}
					},
					v
				);
			}
		};
		
		template<typename Int>
		struct CppTypeGetter<Int, std::enable_if_t<std::is_integral_v<Int>>>{
			enum {
				bitSize = CHAR_BIT * sizeof(Int)
			};
			
			static TypeHandle get(TypeData &data){
				TypeHandle res;
				std::tie(res, data) = getIntegerType(std::move(data), bitSize);
				return res;
			}
			
			static auto getValue(const NumberValue &v){
				auto val = std::visit(
					Overloaded{
						[](const AInt &i){
							if(i.bitsRequired() > bitSize)
								throw std::runtime_error("argument value too large");
							
							return std::stoll(i.toString());
						},
						[](auto){
							throw std::runtime_error("invalid value for integer type");
							return 0;
						}
					},
					v
				);
			}
		};
		
		template<typename Real>
		struct CppTypeGetter<Real, std::enable_if_t<std::is_floating_point_v<Real>>>{
			enum {
				bitSize = CHAR_BIT * sizeof(Real)
			};
			
			static TypeHandle get(TypeData &data){
				TypeHandle res;
				std::tie(res, data) = getRealType(std::move(data), bitSize);
				return res;
			}
			
			static auto getValue(const NumberValue &v){
				auto val = std::visit(
					Overloaded{
						[](const AReal &r){
							std::fprintf(stderr, "warning: Real value type conversions can be lossy\n");
							return std::stold(r.toString());
						},
						[](auto){
							throw std::runtime_error("invalid value for real type");
							return 0;
						}
					},
					v
				);
			}
		};
		
		template<typename Ret, typename ... Params>
		struct CppTypeGetter<Ret(Params...)>{
			static TypeHandle get(TypeData &data){
				using RetTypeGetter = CppTypeGetter<Ret>;
				auto paramTypes = std::vector{CppTypeGetter<Params>::get(data)...};
				auto retType = CppTypeGetter<Ret>::get(data);
				return getFunctionType(std::move(data), std::move(paramTypes), retType);
			}
		};
		
		template<>
		struct CppTypeGetter<void()>{
			static TypeHandle get(TypeData &data){
				auto unitType = findUnitType(data);
				return getFunctionType(data, {unitType}, unitType);
			}
		};
	}
}

#endif // !ILANG_EVAL_DETAIL_HPP

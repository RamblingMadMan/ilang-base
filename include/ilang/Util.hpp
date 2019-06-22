#ifndef ILANG_UTIL_HPP
#define ILANG_UTIL_HPP 1

#include <functional>

namespace ilang{
	template<typename T, typename Fn>
	struct TypeSwitchCase{
		Fn fn;
	};
	
	template<typename T, typename Fn>
	struct ValueSwitchCase{
		T match; Fn fn;
	};
	
	template<typename Fn>
	struct DefaultCase{ Fn fn; };
	
	template<typename T, typename Fn>
	inline auto typeCase(Fn &&fn){
		return TypeSwitchCase<T, Fn>{ std::forward<Fn>(fn) };
	}
	
	template<typename T, typename Fn>
	inline auto valueCase(T &&val, Fn &&fn){
		return ValueSwitchCase<T, Fn>{ std::forward<T>(val), std::forward<Fn>(fn) };
	}
	
	template<typename Fn>
	auto defaultCase(Fn &&fn){
		return DefaultCase<Fn>{ std::forward<Fn>(fn) };
	}
	
	template<typename Val, typename Fn, typename ... Cases>
	inline auto checkValueCases(Val&&, const DefaultCase<Fn> &default_, Cases &&... cases){
		return default_.fn();
	}
	
	template<typename Val, typename T, typename Fn, typename ... Cases>
	inline auto checkValueCases(Val &&v, const ValueSwitchCase<T, Fn> &case_, Cases &&... cases){
		if(std::forward<Val>(v) == case_.match)
			return case_.fn();
		else
			return checkValueCases(std::forward<Val>(v), std::forward<Cases>(cases)...);
	}
	
	template<typename Ptr, typename Fn, typename ... Cases>
	inline auto checkTypeCases(Ptr p, const DefaultCase<Fn> &default_, Cases &&... cases){
		return default_.fn();
	}
	
	template<typename Ptr, typename T, typename Fn, typename ... Cases>
	inline auto checkTypeCases(Ptr p, const TypeSwitchCase<T, Fn> &case_, Cases &&... cases){
		if(auto t = dynamic_cast<T*>(p))
			return case_.fn(t);
		else
			return checkTypeCases(p, std::forward<Cases>(cases)...);
	}
	
	template<typename Val>
	inline auto valueSwitch(Val v){
		return [value{std::move(v)}](auto &&... cases){
			return checkValueCases(value, std::forward<decltype(cases)>(cases)...);
		};
	}
	
	template<typename Ptr>
	inline auto typeSwitch(Ptr p){
		return [p](auto &&... cases){
			return checkTypeCases(p, std::forward<decltype(cases)>(cases)...);
		};
	}
}

#endif // !ILANG_UTIL_HPP

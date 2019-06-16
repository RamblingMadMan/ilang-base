#ifndef ILANG_LEXER_HPP
#define ILANG_LEXER_HPP 1

#include <stdexcept>
#include <optional>
#include <variant>
#include <string>
#include <vector>

#include "Location.hpp"

namespace ilang{
	//! Error type thrown from lex
	class LexError: public std::exception{
		public:
			LexError(Location location_, std::string msg)
				: m_location(location_), m_msg(std::move(msg)){}

			const Location &location() const noexcept{ return m_location; }
			const char *what() const noexcept override{ return m_msg.c_str(); }

		private:
			Location m_location;
			std::string m_msg;
	};

	//! Token type
	enum class TokenType{
		space, id,
		str, int_, real,
		op,
		groupL, groupR,
		listL, listR,
		newLine, eof,
		empty,

		COUNT
	};

	/**
	 * \brief Lexed code item
	 *
	 * Represents a single "token" of source code
	 **/
	struct Token{
		//! The type
		TokenType type = TokenType::empty;

		//! The string representation
		std::string value;

		//! Location within source
		Location location;
	};

	//! Returns the position in source immediately after the given token
	Location locationAfter(const Token &tok);

	//! Result type of lex
	struct LexResult{
		Token token;
		std::string_view remainder;
	};

	/**
	 * \brief Lexing function
	 *
	 * Returns the pair of the lexed token and the remaining source.
	 **/
	LexResult lex(std::string_view src, Location startLoc = {0, 0});

	//! Convienience function for lexing the remainder of a previous lex
	inline LexResult lex(LexResult rest){
		return lex(rest.remainder, locationAfter(rest.token));
	}
	
	inline std::vector<Token> lexAll(std::string_view src){
		std::vector<Token> ret;
		
		LexResult res;
		res.remainder = src;
		
		while(!res.remainder.empty()){
			res = lex(std::move(res));
			ret.emplace_back(res.token);
		}
		
		return ret;
	}

	using TokenIterator = std::vector<Token>::const_iterator;
	
	/**
	 * Implementation details
	 **/
	
	namespace detail{
		template<typename I>
		struct ToInt;
		
		template<typename R>
		struct ToReal;
		
		template<>
		struct ToInt<std::int64_t>{
			static std::int64_t parse(const std::string &s){ return std::stol(s); }
		};
		
		template<>
		struct ToInt<std::int32_t>{
			static std::int32_t parse(const std::string &s){ return std::stoi(s); }
		};
		
		template<>
		struct ToReal<float>{
			static float parse(const std::string &s){ return std::stof(s); }
		};
		
		template<>
		struct ToReal<double>{
			static double parse(const std::string &s){ return std::stod(s); }
		};
		
		std::string asString(TokenType type, const std::string &s);
		
		template<typename T, typename = void>
		struct AsHelper;
		
		template<typename Int>
		struct AsHelper<Int, std::enable_if_t<std::is_integral_v<Int>>>{
			static Int parse(const Token &t){
				if(t.type != TokenType::int_ && t.type != TokenType::real)
					throw std::runtime_error("token is not an integer");
				
				return ToInt<Int>::parse(t.value);
			}
		};
		
		template<typename Real>
		struct AsHelper<Real, std::enable_if_t<std::is_floating_point_v<Real>>>{
			static Real parse(const Token &t){
				if(t.type != TokenType::real && t.type != TokenType::int_)
					throw std::runtime_error("token is not a real number");
				
				return ToReal<Real>::parse(t.value);
			}
		};
		
		template<>
		struct AsHelper<std::string, void>{
			static std::string parse(const Token &t){
				return asString(t.type, t.value);
			}
		};
	}
	
	/**
	 * Function for converting a token into a usable value
	 **/
	
	template<typename T>
	T as(const Token &t){ return detail::AsHelper<std::decay_t<T>>::parse(t); }
}

#endif // !ILANG_LEXER_HPP

#ifndef ILANG_PARSER_HPP
#define ILANG_PARSER_HPP 1

#include <iostream>
#include <vector>
#include <list>

#include "ilang/Lexer.hpp"

#include "Expr.hpp"

//! \file

namespace ilang{
	//! Error thrown by parsing functions
	class ParseError: public std::exception{
		public:
			ParseError(std::string msg): m_msg(std::move(msg)){}

			const char *what() const noexcept override;

		private:
			std::string m_msg;
	};

	struct Scope{
		explicit Scope(const Scope *parent_ = nullptr): parent(parent_){}

		ExprHandle resolveName(const std::string &name) const{
			auto res = boundNames.find(name);
			if(res != end(boundNames))
				return res->second.get();

			return parent ? parent->resolveName(name) : nullptr;
		}

		const Exprs::TypeLiteral *resolveType(TypeHandle type) const{
			auto res = types.find(type);
			if(res != end(types))
				return res->second.get();

			return parent ? parent->resolveType(type) : nullptr;
		}

		const Scope *parent;
		std::map<TypeHandle, std::unique_ptr<Exprs::TypeLiteral>> types;
		std::map<std::string, ExprPtr> boundNames;
		std::vector<std::unique_ptr<Scope>> children;
	};

	//! Abstract syntax tree
	struct Ast{
		Ast() = default;

		Ast(Ast&&) = default;
		Ast(const Ast&) = delete;
		
		Scope rootScope;
	};

	struct AnalysisData{
		TypeData typeData;
	};
	
	//! Parse a single expression
	ExprPtr parse(TokenIterator &it, TokenIterator end, TypeData &typeData, Ast &ast);

	inline auto parse(const std::vector<Token> &toks, TypeData &typeData, Ast &ast){
		auto it = cbegin(toks);
		return parse(it, cend(toks), typeData, ast);
	}
	
	inline auto parse(std::string_view src, TypeData &typeData, Ast &ast){
		auto toks = lexAll(src);
		return parse(toks, typeData, ast);
	}

	//! Parse all tokens in a stream
	inline auto parseAll(TokenIterator it, TokenIterator end, TypeData &typeData, Ast &ast){
		std::vector<ExprPtr> ret;
		//auto itStr = as<std::string>(*it);

		while(it != end){
			auto expr = parse(it, end, typeData, ast);
			ret.emplace_back(std::move(expr));
		}
		
		return ret;
	}
	
	inline auto parseAll(const std::vector<Token> &toks, TypeData &typeData, Ast &ast){
		return parseAll(cbegin(toks), cend(toks), typeData, ast);
	}

	inline auto parseAll(std::string_view src, TypeData &typeData, Ast &ast){
		auto toks = lexAll(src);
		return parseAll(toks, typeData, ast);
	}
}

#endif // !ILANG_PARSER_HPP

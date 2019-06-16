#ifndef ILANG_PARSER_HPP
#define ILANG_PARSER_HPP 1

#include "ilang/Lexer.hpp"

#include "Expr.hpp"

//! \file

namespace ilang{
	//! Error thrown by parsing functions
	class ParseError: public std::exception{
		public:
			ParseError(std::string msg): m_msg(std::move(msg)){}

			const char *what() const noexcept override{ return m_msg.c_str(); }

		private:
			std::string m_msg;
	};

	//! Abstract syntax tree
	struct Ast{
		auto findLValue(std::string id){
			std::vector<ExprHandle> res;
			for(auto expr : root){
				if(auto lvalue = dynamic_cast<const Exprs::LValue*>(expr)){
					if(lvalue->id() == id)
						res.emplace_back(lvalue);
				}
			}
			
			return res;
		}
		
		std::vector<ExprHandle> root;
		std::vector<std::unique_ptr<Expr>> storage;
		std::map<TypeHandle, std::unique_ptr<Exprs::TypeLiteral>> types;
	};

	//! Data type returned from parse
	struct ParseResult{
		//! The expression that got parsed
		ExprHandle expr = nullptr;
		
		//! The remainder after parsing
		TokenIterator remainder, end;

		//! Type data used in calculations
		TypeData *typeData;
		
		//! Abstract syntax tree
		Ast ast;
	};
	
	//! Parse a single expression
	ParseResult parse(TokenIterator begin, TokenIterator end, TypeData &typeData, Ast ast = Ast());

	inline ParseResult parse(const std::vector<Token> &toks, TypeData &typeData, Ast ast = Ast()){
		return parse(cbegin(toks), cend(toks), typeData, std::move(ast));
	}

	//! Parse another expression using the remainder of the last parse
	inline ParseResult parse(ParseResult parsed){
		return parse(parsed.remainder, parsed.end, *parsed.typeData, std::move(parsed.ast));
	}
	
	inline ParseResult parse(std::string_view src, TypeData &typeData, Ast ast = Ast{}){
		auto toks = lexAll(src);
		return parse(toks, typeData, std::move(ast));
	}

	//! Parse all tokens in a stream
	inline Ast parseAll(TokenIterator begin, TokenIterator end, TypeData &typeData, Ast ast = Ast{}){
		auto res = parse(begin, end, typeData, std::move(ast));
		while(res.expr)
			res = parse(std::move(res));
		
		return std::move(res.ast);
	}
	
	inline Ast parseAll(const std::vector<Token> &toks, TypeData &typeData, Ast ast = Ast{}){
		return parseAll(cbegin(toks), cend(toks), typeData, std::move(ast));
	}

	inline Ast parseAll(std::string_view src, TypeData &typeData, Ast ast = Ast{}){
		auto toks = lexAll(src);
		return parseAll(toks, typeData, std::move(ast));
	}
}

#endif // !ILANG_PARSER_HPP

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
		const Exprs::LValue *findLValue(std::string id){
			auto res = boundNames.find(id);
			if(res != end(boundNames))
				return res->second;
			
			return nullptr;
		}
		
		std::vector<ExprHandle> root;
		std::vector<std::unique_ptr<Expr>> storage;
		std::map<TypeHandle, std::unique_ptr<Exprs::TypeLiteral>> types;
		std::map<std::string, const Exprs::LValue*> boundNames;
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
		Ast *ast;
	};
	
	using RefResolveFn = std::function<ExprPtr(TypeData&, Ast&, const Exprs::UnresolvedRef*)>;
	
	inline ExprPtr defaultRefResolve(TypeData &typeData, Ast &ast, const Exprs::UnresolvedRef *unresolved){
		auto lvalue = ast.findLValue(unresolved->name);
		if(lvalue)
			return std::make_unique<Exprs::ResolvedRef>(lvalue);
		
		auto type = findTypeByString(typeData, unresolved->name);
		if(type){
			auto ret = std::make_unique<Exprs::TypeLiteral>(ast.types[findTypeType(typeData)].get());
			ret->value = type;
			return ret;
		}
		
		return nullptr;
	}
	
	//! Parse a single expression
	ParseResult parse(TokenIterator begin, TokenIterator end, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve);

	inline ParseResult parse(const std::vector<Token> &toks, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve){
		return parse(cbegin(toks), cend(toks), typeData, ast, std::move(refResolve));
	}

	//! Parse another expression using the remainder of the last parse
	inline ParseResult parse(ParseResult parsed, RefResolveFn refResolve){
		return parse(parsed.remainder, parsed.end, *parsed.typeData, *parsed.ast, std::move(refResolve));
	}
	
	inline ParseResult parse(std::string_view src, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve){
		auto toks = lexAll(src);
		return parse(toks, typeData, ast, std::move(refResolve));
	}

	//! Parse all tokens in a stream
	inline Ast &parseAll(TokenIterator begin, TokenIterator end, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve){
		auto res = parse(begin, end, typeData, ast, std::move(refResolve));
		while(res.expr)
			res = parse(std::move(res), std::move(refResolve));
		
		return ast;
	}
	
	inline Ast &parseAll(const std::vector<Token> &toks, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve){
		return parseAll(cbegin(toks), cend(toks), typeData, ast, std::move(refResolve));
	}

	inline Ast &parseAll(std::string_view src, TypeData &typeData, Ast &ast, RefResolveFn refResolve = defaultRefResolve){
		auto toks = lexAll(src);
		return parseAll(toks, typeData, ast, std::move(refResolve));
	}
}

#endif // !ILANG_PARSER_HPP

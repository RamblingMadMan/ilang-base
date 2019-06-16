#include <functional>

#include <gmp.h>
#include <mpfr.h>

#include "ilang/Parser.hpp"
#include "ilang/AInt.hpp"
#include "ilang/AReal.hpp"

using namespace ilang;

using DelimPred = std::function<bool(const Token&)>;

struct ParsedExpr{
	std::unique_ptr<Expr> expr;
	TokenIterator it;
};

template<typename T>
ParsedExpr parse_result(std::unique_ptr<T> &ptr, TokenIterator it){
	return {std::move(ptr), it};
}

ParsedExpr parseExpr(TypeData &typeData, Ast &ast, TokenIterator it, TokenIterator end, DelimPred isDelim);

auto storeExpr(Ast &ast, std::unique_ptr<Expr> ptr){
	return ast.storage.emplace_back(std::move(ptr)).get();
}

auto refType(const TypeData &typeData, Ast &ast, TypeHandle type){
	auto res = ast.types.find(type);
	if(res != end(ast.types))
		return std::make_unique<Exprs::TypeRef>(res->second.get());
	
	const Exprs::TypeLiteral *typePtr;
	std::unique_ptr<Exprs::TypeLiteral> newType;
	
	if(isTypeType(type, typeData)){
		auto typeType = ast.types.find(type);
		if(typeType != end(ast.types))
			typePtr = typeType->second.get();
		else{
			newType = std::make_unique<Exprs::TypeLiteral>(nullptr);
			newType->typeType = newType.get();
			typePtr = newType.get();
		}
	}
	else{
		newType = std::make_unique<Exprs::TypeLiteral>(ast.types[findTypeType(typeData)].get());
		typePtr = newType.get();
	}
	
	newType->value = type;
	ast.types[type] = std::move(newType);
	return std::make_unique<Exprs::TypeRef>(typePtr);
}

int binopPrec(std::string op){
	constexpr auto start = __COUNTER__;
	
#define EXPAND(x) x
#define PREC_COUNTER EXPAND(__COUNTER__) - start
	
	if(op == "^") return PREC_COUNTER;
	else if(op == "*" || op == "/") return PREC_COUNTER;
	else if(op == "+") return PREC_COUNTER;
	else if(op == "-") return PREC_COUNTER;
	else if(op == "<" || op == ">" || op == ">=" || op == "<=" || op == "<=>") return PREC_COUNTER;
	else if(op == "==") return PREC_COUNTER;
	else if(op == "|" || op == "&") return PREC_COUNTER;
	else if(op == "=") return PREC_COUNTER;
	else if(op == ",") return PREC_COUNTER;
	else return std::numeric_limits<int>::max();
}

std::unique_ptr<Exprs::BinOp> sortBinop(TypeData &typeData, Ast &ast, std::unique_ptr<Exprs::BinOp> binop){
	auto binopRhs = dynamic_cast<Exprs::BinOp*>(binop->rhs.get());
	if(binopRhs){
		auto lhsPrec = binopPrec(binop->op);
		auto rhsPrec = binopPrec(binopRhs->op);
		
		if(lhsPrec < rhsPrec){
			auto ptr = static_cast<Exprs::BinOp*>(binop->rhs.release());
			std::unique_ptr<Exprs::BinOp> oldRhs(ptr);
			
			binop->rhs = std::move(binopRhs->lhs);
			
			binop = sortBinop(typeData, ast, std::move(binop));
			
			binop->resultType = std::make_unique<Exprs::BinOpType>();
			binop->resultType->lhsTypeExpr = binop->lhs->typeExpr();
			binop->resultType->rhsTypeExpr = binop->rhs->typeExpr();
			binop->resultType->op = binop->op;
						
			binopRhs->lhs = std::move(binop);
			
			binopRhs->resultType = std::make_unique<Exprs::BinOpType>();
			binopRhs->resultType->rhsTypeExpr = binopRhs->rhs->typeExpr();
			binopRhs->resultType->lhsTypeExpr = binopRhs->lhs->typeExpr();
			binopRhs->resultType->op = binopRhs->op;
						
			return oldRhs;
		}
	}
	
	binop->resultType = std::make_unique<Exprs::BinOpType>();
	binop->resultType->lhsTypeExpr = binop->lhs->typeExpr();
	binop->resultType->rhsTypeExpr = binop->rhs->typeExpr();
	binop->resultType->op = binop->op;
	
	return binop;
}

ParsedExpr parseBinaryOp(
	TypeData &typeData, Ast &ast, std::unique_ptr<Expr> lhs, std::string op,
	TokenIterator it, TokenIterator end, DelimPred isDelim
){
	auto storePtr = [&ast](auto &ptr){
		return storeExpr(ast, std::move(ptr));
	};
	
	auto newExpr = std::make_unique<Exprs::BinOp>();
	newExpr->lhs = std::move(lhs);
	if(!newExpr->lhs)
		throw ParseError("internal error: nullptr for left side of binary op");
	
	newExpr->op = std::move(op);
	
	auto[rhs, newIt] = parseExpr(typeData, ast, it, end, isDelim);
	
	newExpr->rhs = std::move(rhs);
	if(!newExpr->rhs)
		throw ParseError("internal error: nullptr for right side of binary op");
	
	auto res = sortBinop(typeData, ast, std::move(newExpr));
	
	return parse_result(res, newIt);
}

ParsedExpr parseLeadingValue(
	TypeData &typeData, Ast &ast, std::unique_ptr<Expr> value,
	TokenIterator it, TokenIterator end, DelimPred isDelim
){
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	switch(it->type){
		case TokenType::op:{
			return parseBinaryOp(typeData, ast, std::move(value), it->value, it+1, end, std::move(isDelim));
		}
			
			// Application
			/*
		case TokenType::id:
		case TokenType::groupL:
			*/
			
		default:
			throw ParseError("Unexpected token " + as<std::string>(*it) + " after " + value->typeExpr()->toString()+ " value");
	}
}

ParsedExpr parseLiteralInner(
	TypeData &typeData, Ast &ast, std::unique_ptr<Expr> lit,
	TokenIterator it, TokenIterator end, DelimPred isDelim
){
	if(it == end || isDelim(*it)) return parse_result(lit, it);
	return parseLeadingValue(typeData, ast, std::move(lit), it, end, std::move(isDelim));
}

ParsedExpr parseIntLiteral(TypeData &typeData, Ast &ast, std::string lit, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto intLit = std::make_unique<Exprs::IntLiteral>(std::move(lit));
	intLit->intType = refType(typeData, ast, findIntegerType(typeData));
	return parseLiteralInner(typeData, ast, std::move(intLit), it, end, std::move(isDelim));
}

ParsedExpr parseRealLiteral(TypeData &typeData, Ast &ast, std::string lit, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto realLit = std::make_unique<Exprs::RealLiteral>(std::move(lit));
	realLit->realType = refType(typeData, ast, findRealType(typeData));
	return parseLiteralInner(typeData, ast, std::move(realLit), it, end, std::move(isDelim));
}

ParsedExpr parseStrLiteral(TypeData &typeData, Ast &ast, std::string lit, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto strLit = std::make_unique<Exprs::StringLiteral>(std::move(lit));
	strLit->strType = refType(typeData, ast, findStringType(typeData));
	return parseLiteralInner(typeData, ast, std::move(strLit), it, end, std::move(isDelim));
}

ParsedExpr parseListLiteral(TypeData &typeData, Ast &ast, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	auto inner = parseExpr(typeData, ast, it, end, [](const Token &tok){ return tok.type == TokenType::listR; });
	
	std::unique_ptr<Expr> result;
	
	if(inner.expr){
		std::vector<std::unique_ptr<Expr>> innerExprs;
		
		while(auto binop = dynamic_cast<Exprs::BinOp*>(inner.expr.get())){
			if(binop->op == ","){
				innerExprs.emplace_back(std::move(binop->lhs));
				inner.expr = std::move(binop->rhs);
			}
			else{
				break;
			}
		}
		
		innerExprs.emplace_back(std::move(inner.expr));
		
		std::vector<TypeExprHandle> innerTypeExprs;
		innerTypeExprs.resize(innerExprs.size());
		
		std::transform(
			begin(innerExprs), std::end(innerExprs),
			begin(innerTypeExprs),
			[](auto &&expr){ return expr->typeExpr(); }
		);
		
		auto list = std::make_unique<Exprs::ListLiteral>(std::move(innerExprs));
		
		list->listType = std::make_unique<Exprs::ListType>(ast.types[findTypeType(typeData)].get());
		list->listType->elementTypes = std::move(innerTypeExprs);
		
		result = std::move(list);
	}
	else{
		auto list = std::make_unique<Exprs::ListLiteral>();
		list->listType = std::make_unique<Exprs::ListType>(ast.types[findTypeType(typeData)].get());
		result = std::move(list);
	}
	
	it = inner.it+1;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	return parseLeadingValue(typeData, ast, std::move(result), it, end, std::move(isDelim));
}

ParsedExpr parseGroupLiteral(TypeData &typeData, Ast &ast, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	auto inner = parseExpr(typeData, ast, it, end, [](const Token &tok){ return tok.type == TokenType::groupR; });
	
	std::unique_ptr<Expr> result;
	
	if(inner.expr){
		std::vector<std::unique_ptr<Expr>> innerExprs;
		
		while(auto binop = dynamic_cast<Exprs::BinOp*>(inner.expr.get())){
			if(binop->op == ","){
				innerExprs.emplace_back(std::move(binop->lhs));
				inner.expr = std::move(binop->rhs);
			}
			else{
				break;
			}
		}
		
		innerExprs.emplace_back(std::move(inner.expr));
		
		auto product = std::make_unique<Exprs::ProductLiteral>();
		
		std::vector<TypeExprHandle> innerTypes;
		innerTypes.resize(innerExprs.size());
		
		std::transform(
			begin(innerExprs), std::end(innerExprs),
			begin(innerTypes),
			[](auto &&expr){ return expr->typeExpr(); }
		);
		
		product->productType = std::make_unique<Exprs::ProductType>(ast.types[findTypeType(typeData)].get());
		product->productType->innerTypes = std::move(innerTypes);
		
		product->elements = std::move(innerExprs);
		result = std::move(product);
	}
	else{
		auto unit = std::make_unique<Exprs::UnitLiteral>();
		unit->unitType = refType(typeData, ast, findUnitType(typeData));
		result = std::move(unit);
	}
	
	it = inner.it+1;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	return parseLeadingValue(typeData, ast, std::move(result), it, end, std::move(isDelim));
}

std::vector<std::unique_ptr<Exprs::ParamDecl>> makeParams(TypeData &typeData, std::unique_ptr<Exprs::ProductLiteral> paramsGroup){
	auto paramExprs = std::move(paramsGroup->elements);
	
	std::vector<std::unique_ptr<Exprs::ParamDecl>> ret;
	ret.reserve(paramExprs.size());
	
	for(auto &&param : paramExprs){
		auto newParam = std::make_unique<Exprs::ParamDecl>();

		if(auto unresolved = dynamic_cast<Exprs::UnresolvedRef*>(param.get())){
			newParam->name = unresolved->name;
			newParam->type = std::move(unresolved->uniqueType);
		}
		else if(auto resolved = dynamic_cast<Exprs::UnresolvedRef*>(param.get())){
			throw ParseError("Parameter name conflicts with a previus binding");
		}
		else if(auto binop = dynamic_cast<Exprs::BinOp*>(param.get())){
			if(binop->op != ":")
				throw ParseError("Unexpected operator in parameter declaration");
			
			auto &&typeExpr = binop->rhs;
			auto &&nameExpr = binop->lhs;
			
			if(auto unresolved = dynamic_cast<const Exprs::UnresolvedRef*>(nameExpr.get()))
				newParam->name = unresolved->name;
			else if(auto resolved = dynamic_cast<const Exprs::UnresolvedRef*>(nameExpr.get()))
				throw ParseError("Parameter name conflicts with a previus binding");
			else
				throw ParseError("Expected parameter name");
			
			auto type = dynamic_cast<TypeExpr*>(typeExpr.get());
			if(!type)
				throw ParseError("Expected a type expression");
		
			newParam->type = std::unique_ptr<TypeExpr>(type);
			
			typeExpr.release();
		}
		
		ret.emplace_back(std::move(newParam));
	}
	
	return ret;
}

ParsedExpr parseFnDecl(
	TypeData &typeData, Ast &ast,
	std::string id, std::unique_ptr<Exprs::ProductLiteral> paramsGroup,
	TokenIterator it, TokenIterator end,
	DelimPred isDelim
){	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)){}
	
	throw ParseError("Function declarations currently unimplemented");
}

ParsedExpr parseId(TypeData &typeData, Ast &ast, std::string id, TokenIterator it, TokenIterator end, DelimPred isDelim){
	std::unique_ptr<Expr> value;
	
	if(auto type = findTypeByString(typeData, id)){
		auto typeLit = std::make_unique<Exprs::TypeLiteral>(ast.types[findTypeType(typeData)].get());
		typeLit->value = type;
		value = std::move(typeLit);
	}
	else if(auto res = ast.findLValue(id); res.empty()){
		auto unresolved = std::make_unique<Exprs::UnresolvedRef>();
		
		auto partialType = getPartialType(typeData);
		
		unresolved->uniqueType = refType(typeData, ast, partialType);
		unresolved->name = id;
		
		value = std::move(unresolved);
	}
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	if(it->type == TokenType::groupL){
		// function declaration
		auto paramsParsed = parseGroupLiteral(
			typeData, ast,
			it+1, end,
			[](const Token &t){
				if(t.type == TokenType::space)
					return true;
				else if(t.type == TokenType::op && t.value == "=")
					return true;
				
				return false;
			}
		);

		auto groupPtr = static_cast<Exprs::ProductLiteral*>(paramsParsed.expr.release());
		
		it = paramsParsed.it+1;
		
		return parseFnDecl(
			typeData, ast,
			std::move(id), std::unique_ptr<Exprs::ProductLiteral>(groupPtr),
			it, end,
			std::move(isDelim)
		);
	}
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	return parseLeadingValue(typeData, ast, std::move(value), it, end, std::move(isDelim));
}

ParsedExpr parseExpr(TypeData &typeData, Ast &ast, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) return {nullptr, it};

	while(it != end && it->type == TokenType::space)
		++it;

	if(it == end || isDelim(*it)) return {nullptr, it};

	switch(it->type){
		case TokenType::eof: return {nullptr, it};
		case TokenType::id: return parseId(typeData, ast, it->value, it+1, end, std::move(isDelim));
		case TokenType::int_: return parseIntLiteral(typeData, ast, it->value, it+1, end, std::move(isDelim));
		case TokenType::real: return parseRealLiteral(typeData, ast, it->value, it+1, end, std::move(isDelim));
		case TokenType::str: return parseStrLiteral(typeData, ast, it->value, it+1, end, std::move(isDelim));
		case TokenType::groupL: return parseGroupLiteral(typeData, ast, it+1, end, std::move(isDelim));
		case TokenType::listL: return parseListLiteral(typeData, ast, it+1, end, std::move(isDelim));
		default: throw ParseError("Unexpected token");
	}
}

ParseResult ilang::parse(TokenIterator it, TokenIterator endIt, TypeData &typeData, Ast ast){
	ParseResult res;

	auto isNewLine = [](const Token &tok) -> bool{
		return tok.type == TokenType::newLine;
	};

	auto parsed = parseExpr(typeData, ast, it, endIt, isNewLine);

	auto handle = parsed.expr.get();
	ast.storage.emplace_back(std::move(parsed.expr));
	ast.root.emplace_back(handle);

	res.expr = handle;

	// must increment iterator because ::parse____ functions do not eat delimiter tokens
	if(parsed.it != endIt)
		++parsed.it;

	res.remainder = parsed.it;
	res.end = endIt;
	
	res.typeData = &typeData;
	res.ast = std::move(ast);

	return res;
}

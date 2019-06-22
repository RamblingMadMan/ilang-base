#include <functional>

#include <gmp.h>
#include <mpfr.h>

#include "utf8.h"

#include "ilang/Parser.hpp"
#include "ilang/AInt.hpp"
#include "ilang/AReal.hpp"

using namespace ilang;

using DelimPred = std::function<bool(const Token&)>;

struct ParsedExpr{
	ExprPtr expr;
	TokenIterator it;
};

template<typename T>
ParsedExpr parse_result(std::unique_ptr<T> &ptr, TokenIterator it){
	return {std::move(ptr), it};
}

struct Indent{
	explicit Indent(std::string_view spaces = ""): numTabs(0), numSpaces(0){
		auto it = cbegin(spaces);
		auto end = cend(spaces);
		
		while(it != end){
			auto cp = utf8::next(it, end);
			if(cp == ' ')
				++numSpaces;
			else if(cp == '\t')
				++numTabs;
			else
				throw ParseError("Unrecognized space character in indentation");
		}
	}
	
	bool operator==(const Indent &other) const{
		return other.numTabs == numTabs && other.numSpaces == numSpaces;
	}
	
	bool operator!=(const Indent &other) const{
		return other.numTabs != numTabs || other.numSpaces != numSpaces;
	}
	
	bool operator<(const Indent &other) const{
		if((numTabs && numSpaces) || (other.numTabs && other.numSpaces))
			throw ParseError("Can't compare mixed (space and tab) indentation");
		else if(numTabs)
			return numTabs < other.numTabs;
		else if(numSpaces)
			return numSpaces < other.numSpaces;
		else if(!other.numTabs && !other.numSpaces)
			return false;
		else
			return true;
	}
	
	std::size_t numTabs, numSpaces;
};

ParsedExpr parseExpr(RefResolveFn refResolve, TypeData &typeData, Ast &ast, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim);

auto storeExpr(Ast &ast, std::unique_ptr<Expr> ptr){
	return ast.storage.emplace_back(std::move(ptr)).get();
}

TypeExprHandle deref(TypeExprHandle typeExpr){
	if(auto typeRef = dynamic_cast<const Exprs::TypeRef*>(typeExpr))
		return deref(typeRef->value);
	else
		return typeExpr;
}

ExprHandle deref(ExprHandle expr){
	if(auto typeExpr = dynamic_cast<TypeExprHandle>(expr))
		return deref(typeExpr);
	else if(auto unresolved = dynamic_cast<const Exprs::UnresolvedRef*>(expr))
		return unresolved;
	else if(auto resolved = dynamic_cast<const Exprs::ResolvedRef*>(expr))
		return deref(resolved->refed);
	else
		return expr;
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

ParsedExpr parseApplication(RefResolveFn refResolve, TypeData &typeData, Ast &ast, ExprPtr callable, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it))
		throw ParseError("Unexpected end of function application");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	// TODO: handle multi-line arguments
	
	std::vector<ExprPtr> args;
	
	auto argsParsed = parseExpr(refResolve, typeData, ast, indent, it, end, isDelim);
	
	it = argsParsed.it;
	
	if(auto argsApp = dynamic_cast<Exprs::Application*>(argsParsed.expr.get())){
		args.reserve(1 + argsApp->args.size());
		args.emplace_back(std::move(argsApp->functor));
		args.insert(
			std::end(args),
			std::make_move_iterator(std::begin(argsApp->args)),
			std::make_move_iterator(std::end(argsApp->args))
		);
	}
	else{
		args.emplace_back(std::move(argsParsed.expr));
	}
	
	auto appExpr = std::make_unique<Exprs::Application>();
	
	auto appType = std::make_unique<Exprs::ApplicationType>();
	appType->appExpr = appExpr.get();
	
	appExpr->functor = std::move(callable);
	appExpr->args = std::move(args);
	appExpr->resultType = std::move(appType);
	
	return parse_result(appExpr, it);
}

ParsedExpr parseBinaryOp(
	RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::unique_ptr<Expr> lhs, std::string op,
	Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim
){
	auto storePtr = [&ast](auto &ptr){
		return storeExpr(ast, std::move(ptr));
	};
	
	auto newExpr = std::make_unique<Exprs::BinOp>();
	newExpr->lhs = std::move(lhs);
	if(!newExpr->lhs)
		throw ParseError("internal error: nullptr for left side of binary op");
	
	newExpr->op = std::move(op);
	
	auto[rhs, newIt] = parseExpr(refResolve, typeData, ast, indent, it, end, isDelim);
	
	newExpr->rhs = std::move(rhs);
	if(!newExpr->rhs)
		throw ParseError("internal error: nullptr for right side of binary op");
	
	auto res = sortBinop(typeData, ast, std::move(newExpr));
	
	return parse_result(res, newIt);
}

ParsedExpr parseLeadingValue(
	RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::unique_ptr<Expr> value,
	Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim
){
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	while(it != end && it->type == TokenType::space){
		++it;
	}
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	switch(it->type){
		case TokenType::op:{
			return parseBinaryOp(std::move(refResolve), typeData, ast, std::move(value), it->value, indent, it+1, end, std::move(isDelim));
		}
			
		// Application
		case TokenType::str:
		case TokenType::id:
		case TokenType::groupL:
		case TokenType::listL:
		case TokenType::int_:
		case TokenType::real:
			return parseApplication(refResolve, typeData, ast, std::move(value), indent, it, end, isDelim);
			
		default:
			throw ParseError("Unexpected token " + as<std::string>(*it) + " after " + value->toString());
	}
}

ParsedExpr parseLiteralInner(
	RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::unique_ptr<Expr> lit,
	Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim
){
	if(it == end || isDelim(*it)) return parse_result(lit, it);
	return parseLeadingValue(std::move(refResolve), typeData, ast, std::move(lit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseIntLiteral(RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto intLit = std::make_unique<Exprs::IntLiteral>(std::move(lit));
	intLit->intType = refType(typeData, ast, findIntegerType(typeData));
	return parseLiteralInner(std::move(refResolve), typeData, ast, std::move(intLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseRealLiteral(RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto realLit = std::make_unique<Exprs::RealLiteral>(std::move(lit));
	realLit->realType = refType(typeData, ast, findRealType(typeData));
	return parseLiteralInner(std::move(refResolve), typeData, ast, std::move(realLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseStrLiteral(RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto strLit = std::make_unique<Exprs::StringLiteral>(std::move(lit));
	strLit->strType = refType(typeData, ast, findStringType(typeData));
	return parseLiteralInner(std::move(refResolve), typeData, ast, std::move(strLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseListLiteral(RefResolveFn refResolve, TypeData &typeData, Ast &ast, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	auto inner = parseExpr(refResolve, typeData, ast, indent, it, end, [](const Token &tok){ return tok.type == TokenType::listR; });
	
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
	
	return parseLeadingValue(std::move(refResolve), typeData, ast, std::move(result), indent, it, end, std::move(isDelim));
}

ParsedExpr parseGroupLiteral(RefResolveFn refResolve, TypeData &typeData, Ast &ast, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	auto inner = parseExpr(refResolve, typeData, ast, indent, it, end, [](const Token &tok){ return tok.type == TokenType::groupR; });
	
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
	
	return parseLeadingValue(std::move(refResolve), typeData, ast, std::move(result), indent, it, end, std::move(isDelim));
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

auto parseBlock(
	RefResolveFn refResolve, TypeData &typeData, Ast &ast,
	Indent indent, TokenIterator it, TokenIterator end,
	DelimPred isDelim
){
	TypeExprPtr blockResType;
	std::vector<TypeExprHandle> resultTypes;
	std::vector<ExprPtr> body;
	while(it != end){
		if(it == end || isDelim(*it))
			break;
		
		if(it->type == TokenType::newLine){
			do{
				++it;
				
				if(it != end && it->type == TokenType::space){
					Indent newIndent(it->value);
					++it;
					
					if(it == end || isDelim(*it))
						break;
					else if(newIndent != indent)
						throw ParseError("Expression at wrong indentation");
					else
						break;
				}
			} while(it != end && it->type == TokenType::newLine);
		}
		
		auto[expr, newIt] = parseExpr(refResolve, typeData, ast, indent, it, end, isDelim);
		
		auto ptr = body.emplace_back(std::move(expr)).get();
		
		if(auto returnExpr = dynamic_cast<const Exprs::Return*>(ptr))
			resultTypes.emplace_back(deref(returnExpr->typeExpr()));
		
		it = newIt;
	}
	
	if(body.empty())
		throw ParseError("Empty expression block");

	if(resultTypes.empty()){
		blockResType = std::make_unique<Exprs::TypeRef>(body.back()->typeExpr());
	}
	else{
		auto resultType = std::make_unique<Exprs::ResultTypes>();
		
		resultType->types.resize(resultTypes.size());
		
		std::transform(
			cbegin(resultTypes), cend(resultTypes),
			begin(resultType->types),
			[](auto ptr){ return std::make_unique<Exprs::TypeRef>(ptr); }
		);
		
		blockResType = std::move(resultType);
	}
	
	if(isDelim(*it))
		++it;
	
	auto newBlock = std::make_unique<Exprs::Block>();
	newBlock->body = std::move(body);
	newBlock->type = std::move(blockResType);
	
	return std::pair{std::move(newBlock), it};
}

ParsedExpr parseFnDecl(
	RefResolveFn refResolve, TypeData &typeData, Ast &ast,
	std::string id, std::unique_ptr<Exprs::ProductLiteral> paramsGroup,
	Indent indent, TokenIterator it, TokenIterator end,
	DelimPred isDelim
){
	auto exprIndent = indent;
	auto newFn = std::make_unique<Exprs::FnDecl>();
	
	newFn->name = std::move(id);
	
	auto params = makeParams(typeData, std::move(paramsGroup));
	
	std::vector<const Exprs::ParamDecl*> paramsSorted;
	paramsSorted.resize(params.size());
	std::transform(
		cbegin(params), cend(params),
		begin(paramsSorted),
		[](auto &&param){ return param.get(); }
	);
	
	std::sort(
		begin(paramsSorted), std::end(paramsSorted),
		[](auto lhs, auto rhs){
			return lhs->name < rhs->name;
		}
	);
	
	auto paramResolver = [&refResolve, &paramsSorted](TypeData &typeData, Ast &ast, const Exprs::UnresolvedRef *unresolved) -> ExprPtr{
		auto it = std::lower_bound(begin(paramsSorted), std::end(paramsSorted), unresolved->name, [](auto lhs, auto name){
			return lhs->name < name;
		});
		
		if(it != std::end(paramsSorted))
			return std::make_unique<Exprs::ResolvedRef>(*it);
		
		return refResolve(typeData, ast, unresolved);
	};
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it->type == TokenType::newLine){
		do{
			++it;
		} while(it != end && it->type == TokenType::newLine);
		
		if(it->type != TokenType::space)
			throw ParseError("Expected indentation before function body");
		
		Indent newIndent(it->value);
		
		if(newIndent < indent || newIndent == indent)
			throw ParseError("Function body's indentation must be greater than its declaration");
		
		exprIndent = newIndent;
		
		++it;
	}
		
	if(it->value != "=")
		throw ParseError("Expected binding after function declaration");
	
	do{		
		++it;
	} while(it != end && it->type == TokenType::space);
	
	ExprPtr fnBody;
	
	if(it->type == TokenType::newLine){
		while(it->type == TokenType::newLine)
			++it;
	
		std::tie(fnBody, it) = parseBlock(paramResolver, typeData, ast, exprIndent, it, end, isDelim);
	}
	else{
		auto[body, newIt] = parseExpr(paramResolver, typeData, ast, exprIndent, it, end, isDelim);
		fnBody = std::move(body);
		it = newIt;
	}
	
	newFn->fnType = std::make_unique<Exprs::FnType>();
	
	newFn->fnType->resultType = std::make_unique<Exprs::TypeRef>(deref(fnBody->typeExpr()));
	newFn->body = std::move(fnBody);
	
	std::vector<TypeExprPtr> paramTypes;
	paramTypes.resize(params.size());
	
	std::transform(
		cbegin(params), cend(params),
		begin(paramTypes),
		[](const auto &ptr){ return std::make_unique<Exprs::TypeRef>(ptr->typeExpr()); }
	);
	
	newFn->fnType->paramTypes = std::move(paramTypes);
	
	newFn->params = std::move(params);
	
	auto ret = std::make_unique<Exprs::ResolvedRef>();
	ret->refed = newFn.get();
	
	ast.boundNames[newFn->name] = newFn.get();
	ast.storage.emplace_back(std::move(newFn));
	
	if(it == end || isDelim(*it))
		return {std::move(ret), it};
		
	return parseLeadingValue(std::move(refResolve), typeData, ast, std::move(ret), indent, it, end, std::move(isDelim));
}

ParsedExpr parseId(RefResolveFn refResolve, TypeData &typeData, Ast &ast, std::string id, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	std::unique_ptr<Expr> value;
	
	if(auto type = findTypeByString(typeData, id)){
		auto typeLit = std::make_unique<Exprs::TypeLiteral>(ast.types[findTypeType(typeData)].get());
		typeLit->value = type;
		value = std::move(typeLit);
	}
	else if(auto lvalue = ast.findLValue(id)){
		value = std::make_unique<Exprs::ResolvedRef>(lvalue);
	}
	else{
		auto unresolved = std::make_unique<Exprs::UnresolvedRef>();
		
		auto partialType = getPartialType(typeData);
		
		unresolved->uniqueType = refType(typeData, ast, partialType);
		unresolved->name = id;
		
		auto resolved = refResolve(typeData, ast, unresolved.get());
		if(resolved)
			value = std::move(resolved);
		else
			value = std::move(unresolved);
	}
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	if(it->type == TokenType::groupL){
		// function declaration
		auto paramsParsed = parseGroupLiteral(
			refResolve, typeData, ast,
			indent, it+1, end,
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
			refResolve, typeData, ast,
			std::move(id), std::unique_ptr<Exprs::ProductLiteral>(groupPtr),
			indent, it, end,
			std::move(isDelim)
		);
	}
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	return parseLeadingValue(std::move(refResolve), typeData, ast, std::move(value), indent, it, end, std::move(isDelim));
}

ParsedExpr parseExpr(RefResolveFn refResolve, TypeData &typeData, Ast &ast, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	// Rummage through newlines and spaces to try and find an expression
	
	if(it == end || isDelim(*it)) return {nullptr, it};

	while(it != end && it->type == TokenType::space)
		++it;

	if(it == end || isDelim(*it)) return {nullptr, it};
	
	if(it->type == TokenType::newLine){
		do{
			++it;
		} while(it != end && it->type == TokenType::newLine);
		
		// At this point we know that if a space or newline comes up it is not a delim
		
		if(it == end || isDelim(*it))
			return {nullptr, it};
		else if(it->type != TokenType::space)
			return {nullptr, it};
		
		Indent newIndent(it->value);
		++it;
		
		if(it == end || isDelim(*it))
			return {nullptr, it};
		else if(newIndent != indent)
			throw ParseError("Wrong indentation before expression");
	}

	switch(it->type){
		case TokenType::eof: return {nullptr, it};
		case TokenType::id: return parseId(std::move(refResolve), typeData, ast, it->value, indent, it+1, end, std::move(isDelim));
		case TokenType::int_: return parseIntLiteral(std::move(refResolve), typeData, ast, it->value, indent, it+1, end, std::move(isDelim));
		case TokenType::real: return parseRealLiteral(std::move(refResolve), typeData, ast, it->value, indent, it+1, end, std::move(isDelim));
		case TokenType::str: return parseStrLiteral(std::move(refResolve), typeData, ast, it->value, indent, it+1, end, std::move(isDelim));
		case TokenType::groupL: return parseGroupLiteral(std::move(refResolve), typeData, ast, indent, it+1, end, std::move(isDelim));
		case TokenType::listL: return parseListLiteral(std::move(refResolve), typeData, ast, indent, it+1, end, std::move(isDelim));
		default: throw ParseError("Unexpected token");
	}
}

ParseResult ilang::parse(TokenIterator it, TokenIterator endIt, TypeData &typeData, Ast &ast, RefResolveFn refResolve){
	ParseResult res;

	auto isNewLine = [](const Token &tok) -> bool{
		return tok.type == TokenType::newLine;
	};

	auto parsed = parseExpr(std::move(refResolve), typeData, ast, Indent(), it, endIt, isNewLine);

	auto handle = parsed.expr.get();
	ast.storage.emplace_back(std::move(parsed.expr)).get();
	ast.root.emplace_back(handle);

	res.expr = handle;

	// must increment iterator because ::parse____ functions do not eat delimiter tokens
	if(parsed.it != endIt)
		++parsed.it;

	res.remainder = parsed.it;
	res.end = endIt;
	
	res.typeData = &typeData;
	res.ast = &ast;

	return res;
}

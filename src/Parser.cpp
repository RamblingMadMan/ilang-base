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

ParsedExpr parseExpr(TypeData &typeData, Scope &scope, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim);

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

auto refType(const TypeData &typeData, Scope &scope, TypeHandle type){
	if(!type)
		throw ParseError("internal error: (refType) nullptr given for type");

	auto res = scope.resolveType(type);
	if(res)
		return std::make_unique<Exprs::TypeRef>(res);

	auto newType = std::make_unique<Exprs::TypeLiteral>(nullptr);
	auto typePtr = newType.get();
	
	if(isTypeType(type, typeData)){
		newType->typeType = newType.get();
	}
	else{
		newType->typeType = scope.types[findTypeType(typeData)].get();
	}
	
	newType->value = type;
	scope.types[type] = std::move(newType);
	return std::make_unique<Exprs::TypeRef>(typePtr);
}

const Exprs::TypeLiteral *typeTypeLit(const TypeData &typeData, Scope &scope){
	auto typeRef = refType(typeData, scope, findTypeType(typeData));
	return static_cast<const Exprs::TypeLiteral*>(deref(typeRef.get()));
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
	else if(op == ":") return PREC_COUNTER;
	else if(op == ",") return PREC_COUNTER;
	else return std::numeric_limits<int>::max();
}

std::unique_ptr<Exprs::BinOp> sortBinop(TypeData &typeData, std::unique_ptr<Exprs::BinOp> binop){
	auto binopRhs = dynamic_cast<Exprs::BinOp*>(binop->rhs.get());
	if(binopRhs){
		auto lhsPrec = binopPrec(binop->op);
		auto rhsPrec = binopPrec(binopRhs->op);
		
		if(lhsPrec < rhsPrec){
			auto ptr = static_cast<Exprs::BinOp*>(binop->rhs.release());
			std::unique_ptr<Exprs::BinOp> oldRhs(ptr);
			
			binop->rhs = std::move(binopRhs->lhs);
			
			binop = sortBinop(typeData, std::move(binop));
			
			binop->resultType = std::make_unique<Exprs::BinOpType>();
			binop->resultType->lhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binop->lhs->typeExpr()));
			binop->resultType->rhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binop->rhs->typeExpr()));
			binop->resultType->op = binop->op;
						
			binopRhs->lhs = std::move(binop);
			
			binopRhs->resultType = std::make_unique<Exprs::BinOpType>();
			binopRhs->resultType->rhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binopRhs->rhs->typeExpr()));
			binopRhs->resultType->lhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binopRhs->lhs->typeExpr()));
			binopRhs->resultType->op = binopRhs->op;
						
			return oldRhs;
		}
	}
	
	binop->resultType = std::make_unique<Exprs::BinOpType>();
	binop->resultType->lhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binop->lhs->typeExpr()));
	binop->resultType->rhsTypeExpr = std::make_unique<Exprs::TypeRef>(deref(binop->rhs->typeExpr()));
	binop->resultType->op = binop->op;
	
	return binop;
}

ParsedExpr parseApplication(TypeData &typeData, Scope &scope, ExprPtr callable, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it))
		throw ParseError("Unexpected end of function application");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	// TODO: handle multi-line arguments
	
	std::vector<ExprPtr> args;
	
	auto argsParsed = parseExpr(typeData, scope, indent, it, end, isDelim);
	
	it = argsParsed.it;
	
	if(auto argsApp = dynamic_cast<Exprs::Application*>(argsParsed.expr.get())){
		args.reserve(1 + argsApp->args.size());
		args.emplace_back(std::move(argsApp->functor));
		std::transform(
			std::begin(argsApp->args), std::end(argsApp->args),
			std::back_inserter(args),
			[](auto &&arg){
				return std::move(arg);
			}
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
	TypeData &typeData, Scope &scope,
	std::unique_ptr<Expr> lhs, std::string op,
	Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim
){
	auto newExpr = std::make_unique<Exprs::BinOp>();
	newExpr->lhs = std::move(lhs);
	if(!newExpr->lhs)
		throw ParseError("internal error: nullptr for left side of binary op");
	
	newExpr->op = std::move(op);
	
	auto[rhs, newIt] = parseExpr(typeData, scope, indent, it, end, isDelim);
	
	newExpr->rhs = std::move(rhs);
	if(!newExpr->rhs)
		throw ParseError("internal error: nullptr for right side of binary op");
	
	auto res = sortBinop(typeData, std::move(newExpr));
	
	return parse_result(res, newIt);
}

ParsedExpr parseLeadingValue(
	TypeData &typeData, Scope &scope,
	std::unique_ptr<Expr> value,
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
			return parseBinaryOp(typeData, scope, std::move(value), it->value, indent, it+1, end, std::move(isDelim));
		}
			
		// Application
		case TokenType::str:
		case TokenType::id:
		case TokenType::groupL:
		case TokenType::listL:
		case TokenType::int_:
		case TokenType::real:
			return parseApplication(typeData, scope, std::move(value), indent, it, end, isDelim);
			
		default:
			throw ParseError("Unexpected token " + as<std::string>(*it) + " after " + value->toString());
	}
}

ParsedExpr parseLiteralInner(
	TypeData &typeData, Scope &scope,
	std::unique_ptr<Expr> lit,
	Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim
){
	if(it == end || isDelim(*it)) return parse_result(lit, it);
	return parseLeadingValue(typeData, scope, std::move(lit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseIntLiteral(TypeData &typeData, Scope &scope, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto intLit = std::make_unique<Exprs::IntLiteral>(std::move(lit));
	intLit->intType = refType(typeData, scope, findIntegerType(typeData));
	return parseLiteralInner(typeData, scope, std::move(intLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseRealLiteral(TypeData &typeData, Scope &scope, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto realLit = std::make_unique<Exprs::RealLiteral>(std::move(lit));
	realLit->realType = refType(typeData, scope, findRealType(typeData));
	return parseLiteralInner(typeData, scope, std::move(realLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseStrLiteral(TypeData &typeData, Scope &scope, std::string lit, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	auto strLit = std::make_unique<Exprs::StringLiteral>(std::move(lit));
	strLit->strType = refType(typeData, scope, findStringType(typeData));
	return parseLiteralInner(typeData, scope, std::move(strLit), indent, it, end, std::move(isDelim));
}

ParsedExpr parseListLiteral(TypeData &typeData, Scope &scope, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of list literal");
	
	auto inner = parseExpr(typeData, scope, indent, it, end, [](const Token &tok){ return tok.type == TokenType::listR; });
	
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
		
		std::vector<TypeExprPtr> innerTypeExprs;
		innerTypeExprs.reserve(innerExprs.size());
		
		std::transform(
			begin(innerExprs), std::end(innerExprs),
			std::back_inserter(innerTypeExprs),
			[](auto &&expr){ return std::make_unique<Exprs::TypeRef>(deref(expr->typeExpr())); }
		);
		
		auto list = std::make_unique<Exprs::ListLiteral>(std::move(innerExprs));
		
		list->listType = std::make_unique<Exprs::ListType>(typeTypeLit(typeData, scope));
		list->listType->elementTypes = std::move(innerTypeExprs);
		
		result = std::move(list);
	}
	else{
		auto list = std::make_unique<Exprs::ListLiteral>();
		list->listType = std::make_unique<Exprs::ListType>(typeTypeLit(typeData, scope));
		result = std::move(list);
	}
	
	it = inner.it+1;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	return parseLeadingValue(typeData, scope, std::move(result), indent, it, end, std::move(isDelim));
}

ParsedExpr parseGroupLiteral(TypeData &typeData, Scope &scope, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) throw ParseError("Unexpected end of group literal");
	
	auto inner = parseExpr(typeData, scope, indent, it, end, [](const Token &tok){ return tok.type == TokenType::groupR; });
	
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
		
		std::vector<TypeExprPtr> innerTypes;
		innerTypes.reserve(innerExprs.size());
		
		std::transform(
			begin(innerExprs), std::end(innerExprs),
			std::back_inserter(innerTypes),
			[](auto &&expr){ return std::make_unique<Exprs::TypeRef>(deref(expr->typeExpr())); }
		);

		product->productType = std::make_unique<Exprs::ProductType>(typeTypeLit(typeData, scope));
		product->productType->innerTypes = std::move(innerTypes);
		
		product->elements = std::move(innerExprs);
		result = std::move(product);
	}
	else{
		auto unit = std::make_unique<Exprs::UnitLiteral>();
		unit->unitType = refType(typeData, scope, findUnitType(typeData));
		result = std::move(unit);
	}
	
	if(inner.it != end && inner.it->type == TokenType::groupR)
		++inner.it;

	it = inner.it;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it)) return parse_result(result, it);
	
	return parseLeadingValue(typeData, scope, std::move(result), indent, it, end, std::move(isDelim));
}

std::vector<std::unique_ptr<Exprs::ParamDecl>> makeParams(TypeData &typeData, std::unique_ptr<Exprs::ProductLiteral> paramsGroup){
	std::vector<std::unique_ptr<Exprs::ParamDecl>> ret;
	ret.reserve(paramsGroup->elements.size());
	
	for(auto &&param : paramsGroup->elements){
		auto newParam = std::make_unique<Exprs::ParamDecl>();

		if(auto unresolved = dynamic_cast<Exprs::UnresolvedRef*>(param.get())){
			newParam->name = unresolved->name;
			auto paramTyStr = unresolved->uniqueType->toString();
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
	TypeData &typeData, Scope &scope,
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
		
		auto[expr, newIt] = parseExpr(typeData, scope, indent, it, end, isDelim);
		
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
	TypeData &typeData, Scope &scope,
	std::string id, std::unique_ptr<Exprs::ProductLiteral> paramsGroup,
	Indent indent, TokenIterator it, TokenIterator end,
	DelimPred isDelim
){
	auto exprIndent = indent;
	auto newFn = std::make_unique<Exprs::FnDecl>();
	
	newFn->name = std::move(id);
	
	auto params = makeParams(typeData, std::move(paramsGroup));
	std::vector<TypeExprPtr> paramTypes;

	paramTypes.reserve(params.size());

	auto &&fnScope = scope.children.emplace_back(std::make_unique<Scope>(&scope));

	for(auto &&param : params){
		auto paramTypeStr = param->typeExpr()->toString();
		fnScope->boundNames[param->name] = std::make_unique<Exprs::ResolvedRef>(param.get());
		paramTypes.emplace_back(std::make_unique<Exprs::TypeRef>(deref(param->typeExpr())));
	}
	
	while(it != end && it->type == TokenType::space)
		++it;

	if(it->value != "=")
		throw ParseError("Expected binding after function declaration");
	
	++it;

	if(it->type == TokenType::newLine){
		do{
			++it;
			if(it != end && it->type == TokenType::space){
				auto nextIt = it+1;
				if(nextIt == end)
					break;
				else if(nextIt->type == TokenType::newLine){
					++it;
					continue;
				}
			}
		} while(it != end && it->type == TokenType::newLine);
		
		if(it->type != TokenType::space)
			throw ParseError("Expected indentation before function body");
		
		Indent newIndent(it->value);
		
		if(newIndent < indent || newIndent == indent)
			throw ParseError("Function body's indentation must be greater than its declaration");
		
		exprIndent = newIndent;
		
		++it;
	}
	else{
		do{
			++it;
		} while(it != end && it->type == TokenType::space);
	}

	ExprPtr fnBody;
	
	bool indentSet = false;

	if(it->type == TokenType::newLine){
		while(1){
			do {
				++it;
			} while (it->type == TokenType::newLine);

			if(it->type != TokenType::space)
				throw ParseError("Expected indentation before function body");

			Indent newIndent(it->value);

			if(newIndent < indent || newIndent == indent)
				throw ParseError("Function body's indentation must be greater than its declaration");

			++it;

			if(it != end && it->type == TokenType::newLine)
				continue;

			if(!indentSet){
				exprIndent = newIndent;
				indentSet = true;
			}

			std::tie(fnBody, it) = parseBlock(typeData, *fnScope, exprIndent, it, end, isDelim);
		}
	}
	else{
		auto[body, newIt] = parseExpr(typeData, *fnScope, indent, it, end, isDelim);
		fnBody = std::move(body);
		it = newIt;
	}
	
	newFn->fnType = std::make_unique<Exprs::FnType>();
	
	newFn->fnType->resultType = std::make_unique<Exprs::TypeRef>(deref(fnBody->typeExpr()));
	newFn->body = std::move(fnBody);
	
	newFn->fnType->paramTypes = std::move(paramTypes);
	newFn->params = std::move(params);

	auto ret = std::make_unique<Exprs::ResolvedRef>(newFn.get());
	
	scope.boundNames[newFn->name] = std::move(newFn);

	if(it == end || isDelim(*it))
		return {std::move(ret), it};
		
	return parseLeadingValue(typeData, scope, std::move(ret), indent, it, end, std::move(isDelim));
}

ParsedExpr parseId(TypeData &typeData, Scope &scope, std::string id, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
	std::unique_ptr<Expr> value;

	if(auto type = findTypeByString(typeData, id)){
		auto typeLit = std::make_unique<Exprs::TypeLiteral>(typeTypeLit(typeData, scope));
		typeLit->value = type;
		value = std::move(typeLit);
	}
	else if(auto resolved = deref(scope.resolveName(id))){
		if(auto lvalue = dynamic_cast<const Exprs::LValue*>(resolved)){
			value = std::make_unique<Exprs::ResolvedRef>(lvalue);
		}
		else {
			value = std::make_unique<Exprs::ValueRef>(resolved);
		}
	}
	else{
		auto unresolved = std::make_unique<Exprs::UnresolvedRef>();
		
		auto partialType = getPartialType(typeData);

		unresolved->uniqueType = refType(typeData, scope, partialType);
		unresolved->name = id;

		value = std::move(unresolved);
	}
	
	auto valueStr = value->toString();

	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	if(it->type == TokenType::groupL){
		// function declaration
		auto paramsParsed = parseGroupLiteral(
			typeData, scope,
			indent, it+1, end,
			[&](const Token &t){
				if(t.type == TokenType::op && t.value == "=")
					return true;
				else
					return isDelim(t);
			}
		);

		auto groupPtr = static_cast<Exprs::ProductLiteral*>(paramsParsed.expr.release());

		it = paramsParsed.it;
		
		return parseFnDecl(
			typeData, scope,
			std::move(id), std::unique_ptr<Exprs::ProductLiteral>(groupPtr),
			indent, it, end,
			std::move(isDelim)
		);
	}
	
	while(it != end && it->type == TokenType::space)
		++it;
	
	if(it == end || isDelim(*it))
		return parse_result(value, it);
	
	return parseLeadingValue(typeData, scope, std::move(value), indent, it, end, std::move(isDelim));
}

ParsedExpr parseExpr(TypeData &typeData, Scope &scope, Indent indent, TokenIterator it, TokenIterator end, DelimPred isDelim){
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

		if(indent.numTabs || indent.numSpaces){
			if(it->type != TokenType::space)
				throw ParseError("Expected indentation after new line");

			Indent newIndent(it->value);
			++it;

			if(it == end || isDelim(*it))
				return {nullptr, it};
			else if(it->type == TokenType::newLine)
				return parseExpr(typeData, scope, indent, it, end, std::move(isDelim));
			else if(newIndent != indent)
				throw ParseError("Wrong indentation before expression");
		}
	}

	auto nextIt = it == end ? it : it+1;

	switch(it->type){
		case TokenType::eof: return {nullptr, it};
		case TokenType::id: return parseId(typeData, scope, as<std::string>(*it), indent, nextIt, end, std::move(isDelim));
		case TokenType::int_: return parseIntLiteral(typeData, scope, as<std::string>(*it), indent, nextIt, end, std::move(isDelim));
		case TokenType::real: return parseRealLiteral(typeData, scope, as<std::string>(*it), indent, nextIt, end, std::move(isDelim));
		case TokenType::str: return parseStrLiteral(typeData, scope, it->value, indent, nextIt, end, std::move(isDelim));
		case TokenType::groupL: return parseGroupLiteral(typeData, scope, indent, nextIt, end, std::move(isDelim));
		case TokenType::listL: return parseListLiteral(typeData, scope, indent, nextIt, end, std::move(isDelim));
		default: throw ParseError("Unexpected token");
	}
}

ExprPtr ilang::parse(TokenIterator &it, TokenIterator endIt, TypeData &typeData, Ast &ast){
	auto isNewLine = [](const Token &tok) -> bool{
		return tok.type == TokenType::newLine;
	};

	auto parsed = parseExpr(typeData, ast.rootScope, Indent(), it, endIt, isNewLine);

	// must increment iterator because ::parse____ functions do not eat delimiter tokens
	if(parsed.it != endIt)
		++parsed.it;

	it = parsed.it;

	return std::move(parsed.expr);
}

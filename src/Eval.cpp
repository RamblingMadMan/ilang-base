#include <iostream>
#include <functional>
#include <string_view>

#include "ilang/Eval.hpp"
#include "ilang/Util.hpp"

using namespace std::string_view_literals;
using namespace ilang;

ResultPtr evalExpr(ExprHandle expr, TypeData &typeData, ResultScope &scope);

ResultPtr evalFnDecl(const Exprs::FnDecl *decl, TypeData &typeData, ResultScope &scope);

ResultPtr evalApplication(const Exprs::Application *app, TypeData &typeData, ResultScope &scope){
	auto fnRes = evalExpr(app->functor.get(), typeData, scope);
	
	std::vector<ResultPtr> argResults;
	argResults.reserve(app->args.size());
	
	for(auto &&arg : app->args)
		argResults.emplace_back(evalExpr(arg.get(), typeData, scope));
	
	return fnRes->call(typeData, std::move(argResults));
}

ResultPtr evalListLiteral(const Exprs::ListLiteral *list, TypeData &typeData, ResultScope &scope){
	std::vector<ResultPtr> results;
	results.reserve(list->elements.size());
	for(auto &&expr : list->elements)
		results.emplace_back(evalExpr(expr.get(), typeData, scope));
	
	auto result = std::make_unique<ListResult>();
	result->values = std::move(results);
	
	return result;
}

ResultPtr evalProductLiteral(const Exprs::ProductLiteral *product, TypeData &typeData, ResultScope &scope){
	std::vector<ResultPtr> results;
	results.reserve(product->elements.size());
	for(auto &&expr : product->elements)
		results.emplace_back(evalExpr(expr.get(), typeData, scope));
	
	if(results.size() == 1)
		return std::move(results[0]);
	
	auto result = std::make_unique<ProductResult>();
	result->values = std::move(results);
	
	return result;
}

ResultPtr evalLiteral(const Exprs::Literal *lit, TypeData &typeData, ResultScope &scope){
	auto type = lit->typeExpr()->resolve(typeData, [](TypeData&, TypeHandle partial){ throw EvalError("Partial typing currently unimplemented"); return partial; });
	
	if(auto list = dynamic_cast<const Exprs::ListLiteral*>(lit))
		return evalListLiteral(list, typeData, scope);
	else if(auto product = dynamic_cast<const Exprs::ProductLiteral*>(lit))
		return evalProductLiteral(product, typeData, scope);
	else if(isUnitType(type, typeData))
		return std::make_unique<UnitResult>();
	else if(auto typeLit = dynamic_cast<const Exprs::TypeLiteral*>(lit))
		return std::make_unique<TypeResult>(typeLit->value);
	else if(isNaturalType(type, typeData) || isIntegerType(type, typeData))
		return std::make_unique<NumberResult>(AInt(lit->toString()));
	else if(isRationalType(type, typeData))
		return std::make_unique<NumberResult>(ARatio(lit->toString()));
	else if(isRealType(type, typeData))
		return std::make_unique<NumberResult>(AReal(lit->toString()));
	else if(isStringType(type, typeData)){
		auto str = lit->toString();
		return std::make_unique<StringResult>(str.substr(1, str.size()-2));
	}
	else
		throw EvalError("Unknown literal type", lit);
}

std::function<ResultPtr(ResultHandle, ResultHandle)> getResultBinOpFn(std::string_view op);

ResultPtr evalBinop(const Exprs::BinOp *binop, TypeData &typeData, ResultScope &scope){
	auto lstr = binop->lhs->toString();
	auto rstr = binop->rhs->toString();
	auto opstr = binop->op;

	auto lhsRes = evalExpr(binop->lhs.get(), typeData, scope);
	auto rhsRes = evalExpr(binop->rhs.get(), typeData, scope);
	auto binopFn = getResultBinOpFn(binop->op);

	return binopFn(lhsRes.get(), rhsRes.get());
}

ResultPtr evalTypeExpr(TypeExprHandle expr, TypeData &typeData, ResultScope&){
	auto type = expr->resolve(
		typeData,
		[](TypeData&, TypeHandle partial){
			throw EvalError("Partial typing currently unimplemented");
			return partial;
		}
	);
	
	return std::make_unique<TypeResult>(type);
}

ResultPtr evalFnDecl(const Exprs::FnDecl *decl, TypeData &typeData, ResultScope &scope){
	auto fn = [&scope, decl](TypeData &typeData, std::vector<ResultPtr> args){
		if(args.size() != decl->params.size())
			throw EvalError("Must pass exact amount of arguments to callable result (auto-currying coming soon)");
		
		std::vector<TypeHandle> argTypes;
		argTypes.resize(args.size());
		std::transform(
			cbegin(args), cend(args),
			begin(argTypes),
			[&](const ResultPtr &ptr){ return ptr->resolveType(typeData); }
		);
		
		ResultScope fnScope(&scope);

		std::map<TypeHandle, TypeHandle> mappedPartials;

		auto typeResolver = [&mappedPartials](auto&, TypeHandle partial){
			auto res = mappedPartials.find(partial);
			if(res != cend(mappedPartials))
				return res->second;

			return partial;
		};
		
		for(std::size_t i = 0; i < decl->params.size(); i++){
			auto paramType = decl->typeExpr()->paramTypes[i]->resolve(typeData, typeResolver);
			if(isPartialType(paramType, typeData))
				mappedPartials[paramType] = argTypes[i];
		}
		
		auto fnType = decl->typeExpr()->resolve(typeData, typeResolver);
		
		std::map<ExprHandle, ResultPtr> paramMap;
		
		for(std::size_t i = 0; i < decl->params.size(); i++){
			if(!hasBaseType(argTypes[i], fnType->types[i]))
				throw EvalError(
					"Could not convert argument " + std::to_string(i) + " from " + argTypes[i]->str + " to " + fnType->types[i]->str
				);
			
			fnScope.boundNames[decl->params[i]->name] = std::move(args[i]);
		}
		
		auto bodyStr = decl->body->toString();
		return evalExpr(decl->body.get(), typeData, fnScope);
	};
	
	auto ret = std::make_unique<CallableResult>(std::move(fn));
	ret->fnType = decl->typeExpr()->resolve(typeData, [](auto&, auto p){ return p; });

	auto ptr = ret.get();

	scope.fns[decl] = std::move(ret);
	scope.boundNames[decl->name] = std::make_unique<RefResult>(ptr);
	
	return std::make_unique<RefResult>(ptr);
}

ResultPtr evalRef(const Exprs::Ref *ref, TypeData &typeData, ResultScope &scope){
	if(auto unresolved = dynamic_cast<const Exprs::UnresolvedRef*>(ref)){
		auto res = scope.resolveName(unresolved->name);

		auto unresolvedStr = unresolved->name;

		if(!res){
			std::cerr << "Could not resolve reference to " << unresolvedStr << '\n';
			throw EvalError("Could not resolve reference", ref);
		}

		return res;
	}
	else if(auto resolved = dynamic_cast<const Exprs::ResolvedRef*>(ref)){
		if(auto decl = dynamic_cast<const Exprs::FnDecl*>(resolved->refed)){
			auto res = scope.fns.find(decl);
			if(res != end(scope.fns))
				return std::make_unique<RefResult>(res->second.get());

			return evalFnDecl(decl, typeData, scope);
		}

		return evalExpr(resolved->refed, typeData, scope);
	}
	else
		throw EvalError("internal error: unrecognized reference type", ref);
}

ResultPtr evalExpr(ExprHandle expr, TypeData &typeData, ResultScope &scope){
	if(auto param = dynamic_cast<const Exprs::ParamDecl*>(expr))
		return scope.resolveName(param->name);
	else if(auto type = dynamic_cast<TypeExprHandle>(expr))
		return evalTypeExpr(type, typeData, scope);
	else if(auto lit = dynamic_cast<const Exprs::Literal*>(expr))
		return evalLiteral(lit, typeData, scope);
	else if(auto binop = dynamic_cast<const Exprs::BinOp*>(expr))
		return evalBinop(binop, typeData, scope);
	else if(auto ref = dynamic_cast<const Exprs::Ref*>(expr))
		return evalRef(ref, typeData, scope);
	else if(auto decl = dynamic_cast<const Exprs::FnDecl*>(expr))
		return evalFnDecl(decl, typeData, scope);
	else if(auto app = dynamic_cast<const Exprs::Application*>(expr))
		return evalApplication(app, typeData, scope);
	else if(!expr)
		throw EvalError("internal error: nullptr given as expression", nullptr);
	else
		throw EvalError("Unexpected expression '" + expr->toString() + "'", expr);
}

EvalResult ilang::eval(ExprIterator begin, ExprIterator end, EvalData &data){
	EvalResult res;

	if(begin != end){
		res.nextIt = begin + 1;
		res.endIt = end;
		res.result = *begin ? evalExpr(*begin, data.typeData, data.root) : nullptr;
	}
	else{
		res.nextIt = end;
		res.endIt = end;
		res.result = nullptr;
	}

	return res;
}

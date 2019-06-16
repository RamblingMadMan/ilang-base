#ifndef ILANG_EXPR_HPP
#define ILANG_EXPR_HPP 1

#include <memory>
#include <functional>

#include "ilang/Type.hpp"
#include "ilang/AReal.hpp"
#include "ilang/ARatio.hpp"

//! \file

namespace ilang{
	struct Expr;
	struct TypeExpr;
	struct Ast;
	
	using ExprHandle = const Expr*;
	using TypeExprHandle = const TypeExpr*;
	
	//! An Expression
	struct Expr{
		virtual std::string toString() const noexcept = 0;
		virtual TypeExprHandle typeExpr() const noexcept = 0;
	};
	
	struct TypeExpr: Expr{
		//! Function taking type data and a partial type to be resolved
		using ResolveFn = std::function<TypeHandle(TypeData &typeData, TypeHandle)>;
		virtual TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const = 0;
	};
	
	TypeHandle staticResolveType(const TypeData &typeData, Ast &ast, ExprHandle typeExpr) noexcept;
	
	//! Used for expression passing and comparison
	using ExprPtr = std::unique_ptr<Expr>;
	using TypeExprPtr = std::unique_ptr<TypeExpr>;
	
	//! Reduce an expression to it's simplest form
	std::unique_ptr<Expr> reduce(ExprHandle expr);
	
	namespace Exprs{
		//! LValue expression
		struct LValue: Expr{
			virtual std::string_view id() const noexcept = 0;
		};
		
		//! Reference expression
		struct Ref: Expr{
			virtual std::string_view id() const noexcept = 0;
			virtual const LValue *lvalue() const noexcept = 0;
		};
		
		//! Literal expression
		struct Literal: Expr{};
		
		//! Type literal expression
		struct TypeLiteral: TypeExpr{
			explicit TypeLiteral(const TypeLiteral *typeType_): typeType(typeType_){}
			
			std::string toString() const noexcept override{ return value->str; }
			const TypeLiteral *typeExpr() const noexcept override{ return typeType; }
			
			TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const override{
				if(isPartialType(value, typeData))
					return resolver(typeData, value);
				
				return value;
			}
			
			TypeHandle value;
			const TypeLiteral *typeType;
		};
		
		struct TypeRef: TypeExpr{
			explicit TypeRef(TypeExprHandle value_): value(value_){}
			
			std::string toString() const noexcept override{ return value->toString(); }
			TypeExprHandle typeExpr() const noexcept override{ return value->typeExpr(); }
			
			TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const override{
				return value->resolve(typeData, std::move(resolver));
			}
			
			TypeExprHandle value;
		};
		
		//! Unit literal expression
		struct UnitLiteral: Literal{
			std::string toString() const noexcept override{ return "()"; }
			
			TypeExprHandle typeExpr() const noexcept override{ return unitType->value; }
			
			std::unique_ptr<TypeRef> unitType;
		};
		
		//! Integer literal expression
		struct IntLiteral: Literal{
			explicit IntLiteral(const std::string &str): value(str){}
			
			std::string toString() const noexcept override{ return value.toString(); }
			
			TypeExprHandle typeExpr() const noexcept override{ return intType->value; }
			
			AInt value;
			std::unique_ptr<TypeRef> intType;
		};
		
		//! Real literal expression
		struct RealLiteral: Literal{
			explicit RealLiteral(const std::string &str): value(str){}
			
			std::string toString() const noexcept override{ return value.toString(); }
			
			TypeExprHandle typeExpr() const noexcept override{ return realType->value; }
			
			AReal value;
			std::unique_ptr<TypeRef> realType;
		};
		
		//! String literal expression
		struct StringLiteral: Literal{
			StringLiteral(std::string str): value(std::move(str)){}
			
			std::string toString() const noexcept override{ return "\"" + value + "\""; }
			
			TypeExprHandle typeExpr() const noexcept override{ return strType->value; }
			
			std::string value;
			std::unique_ptr<TypeRef> strType;
		};
		
		struct ListType: TypeExpr{
			explicit ListType(const TypeLiteral *typeType_): typeType(typeType_){}
			
			std::string toString() const noexcept override;
			
			TypeExprHandle typeExpr() const noexcept override{ return typeType; }
			
			TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const override;
			
			std::vector<TypeExprHandle> elementTypes;
			const TypeLiteral *typeType;
		};
		
		struct ProductType: TypeExpr{
			explicit ProductType(const TypeLiteral *typeType_): typeType(typeType_){}
			
			std::string toString() const noexcept override;
			
			const TypeLiteral *typeExpr() const noexcept override{ return typeType; }
			
			TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const override;
			
			std::vector<TypeExprHandle> innerTypes;
			const TypeLiteral *typeType;
		};
		
		//! Array literal expression
		struct ListLiteral: Literal{
			explicit ListLiteral(std::vector<ExprPtr> elements_ = {}): elements(std::move(elements_)){}
			
			std::string toString() const noexcept override{
				std::string ret = "[" + elements[0]->toString();
				for(std::size_t i = 1; i < elements.size(); i++)
					ret += ", " + elements[i]->toString();
				
				ret += "]";
				return ret;
			}
			
			const ListType *typeExpr() const noexcept override{ return listType.get(); }
			
			std::vector<ExprPtr> elements;
			std::unique_ptr<ListType> listType;
		};
		
		//! Group expression
		struct ProductLiteral: Literal{
			const ProductType *typeExpr() const noexcept override{ return productType.get(); }
			
			std::string toString() const noexcept override{
				std::string ret = "(" + elements[0]->toString();
				for(std::size_t i = 1; i < elements.size(); i++)
					ret += ", " + elements[i]->toString();
				
				ret += ")";
				return ret;
			}
			
			std::vector<std::unique_ptr<Expr>> elements;
			std::unique_ptr<ProductType> productType;
		};
		
		struct ParamDecl: LValue{
			std::string name;
			TypeExprPtr type;
			
			std::string toString() const noexcept override{
				auto ret = name;
				if(type) ret += ": " + type->toString();
				return ret;
			}
			
			TypeExprHandle typeExpr() const noexcept override{ return type.get(); }
			
			std::string_view id() const noexcept override{ return name; }
		};
		
		//! Function declaration expression
		struct FnDecl: LValue{
			TypeExprHandle typeExpr() const noexcept override{ return fnType.get(); }
			
			std::string_view id() const noexcept override{ return name; }
			
			std::string toString() const noexcept override{
				auto ret = name + "(";
				if(!params.empty()){
					ret += params[0]->toString();
					for(std::size_t i = 1; i < params.size(); i++)
						ret += ", " + params[i]->toString();
				}
				ret += ")";
				
				return ret;
			}
			
			std::string name;
			std::vector<std::unique_ptr<ParamDecl>> params;
			TypeExprPtr fnType;
		};
		
		//! Unresolved reference expression
		struct UnresolvedRef: Ref{
			std::string toString() const noexcept override{ return name; }
			TypeExprHandle typeExpr() const noexcept override{ return uniqueType->value; }
			std::string_view id() const noexcept override{ return name; }
			const LValue *lvalue() const noexcept override{ return nullptr; }
			
			std::string name;
			std::unique_ptr<TypeRef> uniqueType;
		};
		
		//! Resolved reference expression
		struct ResolvedRef: Ref{
			std::string toString() const noexcept override{ return std::string(refed->id()); }
			TypeExprHandle typeExpr() const noexcept override{ return refed->typeExpr(); }
			std::string_view id() const noexcept override{ return refed->id(); }
			const LValue *lvalue() const noexcept override{ return refed; }
			
			const LValue *refed;
		};
		
		//! Function application expression
		struct Application: Expr{
			TypeExprHandle typeExpr() const noexcept override{ return resultType.get(); }
			
			ExprHandle functor;
			std::vector<ExprHandle> args;
			TypeExprPtr resultType;
		};
		
		struct BinOpType: TypeExpr{
			std::string toString() const noexcept override{ return "!BINOPTYPE!"; }
			
			TypeExprHandle typeExpr() const noexcept override{ return lhsTypeExpr->typeExpr(); }
			
			TypeHandle resolve(TypeData &typeData, ResolveFn resolver) const override;
			
			TypeExprHandle lhsTypeExpr, rhsTypeExpr;
			std::string op;
		};
		
		//! Binary operation expression
		struct BinOp: Expr{
			std::string toString() const noexcept override{
				return lhs->toString() + " " +  op + " " + rhs->toString();
			}
			
			const BinOpType *typeExpr() const noexcept override{ return resultType.get(); }
			
			ExprPtr lhs, rhs;
			std::unique_ptr<BinOpType> resultType;
			std::string op;
		};
	}
}

#endif // !ILANG_EXPR_HPP

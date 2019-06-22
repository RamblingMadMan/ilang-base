#ifndef ILANG_FWD_HPP
#define ILANG_FWD_HPP 1

namespace ilang{
	struct Type;
	struct Expr;
	
	using TypeHandle = const Type*;
	using ExprHandle = const Expr*;
}

#endif // !ILANG_FWD_HPP

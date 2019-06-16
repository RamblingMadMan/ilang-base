#ifndef ILANG_SRC_IMPLS_HPP
#define ILANG_SRC_IMPLS_HPP 1

#include <gmp.h>
#include <mpfr.h>

#include "ilang/AInt.hpp"
#include "ilang/ARatio.hpp"
#include "ilang/AReal.hpp"

struct ilang::AInt::Impl{
	~Impl(){ mpz_clear(value); }

	mpz_t value;
};

struct ilang::ARatio::Impl{
	~Impl(){ mpq_clear(value); }

	mpq_t value;
};

struct ilang::AReal::Impl{
	~Impl(){ mpfr_clear(value); }

	mpfr_t value;
};

#endif // !ILANG_SRC_IMPLS_HPP 

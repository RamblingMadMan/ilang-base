#include <stdexcept>
#include <algorithm>

#include "Impls.hpp"

using namespace ilang;

AReal::AReal() noexcept: m_impl(std::make_unique<Impl>()){}

AReal::AReal(const AReal &other) noexcept: AReal(){
	mpfr_init_set(m_impl->value, other.m_impl->value, MPFR_RNDN);
}

AReal::AReal(AReal &&other) noexcept: m_impl(std::move(other.m_impl)){}

AReal::~AReal(){}

AReal::AReal(const AInt &i) noexcept: AReal(){
	mpfr_init_set_z(m_impl->value, i.m_impl->value, MPFR_RNDN);
}

AReal::AReal(const ARatio &q) noexcept: AReal(){
	mpfr_init_set_q(m_impl->value, q.m_impl->value, MPFR_RNDN);
}

AReal::AReal(double r) noexcept: AReal(){
	mpfr_init_set_d(m_impl->value, r, MPFR_RNDN);
}

AReal::AReal(long double r) noexcept: AReal(){
	mpfr_init_set_ld(m_impl->value, r, MPFR_RNDN);
}

AReal::AReal(const std::string &s): AReal(){
	if(mpfr_init_set_str(m_impl->value, s.c_str(), 10, MPFR_RNDN) == -1)
		throw std::runtime_error("Invalid real literal (mpfr_set_str)");
}

template<typename Fn>
void mpfrInitOp(mpfr_t rop, mpfr_t lhs, mpfr_t rhs, Fn fn){
	mpfr_init(rop);
	fn(rop, lhs, rhs, MPFR_RNDN);
}

#define DEF_AREAL_OP(op, fn)\
AReal AReal::operator op(const AReal &rhs) const noexcept{\
	AReal ret;\
	mpfrInitOp(ret.m_impl->value, m_impl->value, rhs.m_impl->value, fn);\
	return ret;\
}

DEF_AREAL_OP(+, mpfr_add)
DEF_AREAL_OP(-, mpfr_sub)
DEF_AREAL_OP(*, mpfr_mul)
DEF_AREAL_OP(/, mpfr_div)

AReal AReal::pow(const AInt &exp) const noexcept{
	AReal res;
	mpfr_init(res.m_impl->value);
	mpfr_pow_z(res.m_impl->value, m_impl->value, exp.m_impl->value, MPFR_RNDN);
	return res;
}

AReal AReal::pow(const AReal &exp) const noexcept{
	AReal res;
	mpfr_init(res.m_impl->value);
	mpfr_pow(res.m_impl->value, m_impl->value, exp.m_impl->value, MPFR_RNDN);
	return res;
}

std::string AReal::toString() const{
	mpfr_exp_t exponent;
	auto strPtr = mpfr_get_str(nullptr, &exponent, 10, 0, m_impl->value, MPFR_RNDN);
	std::string str = strPtr;
	mpfr_free_str(strPtr);

	auto pointIdx = 0;

	if(str[pointIdx] == '-' || str[pointIdx] == '+')
		++pointIdx;

	str.insert(pointIdx + exponent, ".");
	
	auto firstNonZero = std::find_if(rbegin(str), rend(str), [](auto c){ return c != '0'; });
	if(*firstNonZero == '.'){
		--firstNonZero;
	}
	
	if(firstNonZero != rend(str))
		str.erase(firstNonZero.base(), end(str));
	
	return str;
}

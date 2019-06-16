#include <stdexcept>

#include "Impls.hpp"

using namespace ilang;

ARatio::ARatio() noexcept: m_impl(std::make_unique<Impl>()){
	mpq_init(m_impl->value);
}

ARatio::ARatio(const ARatio &other) noexcept: ARatio(){
	mpq_set(m_impl->value, other.m_impl->value);
}

ARatio::ARatio(ARatio &&other) noexcept: m_impl(std::move(other.m_impl)){}

ARatio::~ARatio(){}

ARatio::ARatio(const std::string &s): ARatio(){
	if(mpq_set_str(m_impl->value, s.c_str(), 10) == -1)
		throw std::runtime_error("Invalid rational string (mpq_set_str)");
}

ARatio::ARatio(std::int64_t n, std::int64_t d) noexcept: ARatio(){
	mpq_set_si(m_impl->value, n, d);
} 

ARatio::ARatio(const AInt &n, const AInt &d) noexcept: ARatio(){
	ARatio denom;
	mpq_set_z(m_impl->value, n.m_impl->value);
	mpq_set_z(denom.m_impl->value, d.m_impl->value);
	mpq_div(m_impl->value, m_impl->value, denom.m_impl->value);
}

AInt ARatio::numerator() const noexcept{
	AInt res;
	mpz_init(res.m_impl->value);
	mpz_set(res.m_impl->value, mpq_numref(m_impl->value));
	return res;
}

AInt ARatio::denominator() const noexcept{
	AInt res;
	mpz_init(res.m_impl->value);
	mpz_set(res.m_impl->value, mpq_denref(m_impl->value));
	return res;
}

template<typename Fn>
void mpqInitOp(mpq_t rop, mpq_t lhs, mpq_t rhs, Fn fn){
	fn(rop, lhs, rhs);
}

static inline void mpq_pow(mpq_t rop, const mpq_t base, long exp){
	if (exp >= 0) {
		mpz_pow_ui(mpq_numref(rop), mpq_numref(base), static_cast<unsigned long>(exp));
		mpz_pow_ui(mpq_denref(rop), mpq_denref(base), static_cast<unsigned long>(exp));
	} else {
		mpz_pow_ui(mpq_numref(rop), mpq_denref(base), static_cast<unsigned long>(-exp));
		mpz_pow_ui(mpq_denref(rop), mpq_numref(base), static_cast<unsigned long>(-exp));
		if (mpz_sgn(mpq_denref(rop)) < 0) {
			mpz_neg(mpq_numref(rop), mpq_numref(rop));
			mpz_neg(mpq_denref(rop), mpq_denref(rop));
		}
	}
}

#define DEF_ARATIO_OP(op, fn)\
ARatio ARatio::operator op(const ARatio &rhs) const noexcept{\
	ARatio ret;\
	mpqInitOp(ret.m_impl->value, m_impl->value, rhs.m_impl->value, fn);\
	return ret;\
}

DEF_ARATIO_OP(+, mpq_add)
DEF_ARATIO_OP(-, mpq_sub)
DEF_ARATIO_OP(*, mpq_mul)
DEF_ARATIO_OP(/, mpq_div)

bool ARatio::operator<(const ARatio &rhs) const noexcept{
	return mpq_cmp(m_impl->value, rhs.m_impl->value) < 0;
}

bool ARatio::operator>(const ARatio &rhs) const noexcept{
	return mpq_cmp(m_impl->value, rhs.m_impl->value) > 0;
}

bool ARatio::operator<=(const ARatio &rhs) const noexcept{
	return mpq_cmp(m_impl->value, rhs.m_impl->value) <= 0;
}

bool ARatio::operator>=(const ARatio &rhs) const noexcept{
	return mpq_cmp(m_impl->value, rhs.m_impl->value) >= 0;
}

bool ARatio::operator==(const ARatio &rhs) const noexcept{
	return mpq_cmp(m_impl->value, rhs.m_impl->value) == 0;
}

ARatio ARatio::pow(const AInt &exp) const{
	if(exp.bitsRequired() > 64)
		throw std::runtime_error("exponent too large");
	
	auto expInt = std::stoll(exp.toString());
	ARatio res;
	mpq_init(res.m_impl->value);
	mpq_pow(res.m_impl->value, m_impl->value, expInt);
	return res;
}

std::string ARatio::toString() const{
	std::size_t strLen = mpz_sizeinbase(mpq_numref(m_impl->value), 10)
					   + mpz_sizeinbase(mpq_denref(m_impl->value), 10) + 3;
	std::string str;
	str.resize(strLen);
	mpq_get_str(&str[0], 10, m_impl->value);
	return str;
}

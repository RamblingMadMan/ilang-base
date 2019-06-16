#include <stdexcept>
#include <cmath>

#include "Impls.hpp"

using namespace ilang;

AInt::AInt() noexcept: m_impl(std::make_unique<AInt::Impl>()){}

AInt::AInt(AInt &&other) noexcept: m_impl(std::move(other.m_impl)){}

AInt::AInt(const AInt &other) noexcept: AInt(){
	mpz_init_set(m_impl->value, other.m_impl->value);
}

AInt::~AInt(){}

AInt::AInt(std::int64_t i) noexcept: AInt(){
	mpz_init_set_si(m_impl->value, i);
}

AInt::AInt(std::uint64_t i) noexcept: AInt(){
	mpz_init_set_ui(m_impl->value, i);
}

AInt::AInt(const std::string &s): AInt(){
	if(mpz_init_set_str(m_impl->value, s.c_str(), 10) == -1)
		throw std::runtime_error("Invalid integer literal (mpz_set_str");
}

template<typename Fn>
void mpzInitOp(mpz_t rop, mpz_t lhs, mpz_t rhs, Fn fn){
	mpz_init(rop);
	fn(rop, lhs, rhs);
}

#define DEF_AINT_OP(op, fn)\
AInt AInt::operator op(const AInt &rhs) const noexcept{\
	AInt ret;\
	mpzInitOp(ret.m_impl->value, m_impl->value, rhs.m_impl->value, fn);\
	return ret;\
}

DEF_AINT_OP(+, mpz_add)
DEF_AINT_OP(-, mpz_sub)
DEF_AINT_OP(*, mpz_mul)

bool AInt::operator<(const AInt &rhs) const noexcept{
	return mpz_cmp(m_impl->value, rhs.m_impl->value) < 0;
}

bool AInt::operator>(const AInt &rhs) const noexcept{
	return mpz_cmp(m_impl->value, rhs.m_impl->value) > 0;
}

bool AInt::operator<=(const AInt &rhs) const noexcept{
	return mpz_cmp(m_impl->value, rhs.m_impl->value) <= 0;
}

bool AInt::operator>=(const AInt &rhs) const noexcept{
	return mpz_cmp(m_impl->value, rhs.m_impl->value) >= 0;
}

bool AInt::operator==(const AInt &rhs) const noexcept{
	return mpz_cmp(m_impl->value, rhs.m_impl->value) == 0;
}

AInt AInt::pow(const AInt &exp) const{
	if(exp.bitsRequired() > 64)
		throw std::runtime_error("exponent too large");
	
	auto expInt = std::stoll(exp.toString());
	AInt res;
	mpz_init(res.m_impl->value);
	mpz_pow_ui(res.m_impl->value, m_impl->value, expInt);
	
	return res;
}

std::size_t AInt::bitsRequired() const noexcept{
	auto bitSize = mpz_sizeinbase(m_impl->value, 2);
	return bitSize;
}

std::string AInt::toString() const{
	std::string str;
	str.resize(mpz_sizeinbase(m_impl->value, 10) + 2);
	mpz_get_str(&str[0], 10, m_impl->value);
	return str;
}

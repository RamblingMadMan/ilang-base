#ifndef ILANG_ARATIO_HPP
#define ILANG_ARATIO_HPP 1

#include <cstdint>
#include <memory>
#include <string>

#include "AInt.hpp"

namespace ilang{
	class ARatio{
		public:
			ARatio(const ARatio &other) noexcept;
			ARatio(ARatio &&other) noexcept;

			~ARatio();

			ARatio(const AInt &numerator, const AInt &denominator = AInt(1L)) noexcept;

			explicit ARatio(std::int64_t numerator, std::int64_t denominator = 1) noexcept;
			explicit ARatio(const std::string &s);

			AInt numerator() const noexcept;
			AInt denominator() const noexcept;
			
			ARatio operator+(const ARatio &rhs) const noexcept;
			ARatio operator-(const ARatio &rhs) const noexcept;
			ARatio operator*(const ARatio &rhs) const noexcept;
			ARatio operator/(const ARatio &rhs) const noexcept;
			
			bool operator<(const ARatio &rhs) const noexcept;
			bool operator>(const ARatio &rhs) const noexcept;
			bool operator<=(const ARatio &rhs) const noexcept;
			bool operator>=(const ARatio &rhs) const noexcept;
			bool operator==(const ARatio &rhs) const noexcept;
			
			ARatio pow(const AInt &exp) const;

			std::string toString() const;

		private:
			ARatio() noexcept;

			struct Impl;
			std::unique_ptr<Impl> m_impl;

			friend class AReal;
	};
}

inline ilang::ARatio operator/(const ilang::AInt &lhs, const ilang::AInt &rhs) noexcept{
	return ilang::ARatio(lhs, rhs);
}

#endif // !ILANG_ARATIO_HPP 

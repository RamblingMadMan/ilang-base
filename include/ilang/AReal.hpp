#ifndef ILANG_AREAL_HPP
#define ILANG_AREAL_HPP 1

#include <cstdint>
#include <memory>
#include <string>

#include "AInt.hpp"
#include "ARatio.hpp"

namespace ilang{
	class AReal{
		public:
			AReal(const AReal &other) noexcept;
			AReal(AReal &&other) noexcept;

			~AReal();

			explicit AReal(const AInt &i) noexcept;
			explicit AReal(const ARatio &q) noexcept;
			explicit AReal(double r) noexcept;
			explicit AReal(long double r) noexcept;
			explicit AReal(const std::string &s);

			AReal operator+(const AReal &rhs) const noexcept;
			AReal operator-(const AReal &rhs) const noexcept;
			AReal operator*(const AReal &rhs) const noexcept;
			AReal operator/(const AReal &rhs) const noexcept;
			
			AReal pow(const AReal &exp) const noexcept;
						
			AReal pow(const ARatio &exp) const noexcept{
				return pow(AReal(exp));
			}
			
			AReal pow(const AInt &exp) const noexcept;
			
			std::string toString() const;

		private:
			AReal() noexcept;

			struct Impl;
			std::unique_ptr<Impl> m_impl;

			friend class ARational;
	};
}

#endif // !ILANG_AREAL_HPP 

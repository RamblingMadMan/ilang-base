#ifndef ILANG_AINT_HPP
#define ILANG_AINT_HPP 1

#include <cstdint>
#include <memory>
#include <string>

namespace ilang{
	class AInt{
		public:
			AInt(const AInt &other) noexcept;
			AInt(AInt &&other) noexcept;

			~AInt();

			explicit AInt(std::int64_t i) noexcept;
			explicit AInt(std::uint64_t ui) noexcept;
			explicit AInt(const std::string &s);

			AInt operator+(const AInt &rhs) const noexcept;
			AInt operator-(const AInt &rhs) const noexcept;
			AInt operator*(const AInt &rhs) const noexcept;
			
			bool operator<(const AInt &rhs) const noexcept;
			bool operator>(const AInt &rhs) const noexcept;
			bool operator<=(const AInt &rhs) const noexcept;
			bool operator>=(const AInt &rhs) const noexcept;
			bool operator==(const AInt &rhs) const noexcept;
			
			AInt pow(const AInt &exp) const;

			std::string toString() const;
			
			std::size_t bitsRequired() const noexcept;

		private:
			AInt() noexcept;

			struct Impl;
			std::unique_ptr<Impl> m_impl;

			friend class ARatio;
			friend class AReal;
	};
}

#endif // !ILANG_AINT_HPP 

#ifndef ILANG_LOCATION_HPP
#define ILANG_LOCATION_HPP 1

#include <cstdint>

namespace ilang{
	//! Represents a location in source code
	struct Location{
		std::int64_t line = -1, col = -1;
	};
}

inline bool operator ==(const ilang::Location &lhs, const ilang::Location &rhs){
	return (lhs.line == rhs.line) && (lhs.col == rhs.col);
}

inline bool operator <(const ilang::Location &lhs, const ilang::Location &rhs){
	if(lhs.line == rhs.line)
		return lhs.col < rhs.col;
	else
		return lhs.line < rhs.line;
}

#endif // !ILANG_LOCATION_HPP

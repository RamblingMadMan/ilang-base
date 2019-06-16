cmake_minimum_required(VERSION 3.10.0 FATAL_ERROR)

project(ilang-base VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake;${CMAKE_MODULE_PATH}")
set(CMAKE_CXX_STANDARD 17)

include_directories(include) 

find_package(GMP REQUIRED)
find_package(MPFR REQUIRED)

add_subdirectory(deps/utfcpp)
add_subdirectory(deps/fmt)

set(
	ILANG_BASE_HEADERS
	include/ilang/Lexer.hpp
	include/ilang/Location.hpp
	include/ilang/AReal.hpp
	include/ilang/ARatio.hpp
	include/ilang/AInt.hpp
	include/ilang/Type.hpp
	include/ilang/Expr.hpp
	include/ilang/Parser.hpp
	include/ilang/Eval.hpp
)

set(
	ILANG_BASE_SOURCE
	src/Lexer.cpp
	src/Impls.hpp
	src/AReal.cpp
	src/ARatio.cpp
	src/AInt.cpp
	src/Expr.cpp
	src/Type.cpp
	src/Parser.cpp
	src/Eval.cpp
)

add_library(ilang-base ${ILANG_BASE_HEADERS} ${ILANG_BASE_SOURCE})

target_include_directories(ilang-base PUBLIC include)
target_include_directories(ilang-base PRIVATE deps/GSL/include ${GMP_INCLUDES} ${MPFR_INCLUDES})
target_link_libraries(ilang-base utf8cpp fmt ${GMP_LIBRARIES} ${MPFR_LIBRARIES})

install(
	FILES ${ILANG_BASE_HEADERS}
	DESTINATION include/ilang
)

install(
	TARGETS ilang-base
	ARCHIVE
		DESTINATION lib
)

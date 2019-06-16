#include "utf8.h"

#include "ilang/Lexer.hpp"

using namespace ilang;

using SourceIter = std::string_view::const_iterator;

static bool isOpChar(std::uint32_t cp){
	switch(cp){
		case '\\':
		case '!':
		case '+':
		case '-':
		case '*':
		case '/':
		case '^':
		case '=':
		case ':':
		case '%':
		case '|':
		case '&':
		case '.':
		case ',':
		case '<':
		case '>':
		case '?':
			return true;

		default: return false;
	}
}

std::pair<SourceIter, SourceIter> getIters(std::string_view str) noexcept{
	return {begin(str), end(str)};
}

std::string_view fromIters(SourceIter beg, SourceIter end) noexcept{
	return std::string_view(&*beg, std::distance(beg, end));
}

std::string ilang::detail::asString(TokenType type, const std::string &s){		
	switch(type){
		case TokenType::space:
		case TokenType::id:
		case TokenType::int_:
		case TokenType::real:
		case TokenType::op:
			return s;
			
		case TokenType::listL: return "[";
		case TokenType::listR: return "]";
			
		case TokenType::groupL: return "(";
		case TokenType::groupR: return ")";
		
		case TokenType::newLine: return "\n";
		
		case TokenType::str: break;
		
		default:
			throw std::runtime_error("Token can not be converted to a string");
	}
	
	std::string parsed;
	
	auto[it, end] = getIters(s);
	
	while(it != end){
		auto cp = utf8::next(it, end);
		
		if(cp == '\\'){
			cp = utf8::next(it, end);
			switch(cp){
				case '\'': parsed += '\''; break;
				case '"': parsed += '"'; break;
				case 'n': parsed += '\n'; break;
				case 'r': parsed += '\r'; break;
				case 't': parsed += '\t'; break;
				
				default:
					throw std::runtime_error("Invalid escape sequence in string literal");
			}
		}
		else
			utf8::append(cp, std::back_inserter(parsed));
	}
	
	return parsed;
}

LexResult lex_result(Token &tok, std::string_view rem){
	return {std::move(tok), rem};
}

Location ilang::locationAfter(const Token &tok){
	switch(tok.type){
		case TokenType::op:
		case TokenType::int_:
		case TokenType::real:
		case TokenType::str:
		case TokenType::space:
		case TokenType::id:{
			auto[it, end] = getIters(tok.value);
			return {tok.location.line, tok.location.col + utf8::distance(it, end)};
		}

		case TokenType::newLine:{
			return {tok.location.line + 1, 0};
		}

		case TokenType::eof: return {-1, -1};

		case TokenType::empty: return {0, 0};
		
		case TokenType::listL:
		case TokenType::listR:
		case TokenType::groupL:
		case TokenType::groupR: return {tok.location.line, tok.location.col + 1};
		
		default: throw std::runtime_error("Unrecognized lexeme type in 'locationAfter'");
	}
}

using LexFn = LexResult(std::string, std::string_view, Location);

template<typename Check>
LexResult lexInner(
	std::string &cur, std::string_view rem, Location loc,
	TokenType type, Check check
){
	auto[it, end] = getIters(rem);
	while(it != end){
		auto cp = utf8::peek_next(it, end);
		if(check(cp)){
			utf8::append(utf8::next(it, end), std::back_inserter(cur));
			//return lexInner(cur, fromIters(it, end), loc, type, std::move(check));
		}
		else
			break;
	}
	
	Token tok;
	tok.type = type;
	tok.value = std::move(cur);
	tok.location = loc;
	return lex_result(tok, fromIters(it, end));
}

LexResult lexString(std::uint32_t delim, std::string cur, std::string_view rem, Location startLoc){
	auto[it, end] = getIters(rem);
	
	while(it != end){
		auto cp = utf8::next(it, end);
		
		if(cp == '\\'){
			utf8::append(cp, std::back_inserter(cur));
			
			cp = utf8::peek_next(it, end);
			
			if(cp == delim){
				utf8::append(utf8::next(it, end), std::back_inserter(cur));
			}
			else{
				switch(cp){
					case 't':
					case 'r':
					case 'n':
						utf8::append(utf8::next(it, end), std::back_inserter(cur));
						break;
					
					default:
						throw LexError(startLoc, "Unrecognized escape sequence (character after '\\') in string");
				}
			}
		}
		else if(cp == delim){
			Token tok;
			tok.type = TokenType::str;
			tok.value = std::move(cur);
			tok.location = startLoc;
			return lex_result(tok, fromIters(it, end));
		}
		else{
			utf8::append(cp, std::back_inserter(cur));
		}
	}
	
	throw LexError(startLoc, "Unexpected end of input in string");
}

LexResult lexId(std::string cur, std::string_view rem, Location startLoc){
	return lexInner(
		cur, rem, startLoc,
		TokenType::id,
		[](auto cp){
			return std::isalnum(cp) || (cp == '_');
		}
	);
}

LexResult lexSpace(std::string cur, std::string_view rem, Location startLoc){
	return lexInner(
		cur, rem, startLoc,
		TokenType::space,
		[](auto cp){
			return std::isspace(cp) && (cp != '\n');
		}
	);
}

LexResult lexOp(std::string cur, std::string_view rem, Location startLoc){
	return lexInner(
		cur, rem, startLoc,
		TokenType::op,
		[](auto cp){
			return isOpChar(cp);
		}
	);
}

LexResult lexReal(std::string cur, std::string_view rem, Location startLoc){
	return lexInner(
		cur, rem, startLoc,
		TokenType::real,
		[&](auto cp){
			if(cp == '.'){
				Token dummy;
				dummy.type = TokenType::id;
				dummy.value = std::move(cur);
				dummy.location = startLoc;
				throw LexError(locationAfter(dummy), "Unexpected extra '.' in real number literal");
			}

			return std::isdigit(cp);
		}
	);
}

LexResult lexNum(std::string cur, std::string_view rem, Location startLoc){
	auto[it, end] = getIters(rem);
	while(it != end){
		auto cp = utf8::peek_next(it, end);
		if(std::isdigit(cp)){
			utf8::append(utf8::next(it, end), std::back_inserter(cur));
		}
		else if(cp == '.'){
			utf8::append(utf8::next(it, end), std::back_inserter(cur));
			return lexReal(std::move(cur), fromIters(it, end), startLoc);
		}
		else
			break;
	}

	Token tok;
	tok.type = TokenType::int_;
	tok.value = std::move(cur);
	tok.location = startLoc;
	return lex_result(tok, fromIters(it, end));
}

LexResult ilang::lex(std::string_view src, Location startLoc){
	auto it = begin(src);
	auto endIt = end(src);

	if(it == endIt){
		Token tok;
		tok.type = TokenType::eof;
		return lex_result(tok, src);
	}

	std::string str;
	std::int32_t lines = 0, cols = 0;

	auto cp = utf8::next(it, endIt);

	LexFn *lexFn = nullptr; 

	if(cp == '\n'){
		Token tok;
		tok.type = TokenType::newLine;
		tok.location = startLoc;
		return lex_result(tok, fromIters(it, endIt));
	}
	else if(std::isspace(cp)) lexFn = lexSpace;
	else if(std::isalpha(cp) || (cp == '_')) lexFn = lexId;
	else if((cp == '\'') || (cp == '"')){
		return lexString(cp, "", fromIters(it, endIt), startLoc);
	}
	else if(cp == '[' || cp == ']'){
		Token tok;
		tok.type = cp == '[' ? TokenType::listL : TokenType::listR;
		tok.location = startLoc;
		return lex_result(tok, fromIters(it, endIt));
	}
	else if(cp == '(' || cp == ')'){
		Token tok;
		tok.type = cp == '(' ? TokenType::groupL : TokenType::groupR;
		tok.location = startLoc;
		return lex_result(tok, fromIters(it, endIt));
	}
	else if(isOpChar(cp)) lexFn = lexOp;
	else if(std::isdigit(cp)) lexFn = lexNum;
	else{
		std::string errMsg = "Unexpected character '";
		utf8::append(cp, std::back_inserter(errMsg));
		errMsg += "'";
		throw LexError(
			{startLoc.line + lines, lines ? cols : startLoc.col + cols},
			std::move(errMsg)
		);
	}

	utf8::append(cp, std::back_inserter(str));
	return lexFn(std::move(str), fromIters(it, endIt), startLoc);
}

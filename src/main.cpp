#include "automata.hpp"
#include <algorithm>
#include <iostream>
#include <optional>

using namespace automata;

template<literal... Ls>
using any_string = any<string_match<Ls>...>;

template<literal L>
struct a
{
	static constexpr auto name = L;
};

int
main()
{

	std::string str = " -123";
	utf8_iterator it{ str };

	using r1 = compound<
	  repeat<any_string<" ", "\t">>,
	  named<
	    "value",
	    compound<
	      named<"sign", maybe<any_string<"+", "-">>>,
	      named<
	        "number",
	        repeat<any_string<"0", "1", "2", "3", "4", "5", "6", "7", "8", "9">,
	               1>>>>>;

	r1::state s;
	auto [pos, result] = r1::match_result(it, s).value();
	std::cout << pos << std::endl;

	std::cout << get<"value.sign">(result).value().n << std::endl;

	return 0;
}

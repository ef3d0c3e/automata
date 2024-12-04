#include "automata.hpp"
#include <algorithm>
#include <iostream>
#include <optional>

using namespace automata;

template<literal... Ls>
struct any_string : any<string_match<Ls>...>
{
	static constexpr inline auto name = literal<11>{ "any_string" };
};

template<literal L>
struct a
{
	static constexpr auto name = L;
};

int
main()
{

	std::string str = "  A , B ,    C TEST";
	utf8_iterator it{ str };
	using remove_spacing = ignore_result<repeat<any<string_match<" ">, string_match<"\t">>>>;
	using csv_line =
		repeat<
			compound<
				/* Stop at line end */
				exclude<line_end, 0>,
				/* Remove spacing */
				remove_spacing,
				/* Transforms range into value */
				named<"cell",
					result_aggregator<decltype([]<Iterator I>(std::pair<I, I> range, [[maybe_unused]] auto&& result)
						{
							return range.first.until(range.second);
						}),
						/* Match until separator, removing spaces before separator */					
						repeat<exclude<compound<remove_spacing, any<string_match<",">, line_end>>>>
					>
				>,
				/* Remove spacing */
				remove_spacing,
				/* Match separator */
				ignore_result<any<string_match<",">, line_end>>
			>
		>;

	csv_line::state s;
	const auto [pos, result] = csv_line::match_result(it, s).value();

	for (const auto& data : result.result)
	{
		std::cout << get<"cell">(data) << std::endl;
	}
	/*
	    using r1 = compound<
	      repeat<any_string<" ", "\t">>,
	      named<
	        "value",
	        compound<
	          named<"sign", maybe<any_string<"+", "-">>>,
	          named<
	            "number",
	            repeat<any_string<"0", "1", "2", "3", "4", "5", "6", "7", "8",
	   "9">, 1>>>>>;

	    r1::state s;
	    const auto [pos, result] = r1::match_result(it, s).value();
	    std::cout << pos << std::endl;

	    std::cout << get<"value.sign">(result).value().n << std::endl;
	    */

	return 0;
}

#include "automata.hpp"
#include <algorithm>
#include <iostream>
#include <optional>

using namespace automata;

template <literal... Ls>
using any_string = any<string_match<Ls>...>;

template <literal S>
struct A {};

template <literal S, class T>
struct B { T t; };

template <literal S>
static consteval auto tosz()
{
	std::size_t i{};

	for (const auto pos : std::ranges::iota_view{std::size_t{}, S.size()})
	{
		i = i * 10 + S[pos] - '0';
	}
	return i;
}

template <literal S, size_t N = 0>
static consteval auto next_impl()
{
	if constexpr (S[N] == '.')
	{
		static_assert(N + 1 < S.size(), "Invalid key");
		return std::make_pair(S.template substr<0, N>(), S.template substr<N + 1, S.size()>());
	}
	else if constexpr (N + 1 < S.size())
	{
		return next_impl<S, N + 1>();
	}
	else
	{
		[]<bool v>()
		{
			static_assert(v, "Failed to split");
		}.template operator()<false>();
	}
}

template <literal S>
static consteval auto next()
{
	return next_impl<S, 0>();
}

int
main()
{

	std::string str = "  \t -123";
	utf8_iterator it{ str };


	using r1 = compound<named<"val", string_match<" ">>>;

		/* = compound<
		repeat<any_string<" ", "\t">>,
		named<"value", compound<
			named<"sign", maybe<any_string<"+", "-">>>,
			named<"nunber", repeat<
				any_string<"0", "1", "2", "3", "4", "5", "6", "7", "8", "9">,
				1
			>>
		>>
	>;*/

	r1::state s;
	auto [pos, result] = r1::match_result(it, s).value();
	std::cout << pos << std::endl;

	std::tuple<int, char, float> ff{64, 'c', 3.14159f};
	std::cout <<
		get<"val">(result)
		<< std::endl;

	return 0;
}

#ifndef AUTOMATA_RULE_HPP
#define AUTOMATA_RULE_HPP

#include "iterator.hpp"
#include "utils.hpp"
#include <variant>
#include <tuple>
#include <limits>
#include <vector>

namespace automata {
/**
 * @brief Concept for rules
 */
template<class T>
concept rule = requires(T::state& state, utf8_iterator& it) {
	requires is_literal<decltype(T::name)>;
	typename T::result;
	typename T::state;

	// Defaults
	{ typename T::result{} } -> std::same_as<typename T::result>;
	{ typename T::state{} } -> std::same_as<typename T::state>;

	{
		[]<Iterator I>(I& it, T::state& state) {
			return T::template match<I>(it, state);
		}(it, state)
	} -> std::same_as<std::optional<std::size_t>>;
	{
		[]<Iterator I>(I& it, T::state& state) {
			return T::template match_result<I>(it, state);
		}(it, state)
	}
	-> std::same_as<std::optional<std::pair<std::size_t, typename T::result>>>;
}; // concept rule

/**
 * @brief Checks for literal string match
 *
 * @tparam S String to compare against
 */
template<literal S>
struct string_match
{
	static constexpr inline auto name = literal<13>{ "string_match" };
	struct result
	{};
	struct state
	{};

	template<Iterator I>
	static constexpr auto match(I& it,
	                            state&) noexcept -> std::optional<std::size_t>
	{
		if (it.peek(S.size()) == (std::string_view)S)
			return { S.size() };
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return { { res.value(), result{} } };
		return {};
	}
}; // struct string_match

/**
 * @brief Checks for end of line
 *
 * A line_end is either a `\n` or the iterator reaches `at_end()`
 */
struct line_end
{
	static constexpr inline auto name = literal<9>{"line_end"};
	struct result
	{};
	struct state
	{};

	template<Iterator I>
	static constexpr auto match(I& it,
	                            state&) noexcept -> std::optional<std::size_t>
	{
		if (it.at_end())
			return { 0 };
		else if (it.peek(1) == "\n")
			return { 1 };
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return { { res.value(), result{} } };
		return {};
	}
}; // struct line_end

/**
 * @brief Checks for start of line
 *
 * A line_begin is either a `\n` or the iterator reaches `at_start()`
 */
struct line_begin
{
	static constexpr inline auto name = literal<11>{"line_begin"};
	struct result
	{};
	struct state
	{};

	template<Iterator I>
	static constexpr auto match(I& it,
	                            state&) noexcept -> std::optional<std::size_t>
	{
		if (it.at_start())
			return { 0 };
		else if (it.peek(1) == "\n")
			return { 1 };
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return { { res.value(), result{} } };
		return {};
	}
}; // struct line_begin

/**
 * @brief Matches any of the given rules
 *
 * @tparam Rs... The list of rules to match
 */
template<rule... Rs>
struct any
{
	static constexpr inline auto name = literal<4>{"any"};
	struct result : std::variant<typename Rs::result...>
	{
		size_t n;
	};
	using state = std::tuple<typename Rs::state...>;

	template<Iterator I, size_t N, rule T, rule... Ts>
	static constexpr auto match_impl(I& it, state& state) noexcept
	  -> std::optional<std::size_t>
	{
		if (const auto res = T::template match<I>(it, std::get<N>(state));
		    res.has_value())
			return { res.value() };
		if constexpr (sizeof...(Ts) != 0)
			return match_impl<I, N + 1, Ts...>(it, state);
		return {};
	}

	template<Iterator I>
	static constexpr auto match(I& it, state& state) noexcept
	  -> std::optional<std::size_t>
	{
		return match_impl<I, 0, Rs...>(it, state);
	}

	template<Iterator I, size_t N, rule T, rule... Ts>
	static constexpr auto match_result_impl(I& it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res =
		      T::template match_result<I>(it, std::get<N>(state));
		    res.has_value())
			return { { res.value().first,
				       result{ std::move(res.value().second), N } } };
		if constexpr (sizeof...(Ts) != 0)
			return match_result_impl<I, N + 1, Ts...>(it, state);
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		return match_result_impl<I, 0, Rs...>(it, state);
	}
}; // struct any

/**
 * @brief Prevent a rule from matching
 *
 * If the specified rule matches, this rule will return `nullopt_t{}` to stop further matches
 *
 * @tparam R Rule to exclude from matching
 */
template<rule R>
struct exclude
{
	static constexpr inline auto name = literal<8>{"exclude"};
	struct result
	{};
	using state = typename R::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept
	  -> std::optional<std::size_t>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return {};
		return { 0 };
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return {};
		return { { 0, {} } };
	}
}; // struct exclude

/**
 * @brief Matches a rule repeatedly until the condition stops matching, or Max is reached
 *
 * Stores the result of every invocation of C.
 *
 * @tparam C The condition to keep matching
 * @tparam Min Minimum amount of matches
 * @tparam Max Maximum amount of matches
 */
template<rule C,
         std::size_t Min = 0,
         std::size_t Max = std::numeric_limits<size_t>::max()>
struct repeat
{
	static constexpr inline auto name = literal<7>{"repeat"};
	struct result
	{
		std::size_t n;
		std::vector<typename C::result> result;
	};
	using state = C::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept
	  -> std::optional<std::size_t>
	{
		std::size_t n = 0;
		std::size_t offset = 0;

		while (!it.at_end()) {
			it = it.move(offset);
			if (const auto res = C::template match(it, state);
			    !res.has_value()) {
				if (n >= Min)
					return { offset };
				else
					return {};
			} else
				offset += res.value();
			++n;
			if (n == Max)
				return { offset };
		}
		if (n >= Min)
			return { offset };
		else
			return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		std::size_t n = 0;
		std::vector<typename C::result> res;
		std::size_t i = 0;
		std::size_t offset = 0;

		while (!it.at_end()) {
			it = it.move(i);
			if (const auto r = C::template match_result<I>(it, state);
			    !r.has_value()) {
				if (n >= Min)
					return { { offset, result{ n, std::move(res) } } };
				else
					return {};
			} else {
				res.push_back(std::move(r.value().second));
				i = r.value().first;
			}
			++n;
			offset += i;
			if (n == Max)
				return { { offset, result{ n, std::move(res) } } };
		}
		if (n >= Min)
			return { { offset, result{ n, std::move(res) } } };
		else
			return {};
	}

}; // struct repeat

/**
 * @brief Optionnaly matches a rule
 *
 * On failures, `nullopt_t` will be stored in the result, and the iterator will not advance
 *
 * @param R Rule to match
 */
template<rule R>
struct maybe
{
	static constexpr inline auto name = literal<6>{"maybe"};
	struct result : std::optional<typename R::result>
	{};
	using state = R::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept
	  -> std::optional<std::size_t>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return { res.value() };
		return { 0 };
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept
	  -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = R::match_result(it, state); res.has_value())
			return { { res.value().first, { std::move(res.value().second) } } };
		return { { 0, {} } };
	}
}; // struct maybe

/**
 * @brief Ignores the result of a rule
 *
 * @param R Rule to match
 */
template<rule R>
struct ignore_result
{
	static constexpr inline auto name = literal<14>{"ignore_result"};
	using state = typename R::state;
	struct result
	{};

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return R::match(it, state);
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state)
	  -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return std::make_pair(res.value(), result{});
		return {};
	}
}; // struct ignore_result

/**
 * @brief Names the result of a rule
 *
 * @tparam N Name of the result
 * @tparam R The rule
 */
template<literal N, rule R>
struct named
{
	static constexpr inline auto name = literal<6>{"named"};
	struct result
	{
		static inline constexpr auto name = N;
		R::result result;
	};
	using state = R::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return R::match(it, state);
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state)
	  -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res = R::match_result(it, state); res.has_value())
			return std::make_pair(res.value().first,
			                      result{ std::move(res.value().second) });
		return {};
	}

}; // struct named

/**
 * @brief Matches a sequence of rules
 *
 * @tparam Rs... The sequence of rules to match
 */
template<rule... Rs>
struct compound
{
	static constexpr inline auto name = literal<9>{"compound"};
	using result = std::tuple<typename Rs::result...>;
	using state = std::tuple<typename Rs::state...>;

	template<Iterator I, size_t N, rule T, rule... Ts>
	static constexpr auto match_impl(I it,
	                                 state& state,
	                                 size_t offset) -> std::optional<size_t>
	{
		if (const auto res = T::template match<I>(it, std::get<N>(state));
		    res.has_value()) {
			if constexpr (sizeof...(Ts) != 0)
				return match_impl<I, N + 1, Ts...>(
				  it.move(res.value()), state, offset + res.value());
			return offset + res.value();
		}
		return {};
	}

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return match_impl<I, 0, Rs...>(it, state, 0);
	}

	template<Iterator I, size_t N, rule T, rule... Ts>
	static constexpr auto match_result_impl(I it,
	                                        state& state,
	                                        size_t offset,
	                                        result&& r)
	  -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res =
		      T::template match_result<I>(it, std::get<N>(state));
		    res.has_value()) {
			std::get<N>(r) = std::move(res.value().second);
			if constexpr (sizeof...(Ts) != 0)
				return match_result_impl<I, N + 1, Ts...>(
				  it.move(res.value().first),
				  state,
				  offset + res.value().first,
				  std::move(r));
			return { { offset + res.value().first, std::move(r) } };
		}
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state)
	  -> std::optional<std::pair<size_t, result>>
	{
		result result{};
		return match_result_impl<I, 0, Rs...>(it, state, 0, std::move(result));
	}
};

/**
 * @brief Transforms the result of a sequence of rules
 *
 * @tparam F The function to apply on the results
 * @tparam Rs... The rules to match
 */
template<typename F, rule... Rs>
struct result_aggregator
{
	static constexpr inline auto name = literal<18>{"result_aggregator"};
	using comp = compound<Rs...>;
	using result = std::invoke_result_t<F,
	                                    std::pair<utf8_iterator, utf8_iterator>,
	                                    typename comp::result>;
	using state = comp::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return comp::match(it, state);
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state)
	  -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res = comp::match_result(it, state); res.has_value())
			return std::make_pair(
			  res.value().first,
			  result{
			    F{}.template operator()<I>({ it, it.move(res.value().first) },
			                               std::move(res.value().second)),
			  });
		return {};
	}
}; // struct result_aggregator
} // namespace automata

#endif // AUTOMATA_RULE_HPP

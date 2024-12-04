#ifndef AUTOMATA_RESULT_HPP
#define AUTOMATA_RESULT_HPP

#include "utils.hpp"
#include <ranges>

namespace automata {
namespace detail {
/**
 * @brief Checks if a type has the `name` field and the field verifies @ref
 * is_literal
 */
template<typename T>
concept is_named =
  requires() { requires is_literal<std::remove_cvref_t<decltype(T::name)>>; };

/**
 * @brief Checks if a type implements tuple_element
 */
template<class T, std::size_t I>
concept has_tuple_element = requires(T t) {
	typename std::tuple_element_t<I, std::remove_const_t<T>>;
	{
		std::get<I>(t)
	} -> std::convertible_to<const std::tuple_element_t<I, T>&>;
};

/**
 * @brief Checks if a type is tuple-like
 */
template<class T>
concept tuple_like = requires(T t) {
	requires !std::is_reference_v<T>;
	typename std::tuple_size<T>::type;
	requires std::derived_from<
	  std::tuple_size<T>,
	  std::integral_constant<std::size_t, std::tuple_size_v<T>>>;
} && []<std::size_t... N>(std::index_sequence<N...>) {
	return (has_tuple_element<T, N> && ...);
}(std::make_index_sequence<std::tuple_size_v<T>>());

template<literal S, size_t N = 0>
static consteval bool
has_next_key()
{
	if constexpr (S[N] == '.')
		return true;
	else if constexpr (N + 1 < S.size())
		return has_next_key<S, N + 1>();
	else
		return false;
}

template<literal Key>
static consteval auto
make_key()
{
	if constexpr (Key[0] >= '0' && Key[0] <= '9') {
		std::size_t i{};

		for (const auto pos :
		     std::ranges::iota_view{ std::size_t{}, Key.size() })
			i = i * 10 + Key[pos] - '0';

		return i;
	} else
		return Key;
}

template<literal S, size_t N = 0>
static consteval auto
get_next_key()
{
	if constexpr (S[N] == '.') {
		static_assert(N + 1 < S.size(), "Invalid key");
		return std::make_pair(S.template substr<0, N>(),
		                      S.template substr<N + 1, S.size()>());
	} else if constexpr (N + 1 < S.size()) {
		return get_next_key<S, N + 1>();
	} else {
		[]<bool v>() {
			static_assert(v, "Failed to split");
		}.template operator()<false>();
	}
}

template<literal key, typename T>
constexpr bool
key_compare()
{
	if constexpr (is_named<std::remove_cvref_t<T>>) {
		return std::remove_cvref_t<T>::name == key;
	} else {
		return false;
	}
}

template<std::size_t key, std::size_t val>
constexpr bool
key_compare()
{
	return val == key;
}

template<literal Key, size_t N, typename R>
static constexpr decltype(auto)
get_impl(R&& r)
{
	if constexpr (has_next_key<Key>()) {
		static constexpr auto val = detail::get_next_key<Key>();
		static constexpr auto key = detail::make_key<val.first>();
		using T =
		  std::remove_cvref_t<std::tuple_element_t<N, std::remove_cvref_t<R>>>;

		if constexpr (std::is_same_v<std::remove_cvref_t<decltype(key)>,
		                             std::size_t>) {
			if constexpr (is_named<T>) {
				return get_impl<val.second, 0>(
				  std::get<key>(std::forward<R>(r)).result);
			} else {
				return get_impl<val.second, 0>(
				  std::get<key>(std::forward<R>(r)));
			}
		} else if constexpr (is_named<T> && key_compare<key, T>()) {
			return get_impl<val.second, 0>(
			  std::get<N>(std::forward<R>(r)).result);
		} else if constexpr (N + 1 <
		                     std::tuple_size_v<std::remove_cvref_t<R>>) {
			return get_impl<Key, N + 1>(std::forward<R>(r));
		}
	} else {
		static constexpr auto key = detail::make_key<Key>();
		if constexpr (tuple_like<std::remove_cvref_t<R>>) {
			using T = std::remove_cvref_t<
			  std::tuple_element_t<N, std::remove_cvref_t<R>>>;
			if constexpr (std::is_same_v<std::remove_cvref_t<decltype(key)>,
			                             std::size_t>) {
				if constexpr (is_named<
				                std::tuple_element_t<key,
				                                     std::remove_cvref_t<R>>>) {
					return (std::get<key>(std::forward<R>(r)).result);
				} else {
					return (std::get<key>(std::forward<R>(r)));
				}
			} else if constexpr (is_named<T> && key_compare<key, T>()) {
				return (std::get<N>(std::forward<R>(r)).result);
			} else if constexpr (N + 1 <
			                     std::tuple_size_v<std::remove_cvref_t<R>>) {
				return get_impl<Key, N + 1, R>(std::forward<R>(r));
			}
		} else {
			if constexpr (std::is_same_v<std::remove_cvref_t<decltype(key)>,
		                             std::size_t>) {
				if constexpr (is_named<
				                std::tuple_element_t<key,
				                                     std::remove_cvref_t<R>>>) {
					return (std::get<key>(std::forward<R>(r)).result);
				} else {
					return (std::get<key>(std::forward<R>(r)));
				}
			} else if constexpr (is_named<std::remove_cvref_t<R>> &&
			                     key_compare<key, std::remove_cvref_t<R>>()) {
				return (std::forward<R>(r).result);
			}
		}
	}
}
} // namespace detail

/**
 * @brief Gets a result value by key
 *
 * # Example
 *
 * Consider the following rule:
 * @code{.cpp}
 * using num_parser = compound<
 *	  repeat<any_string<" ", "\t">>,
 *	  named<
 *	    "value",
 *	    compound<
 *	      named<"sign", maybe<any_string<"+", "-">>>,
 *	      named<
 *	        "number",
 *	        repeat<any_string<"0", "1", "2", "3", "4", "5", "6", "7", "8", "9">,
 *	               1>>>>>;
 * @endcode
 *
 * If you want to extract the value of the sign, use the following:
 * @code{.cpp}
 * automata::get<"value.sign">(result);
 * @endcode
 * Since the `number` is the second token in the second `compound` rule, you can
 *use
 * @code{.cpp}
 * automata::get<"value.1">(result);
 * @endcode
 *
 * @param r The result
 * @tparam Key The key to extract from
 */
template<literal Key, typename R>
static constexpr decltype(auto)
get(R&& r)
{
	return detail::get_impl<Key, 0, R>(std::forward<R>(r));
}

} // namespace automata

#endif // AUTOMATA_RESULT_HPP

#ifndef AUTOMATA_HPP
#define AUTOMATA_HPP

#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>
#include <optional>
#include <ostream>
#include <ranges>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

namespace automata {
template<typename I>
concept Iterator = requires(I& it, const I& it2, ssize_t n) {
	{ it.move(n) } -> std::same_as<I>;
	{ it.next() } -> std::same_as<I>;
	{ it.peek(n) } -> std::same_as<std::string_view>;
	{ it.at_start() } -> std::same_as<bool>;
	{ it.at_end() } -> std::same_as<bool>;
	{ it != it2 } -> std::same_as<bool>;
	{ it == it2 } -> std::same_as<bool>;
};

// TODO
struct utf8_iterator
{
	ssize_t pos;
	std::string_view str;

	utf8_iterator(ssize_t pos, const std::string_view str)
	  : pos{ pos }
	  , str{ str }
	{
	}

	utf8_iterator(const std::string_view str)
	  : pos{}
	  , str{ str }
	{
	}

	ssize_t pos_at(ssize_t pos) const
	{
		if (pos < 0)
			return 0;
		if (pos >= (ssize_t)str.size())
			return str.size();
		return pos;
	}

	bool at_start() const { return pos == 0; }

	bool at_end() const { return pos >= (ssize_t)str.size() - 1; }

	utf8_iterator next() { return utf8_iterator{ pos_at(pos + 1), str }; }

	utf8_iterator move(ssize_t n)
	{
		if (pos + n < 0)
			return utf8_iterator{ 0, str };
		if (pos + n >= (ssize_t)str.size())
			return utf8_iterator{ (ssize_t)str.size(), str };
		return utf8_iterator{ pos + n, str };
	}

	std::string_view peek(size_t n)
	{
		return std::string_view{ str.data() + pos,
			                     str.data() + pos_at(pos + n) };
	}

	friend bool operator==(const utf8_iterator& self,
	                       const utf8_iterator& other)
	{
		return self.pos == other.pos;
	}

	friend bool operator!=(const utf8_iterator& self,
	                       const utf8_iterator& other)
	{
		return self.pos != other.pos;
	}
}; // struct utf8_iterator

template<std::size_t N>
struct literal
{
	explicit constexpr literal(const char* s, int)
	{
		std::copy_n(s, N, m_data);
	}

	constexpr literal(const char (&literal)[N])
	{
		std::copy_n(literal, N, m_data);
	}

	explicit constexpr operator std::string_view() const
	{
		return std::string_view(m_data, N - 1);
	}

	template<std::size_t Start, std::size_t Sz>
	consteval auto substr() const
	{
		constexpr size_t sz = std::min(Sz, size());

		return literal<sz - Start + 1>(m_data + Start, 0);
	}

	[[nodiscard]] static consteval std::size_t size() noexcept { return N - 1; }

	[[nodiscard]] consteval char* data() noexcept { return m_data; }

	char m_data[N]; ///< Literal content

	template<std::size_t M>
	[[nodiscard]] constexpr friend bool operator==(const literal<N>& s,
	                                               const literal<M>& t) noexcept
	{
		if constexpr (N != M)
			return false;

		return [&]<std::size_t... i>(std::index_sequence<i...>) {
			return ((s.m_data[i] == t.m_data[i]) && ...);
		}(std::make_index_sequence<N - 1>{});
	}

	[[nodiscard]] constexpr char& operator[](std::size_t i) noexcept
	{
		return m_data[i];
	}

	[[nodiscard]] constexpr char operator[](std::size_t i) const noexcept
	{
		return m_data[i];
	}
}; // struct literal

template<typename T>
concept is_literal = requires {
	{
		([]<std::size_t N>(const literal<N>*) {})(
		  static_cast<const std::remove_cvref_t<T>*>(nullptr))
	};
};

template<class T>
concept Rule = requires(T::state& state) {
	typename T::result;
	typename T::state;

	// Defaults
	{ typename T::result{} } -> std::same_as<typename T::result>;
	{ typename T::state{} } -> std::same_as<typename T::state>;

	//{ T::template match<I>(it, state) } ->
	// std::same_as<std::optional<std::size_t>>; { T::template
	// match_result<I>(it, state) } ->
	// std::same_as<std::optional<std::pair<std::size_t, typename T::result>>>;
}; // concept Rule

template<literal S>
struct string_match
{
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

struct line_end
{
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

struct line_begin
{
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

template<Rule... Rs>
struct any
{
	struct result : std::variant<typename Rs::result...>
	{
		size_t n;
	};
	struct state : std::tuple<typename Rs::state...>
	{};

	template<Iterator I, size_t N, Rule T, Rule... Ts>
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

	template<Iterator I, size_t N, Rule T, Rule... Ts>
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

template<Rule R>
struct exclude
{
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

template<Rule C,
         std::size_t Min = 0,
         std::size_t Max = std::numeric_limits<size_t>::max()>
struct repeat
{
	struct result
	{
		std::size_t n;
		std::vector<typename C::result> result;
	};
	struct state : C::state
	{};

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

template<Rule R>
struct maybe
{
	struct result : std::optional<typename R::result>
	{};
	struct state : R::state
	{};

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

template<Rule R>
struct ignore_result
{
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

template<literal N, Rule R>
struct named
{
	using state = R::state;
	struct result
	{
		static inline constexpr auto name = N;
		R::result result;
	};

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

// TODO: class result_aggregator
// TODO: class state_aggregator
// TODO: partial result
template<Rule... Rs>
struct compound
{
	// TODO:
	using result = std::tuple<typename Rs::result...>;
	using state = std::tuple<typename Rs::state...>;

	template<Iterator I, size_t N, Rule T, Rule... Ts>
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

	template<Iterator I, size_t N, Rule T, Rule... Ts>
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

template<typename F, Rule... Rs>
struct result_aggregator
{
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

namespace detail {
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
		     std::ranges::iota_view{ std::size_t{}, Key.size() }) {
			i = i * 10 + Key[pos] - '0';
		}
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

template<typename T>
concept Named = requires() {
	requires is_literal<std::remove_cvref_t<decltype(T::name)>>;
};

/// @brief Checks if a type implements tuple_element
template<class T, std::size_t I>
concept has_tuple_element = requires(T t) {
	typename std::tuple_element_t<I, std::remove_const_t<T>>;
	{
		std::get<I>(t)
	} -> std::convertible_to<const std::tuple_element_t<I, T>&>;
};

/// @brief Checks if a type is tuple-like
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

template<literal key, typename T>
constexpr bool
key_compare()
{
	if constexpr (Named<std::remove_cvref_t<T>>)
	{
		return std::remove_cvref_t<T>::name == key;
	}
	else
	{
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
		using T = std::remove_cvref_t<std::tuple_element_t<N, std::remove_cvref_t<R>>>;

		if constexpr (std::is_same_v<std::remove_cvref_t<decltype(key)>,
		                             std::size_t>) {
			if constexpr (Named<T>) {
				return get_impl<val.second, 0>(
				  std::get<key>(std::forward<R>(r)).result);
			} else {
				return get_impl<val.second, 0>(
				  std::get<key>(std::forward<R>(r)));
			}
		} else if constexpr (Named<T> && key_compare<key, T>()) {
			return get_impl<val.second, 0>(
			  std::get<N>(std::forward<R>(r)).result);
		} else if constexpr (N + 1 <
		                     std::tuple_size_v<std::remove_cvref_t<R>>) {
			return get_impl<Key, N + 1>(std::forward<R>(r));
		}
	} else {
		static constexpr auto key = detail::make_key<Key>();
		if constexpr (tuple_like<std::remove_cvref_t<R>>) {
			using T = std::remove_cvref_t<std::tuple_element_t<N, std::remove_cvref_t<R>>>;
			if constexpr (std::is_same_v<std::remove_cvref_t<decltype(key)>,
			                             std::size_t>) {
				if constexpr (Named<
				                std::tuple_element_t<key,
				                                     std::remove_cvref_t<R>>>) {
					return (std::get<key>(std::forward<R>(r)).result);
				} else {
					return (std::get<key>(std::forward<R>(r)));
				}
			} else if constexpr (Named<T> && key_compare<key, T>()) {
				return (std::get<N>(std::forward<R>(r)).result);
			} else if constexpr (N + 1 <
			                     std::tuple_size_v<std::remove_cvref_t<R>>) {
				return get_impl<Key, N + 1, R>(std::forward<R>(r));
			}
		} else {
			if constexpr (key_compare<key, 0>()) {
				if constexpr (Named<
				                std::tuple_element_t<key,
				                                     std::remove_cvref_t<R>>>) {
					return (std::forward<R>(r).result);
				} else {
					return (std::forward<R>(r));
				}
			} else if constexpr (Named<std::remove_cvref_t<R>> &&
			                     key_compare<key, std::remove_cvref_t<R>>()) {
				return (std::forward<R>(r).result);
			}
		}
	}
}
} // namespace detail

template<literal Key, typename R>
static constexpr decltype(auto)
get(R&& r)
{
	return detail::get_impl<Key, 0, R>(std::forward<R>(r));
}

} // namespace automata

#endif // AUTOMATA_HPP

#ifndef AUTOMATA_HPP
#define AUTOMATA_HPP

#include <iostream>
#include <iterator>
#include <limits>
#include <optional>
#include <algorithm>
#include <ostream>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <vector>
#include <utility>
#include <tuple>

namespace automata
{
template <typename I>
concept Iterator = requires(I& it, const I& it2, ssize_t n)
{
	{ it.move(n) } -> std::same_as<I>;
	{ it.next() } -> std::same_as<I>;
	{ it.peek(n) } -> std::same_as<std::string_view>;
	{ it.at_start() } ->std::same_as<bool>;
	{ it.at_end() } ->std::same_as<bool>;
	{ it != it2 } ->std::same_as<bool>;
	{ it == it2 } ->std::same_as<bool>;
};

// TODO
struct utf8_iterator
{
	ssize_t pos;
	std::string_view str;

	utf8_iterator(ssize_t pos, const std::string_view str):
		pos{pos}, str{str} {}

	utf8_iterator(const std::string_view str):
		pos{}, str{str} {}

	ssize_t pos_at(ssize_t pos) const
	{
		if (pos < 0)
			return 0;
		if (pos >= (ssize_t)str.size())
			return str.size();
		return pos;
	}

	bool at_start() const
	{
		return pos == 0;
	}

	bool at_end() const
	{
		return pos >= (ssize_t)str.size() - 1;
	}

	utf8_iterator next()
	{
		return utf8_iterator{pos_at(pos + 1), str};
	}

	utf8_iterator move(ssize_t n)
	{
		if (pos + n < 0)
			return utf8_iterator{0, str};
		if (pos + n >= (ssize_t)str.size())
			return utf8_iterator{(ssize_t)str.size(), str};
		return utf8_iterator{pos + n, str};
	}

	std::string_view peek(size_t n)
	{
		return std::string_view{str.data() + pos, str.data() + pos_at(pos + n)};
	}

	friend bool operator==(const utf8_iterator& self, const utf8_iterator& other)
	{
		return self.pos == other.pos;
	}

	friend bool operator!=(const utf8_iterator& self, const utf8_iterator& other)
	{
		return self.pos != other.pos;
	}
}; // struct utf8_iterator

template <std::size_t N>
struct literal
{
	constexpr literal(const char (&literal)[N])
	{
		std::copy_n(literal, N, m_data);
	}

	explicit constexpr operator std::string_view() const
	{
		return std::string_view(m_data, N-1);
	}

	[[nodiscard]] static consteval std::size_t size() noexcept
	{ return N - 1; }

	[[nodiscard]] consteval char* data() noexcept
	{ return m_data; }

	char m_data[N]; ///< Literal content @note Null terminated

	template <std::size_t M>
		[[nodiscard]] constexpr friend bool operator==(const literal<N>& s, const literal<M>& t) noexcept
		{
			if constexpr (N != M)
				return false;

			return [&]<std::size_t... i>(std::index_sequence<i...>)
			{
				return ((s.m_data[i] == t.m_data[i]) && ...);
			}(std::make_index_sequence<N>{});
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

template <class T>
concept Rule = requires(T::state& state) {
	typename T::result;
	typename T::state;

	// Defaults
	{ typename T::result{} } -> std::same_as<typename T::result>;
	{ typename T::state{} } -> std::same_as<typename T::state>;

	//{ T::template match<I>(it, state) } -> std::same_as<std::optional<std::size_t>>;
	//{ T::template match_result<I>(it, state) } -> std::same_as<std::optional<std::pair<std::size_t, typename T::result>>>;
}; // concept Rule

template <literal S>
struct string_match {
	struct result {};
	struct state {};

	template <Iterator I>
	static constexpr auto match(I& it, state&) noexcept
		-> std::optional<std::size_t>
	{
		if (it.peek(S.size()) == (std::string_view)S)
			return {S.size()};
		return {};
	}

	template <Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
		-> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return {{res.value(), result{}}};
		return {};
	}
}; // struct string_match

struct line_end {
	struct result {};
	struct state {};

	template <Iterator I>
	static constexpr auto match(I& it, state&) noexcept
		-> std::optional<std::size_t>
	{
		if (it.at_end())
			return {0};
		else if (it.peek(1) == "\n")
			return {1};
		return {};
	}

	template <Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
		-> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return {{res.value(), result{}}};
		return {};
	}
}; // struct line_end

struct line_begin {
	struct result {};
	struct state {};

	template <Iterator I>
	static constexpr auto match(I& it, state&) noexcept
		-> std::optional<std::size_t>
	{
		if (it.at_start())
			return {0};
		else if (it.peek(1) == "\n")
			return {1};
		return {};
	}

	template <Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
		-> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = match(it, state); res.has_value())
			return {{res.value(), result{}}};
		return {};
	}
}; // struct line_begin

template <Rule... Rs>
struct any
{
	struct result : std::variant<typename Rs::result...> {
		size_t n;
	};
	struct state : std::tuple<typename Rs::state...> {};

	template <Iterator I, size_t N, Rule T, Rule... Ts>
	static constexpr auto match_impl(I& it, state& state) noexcept
		-> std::optional<std::size_t>
	{
		if (const auto res = T::template match<I>(it, std::get<N>(state)); res.has_value())
			return {res.value()};
		if constexpr (sizeof...(Ts) != 0)
			return match_impl<I, N + 1, Ts...>(it, state);
		return {};
	}

	template <Iterator I>
	static constexpr auto match(I& it, state& state) noexcept
		-> std::optional<std::size_t>
	{
		return match_impl<I, 0, Rs...>(it, state);
	}

	template <Iterator I, size_t N, Rule T, Rule... Ts>
	static constexpr auto match_result_impl(I& it, state& state) noexcept
		-> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = T::template match_result<I>(it, std::get<N>(state)); res.has_value())
			return {{res.value().first, result{std::move(res.value().second), N}}};
		if constexpr (sizeof...(Ts) != 0)
			return match_result_impl<I, N + 1, Ts...>(it, state);
		return {};
	}

	template <Iterator I>
	static constexpr auto match_result(I& it, state& state) noexcept
		-> std::optional<std::pair<std::size_t, result>>
	{
		return match_result_impl<I, 0, Rs...>(it, state);
	}
}; // struct any

template <Rule R>
struct noto
{
	struct result {};
	using state = typename R::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept -> std::optional<std::size_t>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return {};
		return {0};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return {};
		return {{0, {}}};
	}
}; // struct not

template <Rule C, std::size_t Min = 0, std::size_t Max = std::numeric_limits<size_t>::max()>
struct repeat
{
	struct result {
		std::size_t n;
		std::vector<typename C::result> result;
	};
	struct state : C::state {};

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept -> std::optional<std::size_t>
	{
		std::size_t n = 0;
		std::size_t offset = 0;

		while (!it.at_end())
		{
			it = it.move(offset);
			if (const auto res = C::template match(it, state); !res.has_value())
			{
				if (n >= Min)
					return {offset};
				else
					return {};
			}
			else
				offset += res.value();
			++n;
			if (n == Max)
				return {offset};
		}
		if (n >= Min)
			return {offset};
		else
			return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept -> std::optional<std::pair<std::size_t, result>>
	{
		std::size_t n = 0;
		std::vector<typename C::result> res;
		std::size_t i = 0;
		std::size_t offset = 0;

		while (!it.at_end())
		{
			it = it.move(i);
			if (const auto r = C::template match_result<I>(it, state); !r.has_value())
			{
				if (n >= Min)
					return {{offset, result{n, std::move(res)}}};
				else
					return {};
			}
			else
			{
				res.push_back(std::move(r.value().second));
				i = r.value().first;
			}
			++n;
			offset += i;
			if (n == Max)
				return {{offset, result{n, std::move(res)}}};
		}
		if (n >= Min)
			return {{offset, result{n, std::move(res)}}};
		else
			return {};
	}
	
}; // struct repeat

template <Rule R>
struct maybe
{
	struct result : std::optional<typename R::result> {};
	struct state : R::state {};

	template<Iterator I>
	static constexpr auto match(I it, state& state) noexcept -> std::optional<std::size_t>
	{
		if (const auto res = R::match(it, state); res.has_value())
			return {res.value()};
		return {0};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) noexcept -> std::optional<std::pair<std::size_t, result>>
	{
		if (const auto res = R::match_result(it, state); res.has_value())
			return {{res.value().first, {std::move(res.value().second)}}};
		return {{0, {}}};
	}
}; // struct maybe

// TODO: class result_aggregator
// TODO: class state_aggregator
// TODO: partial result
template <Rule... Rs>
struct compound
{
	// TODO:
	using result = std::tuple<typename Rs::result...>;
	using state = std::tuple<typename Rs::state...>;

	template <Iterator I, size_t N, Rule T, Rule... Ts>
	static constexpr auto match_impl(I it, state& state, size_t offset) -> std::optional<size_t>
	{
		if (const auto res = T::template match<I>(it, std::get<N>(state)); res.has_value())
		{
			if constexpr (sizeof...(Ts) != 0)
				return match_impl<I, N + 1, Ts...>(it.move(res.value()), state, offset + res.value());
			return offset + res.value();
		}
		return {};
	}

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return match_impl<I, 0, Rs...>(it, state, 0);
	}

	template <Iterator I, size_t N, Rule T, Rule... Ts>
	static constexpr auto match_result_impl(I it, state& state, size_t offset, result&& r) -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res = T::template match_result<I>(it, std::get<N>(state)); res.has_value())
		{
			std::get<N>(r) = std::move(res.value().second);
			if constexpr (sizeof...(Ts) != 0)
				return match_result_impl<I, N + 1, Ts...>(it.move(res.value().first), state, offset + res.value().first, std::move(r));
			return {{offset + res.value().first, std::move(r)}};
		}
		return {};
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) -> std::optional<std::pair<size_t, result>>
	{
		result result{};
		return match_result_impl<I, 0, Rs...>(it, state, 0, std::move(result));
	}
};

template <typename F, Rule... Rs>
struct result_aggregator
{
	using comp = compound<Rs...>;
	using result = std::invoke_result_t<F, std::pair<utf8_iterator, utf8_iterator>, typename comp::result>;
	using state = comp::state;

	template<Iterator I>
	static constexpr auto match(I it, state& state) -> std::optional<size_t>
	{
		return comp::match(it, state);
	}

	template<Iterator I>
	static constexpr auto match_result(I it, state& state) -> std::optional<std::pair<size_t, result>>
	{
		if (const auto res = comp::match_result(it, state); res.has_value())
			return std::make_pair(res.value().first, result{
				F{}.template operator()<I>({it, it.move(res.value().first)}, std::move(res.value().second)),
			});
		return {};
	}
}; // struct result_aggregator

} // namespace automata

#endif // AUTOMATA_HPP

#ifndef AUTOMATA_UTILS_HPP
#define AUTOMATA_UTILS_HPP

#include <algorithm>
#include <cstddef>
#include <string_view>

namespace automata {
/**
 * @brief Class that holds a string literal as a Non-Type Template Parameter
 *
 * @tparam N Size of the literal string (+1) for the null terminator
 */
template<std::size_t N>
struct literal
{
	/**
	 * @brief Content of the literal
	 * The last element should be ignored
	 */
	char m_data[N];

	/**
	 * @brief Constructs a new literal from a char*, the second parameter is
	 * used to avoid confusion with the other constructor
	 *
	 * @param s The string to construct from
	 * @param ignored Ignored parameter
	 */
	explicit constexpr literal(const char* s, [[maybe_unused]] int ignored)
	{
		std::copy_n(s, N, m_data);
	}

	/**
	 * @brief Constructs a literal from a char array
	 *
	 * @param literal The char array to construct from
	 */
	constexpr literal(const char (&literal)[N])
	{
		std::copy_n(literal, N, m_data);
	}

	/**
	 * @brief Creates a std::string_view from a literal
	 *
	 * @returns A std::string_view
	 */
	explicit constexpr operator std::string_view() const
	{
		return std::string_view(m_data, N - 1);
	}

	/**
	 * @brief Creates a substring literal
	 *
	 * @tparam Start Start position in the string
	 * @tparam Sz Number of bytes of the substring
	 */
	template<std::size_t Start, std::size_t Sz>
	consteval auto substr() const
	{
		constexpr size_t sz = std::min(Sz, size());

		return literal<sz - Start + 1>(m_data + Start, 0);
	}

	/**
	 * @brief Gets the size of the literal
	 *
	 * @returns The size of the literal
	 */
	[[nodiscard]] static consteval std::size_t size() noexcept { return N - 1; }

	/**
	 * @brief Gets a pointer to the literal's content
	 *
	 * @returns A pointer to the literal's data
	 */
	[[nodiscard]] consteval char* data() noexcept { return m_data; }

	/**
	 * @brief Checks if two literals are equal
	 *
	 * @param s The first literal
	 * @param t The second literal
	 *
	 * @returns true If `s == t`
	 */
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

	/**
	 * @brief Gets the byte at an offset in the literal
	 */
	[[nodiscard]] constexpr char& operator[](std::size_t i) noexcept
	{
		return m_data[i];
	}

	/**
	 * @brief Gets the byte at an offset in the literal
	 */
	[[nodiscard]] constexpr char operator[](std::size_t i) const noexcept
	{
		return m_data[i];
	}
}; // struct literal

/**
 * @brief Concept to check if a typename is @ref literal
 */
template<typename T>
concept is_literal = requires {
	{
		([]<std::size_t N>(const literal<N>*) {})(
		  static_cast<const std::remove_cvref_t<T>*>(nullptr))
	};
}; // concept is_literal
} // namespace automata

#endif // AUTOMATA_UTILS_HPP

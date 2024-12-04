#ifndef AUTOMATA_ITERATOR_HPP
#define AUTOMATA_ITERATOR_HPP

#include <concepts>
#include <string_view>
#include <sys/types.h>

namespace automata {
/**
 * @brief The iterator concept
 */
template<typename I>
concept Iterator = requires(I& it, const I& it2, ssize_t n) {
	{ it.move(n) } -> std::same_as<I>;
	{ it.next() } -> std::same_as<I>;
	{ it.peek(n) } -> std::same_as<std::string_view>;
	{ it.until(it2) } -> std::same_as<std::string_view>;
	{ it.at_start() } -> std::same_as<bool>;
	{ it.at_end() } -> std::same_as<bool>;
	{ it != it2 } -> std::same_as<bool>;
	{ it == it2 } -> std::same_as<bool>;
};

/**
 * @brief The default iterator
 *
 * TODO: Fix unicode support
 */
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

	bool at_end() const { return pos >= (ssize_t)str.size(); }

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

	std::string_view until(const utf8_iterator& other)
	{
		return std::string_view{ str.data() + pos,
			                     str.data() + other.pos };
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
} // namespace automata

#endif // AUTOMATA_ITERATOR_HPP

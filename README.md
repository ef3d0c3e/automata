# Automata -- Compile-time parser generator for C++

# Examples

## Parse comma-separated arguments:
```C++
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
```

# License

Automata is licensed under MIT, see [LICENSE](./LICENSE) for more information.

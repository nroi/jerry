defmodule Jerry do
  @moduledoc """
  Documentation for Jerry.
  """

  def intermediate_repr(s, kv_pairs \\ []) do
    # Append \n just to make things simpler, where we can assume lines always end with \n.
    case key_value_pairs(normalize(s), kv_pairs, false) do
      {:eof, pairs} -> Enum.reverse(pairs)
      {:continue, {rest, pairs}} ->
        intermediate_repr(rest, pairs)
    end
  end

  # the intermediate representation contains tokens in the order they have occured in the original
  # string. This also means that for array of tables, different key-value-pairs belonging to the
  # same array-of-table are spread at different positions in the list.
  # Hence, all arrays-of-tables are "compressed" such that we can subsequently generate the
  # final map by looking at each item of the list, one by one.
  def compress_intermediate(intermediate_repr) do
    {arrays_of_tables, other} = Enum.split_with(intermediate_repr, fn
      {:toml_array_of_tables, _, _} -> true
      _ -> false
    end)
    compressed = arrays_of_tables |> Enum.group_by(fn {:toml_array_of_tables, name, _} ->
      unquote_array_of_tables(name)
    end)
    Enum.map(compressed, fn {name, arrays} ->
      {:toml_arrays_of_tables, name, arrays}
    end) ++ other
  end

  def decode!(s) do
    s |> intermediate_repr |> compress_intermediate |> kv_pairs_to_map
  end

  def concat(r1, r2) do
    s1 = Regex.source(r1)
    s2 = Regex.source(r2)
    # Note that this is not the most performant way:
    # We take two precompiled regexes, then "uncompile" them only to recompile them.
    # Perhaps we can make use of macros to make this faster.
    Regex.compile(s1 <> s2)
  end

  def iv(r) do
    # just a workaround to allow us to have regex syntax highlighting
    # and to compose multiple regex using string interpolation.
    Regex.source(r)
  end

  # Used to fetch keys from regex captures: "" is treated as if it weren't present.
  defp fetch(m = %{}, key) do
    case Map.fetch(m, key) do
      {:ok, ""} -> nil
      {:ok, x } -> x
      :error -> nil
    end
  end

  def intermediate2val({:toml_integer, int_str}) do
    # Leading zeroes are prohibited.
    int_regex = ~r{^(?<sign>\+|-)?(?<number>\d|([1-9](\d|(_\d))+))$}
    {factor, int_str} = case Regex.named_captures(int_regex, int_str) do
      %{"sign" => "+", "number" => number} -> { 1, number}
      %{"sign" => "-", "number" => number} -> {-1, number}
      %{"sign" => "", "number" => number}  -> { 1, number}
      nil -> raise "Unable to parse integer: #{inspect int_str}"
    end
    factor * (int_str |> String.replace("_", "") |> String.to_integer)
  end

  def intermediate2val({:toml_float, float}) do
    zero_prefixable_int = iv ~r/\d(\d|_\d)*/
    frac = iv ~r/\.(#{zero_prefixable_int})/
    integer = iv ~r/(\+|-)?\d/
    exp = iv ~r/(e|E)(#{integer})/
    re = ~r/^(?<pre>#{integer})((?<f1>#{frac})|((?<f2>#{frac})(?<e1>#{exp}))|(?<e2>#{exp}))$/
    case Regex.named_captures(re, float) do
      nil ->
        nil
      captures ->
        pre = captures["pre"]
        frac = fetch(captures, "f1") || fetch(captures, "f2") || ".0"
        exp = case fetch(captures, "e1") || fetch(captures, "e2") do
          "e" <> rest -> rest
          "E" <> rest -> rest
          nil -> nil
        end
        mantissa = String.to_float(pre <> frac)
        factor = case exp do
          nil -> 1
          e ->
            exponent = String.to_integer(e)
            :math.pow(10, exponent)
        end
        mantissa * factor
    end
  end

  def intermediate2val({:toml_boolean, "false"}), do: false
  def intermediate2val({:toml_boolean, "true"}), do: true

  # TODO datetime not supported for now.
  def intermediate2val(m = {:toml_datetime, _}), do: m

  def intermediate2val({:toml_array, array}) do
    Enum.map(array, &intermediate2val/1)
  end

  def intermediate2val({:toml_table, name, table_pairs}) do
    kv_pairs = Enum.map(table_pairs, fn
      {:key, name, value} -> {unquote_string(name), intermediate2val(value)}
    end)
    kv_map = Map.new(kv_pairs)
    {unquote_table_name(name), kv_map}
  end

  def intermediate2val({:toml_inline_table, table_pairs}) do
    kv_pairs_to_map(table_pairs)
  end

  def intermediate2val({:toml_arrays_of_tables, name, arrays}) do
    {name, Enum.map(arrays, fn
      {:toml_array_of_tables, _name, kv_pairs} ->
        kv_pairs_to_map(kv_pairs)
    end)}
  end

  def intermediate2val({:key, name, value}) do
    {unquote_string(name), intermediate2val(value)}
  end

  def intermediate2val({:toml_basic_string, string = ~s(") <> _}) do
    # TODO not finished, does not support multiline strings.
    Regex.named_captures(~r/^"(?<value>.*)"$/, string)["value"]
  end
  def intermediate2val({:toml_basic_string, string = ~s(') <> _}) do
    Regex.named_captures(~r/^'(?<value>.*)'$/, string)["value"]
  end

  def unquote_string(~s(") <> rest), do: String.replace_suffix(rest, ~s("), "")
  def unquote_string(~s(') <> rest), do: String.replace_suffix(rest, ~s('), "")
  def unquote_string(key_name), do: key_name

  def unquote_table_name("[" <> rest) do
    rest |> String.replace_suffix("]", "") |> unquote_string
  end
  def unquote_array_of_tables("[[" <> rest) do
    # TODO perhaps we can refactor this by taking into account that:
    # "Naming rules for each dot separated part are the same as for keys
    # (see definition of Key/Value Pairs)."
    # i.e., use the same function for both keys and table names.
    rest |> String.replace_suffix("]]", "") |> unquote_string
  end

  # Given a list such as [{:key, "foo", 1}], return the corresponding map, e.g. %{"foo" => 1}
  # TODO introduce a type such as kv_pairs :: [kv_pair], kv_pair == {:key, String.t, value}
  def kv_pairs_to_map(kv_pairs) do
    pairs = Enum.map(kv_pairs, &intermediate2val/1)
    Map.new(pairs)
  end

  def normalize(s) do
    # Use unix convention, using \n as line breaks and at least one newline after each line (i.e.,
    # the last line is guaranteed to end with \n).
    (s |> String.trim_leading |> String.replace("\r\n", "\n")) <> "\n"
  end

  def parse_key(""), do: :eof
  def parse_key(~s("") <> rest) do
    # A key consisting of "" is allowed in Toml (although discouraged).
    {{:key, ~s("")}, rest}
  end
  def parse_key("#" <> rest) do
    # Skip comment and trailing space after that comment.
    next = String.replace(rest, ~r{^.*\n\s*}, "", global: :false)
    parse_key(next)
  end
  def parse_key("\"" <> rest) do
    case parse_quoted_string(rest) do
      {{:quoted_string, ss}, rest} -> {{:key, ss}, rest}
    end
  end
  def parse_key(k = "[[" <> _) do
    {table, rest} = split_newline(k)
    {:parse_array_of_tables, {table, rest}}
  end
  def parse_key(k = "[" <> _) do
    {table, rest} = split_newline(k)
    IO.puts "It's a table, continue with #{inspect rest}"
    {:parse_table, {table, rest}}
  end
  def parse_key(s) do
    [key, rest] = String.split(s, ~r(\s*=\s*), parts: 2)
    {{:key, key}, rest}
  end

  defp split_newline(s) do
    case Regex.named_captures(~r/^(?<name>.*?)\s*(#.*?)?\n(?<rest>.*)/s, s) do
      %{"name" => name, "rest" => rest} -> {name, rest}
    end
  end

  defp skip_comment(s) do
    # Ignore all commente lines, as well as leading whitespace followed by those comment lines.
    String.replace(s, ~r/^(\s*(#.*)?\n)*\s*/, "")
  end

  def key_value_pairs("", pairs, _), do: {:eof, pairs}
  def key_value_pairs(s, pairs, inside_table) do
    IO.puts "key_value_pairs(#{inspect s})"
    case parse_key(String.trim_leading(s)) do
      {:parse_array_of_tables, {table, rest}} when inside_table ->
        # Do not parse this table as the values of a preceding table.
        {:continue, {table <> "\n" <> rest, pairs}}
      {:parse_array_of_tables, {table, rest}} when not inside_table ->
        # TODO copy-pasted from below, refactor.
        case key_value_pairs(rest, [], true) do
          {:continue, {rest, table_pairs}} ->
            table = {:toml_array_of_tables, table, Enum.reverse(table_pairs)}
            {:continue, {rest, [table | pairs]}}
          {:parse_array_of_tables, {_table, _rest}} ->
            raise "bang"
          {:eof, table_pairs} ->
            table = {:toml_array_of_tables, table, Enum.reverse(table_pairs)}
            {:eof, [table | pairs]}
        end
      {:parse_table, {table, rest}} when inside_table ->
        # Do not parse this table as the values of a preceding table.
        {:continue, {table <> "\n" <> rest, pairs}}
      {:parse_table, {table, rest}} when not inside_table ->
        IO.inspect "Fetch values for table #{inspect table}…"
        case key_value_pairs(rest, [], true) do
          {:continue, {rest, table_pairs}} ->
            table = {:toml_table, table, table_pairs}
            {:continue, {rest, [table | pairs]}}
          {:parse_table, {_table, _rest}} ->
            raise "bang"
          {:eof, table_pairs} ->
            table = {:toml_table, table, table_pairs}
            {:eof, [table | pairs]}
        end
      {{:key, key}, rest} ->
        IO.puts "rest: #{inspect rest}"
        rest = String.replace(rest, ~r(^\s*=\s*), "")
        IO.puts "rest: #{inspect rest}"
        {value, rest} = parse_value(skip_comment(rest))
        new_pair = {:key, key, value}
        key_value_pairs(String.trim_leading(rest), [new_pair | pairs], inside_table)
      :eof -> {:eof, pairs}
    end
  end

  def parse_value("true" <> rest) do
    {{:toml_boolean, "true"}, rest}
  end

  def parse_value("false" <> rest) do
    {{:toml_boolean, "false"}, rest}
  end

  def parse_value("'''\n" <> rest) do
    [s, rest] = String.split(rest, "'''", parts: 2)
    {{:toml_multiline_basic_string, "'''\n" <> s <> "'''"}, rest}
  end
  def parse_value("'''" <> rest) do
    [s, rest] = String.split(rest, "'''", parts: 2)
    {{:toml_multiline_basic_string, "'''" <> s <> "'''"}, rest}
  end

  def parse_value("'" <> rest) do
    # single quoted strings must not contain single quoted strings, so we can just assume that the
    # string ends with the next single quote.
    case Regex.run(~r{^(.*?)'(.*)}, rest, capture: :all_but_first) do
      [match, rest] ->
        {{:toml_basic_string, "'#{match}'"}, rest}
    end
  end

  def parse_value("\"\"\"\n" <> rest) do
    # A newline immediately following the opening delimiter will be trimmed
    # TODO consider this ^^ when turning the AST into a map.
    [s, rest] = String.split(rest, "\"\"\"", parts: 2)
    {{:toml_multiline_basic_string, "\"\"\"\n" <> s <> "\"\"\""}, rest}
  end
  def parse_value("\"\"\"" <> rest) do
    [s, rest] = String.split(rest, "\"\"\"", parts: 2)
    {{:toml_multiline_basic_string, "\"\"\"" <> s <> "\"\"\""}, rest}
  end

  def parse_value("\"" <> rest) do
    case parse_quoted_string(rest) do
      {{:quoted_string, s}, rest} -> {{:toml_basic_string, s}, rest}
    end
  end

  def parse_value(n = "+" <> _) do
    parse_number(n)
  end
  def parse_value(n = "-" <> _) do
    parse_number(n)
  end
  def parse_value("[" <> rest) do
    {values, rest} = parse_values(skip_comment(rest), [])
    {{:toml_array, values}, rest}
  end
  def parse_value("{" <> rest) do
    # TODO:
    # "Inline tables are intended to appear on a single line. No newlines are allowed between the
    # curly braces unless they are valid within a value. Even so, it is strongly discouraged to
    # break an inline table onto multiples lines. If you find yourself gripped with this desire, it
    # means you should be using standard tables."
    # To make things easier, we just assume that inline tables appear on a single line for now.
    IO.puts "parse_value({ rest = #{inspect rest}"
    {value_string, rest} = case Regex.run(~r/(.*?)}(.*)/s, rest, capture: :all_but_first) do
      [vs, r] -> {String.trim_leading(vs), r}
    end
    IO.puts "run key_value_str with #{inspect(value_string)}"
    kv_pairs = parse_comma_separated(value_string <> "\n", [])
    {{:toml_inline_table, kv_pairs}, rest}
  end
  def parse_value(n) do
    parse_number(n)
  end

  def parse_values("]" <> rest, acc), do: { Enum.reverse(acc), rest }
  def parse_values(n, acc) do
    {value, rest} = case parse_value(n) do
      {v, r} -> {v, Regex.replace(~r(^\s*,?\s*), r, "", global: false)}
    end
    parse_values(skip_comment(rest), [value | acc])
  end

  # Given a string such as "foo = 1, bar = 2\n", return a list of key-value pairs.
  def parse_comma_separated("", pairs), do: Enum.reverse pairs
  def parse_comma_separated(s, pairs) do
    case parse_key(String.trim_leading(s)) do
      {:parse_table, {_table, _rest}} ->
        raise "Unexpected: table where comma-separated values were expected."
      {{:key, key}, rest} ->
        IO.puts "rest: #{inspect rest}"
        rest = String.replace(rest, ~r(^\s*=\s*), "")
        IO.puts "> rest: #{inspect rest}"
        {value, rest} = parse_value(rest)
        IO.puts ">> rest: #{inspect rest}"
        rest = String.replace(rest, ~r{^\s*,\s*}, "")
        IO.puts ">> rest: #{inspect rest}"
        new_pair = {:key, key, value}
        parse_comma_separated(rest, [new_pair | pairs])
      :eof -> Enum.reverse pairs
    end
  end

  def parse_number(n) do
    # TODO do we still need $ in the regexes, now that we append \n to the end of the string?
    num_regex = ~r{^(?<number>(\d|-|e|E|\+|-|_|\.|:|Z|T)*)(?<rest>(\s|,|]|$).*)}s
    {number, rest} = case Regex.named_captures(num_regex, n) do
      %{"number" => nn, "rest" => r} -> {nn, r}
    end
    float_regex = ~r{^((\+|-)?(\d((_\d)*)?)+(\.(\d((_\d)*)?)+)?((e|E)(\+|-)?\d+)?)$}s
    result = case Regex.run(float_regex, number) do
      nil -> :nomatch
      _match ->
        if String.contains?(number, ".") ||
           String.contains?(number, "e") ||
           String.contains?(number, "E") do
          # TODO do something useful with the matched variables, so we don't have to run the regex
          # once more.
          {{:toml_float, number}, rest}
        else
          {{:toml_integer, number}, rest}
        end
    end
    with :nomatch <- result do
      case String.split(n, ~r(\s|,), parts: 2) do
        [n, rest] -> {{:toml_datetime, n}, rest}
        [n] -> {{:toml_datetime, n}, ""}
      end
    end
  end

  # Given a string that does NOT start with ", return the prefix up to and including ",
  # where no backslash is before the "
  def parse_quoted_string(s, prev \\ "\"") do
    [start, rest] = String.split(s, "\"", parts: 2)
    if String.ends_with?(start, "\\") do
      parse_quoted_string(rest, prev <> start <> "\"")
    else
      {{:quoted_string, prev <> start <> "\""}, rest}
    end
  end

end

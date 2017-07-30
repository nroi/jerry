defmodule Jerry do
  @moduledoc """
  Documentation for Jerry.
  """

  require Jerry.Utils.Macros
  import Jerry.Utils.Macros
  import Jerry.StringUtils, only: [remove_suffix: 2]

  # TODO Check out the abnf file for the TOML grammar, then introduce more regexes.
  @wschar source ~r/ |\t/
  @ws     source ~r/(#{@wschar})*/
  @wsn    source ~r/(#{@wschar}|\n)*/
  @hexdig source ~r/\d|[A-F]/
  @hex4   source ~r/\\u(#{@hexdig}){4}/
  @hex8   source ~r/\\U(#{@hexdig}){8}/
  @unquoted_key source ~r/([[:alnum:]]|-|_)+/
  @basic_unescaped source ~r/[#{"\u0020"}-#{"\u0021"}]|[#{"\u0023"}-#{"\u005B"}]|[#{"\u005d"}-#{"\u10FFFF"}]/
  @escaped source ~r{\\("|\\|/|b|f|n|r|t||(#{@hex4})|(#{@hex8}))}
  @basic_char source ~r/(#{@basic_unescaped})|(#{@escaped})/
  @quoted_key source ~r/"(#{@basic_char})+"/
  @key source ~r/(#{@quoted_key})|(#{@unquoted_key})/

  # FIXME used for debugging purposes.
  def escaped, do: @escaped
  def quoted_key, do: @quoted_key
  def basic_unesaped, do: @basic_unescaped
  def key, do: @key

  def intermediate_repr(s, kv_pairs \\ []) do
    # Append \n just to make things simpler, so we can assume lines always end with \n.
    case key_value_pairs(normalize(s), kv_pairs, false) do
      {:eof, pairs} -> Enum.reverse(pairs)
      {:continue, {rest, pairs}} ->
        intermediate_repr(rest, pairs)
    end
  end


  # the intermediate representation contains tokens in the order they have occured in the original
  # string. This also means that for arrays of tables, different key-value-pairs belonging to the
  # same array-of-table are spread at different positions in the list.
  # Hence, all arrays-of-tables are "compressed" such that we can subsequently generate the
  # final map by looking at each item of the list, one by one.
  def compress_intermediate(intermediate_repr) do
    {tables_and_array_items, other} = Enum.split_with(intermediate_repr, fn
      {:toml_array_of_tables_item, _, _} -> true
      {:toml_table, _, _} -> true
      _ -> false
    end)
    tables_and_array_items = compress_tables(tables_and_array_items)
    {array_items, tables} = Enum.split_with(tables_and_array_items, fn
      {:toml_array_of_tables_item, _, _} -> true
      _ -> false
    end)
    compressed_array_items = Enum.group_by(array_items, fn {:toml_array_of_tables_item, name, _} ->
      name
    end)
    arrays_of_tables = Enum.map(compressed_array_items, fn {name, arrays} ->
      arrays = Enum.map(arrays, fn
        {:toml_array_of_tables_item, nname, kv_pairs} ->
          {:toml_array_of_tables_item, nname, compress_intermediate(kv_pairs)}
      end)
      {:toml_array_of_tables, name, arrays}
    end)
    arrays_of_tables ++ tables ++ other
  end

  # Given two lists l1, l2, where l1 is a prefix of l2. Return the rest of l2, i.e., the part that
  # does not match l1.
  def suffix_after_prefix([], l2) do
    l2
  end
  def suffix_after_prefix([x | rest1], [y | rest2])   when x == y do
    suffix_after_prefix(rest1, rest2)
  end
  def suffix_after_prefix([_x|_rest1], []) do
    :no_prefix
  end
  def suffix_after_prefix([x | _rest1], [y | _rest2]) when x != y do
    :no_prefix
  end

  # Given tname = [foo.bar.soo], returns a tuple {table, rest}, where
  # table is the table with name "foo.bar", rest is everything else.
  def immediate_predecessor(tname, intermediate_repr) when is_list(tname) do
    default = case tname do
      [_ | _] -> {:toml_table, :lists.droplast(tname), []}
      _ -> nil
    end
    # The default TOML table with an empty list as kv_pairs should be used if and only if a table
    # such as a.b.c is referred to in the toml file, but no table a.b was declared.
    tmp = Enum.split_with(intermediate_repr, fn
      {decl, n1, _kv_pairs} when decl == :toml_table or decl == :toml_array_of_tables_item ->
        case suffix_after_prefix(n1, tname) do
          [_name] -> true
          _ -> false
        end
    end)
    case tmp do
      {[], rest} -> {default, rest}
      {[x], rest} -> {x, rest}
      # TODO caution! We blindly assume that the first table (x) is the predecessor.
      # Write a failing test case if possible, then fix this part.
      {[x | xs], rest} -> {x, xs ++ rest}
    end
  end

  defp sort_toml_tables(tables) do
    # TODO performance: we sort the tables multiple times, when in fact we only need fast access to
    # the element with the highest nesting level. Perhaps we should use Erlang's :gb_trees module
    # instead of continuously sorting a list.
    Enum.sort(tables, fn {_, n1, _}, {_, n2, _} ->
      length(n1) >= length(n2)
    end)
  end
  # Given a flat list of {:toml_table, _, _}, return the nested list where each table with a name
  # containing a dot is put inside the appropriate table. For example, a table named "foo.bar" is
  # put inside the table named foo. Also, tables are renamed such that they contain the last part
  # only (e.g. ["foo", "bar", "baz"] is renamed to ["baz"]).
  def compress_tables(tables) do
    tables
    |> sort_toml_tables
    |> compress_tables_rec
  end

  # Input is sorted by the nesting level of the table's name, in descending order:
  # If the name is a singleton list, we are done.
  defp compress_tables_rec([]), do: []
  defp compress_tables_rec(tables = [{:toml_array_of_tables_item, [_], _} | _]), do: tables
  defp compress_tables_rec(tables = [{:toml_table, [_], _} | _]), do: tables
  defp compress_tables_rec([{tdecl, tname, tkv_pairs} | rest])
    when is_list(tname) and (tdecl == :toml_table or tdecl == :toml_array_of_tables_item) do
    case immediate_predecessor(tname, rest) do
      {m = {decl, name, kv_pairs}, rest2} when decl == :toml_table or decl == :toml_array_of_tables_item ->
        IO.puts "Nest #{inspect tname} inside #{inspect name} (#{inspect m})"
        IO.puts "rest: #{inspect rest2}"
        inner = {tdecl, [:lists.last(tname)], tkv_pairs}
        rest_tables = [{decl, name, [inner | kv_pairs]} | rest2] |> sort_toml_tables
        compress_tables_rec(rest_tables)
    end
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
    int_regex = ~r/^(?<sign>\+|-)?(?<number>\d|([1-9](\d|(_\d))+))$/
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

  def intermediate2val({:toml_table, [name], table_pairs}) do
    kv_pairs = Enum.map(table_pairs, fn
      {:key, name, value} -> {name, intermediate2val(value)}
      {:toml_table, [name], kv_pairs} ->
        {name, kv_pairs_to_map(kv_pairs)}
    end)
    kv_map = Map.new(kv_pairs)
    {unquote_string(name), kv_map}
  end

  def intermediate2val({:toml_inline_table, table_pairs}) do
    kv_pairs_to_map(table_pairs)
  end

  def intermediate2val({:toml_array_of_tables, [name], tables}) do
    {name, Enum.map(tables, fn
      {:toml_array_of_tables_item, [^name], kv_pairs} ->
        kv_pairs_to_map(kv_pairs)
    end)}
  end

  def intermediate2val({:key, name, value}) do
    {unquote_string(name), intermediate2val(value)}
  end

  def intermediate2val({:toml_basic_string, ~s(") <> rest}) do
    rest |> String.replace_suffix(~s("), "") |> unescape
  end
  def intermediate2val({:toml_basic_string, ~s(') <> rest}) do
    String.replace_suffix(rest, "'", "")
  end

  def intermediate2val({:toml_multiline_basic_string, ~s("""\n) <> rest}) do
    # "A newline immediately following the opening delimiter will be trimmed."
    rest |> String.replace_suffix(~s("""), "") |> trim_multiline_basic_string
  end
  def intermediate2val({:toml_multiline_basic_string, ~s(""") <> rest}) do
    rest |> String.replace_suffix(~s("""), "") |> trim_multiline_basic_string
  end
  def intermediate2val({:toml_multiline_basic_string, ~s('''\n) <> rest}) do
    String.replace_suffix(rest, ~s('''), "")
  end
  def intermediate2val({:toml_multiline_basic_string, ~s(''') <> rest}) do
    String.replace_suffix(rest, ~s('''), "")
  end

  def table_array_name("[[" <> rest) do
    table_name_rec("." <> remove_suffix(rest, "]]"))
  end
  def table_name("[" <> rest) do
    table_name_rec("." <> remove_suffix(rest, "]"))
  end
  def table_name_rec(""), do: []
  def table_name_rec("." <> s) do
    case Regex.named_captures(~r/^(#{@ws})(?<key>(#{@key}))((#{@ws})|$)(?<rest>.*)/, s) do
      %{"key" => key, "rest" => rest} ->
        [unquote_string(key) | table_name_rec(rest)]
    end
  end

  def unescape(~S(\b) <> rest), do: "\b" <> unescape(rest)
  def unescape(~S(\t) <> rest), do: "\t" <> unescape(rest)
  def unescape(~S(\n) <> rest), do: "\n" <> unescape(rest)
  def unescape(~S(\f) <> rest), do: "\f" <> unescape(rest)
  def unescape(~S(\r) <> rest), do: "\r" <> unescape(rest)
  def unescape(~S(\") <> rest), do: "\"" <> unescape(rest)
  def unescape(~S(\\) <> rest), do: "\\" <> unescape(rest)
  def unescape(~S(\u) <> <<hex::bytes-size(4)>> <> rest) do
    hex2scalar_unicode(hex) <> unescape(rest)
  end
  def unescape(~S(\U) <> <<hex::bytes-size(8)>> <> rest) do
    hex2scalar_unicode(hex) <> unescape(rest)
  end
  def unescape(<<c::utf8, rest::binary>>) do
    to_string([c]) <> unescape(rest)
  end
  def unescape(""), do: ""

  def replace_unicode_scalar("\\u" <> rest) when byte_size(rest) == 4 do
    hex2scalar_unicode(rest)
  end
  def replace_unicode_scalar("\\U" <> rest) when byte_size(rest) == 8 do
    hex2scalar_unicode(rest)
  end

  def hex2scalar_unicode(hex) do
    {codepoint, ""} = Integer.parse(hex, 16)
    is_scalar = codepoint >= 0 && codepoint <= 0xD7FF ||
                codepoint >= 0xE000 && codepoint <= 0x10FFFF
    if is_scalar do
      <<codepoint::utf8>>
    else
      raise "Not a unicode scalar value: #{inspect hex}"
    end
  end

  def trim_multiline_basic_string(s) do
    String.replace(s, ~r/\\(#{@wsn})*/, "")
  end

  def unquote_string(~s(") <> rest), do: String.replace_suffix(rest, ~s("), "")
  def unquote_string(~s(') <> rest), do: String.replace_suffix(rest, ~s('), "")
  def unquote_string(key_name) do
    Regex.named_captures(~r/^(#{@ws})(?<key>.*?)(#{@ws})$/, key_name)["key"]
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
    {{:key, ""}, rest}
  end
  def parse_key("#" <> rest) do
    # Skip comment and trailing space after that comment.
    next = String.replace(rest, ~r/^.*\n(#{@wsn})*/, "", global: :false)
    parse_key(next)
  end
  def parse_key("\"" <> rest) do
    case parse_quoted_string(rest) do
      {{:quoted_string, ss}, rest} -> {{:key, unquote_string(ss)}, rest}
    end
  end
  def parse_key(k = "[[" <> _) do
    {table, rest} = split_newline(k)
    {:parse_array_of_tables, {table, rest}}
  end
  def parse_key("[\"" <> rest) do
    {{:quoted_string, table}, "]" <> rest} = parse_quoted_string(rest)
    {:parse_table, {"[" <> table <> "]", skip_comment(rest)}}
  end
  def parse_key(k = "[" <> _) do
    {table, rest} = split_newline(k)
    {:parse_table, {table, rest}}
  end
  def parse_key(s) do
    [key, rest] = String.split(s, ~r/(#{@ws})=(#{@ws})/, parts: 2)
    {{:key, unquote_string(key)}, rest}
  end

  defp split_newline(s) do
    case Regex.named_captures(~r/^(?<name>.*?)(#{@ws})(#.*?)?\n(?<rest>.*)/s, s) do
      %{"name" => name, "rest" => rest} -> {name, rest}
    end
  end

  defp skip_comment(s) do
    # Ignore all comment lines, as well as leading whitespace followed by those comment lines.
    String.replace(s, ~r/^((#{@ws})(#.*)?\n)*(#{@ws})/, "")
  end

  def key_value_pairs("", pairs, _), do: {:eof, pairs}
  def key_value_pairs(s, pairs, inside_table) do
    case parse_key(String.trim_leading(s)) do
      {:parse_array_of_tables, {table, rest}} when inside_table ->
        # Do not parse this table as the values of a preceding table.
        {:continue, {table <> "\n" <> rest, pairs}}
      {:parse_array_of_tables, {table, rest}} when not inside_table ->
        # TODO copy-pasted from below, refactor.
        case key_value_pairs(rest, [], true) do
          {:continue, {rest, table_pairs}} ->
            table = {:toml_array_of_tables_item, table_array_name(table), Enum.reverse(table_pairs)}
            {:continue, {rest, [table | pairs]}}
          {:parse_array_of_tables, {_table, _rest}} ->
            raise "bang"
          {:eof, table_pairs} ->
            table = {:toml_array_of_tables_item, table_array_name(table), Enum.reverse(table_pairs)}
            {:eof, [table | pairs]}
        end
      {:parse_table, {table, rest}} when inside_table ->
        # Do not parse this table as the values of a preceding table.
        {:continue, {table <> "\n" <> rest, pairs}}
      {:parse_table, {table, rest}} when not inside_table ->
        case key_value_pairs(rest, [], true) do
          {:continue, {rest, table_pairs}} ->
            table = {:toml_table, table_name(table), table_pairs}
            {:continue, {rest, [table | pairs]}}
          {:eof, table_pairs} ->
            table = {:toml_table, table_name(table), table_pairs}
            {:eof, [table | pairs]}
        end
      {{:key, key}, rest} ->
        rest = String.replace(rest, ~r/^(#{@ws})=(#{@ws})/, "")
        {value, rest} = parse_value(skip_comment(rest))
        new_pair = {:key, unquote_string(key), value}
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
    case Regex.run(~r/^(.*?)'(.*)/s, rest, capture: :all_but_first) do
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
    {value_string, rest} = case Regex.run(~r/(.*?)}(.*)/s, rest, capture: :all_but_first) do
      [vs, r] -> {String.trim_leading(vs), r}
    end
    kv_pairs = parse_comma_separated(value_string <> "\n", [])
    {{:toml_inline_table, kv_pairs}, rest}
  end
  def parse_value(n) do
    parse_number(n)
  end

  def parse_values("]" <> rest, acc), do: { Enum.reverse(acc), rest }
  def parse_values(n, acc) do
    {value, rest} = case parse_value(n) do
      {v, r} -> {v, Regex.replace(~r/^(#{@ws}),?(#{@ws})/, r, "", global: false)}
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
        rest = String.replace(rest, ~r/^(#{@ws})=(#{@ws})/, "")
        {value, rest} = parse_value(rest)
        rest = String.replace(rest, ~r/^(#{@ws}),(#{@ws})/, "")
        new_pair = {:key, unquote_string(key), value}
        parse_comma_separated(rest, [new_pair | pairs])
      :eof -> Enum.reverse pairs
    end
  end

  def parse_number(n) do
    # TODO do we still need $ in the regexes, now that we append \n to the end of the string?
    num_regex = ~r/^(?<number>(\d|-|e|E|\+|-|_|\.|:|Z|T)*)(?<rest>(\s|,|]|$).*)/s
    {number, rest} = case Regex.named_captures(num_regex, n) do
      %{"number" => nn, "rest" => r} -> {nn, r}
    end
    float_regex = ~r/^((\+|-)?(\d((_\d)*)?)+(\.(\d((_\d)*)?)+)?((e|E)(\+|-)?\d+)?)$/s
    result = case Regex.run(float_regex, number) do
      nil -> :nomatch
      _match ->
        if String.contains?(number, ".") ||
           String.contains?(number, "e") ||
           String.contains?(number, "E") do
          {{:toml_float, number}, rest}
        else
          {{:toml_integer, number}, rest}
        end
    end
    with :nomatch <- result do
      case String.split(n, ~r/\s|,/, parts: 2) do
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

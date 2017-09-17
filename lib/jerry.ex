defmodule Jerry do
  @moduledoc """
  Jerry, a TOML parser.
  """

  require Jerry.Utils.Macros
  import Jerry.Utils.Macros
  import Jerry.Utils.StringUtils, only: [remove_suffix: 2]
  import Jerry.Utils.ListUtils, only: [nest_children: 2]

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

  @doc false
  def intermediate_repr(s, kv_pairs \\ []) do
    # Append \n just to make things simpler, so we can assume lines always end with \n.
    case key_value_pairs(normalize(s), kv_pairs, false) do
      {:eof, pairs} -> Enum.reverse(pairs)
      {:continue, {rest, pairs}} ->
        intermediate_repr(rest, pairs)
    end
  end


  # the intermediate representation contains tokens in the order they have occurred in the original
  # string. This also means that for arrays of tables, different key-value-pairs belonging to the
  # same array-of-table are spread at different positions in the list.
  # This function will "compress" all arrays-of-tables such that we can subsequently generate the
  # final map by looking at each item of the list, one by one.
  defp compress_intermediate(intermediate_repr) do
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
  defp suffix_after_prefix([], l2) do
    l2
  end
  defp suffix_after_prefix([x | rest1], [y | rest2])   when x == y do
    suffix_after_prefix(rest1, rest2)
  end
  defp suffix_after_prefix([_x|_rest1], []) do
    :no_prefix
  end
  defp suffix_after_prefix([x | _rest1], [y | _rest2]) when x != y do
    :no_prefix
  end

  defp immediate_predecessor?({:toml_table, pname, _},
                             {:toml_table, cname, _}) do
      immediate_predecessor?(pname, cname)
  end
  defp immediate_predecessor?({:toml_array_of_tables_item, pname, _},
                             {:toml_array_of_tables_item, cname, _}) do
      immediate_predecessor?(pname, cname)
  end
  defp immediate_predecessor?(pname, cname) when is_list(pname) and is_list(cname) do
    case suffix_after_prefix(pname, cname) do
      [_name] -> true
      _ -> false
    end
  end
  defp immediate_predecessor?({:toml_table, _, _}, _), do: false
  defp immediate_predecessor?({:toml_array_of_tables_item, _, _}, _), do: false

  # Given a list of entries, sort all toml tables inside it.
  defp sort_toml_tables(entries) do
    # The entries have to be nested already (e.g. a toml table with name ["foo", "bar"] is nested
    # inside a toml table with name ["foo"].
    {tables, other} = Enum.split_with(entries, fn
      {:toml_table, _, _} -> true
      _ -> false
    end)
    sorted = Enum.sort(tables, fn
      {:toml_table, n1, _}, {:toml_table, n2, _} ->
        length(n1) <= length(n2)
    end)
    nested_sorted = Enum.map(sorted, fn
      {:toml_table, name, kv_pairs} -> {:toml_table, name, sort_toml_tables(kv_pairs)}
    end)
    nested_sorted ++ other
  end

  # Given a flat list of {:toml_table, _, _}, return the nested list where each table with a name
  # containing a dot is put inside the appropriate table. For example, a table named "foo.bar" is
  # put inside the table named foo. Also, tables are renamed such that they contain the last part
  # only (e.g. ["foo", "bar", "baz"] is renamed to ["baz"]).
  defp compress_tables(tables) do
    {tables, arrays} = Enum.split_with(tables, fn
      {:toml_table, _, _} -> true
      {:toml_array_of_tables_item, _, _} -> false
    end)
    compress_tables_rec(tables) ++ compress_tables_of_arrays_rec(arrays)
  end


  # Input is sorted by the nesting level of the table's name, in descending order:
  # If the name is a singleton list, we are done.
  defp compress_tables_rec([]), do: []
  defp compress_tables_rec(tables = [{:toml_table, tname, _tkv_pairs} | _rest]) when is_list(tname)  do
    tables
    |> nest_children(&immediate_predecessor?/2)
    |> Enum.map(&nest_toml_tables/1)
  end

  defp compress_tables_of_arrays_rec([]), do: []
  defp compress_tables_of_arrays_rec([f = {:toml_array_of_tables_item, tname, _tkv_pairs} | rest]) when is_list(tname) do
    {relevant, irrelevant} = Enum.split_while(rest, fn
      {:toml_array_of_tables_item, name, _} -> length(name) > length(tname)
    end)
    relevant = [f | relevant]
    # relevant contains f as well as potential children of f.
    nested = nest_children(relevant, &immediate_predecessor?/2)
    properly_nested = Enum.map(nested, fn n ->
      nest_array_of_tables(n, nil)
    end)
    properly_nested ++ compress_tables_of_arrays_rec(irrelevant)
  end

  # the function "nest_children" returns an abstract representation {parent, descendants}.
  # this function turns this representation into the one required for properly representing arrays
  # of tables, by nesting the children inside the key-value-pairs.
  defp nest_array_of_tables({child = {:toml_array_of_tables_item, name, kv_pairs}, descendants}, parent) do
    new_children = Enum.map(descendants, fn descendant ->
      nest_array_of_tables(descendant, child)
    end)
    new_name = case parent do
      nil ->
        name
      _   ->
        # TODO it's not that simple: consider the case when [[a]] and [[a.b.c]] exists, but not
        # [[a.b]]. We should then detect [[a]] as the parent of [[a.b.c]], and then use the child's
        # suffix (i.e., [[b.c]]) as the name.
        [:lists.last(name)]
    end
    {:toml_array_of_tables, new_name, new_children ++ kv_pairs}
  end

  defp nest_toml_tables({{:toml_table, name, kv_pairs}, descendants}) do
    {:toml_table, [:lists.last(name)], Enum.map(descendants, &nest_toml_tables/1) ++ kv_pairs}
  end

  defp prepend_implicit(explicit) do
    Enum.reduce(explicit, [], fn
      t = {:toml_table, _name, _}, acc ->
        acc ++ predecessors_to_insert(t, explicit, []) ++ [t]
      other, acc ->
        acc ++ [other]
    end)
  end

  # returns all implicit predecessors of this entry (i.e., predecessors not yet contained in explicit)
  defp predecessors_to_insert(_entry = {:toml_table, [_name], _}, _explicit, acc), do: acc
  defp predecessors_to_insert(_entry = {:toml_table, name, _}, explicit, acc) do
    predecessor_name = :lists.droplast(name)
    has_predecessor = Enum.any?(explicit, fn
      {:toml_table, ^predecessor_name, _} -> true
      _ -> false
    end)
    case has_predecessor do
      true ->
        acc
      false ->
        predecessor = {:toml_table, predecessor_name, []}
        predecessors_to_insert(predecessor, explicit, [predecessor | acc])
    end
  end

  @doc """
  Parse the given TOML string and return the corresponding map
  """
  def decode!(s) do
    s
    |> intermediate_repr
    |> prepend_implicit
    |> sort_toml_tables
    |> compress_intermediate
    |> kv_pairs_to_map
  end

  # Used to fetch keys from regex captures: "" is treated as if it weren't present.
  defp fetch(m = %{}, key) do
    case Map.fetch(m, key) do
      {:ok, ""} -> nil
      {:ok, x } -> x
      :error -> nil
    end
  end

  defp intermediate2val({:toml_integer, int_str}) do
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

  defp intermediate2val({:toml_float, float}) do
    zero_prefixable_int = source ~r/\d(\d|_\d)*/
    frac = source ~r/\.(#{zero_prefixable_int})/
    int = source ~r/(\d)|([1-9](\d|_\d)+)/
    integer = source ~r/(\+|-)?(#{int})/
    exp = source ~r/(e|E)(#{integer})/
    re = ~r/^(?<pre>#{integer})((?<f1>#{frac})|((?<f2>#{frac})(?<e1>#{exp}))|(?<e2>#{exp}))$/
    case Regex.named_captures(re, float) do
      nil ->
        raise "Unable to parse float: #{inspect float}"
      captures ->
        pre = String.replace(captures["pre"], "_", "")
        frac = String.replace(fetch(captures, "f1") || fetch(captures, "f2") || ".0", "_", "")
        exp = case fetch(captures, "e1") || fetch(captures, "e2") do
          "e" <> rest -> rest
          "E" <> rest -> rest
          nil -> nil
        end
        mantissa = String.to_float(pre <> frac)
        case exp do
          nil ->
            mantissa
          exponent ->
            # Parsing the string expression into their integer components only to create a string
            # again seems rather involved, however, we cannot give the TOML float to
            # String.to_float/1 since something like "1e2" is a valid TOML float, but not a
            # valid Erlang float. Also, calculating the number from the integer components
            # (using :math.pow/1) will result in rounding errors.
            String.to_float("#{mantissa}e#{exponent}")
        end
    end
  end

  defp intermediate2val({:toml_boolean, "false"}), do: false
  defp intermediate2val({:toml_boolean, "true"}), do: true

  defp intermediate2val({:toml_datetime, dt_string}) do
    # TODO note that TOML is using RFC3339, not ISO8601. There are some subtle differences that
    # have to be taken into account.
    case DateTime.from_iso8601(dt_string) do
      {:ok, dt, _offset} -> dt
      {:error, :missing_offset} ->
        case NaiveDateTime.from_iso8601(dt_string) do
          {:ok, ndt} -> ndt
        end
    end
  end

  defp intermediate2val({:toml_array, array}) do
    mixed_types = case Enum.uniq_by(array, fn {type, _} -> type end) do
      []  -> false
      [_] -> false
      # If there is more than one type, we need to make an exception for strings:
      # Internally, we use two types which both represent strings, so in this case, no exception
      # should be raised even if the types are different.
      unique -> Enum.all?(unique, fn
        {:toml_basic_string, _} -> false
        {:toml_multiline_basic_string, _} -> false
        _ -> true
      end)
    end
    case mixed_types do
      false ->
        Enum.map(array, &intermediate2val/1)
      true ->
        raise "Mixed types are not allowed in arrays"
    end
  end

  defp intermediate2val({:toml_table, [name], table_pairs}) do
    kv_pairs = Enum.map(table_pairs, fn
      {:toml_table, [name], kv_pairs} ->
        {name, kv_pairs_to_map(kv_pairs)}
      m = {:key, _, _} -> intermediate2val(m)
    end)
    {unquote_string(name), Map.new(kv_pairs)}
  end

  defp intermediate2val({:toml_inline_table, table_pairs}) do
    kv_pairs = Enum.map(table_pairs, fn
      m = {:key, _, _} -> intermediate2val(m)
    end)
    Map.new(kv_pairs)
  end

  defp intermediate2val({:toml_array_of_tables, [name], items}) when is_list(items) do
    kv_map = Enum.map(items, &intermediate2val/1)
    {{:toml_array_of_tables!, unquote_string(name)}, kv_map}
  end
  defp intermediate2val({:toml_array_of_tables, [x|xs], items}) when is_list(items) do
    # Create implicit tables without any entries.
    {{:toml_array_of_tables!, unquote_string(x)}, intermediate2val({:toml_array_of_tables, xs, items})}
  end

  defp intermediate2val({:key, name, value}) do
    {unquote_string(name), intermediate2val(value)}
  end

  defp intermediate2val({:toml_basic_string, ~s(") <> rest}) do
    rest |> String.replace_suffix(~s("), "") |> unescape
  end
  defp intermediate2val({:toml_basic_string, ~s(') <> rest}) do
    String.replace_suffix(rest, "'", "")
  end

  defp intermediate2val({:toml_multiline_basic_string, ~s("""\n) <> rest}) do
    # "A newline immediately following the opening delimiter will be trimmed."
    rest |> String.replace_suffix(~s("""), "") |> trim_multiline_basic_string
  end
  defp intermediate2val({:toml_multiline_basic_string, ~s(""") <> rest}) do
    rest |> String.replace_suffix(~s("""), "") |> trim_multiline_basic_string
  end
  defp intermediate2val({:toml_multiline_basic_string, ~s('''\n) <> rest}) do
    String.replace_suffix(rest, ~s('''), "")
  end
  defp intermediate2val({:toml_multiline_basic_string, ~s(''') <> rest}) do
    String.replace_suffix(rest, ~s('''), "")
  end

  defp table_array_name("[[" <> rest) do
    table_name_rec("." <> remove_suffix(rest, "]]"))
  end
  defp table_name("[" <> rest) do
    table_name_rec("." <> remove_suffix(rest, "]"))
  end
  defp table_name_rec(""), do: []
  defp table_name_rec("." <> s) do
    case Regex.named_captures(~r/^(#{@ws})(?<key>(#{@key}))((#{@ws})|$)(?<rest>.*)/, s) do
      %{"key" => key, "rest" => rest} ->
        [unquote_string(key) | table_name_rec(rest)]
    end
  end

  defp unescape(~S(\b) <> rest), do: "\b" <> unescape(rest)
  defp unescape(~S(\t) <> rest), do: "\t" <> unescape(rest)
  defp unescape(~S(\n) <> rest), do: "\n" <> unescape(rest)
  defp unescape(~S(\f) <> rest), do: "\f" <> unescape(rest)
  defp unescape(~S(\r) <> rest), do: "\r" <> unescape(rest)
  defp unescape(~S(\") <> rest), do: "\"" <> unescape(rest)
  defp unescape(~S(\\) <> rest), do: "\\" <> unescape(rest)
  defp unescape(~S(\u) <> <<hex::bytes-size(4)>> <> rest) do
    hex2scalar_unicode(hex) <> unescape(rest)
  end
  defp unescape(~S(\U) <> <<hex::bytes-size(8)>> <> rest) do
    hex2scalar_unicode(hex) <> unescape(rest)
  end
  defp unescape(<<c::utf8, rest::binary>>) do
    to_string([c]) <> unescape(rest)
  end
  defp unescape(""), do: ""

  defp hex2scalar_unicode(hex) do
    {codepoint, ""} = Integer.parse(hex, 16)
    is_scalar = codepoint >= 0 && codepoint <= 0xD7FF ||
                codepoint >= 0xE000 && codepoint <= 0x10FFFF
    if is_scalar do
      <<codepoint::utf8>>
    else
      raise "Not a unicode scalar value: #{inspect hex}"
    end
  end

  defp trim_multiline_basic_string(s) do
    String.replace(s, ~r/\\(#{@wsn})*/, "")
  end

  defp unquote_string(~s(") <> rest), do: String.replace_suffix(rest, ~s("), "")
  defp unquote_string(~s(') <> rest), do: String.replace_suffix(rest, ~s('), "")
  defp unquote_string(key_name) do
    Regex.named_captures(~r/^(#{@ws})(?<key>.*?)(#{@ws})$/, key_name)["key"]
  end

  # Given a list such as [{:key, "foo", 1}], return the corresponding map, e.g. %{"foo" => 1}
  defp kv_pairs_to_map(kv_pairs) do
    pairs = Enum.map(kv_pairs, &intermediate2val/1)
    merge_arrays_of_tables(pairs)
  end

  defp rec_merge(m1, m2) do
    merger = fn
      _key, v1, v2 when is_list(v1) and is_list(v2) ->
        v1 ++ v2
      _key, v1, v2 when is_map(v1) and is_map(v2) ->
        rec_merge(v1, v2)
    end
    Map.merge(m1, m2, merger)
  end

  # Used for post-processing after the values have been parsed using the function intermediate2val/1.
  # intermediate2val/1 does not create the final representation of arrays of tables, since this
  # function does not have enough information available to do so.
  defp merge_arrays_of_tables(arrays_of_tables) do
    Enum.reduce(arrays_of_tables, %{}, fn
      ({{:toml_array_of_tables!, key}, kv_pairs}, acc) when is_list(kv_pairs) ->
        prev = Map.get(acc, key, [])
        Map.put(acc, key, prev ++ [merge_arrays_of_tables(kv_pairs)])
      ({{:toml_array_of_tables!, key}, entry}, acc) when is_tuple(entry) ->
        map = merge_arrays_of_tables([entry])
        rec_merge(acc, %{key => map})
      ({key, value}, acc) ->
        if Map.has_key?(acc, key) do
          raise "Duplicate key: #{inspect key}"
        end
        Map.put(acc, key, value)
    end)
  end

  defp normalize(s) do
    # Use unix convention, using \n as line breaks and at least one newline after each line (i.e.,
    # the last line is guaranteed to end with \n).
    (s |> String.trim_leading |> String.replace("\r\n", "\n")) <> "\n"
  end

  defp parse_key(""), do: :eof
  defp parse_key(~s("") <> rest) do
    # A key consisting of "" is allowed in TOML (although discouraged).
    {{:key, ""}, rest}
  end
  defp parse_key("#" <> rest) do
    # Skip comment and trailing space after that comment.
    next = String.replace(rest, ~r/^.*\n(#{@wsn})*/, "", global: :false)
    parse_key(next)
  end
  defp parse_key("\"" <> rest) do
    case parse_quoted_string(rest) do
      {{:quoted_string, ss}, rest} -> {{:key, unquote_string(ss)}, rest}
    end
  end
  defp parse_key(k = "[[" <> _) do
    {table, rest} = split_newline(k)
    {:parse_array_of_tables, {table, rest}}
  end
  defp parse_key("[\"" <> rest) do
    {{:quoted_string, table}, "]" <> rest} = parse_quoted_string(rest)
    {:parse_table, {"[" <> table <> "]", skip_comment(rest)}}
  end
  defp parse_key(k = "[" <> _) do
    {table, rest} = split_newline(k)
    {:parse_table, {table, rest}}
  end
  defp parse_key(s) do
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

  defp key_value_pairs("", pairs, _), do: {:eof, pairs}
  defp key_value_pairs(s, pairs, inside_table) do
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

  defp parse_value("true" <> rest) do
    {{:toml_boolean, "true"}, rest}
  end

  defp parse_value("false" <> rest) do
    {{:toml_boolean, "false"}, rest}
  end

  defp parse_value("'''\n" <> rest) do
    [s, rest] = String.split(rest, "'''", parts: 2)
    {{:toml_multiline_basic_string, "'''\n" <> s <> "'''"}, rest}
  end
  defp parse_value("'''" <> rest) do
    [s, rest] = String.split(rest, "'''", parts: 2)
    {{:toml_multiline_basic_string, "'''" <> s <> "'''"}, rest}
  end

  defp parse_value("'" <> rest) do
    # single quoted strings must not contain single quoted strings, so we can just assume that the
    # string ends with the next single quote.
    case Regex.run(~r/^(.*?)'(.*)/s, rest, capture: :all_but_first) do
      [match, rest] ->
        {{:toml_basic_string, "'#{match}'"}, rest}
    end
  end

  defp parse_value("\"\"\"\n" <> rest) do
    # A newline immediately following the opening delimiter will be trimmed
    # TODO consider this ^^ when turning the AST into a map.
    [s, rest] = String.split(rest, "\"\"\"", parts: 2)
    {{:toml_multiline_basic_string, "\"\"\"\n" <> s <> "\"\"\""}, rest}
  end
  defp parse_value("\"\"\"" <> rest) do
    [s, rest] = String.split(rest, "\"\"\"", parts: 2)
    {{:toml_multiline_basic_string, "\"\"\"" <> s <> "\"\"\""}, rest}
  end

  defp parse_value("\"" <> rest) do
    case parse_quoted_string(rest) do
      {{:quoted_string, s}, rest} -> {{:toml_basic_string, s}, rest}
    end
  end

  defp parse_value(n = "+" <> _) do
    parse_number(n)
  end
  defp parse_value(n = "-" <> _) do
    parse_number(n)
  end
  defp parse_value("[" <> rest) do
    {values, rest} = parse_values(skip_comment(rest), [])
    {{:toml_array, values}, rest}
  end
  defp parse_value("{" <> rest) do
    # TODO:
    # "Inline tables are intended to appear on a single line. No newlines are allowed between the
    # curly braces unless they are valid within a value. Even so, it is strongly discouraged to
    # break an inline table onto multiples lines. If you find yourself gripped with this desire, it
    # means you should be using standard tables."
    # To make things easier, we just assume that inline tables appear on a single line for now.
    {value_string, rest} = case Regex.run(~r/(.*?)}(.*)/s, rest, capture: :all_but_first) do
      # TODO the regex from above is not entirely correct, in particular, it doesn't work with
      # nested arrays of tables (e.g. "x = { y = {} }"). Perhaps we should try to reuse the regexes
      # constructed from the official abnf grammar.
      [vs, r] -> {String.trim_leading(vs), r}
    end
    kv_pairs = parse_comma_separated(value_string <> "\n", [])
    {{:toml_inline_table, kv_pairs}, rest}
  end
  defp parse_value(n) do
    parse_number(n)
  end

  defp parse_values("]" <> rest, acc), do: { Enum.reverse(acc), rest }
  defp parse_values(n, acc) do
    {value, rest} = case parse_value(n) do
      {v, r} -> {v, Regex.replace(~r/^(#{@ws}),?(#{@ws})/, r, "", global: false)}
    end
    parse_values(skip_comment(rest), [value | acc])
  end

  # Given a string such as "foo = 1, bar = 2\n", return a list of key-value pairs.
  defp parse_comma_separated(s, pairs) do
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

  defp parse_number(n) do
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
  defp parse_quoted_string(s, prev \\ "\"") do
    [start, rest] = String.split(s, "\"", parts: 2)
    if String.ends_with?(start, "\\") do
      parse_quoted_string(rest, prev <> start <> "\"")
    else
      {{:quoted_string, prev <> start <> "\""}, rest}
    end
  end

end

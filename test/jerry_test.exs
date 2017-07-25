defmodule JerryTest do
  use ExUnit.Case
  doctest Jerry

  test "intermediate representation for simple key-value pairs" do
    expected = [{:key, ~s(foo), {:toml_basic_string, ~s("bar")}}]
    assert Jerry.intermediate_repr(~s(foo = "bar")) == expected
    assert Jerry.intermediate_repr(~s(foo   = "bar")) == expected
    assert Jerry.intermediate_repr(~s(foo   =    "bar")) == expected
    assert Jerry.intermediate_repr(~s(foo	=    "bar")) == expected
  end

  test "intermediate representation when no space between key and value" do
    expected = [{:key, ~s(foo), {:toml_basic_string, ~s("bar")}}]
    assert Jerry.intermediate_repr(~s(foo="bar")) == expected
  end

  test "keys may be quoted" do
    expected = [{:key, "foo", {:toml_basic_string, ~s("bar")}}]
    assert Jerry.intermediate_repr(~s("foo"="bar")) == expected
  end

  test "empty keys are valid (although discouraged)" do
    expected = [{:key, "", {:toml_basic_string, ~s("bar")}}]
    assert Jerry.intermediate_repr(~s(""="bar")) == expected
  end

  test "quoted keys can contain spaces" do
    expected = [{:key, "a key with some spaces", {:toml_basic_string, ~s("bar")}}]
    assert Jerry.intermediate_repr(~s("a key with some spaces"="bar")) == expected
  end

  test "keys can contain an equals sign" do
    expected = [{:key, "foo=bar", {:toml_basic_string, ~s("foobar")}}]
    assert Jerry.intermediate_repr(~s("foo=bar" = "foobar")) == expected
  end

  test "values can be integers" do
    expected = [{:key, "foo", {:toml_integer, ~s(10)}}]
    assert Jerry.intermediate_repr(~s(foo = 10)) == expected
  end

  test "integers may contain underscores" do
    expected = [{:key, "foo", {:toml_integer, ~s(10_000_000)}}]
    assert Jerry.intermediate_repr(~s(foo = 10_000_000)) == expected
  end

  test "integers may be preceded by + or -" do
    expected1 = [{:key, ~s(foo), {:toml_integer, ~s(-20)}}]
    expected2 = [{:key, ~s(foo), {:toml_integer, ~s(+20)}}]
    assert Jerry.intermediate_repr(~s(foo = -20)) == expected1
    assert Jerry.intermediate_repr(~s(foo = +20)) == expected2
  end

  test "values can be floating point numbers" do
    expected = [{:key, ~s(foo), {:toml_float, ~s(10.0)}}]
    assert Jerry.intermediate_repr(~s(foo = 10.0)) == expected
  end

  test "floats may be preceded by + or -" do
    expected1 = [{:key, ~s(foo), {:toml_float, ~s(-20.0)}}]
    expected2 = [{:key, ~s(foo), {:toml_float, ~s(+20.0)}}]
    assert Jerry.intermediate_repr(~s(foo = -20.0)) == expected1
    assert Jerry.intermediate_repr(~s(foo = +20.0)) == expected2
  end

  test "floats can be defined as fractional part and exponential part." do
    expected = [{:key, ~s(foo), {:toml_float, ~s(-2E-2)}}]
    assert Jerry.intermediate_repr(~s(foo = -2E-2)) == expected
  end

  test "values can be booleans" do
    expected1 = [{:key, ~s(foo), {:toml_boolean, ~s(true)}}]
    expected2 = [{:key, ~s(foo), {:toml_boolean, ~s(false)}}]
    assert Jerry.intermediate_repr(~s(foo = true)) == expected1
    assert Jerry.intermediate_repr(~s(foo = false)) == expected2
  end

  test "values can be offset-datetimes" do
    expected1 = [{:key, ~s(odt1), {:toml_datetime, ~s(1979-05-27T07:32:00Z)}}]
    expected2 = [{:key, ~s(odt2), {:toml_datetime, ~s(1979-05-27T00:32:00-07:00)}}]
    expected3 = [{:key, ~s(odt3), {:toml_datetime, ~s(1979-05-27T00:32:00.999999-07:00)}}]
    assert Jerry.intermediate_repr(~s(odt1 = 1979-05-27T07:32:00Z)) == expected1
    assert Jerry.intermediate_repr(~s(odt2 = 1979-05-27T00:32:00-07:00)) == expected2
    assert Jerry.intermediate_repr(~s(odt3 = 1979-05-27T00:32:00.999999-07:00)) == expected3
  end

  test "tables can contain key-value mappings" do
    expected = [{:toml_table, ["table1"], [{:key, ~s(key1), {:toml_integer, ~s(1)}}]}]
    assert Jerry.intermediate_repr("[table1]\nkey1 = 1") == expected
  end

  test "multiple tables are supported" do
    expected = [
      {:toml_table, ["table1"], [{:key, ~s(key1), {:toml_integer, ~s(1)}}]},
      {:toml_table, ["table2"], [{:key, ~s(key2), {:toml_integer, ~s(2)}}]}
    ]
    s = ~s([table1]\nkey1 = 1\n[table2]\nkey2 = 2\n)
    assert Jerry.intermediate_repr(s) == expected
  end

  test "toml files can contain tables after top-level mappings" do
    s = "foo = 10\n[table1]\nkey1 = 1"
    expected = [
      {:key, ~s(foo), {:toml_integer, ~s(10)}},
      {:toml_table, ["table1"], [{:key, ~s(key1), {:toml_integer, ~s(1)}}]}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "arrays are supported" do
    s = "arr1 = [ 1, 2, 3 ]"
    expected = [
      {:key, "arr1", {:toml_array, [toml_integer: "1", toml_integer: "2", toml_integer: "3"]}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "multiline-arrays are supported" do
    s = "arr1 = [\n1,\n  2,\n3\n]"
    expected = [
      {:key, "arr1", {:toml_array, [toml_integer: "1", toml_integer: "2", toml_integer: "3"]}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "multiline-arrays may have a trailing comma" do
    s = "arr1 = [\n1,\n  2,\n3,\n]"
    expected = [
      {:key, "arr1", {:toml_array, [toml_integer: "1", toml_integer: "2", toml_integer: "3"]}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "comments are supported" do
    expected = [{:key, ~s(foo), {:toml_boolean, ~s(true)}}]
    assert Jerry.intermediate_repr("foo = true # foo is true.") == expected
  end

  test "comments in multiline-files are supported" do
    s = "foo = true # and then some\n # blank line without anything.\nbar = false # more"
    expected = [
      {:key, ~s(foo), {:toml_boolean, ~s(true)}},
      {:key, ~s(bar), {:toml_boolean, ~s(false)}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "strings may be escaped" do
    s = ~S(str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF.")
    value = ~S("I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF.")
    expected = [ {:key, "str", {:toml_basic_string, value}} ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "multiline strings are supported" do
    s = ~S(str3 = """\
       The quick brown \
       fox jumps over \
       the lazy dog.\
       """)
    value = ~S("""\
       The quick brown \
       fox jumps over \
       the lazy dog.\
       """)
    expected = [ {:key, "str3", {:toml_multiline_basic_string, value}} ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "strings may be defined with single quotes" do
    s = ~S(winpath  = 'C:\Users\nodejs\templates')
    expected = [ {:key, "winpath", {:toml_basic_string, ~S('C:\Users\nodejs\templates')}} ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "strings may be defined with triple single quotes" do
    s = ~S(regex2 = '''I [dw]on't need \d{2} apples''')
    value = ~S('''I [dw]on't need \d{2} apples''')
    expected = [ {:key, "regex2", {:toml_multiline_basic_string, value}} ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "inline tables are supported" do
    s = ~S(name = { first = "Tom", last = "Preston-Werner" })
    expected = [
      {:key, "name", {:toml_inline_table, [
        {:key, ~s(first), {:toml_basic_string, ~s("Tom")}},
        {:key, ~s(last),  {:toml_basic_string, ~s("Preston-Werner")}}
      ]}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "inline tables with a bunch of values are supported" do
    s = ~S(values = { 1 = 1, 2 = 2, 3 = 3, 4 = 4})
    expected = [
      {:key, "values", {:toml_inline_table, [
        {:key, "1", {:toml_integer, ~s(1)}},
        {:key, "2", {:toml_integer, ~s(2)}},
        {:key, "3", {:toml_integer, ~s(3)}},
        {:key, "4", {:toml_integer, ~s(4)}}
      ]}}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "inline tables spreading over multiple lines are supported" do
    s = ~s(points = [ { x = 1, y = 2, z = 3 },\n
           { x = 7, y = 8, z = 9 },\n
           { x = 2, y = 4, z = 8 }]\n)
    expected = [
      {:key, "points", {:toml_array, [
        {:toml_inline_table, [
          {:key, "x", {:toml_integer, ~s(1)}},
          {:key, "y", {:toml_integer, ~s(2)}},
          {:key, "z", {:toml_integer, ~s(3)}}
        ]},
        {:toml_inline_table, [
          {:key, "x", {:toml_integer, ~s(7)}},
          {:key, "y", {:toml_integer, ~s(8)}},
          {:key, "z", {:toml_integer, ~s(9)}}
        ]},
        {:toml_inline_table, [
          {:key, "x", {:toml_integer, ~s(2)}},
          {:key, "y", {:toml_integer, ~s(4)}},
          {:key, "z", {:toml_integer, ~s(8)}}
        ]}
      ]}}]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "arrays of tables are supported" do
    s = ~S([[products]]
           name = "Hammer"
           sku = 738594937

           [[products]]

           [[products]]
           name = "Nail"
           sku = 284758393
           color = "gray")
    expected = [
      {:toml_array_of_tables_item, ["products"], [
        {:key, ~s(name), {:toml_basic_string, ~s("Hammer")}},
        {:key, ~s(sku), {:toml_integer, ~s(738594937)}}
      ]},
      {:toml_array_of_tables_item, ["products"], []},
      {:toml_array_of_tables_item, ["products"], [
        {:key, ~s(name), {:toml_basic_string, ~s("Nail")}},
        {:key, ~s(sku), {:toml_integer, ~s(284758393)}},
        {:key, ~s(color), {:toml_basic_string, ~s("gray")}}
      ]}
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "CRLF line endings can also used" do
    s = "x = 1\r\ny = 2\r\nz=3\r\n"
    expected = [
      {:key, ~s(x), {:toml_integer, ~s(1)}},
      {:key, ~s(y), {:toml_integer, ~s(2)}},
      {:key, ~s(z), {:toml_integer, ~s(3)}},
    ]
    assert Jerry.intermediate_repr(s) == expected
  end

  test "compress_tables" do
    repr = [
      {:toml_table, ["foo"], [1]},
      {:toml_table, ["foo", "bar"], [2]},
      {:toml_table, ["foo", "bbb"], []},
      {:toml_table, ["foo", "bar", "baz"], [3]},
    ]
    expected = [
      {:toml_table, ["foo"], [
        {:toml_table, ["bbb"], []},
        {:toml_table, ["bar"], [
          {:toml_table, ["baz"], [3]},
        2]},
      1]}
    ]
    assert Jerry.compress_tables(repr) == expected
  end

  test "arrays of tables may contain tables" do
    s = ~S([[fruit]]
            name = "apple"
            [fruit.physical]
              color = "red"
              shape = "round"
    )
    intermediate_expected = [
      {:toml_array_of_tables_item, ["fruit"], [
        {:key, "name", {:toml_basic_string, ~s("apple")}},
      ]},
      {:toml_table, ["fruit", "physical"], [
        {:key, "shape", {:toml_basic_string, ~s("round")}},
        {:key, "color", {:toml_basic_string, ~s("red")}},
      ]}
    ]
    expected = [
      {:toml_array_of_tables, ["fruit"], [
        {:toml_array_of_tables_item, ["fruit"], [
          {:key, "name", {:toml_basic_string, ~s("apple")}},
          {:toml_table_name, ["physical"], [
            {:key, "color", {:toml_basic_string, ~s("red")}},
            {:key, "shape", {:toml_basic_string, ~s("round")}},
          ]},
        ]},
      ]}
    ]
    assert s |> Jerry.intermediate_repr == intermediate_expected
    assert s |> Jerry.intermediate_repr |> Jerry.compress_intermediate == expected
  end
end

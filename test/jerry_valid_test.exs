defmodule JerryValidTest do
  use ExUnit.Case

  # Each test in this file corresponds to exactly one TOML file in the test/valid directory.
  # The TOML files have been copied from https://github.com/BurntSushi/toml-test

  test "array-empty" do
    toml = File.read!("test/valid/array-empty.toml") |> Jerry.decode!
    expected = %{"thevoid" => [[[[[]]]]]}
    assert toml == expected
  end

  test "array-nospaces" do
    toml = File.read!("test/valid/array-nospaces.toml") |> Jerry.decode!
    expected = %{"ints" => [1, 2, 3]}
    assert toml == expected
  end

  test "arrays-nested" do
    toml = File.read!("test/valid/arrays-nested.toml") |> Jerry.decode!
    expected = %{"nest" => [["a"], ["b"]]}
    assert toml == expected
  end

  test "arrays" do
    toml = File.read!("test/valid/arrays.toml") |> Jerry.decode!
    expected = %{
      "ints" => [1, 2, 3],
      "floats" => [1.1, 2.1, 3.1],
      "strings" => ["a", "b", "c"],
      "dates" => [
        # TODO datetimes are not supported yet.
        {:toml_datetime, "1987-07-05T17:45:00Z"},
        {:toml_datetime, "1979-05-27T07:32:00Z"},
        {:toml_datetime, "2006-06-01T11:00:00Z"},
      ]
    }
    assert toml == expected
  end

  test "bool" do
    toml = File.read!("test/valid/bool.toml") |> Jerry.decode!
    expected = %{
      "t" => true,
      "f" => false,
    }
    assert toml == expected
  end

  test "comments-everywhere" do
    toml = File.read!("test/valid/comments-everywhere.toml") |> Jerry.decode!
    expected = %{
      "group" => %{
        "answer" => 42,
        "more" => [42, 42]
      }
    }
    assert toml == expected
  end

  test "datetime" do
    toml = File.read!("test/valid/datetime.toml") |> Jerry.decode!
    expected = %{
      # TODO datetimes are not supported yet.
      "bestdayever" => {:toml_datetime, "1987-07-05T17:45:00Z"}
    }
    assert toml == expected
  end

  test "empty" do
    toml = File.read!("test/valid/empty.toml") |> Jerry.decode!
    expected = %{}
    assert toml == expected
  end

  test "example" do
    toml = File.read!("test/valid/example.toml") |> Jerry.decode!
    expected = %{
      # TODO datetimes are not supported yet.
      "best-day-ever" => {:toml_datetime, "1987-07-05T17:45:00Z"},
      "numtheory" => %{
        "boring" => false,
        "perfection" => [6, 28, 496]
      }
    }
    assert toml == expected
  end

  test "float" do
    toml = File.read!("test/valid/float.toml") |> Jerry.decode!
    expected = %{
      "pi" => 3.14,
      "negpi" => -3.14
    }
    assert toml == expected
  end

  test "implicit-and-explicit-after" do
    toml = File.read!("test/valid/implicit-and-explicit-after.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => %{
          "c" => %{
            "answer" => 42
          }
        },
        "better" => 43
      }
    }
    assert toml == expected
  end

  test "implicit-and-explicit-before" do
    toml = File.read!("test/valid/implicit-and-explicit-before.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => %{
          "c" => %{
            "answer" => 42
          }
        },
        "better" => 43
      }
    }
    assert toml == expected
  end

  test "implicit-and-explicit-before-reverse" do
    # Same as the previous test case, only the order of the [a] and the [a.b.c] table is swapped.
    toml = File.read!("test/valid/implicit-and-explicit-before-reverse.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => %{
          "c" => %{
            "answer" => 42
          }
        },
        "better" => 43
      }
    }
    assert toml == expected
  end

  test "implicit-groups" do
    toml = File.read!("test/valid/implicit-groups.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => %{
          "c" => %{
            "answer" => 42
          }
        }
      }
    }
    assert toml == expected
  end

  test "integer" do
    toml = File.read!("test/valid/integer.toml") |> Jerry.decode!
    expected = %{
      "answer" => 42,
      "neganswer" => -42,
    }
    assert toml == expected
  end

  test "key-equals-nospace" do
    toml = File.read!("test/valid/key-equals-nospace.toml") |> Jerry.decode!
    expected = %{"answer" => 42}
    assert toml == expected
  end

  test "key-space" do
    toml = File.read!("test/valid/key-space.toml") |> Jerry.decode!
    expected = %{"a b" => 1}
    assert toml == expected
  end

  test "key-special-chars" do
    toml = File.read!("test/valid/key-special-chars.toml") |> Jerry.decode!
    expected = %{~s{~!@$^&*()_+-`1234567890[]|/?><.,;:'} => 1}
    assert toml == expected
  end

  test "long-float" do
    toml = File.read!("test/valid/long-float.toml") |> Jerry.decode!
    expected = %{
      "longpi" => 3.141592653589793,
      "neglongpi" => -3.141592653589793
    }
    assert toml == expected
  end

  test "long-integer" do
    toml = File.read!("test/valid/long-integer.toml") |> Jerry.decode!
    expected = %{
      "answer" => 9223372036854775807,
      "neganswer" => -9223372036854775808,
    }
    assert toml == expected
  end

  test "multiline-string" do
    toml = File.read!("test/valid/multiline-string.toml") |> Jerry.decode!
    expected = %{
      "multiline_empty_one" => "",
      "multiline_empty_two" => "",
      "multiline_empty_three" => "",
      "multiline_empty_four" => "",
      "equivalent_one" => "The quick brown fox jumps over the lazy dog.",
      "equivalent_two" => "The quick brown fox jumps over the lazy dog.",
      "equivalent_three" => "The quick brown fox jumps over the lazy dog."
    }
    assert toml == expected
  end

  test "raw-multiline-string" do
    toml = File.read!("test/valid/raw-multiline-string.toml") |> Jerry.decode!
    expected = %{
      "oneline" => ~s(This string has a ' quote character.),
      "firstnl" => ~s(This string has a ' quote character.),
      "multiline" => ~s(This string\nhas ' a quote character\nand more than\none newline\nin it.)
    }
    assert toml == expected
  end

  test "raw-string" do
    toml = File.read!("test/valid/raw-string.toml") |> Jerry.decode!
    expected = %{
      "backspace" => ~S(This string has a \b backspace character.),
      "tab" => ~S(This string has a \t tab character.),
      "newline" => ~S(This string has a \n new line character.),
      "formfeed" => ~S(This string has a \f form feed character.),
      "carriage" => ~S(This string has a \r carriage return character.),
      "slash" => ~S(This string has a \/ slash character.),
      "backslash" => ~S(This string has a \\ backslash character.)
    }
    assert toml == expected
  end

  test "string-empty" do
    toml = File.read!("test/valid/string-empty.toml") |> Jerry.decode!
    expected = %{"answer" => ""}
    assert toml == expected
  end

  test "string-escapes" do
    toml = File.read!("test/valid/string-escapes.toml") |> Jerry.decode!
    expected = %{
      "backspace" => "This string has a \b backspace character.",
      "tab" => "This string has a \t tab character.",
      "newline" => "This string has a \n new line character.",
      "formfeed" => "This string has a \f form feed character.",
      "carriage" => "This string has a \r carriage return character.",
      "quote" => "This string has a \" quote character.",
      "backslash" => "This string has a \\ backslash character.",
      "notunicode1" => "This string does not have a unicode \\u escape.",
      "notunicode2" => "This string does not have a unicode \\u escape.",
      "notunicode3" => "This string does not have a unicode \\u0075 escape.",
      "notunicode4" => "This string does not have a unicode \\u escape.",
    }
    assert toml == expected
  end

  test "string-simple" do
    toml = File.read!("test/valid/string-simple.toml") |> Jerry.decode!
    expected = %{"answer" => "You are not drinking enough whisky."}
    assert toml == expected
  end

  test "string-with-pound" do
    toml = File.read!("test/valid/string-with-pound.toml") |> Jerry.decode!
    expected = %{
      "pound" => "We see no # comments here.",
      "poundcomment" => "But there are # some comments here."
    }
    assert toml == expected
  end

  # TODO nested tables not supported yet.
  # test "table-array-implicit" do
  #   toml = File.read!("test/valid/table-array-implicit.toml") |> Jerry.decode!
  #   expected = %{
  #     "albums" => %{
  #       "songs" => %{
  #         "name" => "Glory Days"
  #       }
  #     }
  #   }
  #   assert toml == expected
  # end

  test "table-array-many" do
    toml = File.read!("test/valid/table-array-many.toml") |> Jerry.decode!
    expected = %{
      "people" => [
        %{"first_name" => "Bruce", "last_name" => "Springsteen"},
        %{"first_name" => "Eric", "last_name" => "Clapton"},
        %{"first_name" => "Bob", "last_name" => "Seger"}
      ]
    }
    assert toml == expected
  end

  test "table-array-nest" do
    toml = File.read!("test/valid/table-array-nest.toml") |> Jerry.decode!
    expected = %{
      "albums" => [
        %{
          "name" => "Born to Run",
          "songs" => [
            %{"name" => "Jungleland"},
            %{"name" => "Meeting Across the River"}
          ]
        },
        %{
          "name" => "Born in the USA",
          "songs" => [
            %{"name" => "Glory Days"},
            %{"name" => "Dancing in the Dark"}
          ]
        }
      ]
    }
    assert toml == expected
  end

  test "table-array-one" do
    toml = File.read!("test/valid/table-array-one.toml") |> Jerry.decode!
    expected = %{
      "people" => [
        %{"first_name" => "Bruce", "last_name" => "Springsteen"}
      ]
    }
    assert toml == expected
  end

  test "table-empty" do
    toml = File.read!("test/valid/table-empty.toml") |> Jerry.decode!
    expected = %{"a" => %{}}
    assert toml == expected
  end

  test "table-sub-empty" do
    toml = File.read!("test/valid/table-sub-empty.toml") |> Jerry.decode!
    expected = %{"a" => %{"b" => %{}}}
    assert toml == expected
  end

  test "table-whitespace-key" do
    toml = File.read!("test/valid/table-whitespace-key.toml") |> Jerry.decode!
    expected = %{"table_key" => %{}}
    assert toml == expected
  end

  test "table-whitespace" do
    toml = File.read!("test/valid/table-whitespace.toml") |> Jerry.decode!
    expected = %{"valid key" => %{}}
    assert toml == expected
  end

  test "table-with-pound" do
    toml = File.read!("test/valid/table-with-pound.toml") |> Jerry.decode!
    expected = %{"key#group" => %{"answer" => 42}}
    assert toml == expected
  end

  test "table-key-quoted" do
    toml = File.read!("test/valid/table-key-quoted.toml") |> Jerry.decode!
    expected = %{
      "foo" => %{
        " bar " => %{}
      }
    }
    assert toml == expected
  end

  test "unicode-escape" do
    toml = File.read!("test/valid/unicode-escape.toml") |> Jerry.decode!
    expected = %{
      "answer4" => "δ",
      "answer8" => "δ"
    }
    assert toml == expected
  end

  test "unicode-literal" do
    toml = File.read!("test/valid/unicode-literal.toml") |> Jerry.decode!
    expected = %{"answer" => "δ"}
    assert toml == expected
  end

end

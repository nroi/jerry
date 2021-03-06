defmodule JerryValidTest do
  use ExUnit.Case

  # Each test in this file corresponds to exactly one TOML file in the test/valid directory.
  # The TOML files have mostly been copied from https://github.com/BurntSushi/toml-test


  test "numbers" do
    toml = File.read!("test/valid/numbers.toml") |> Jerry.decode!
    expected = %{
      "flt1" => 1.0,
      "flt2" => 3.1415,
      "flt3" => -0.01,
      "flt4" => 5.0e+22,
      "flt5" => 5.0e-22,
      "flt6" => 1.0e6,
      "flt7" => -2.0e-2,
      "flt8" => 6.626e-34,
      "flt9" => 9_224_617.445_991_228_313,
      "int1" => 99,
      "int2" => 42,
      "int3" => 0,
      "int4" => -17,
      "int5" => 1_000,
      "int6" => 5_349_221,
      "int7" => 1_2_3_4_5
    }
    assert toml == expected
  end

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
    {:ok, dt1, 0} = DateTime.from_iso8601("1987-07-05T17:45:00Z")
    {:ok, dt2, 0} = DateTime.from_iso8601("1979-05-27T07:32:00Z")
    {:ok, dt3, 0} = DateTime.from_iso8601("2006-06-01T11:00:00Z")
    expected = %{
      "ints" => [1, 2, 3],
      "floats" => [1.1, 2.1, 3.1],
      "strings" => ["a", "b", "c"],
      "dates" => [{dt1, 0}, {dt2, 0}, {dt3, 0}]
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
    {:ok, dt, 0} = DateTime.from_iso8601("1987-07-05T17:45:00Z")
    expected = %{
      "bestdayever" => {dt, 0}
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
    {:ok, dt, 0} = DateTime.from_iso8601("1987-07-05T17:45:00Z")
    expected = %{
      "best-day-ever" => {dt, 0},
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

  test "table-array-implicit" do
    toml = File.read!("test/valid/table-array-implicit.toml") |> Jerry.decode!
    expected = %{
      "albums" => %{
        "songs" => [
          %{"name" => "Glory Days"}
        ]
      }
    }
    assert toml == expected
  end

  test "table-array-implicit-extended1" do
    toml = File.read!("test/valid/table-array-implicit-extended1.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => [
          %{"x" => 1}, %{"x" => 2}
        ]
      }
    }
    assert toml == expected
  end

  test "table-array-implicit-extended2" do
    toml = File.read!("test/valid/table-array-implicit-extended2.toml") |> Jerry.decode!
    expected = %{
      "a" => %{
        "b" => [
          %{"x" => 1},
          %{"x" => 2, "y" => 3}
        ]
      }
    }
    assert toml == expected
  end

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

  test "table-hierarchy" do
    toml = File.read!("test/valid/table-hierarchy.toml") |> Jerry.decode!
    expected = %{
      "foo" => %{
        "one" => 1,
        "bar" => %{
          "two" => 2,
          "baz" => %{"three" => 3},
          "boo" => %{"four" => 4},
          "bam" => %{"five" => 5},
        },
        "bbb" => %{},
      }
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
        },
      ]
    }
    assert toml == expected
  end

  test "table-array-nest-simple" do
    toml = File.read!("test/valid/table-array-nest-simple.toml") |> Jerry.decode!
    expected = %{
      "albums" => [
        %{
          "name" => 1,
          "songs" => [
            %{"name" => 2},
            %{"name" => 3}
          ]
        },
        %{
          "name" => 4,
          "songs" => [
            %{"name" => 5},
            %{"name" => 6}
          ]
        },
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

  test "table-array-deeply-nested" do
    toml = File.read!("test/valid/table-array-deeply-nested-empty.toml") |> Jerry.decode!
    expected = %{
      "1" => %{"2" => %{"3" => %{"4" => %{"5" => %{"6" => %{"7" => %{"8" => %{"9" => %{"10" => [%{}]}}}}}}}}}
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
      },
      "j" => %{"ʞ" => %{"l" => %{}}},
      "single quoted" => [%{}]
    }
    assert toml == expected
  end

  test "tables-and-arrays-of-tables" do
    toml = File.read!("test/valid/tables-and-arrays-of-tables.toml") |> Jerry.decode!
    expected = %{
      "table1" => %{"a" => 1},
      "table2" => %{"b" => 2},
      "foo" => [%{"c" => 3}],
      "bar" => [%{"d" => 4}],
      "table3" => [%{"e" => 5}],
      "table4" => [%{"f" => 6}],
    }
    assert toml == expected
  end

  test "array-of-table-inside-table" do
    toml = File.read!("test/valid/array-of-table-inside-table.toml") |> Jerry.decode!
    expected = %{
      "accounts" => %{
        "read_permission" => true,
        "admin" => %{
          "write_permission" => true,
          "account" => [
            %{"name" => "root", "password" => "r00t"},
            %{"name" => "superuser", "password" => "usersuper"}
          ],
        },
        "user" => %{
          "write_permission" => false,
          "account" => [
            %{"name" => "john doe", "password" => "topsecret"},
            %{"name" => "alice", "password" => "confidential"}
          ],
        }
      }
    }
    assert toml == expected
  end

  test "unicode-escape" do
    toml = File.read!("test/valid/unicode-escape.toml") |> Jerry.decode!
    expected = %{
      "answer4" => "δ",
      "answer8" => "δ",
      "smiley" => "😀"
    }
    assert toml == expected
  end

  test "unicode-literal" do
    toml = File.read!("test/valid/unicode-literal.toml") |> Jerry.decode!
    expected = %{
      "answer" => "δ",
      "smiley" => "😀"
    }
    assert toml == expected
  end

  test "toml-example-github" do
    toml = File.read!("test/valid/toml-example-github.toml") |> Jerry.decode!
    {:ok, dt, offset} = DateTime.from_iso8601("1979-05-27T07:32:00-08:00")
    expected = %{
      "clients" => %{
        "data" => [["gamma", "delta"], [1, 2]],
        "hosts" => ["alpha", "omega"]
      },
      "database" => %{
        "connection_max" => 5000,
        "enabled" => true,
        "ports" => [8001, 8001, 8002],
        "server" => "192.168.1.1"},
      "owner" => %{
        "dob" => {dt, offset},
        "name" => "Tom Preston-Werner"
      },
      "servers" => %{
        "alpha" => %{
          "dc" => "eqdc10", "ip" => "10.0.0.1"
        },
        "beta" => %{
          "dc" => "eqdc10",
          "ip" => "10.0.0.2"}
      },
      "title" => "TOML Example"
    }
    assert toml == expected
  end

  test "hugo-config" do
    toml = File.read!("test/valid/hugo-config.toml") |> Jerry.decode!
    expected = %{
      "baseURL" => "https://gohugo.io/",
      "blackfriday" => %{
        "angledQuotes" => false,
        "hrefTargetBlank" => true,
        "latexDashes" => true,
        "plainIDAnchors" => true},
      "enableEmoji" => true,
      "footnotereturnlinkcontents" => "↩",
      "googleAnalytics" => "UA-7131036-4",
      "mediaTypes" => %{
        "text/netlify" => %{
          "delimiter" => "",
          "suffix" => ""
        }
      },
      "menu" => %{
        "docs" => [
          %{"name" => "About Hugo", "weight" => 1},
          %{"name" => "Getting Started", "weight" => 5},
          %{"name" => "Themes", "url" => "/themes/", "weight" => 15},
          %{"name" => "Content Management", "url" => "/content-management/", "weight" => 20},
          %{"identifier" => "templates", "name" => "Templates", "url" => "/templates/", "weight" => 25},
          %{"identifier" => "functions", "name" => "Functions", "url" => "/functions/", "weight" => 30},
          %{"identifier" => "variables", "name" => "Variables", "url" => "/variables/", "weight" => 35},
          %{"identifier" => "commands", "name" => "CLI", "post" => "break", "url" => "/commands/", "weight" => 40}
        ],
        "global" => [
          %{"name" => "News", "weight" => 1},
          %{"name" => "Docs", "weight" => 5},
          %{"name" => "Themes", "weight" => 10},
          %{"name" => "Community", "weight" => 150},
          %{"name" => "GitHub", "weight" => 200}],
        "quicklinks" => [
          %{"name" => "Fundamentals", "weight" => 1}
        ]
      },
      "outputFormats" => %{
        "HEADERS" => %{"mediatype" => "text/netlify"},
        "REDIR" => %{"baseName" => "_redirects", "mediatype" => "text/netlify"}
      },
      "outputs" => %{
        "home" => ["HTML", "RSS", "REDIR", "HEADERS"],
        "section" => ["HTML", "RSS"]
      },
      "paginate" => 100,
      "params" => %{
        "description" => "The world’s fastest framework for building websites",
        "images" => ["images/gohugoio-card.png"]
      },
      "pluralizeListTitles" => false,
      "social" => %{"twitter" => "GoHugoIO"},
      "taxonomies" => %{"category" => "categories"}
    }
    assert toml == expected
  end

  test "arrays-same-type" do
    toml = File.read!("test/valid/arrays-same-type.toml") |> Jerry.decode!
    expected = %{
      "arr1" => [1, 2, 3],
      "arr2" => ["red", "yellow", "green"],
      "arr3" => [[1, 2], [3, 4, 5]],
      "arr4" => ["all", "strings", "are the same", "type"],
      "arr5" => [[1, 2], ["a", "b", "c"]]
    }
    assert toml == expected
  end

  test "arrays-multiline" do
    toml = File.read!("test/valid/arrays-multiline.toml") |> Jerry.decode!
    expected = %{
      "arr7" => [1, 2, 3],
      "arr8" => [1, 2]
    }
    assert toml == expected
  end

  test "inline-tables" do
    toml = File.read!("test/valid/inline-tables.toml") |> Jerry.decode!
    expected = %{
      "name" => %{"first" => "Tom", "last" => "Preston-Werner"},
      "point" => %{"x" => 1, "y" => 2}
    }
    assert toml == expected
  end

  test "inline-tables-multiple-lines" do
    toml = File.read!("test/valid/inline-tables-multiple-lines.toml") |> Jerry.decode!
    expected = %{
      "points" => [
        %{"x" => 1, "y" => 2, "z" => 3},
        %{"x" => 7, "y" => 8, "z" => 9},
        %{"x" => 2, "y" => 4, "z" => 8}
      ]
    }
    assert toml == expected
  end

  test "inline_tables-nested.toml" do
    toml = File.read!("test/valid/inline-tables-nested.toml") |> Jerry.decode!
    expected = %{"a" => %{"b" => %{"c" => %{"d" => 5}}}}
    assert toml == expected
  end

  test "inline_tables-nested-with-comma.toml" do
    toml = File.read!("test/valid/inline-tables-nested-with-comma.toml") |> Jerry.decode!
    expected = %{"a" => %{"depth" => 1, "b" => %{"depth" => 2, "c" => %{"depth" => 3, "d" => 5}}}}
    assert toml == expected
  end

  test "inline_tables-multiline-value.toml" do
    toml = File.read!("test/valid/inline-tables-multiline-value.toml") |> Jerry.decode!
    expected = %{"x" => %{"y" =>
      "inline tables may stretch multiple lines.\n" <>
      "but only if:\n" <>
      "the newline occurs inside a value."
    }}
    assert toml == expected
  end

  test "empty-inline-table" do
    toml = File.read!("test/valid/empty-inline-table.toml") |> Jerry.decode!
    expected = %{"a" => %{}}
    assert toml == expected
  end

  test "empty-quoted-keys" do
    toml = File.read!("test/valid/empty-quoted-keys.toml") |> Jerry.decode!
    expected = %{
      "a" => %{"" => "blank"},
      "b" => %{"" => "also blank"},
    }
    assert toml == expected
  end

  @tag :wip
  test "tables-inside-arrays-of-tables2" do
    toml = File.read!("test/valid/tables-inside-arrays-of-tables2.toml") |> Jerry.decode!
    expected = %{
      "fruit" => [
        %{
          "name" => "apple",
          "physical" => %{"color" => "red"},
        },
      ]
    }
    assert toml == expected
  end

  test "tables-spread" do
    toml = File.read!("test/valid/tables-spread.toml") |> Jerry.decode!
    expected = %{
      "table1" => %{
        "key" => 5,
        "subtable1" => %{
          "key" => 7
        }
      },
      "table2" => %{
        "key" => 6
      }
    }
    assert toml == expected
  end

  @tag :wip
  test "tables-inside-arrays-of-tables" do
    toml = File.read!("test/valid/tables-inside-arrays-of-tables.toml") |> Jerry.decode!
    expected = %{
      "fruit" => [
        %{
          "name" => "apple",
          "physical" => %{"color" => "red", "shape" => "round"},
          "variety" => [
            %{"name" => "red delicious"},
            %{"name" => "granny smith"}
          ]
        },
        %{
          "name" => "banana",
          "variety" => [%{"name" => "plantain"}]
        }
      ]
    }
    assert toml == expected
  end

end

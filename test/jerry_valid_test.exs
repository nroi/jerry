defmodule JerryValidTest do
  use ExUnit.Case

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

end

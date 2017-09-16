defmodule JerryInValidTest do
  use ExUnit.Case

  # Each test in this file corresponds to exactly one TOML file in the test/invalid directory.


  test "duplicate-keys-array-of-tables" do
    assert_raise RuntimeError, fn ->
      File.read!("test/invalid/duplicate-keys-array-of-tables.toml") |> Jerry.decode!
    end
  end

  test "array-mixed-types-arrays-and-ints" do
    assert_raise RuntimeError, fn ->
      File.read!("test/invalid/array-mixed-types-arrays-and-ints.toml") |> Jerry.decode!
    end
  end

end

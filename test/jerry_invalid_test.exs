defmodule JerryInValidTest do
  use ExUnit.Case

  # Each test in this file corresponds to exactly one TOML file in the test/invalid directory.


  test "duplicate-keys-array-of-tables" do
    expected = {:error, "Duplicate key: \"x\""}
    actual = Jerry.decode(File.read!("test/invalid/duplicate-keys-array-of-tables.toml"))
    assert expected == actual
  end

  test "array-mixed-types-arrays-and-ints" do
    expected = {:error, "Mixed types are not allowed in arrays"}
    actual = Jerry.decode(File.read!("test/invalid/array-mixed-types-arrays-and-ints.toml"))
    assert expected == actual
  end

end

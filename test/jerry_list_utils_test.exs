defmodule ListUtilsTest do
  import Jerry.Utils.ListUtils
  use ExUnit.Case

  test "split_children" do
    s = [["foo"], ["foo", "bar"], ["foo", "baz"], ["foo", "bar", "soo"], ["bang"]]
    result = nest_children(s)
    expected = [
      {["foo"], [
        {["foo", "bar"], [
          {["foo", "bar", "soo"], []},
        ]},
        {["foo", "baz"], []},
      ]},
      {["bang"], []},
    ]
    assert(result == expected)
  end

end

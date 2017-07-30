defmodule ListUtilsTest do
  import Jerry.Utils.ListUtils
  test "children" do
    l = [1, 2, 3, 1001, 1002, 1003]
    expected = [{1, [{2, [{3, []}]}]}, {1001, [{1002, [{1003, []}]}]}]
    immediate_predecessor? = fn potential_parent, potential_child ->
      potential_parent + 1 == potential_child
    end
    children1 = children(1, l, immediate_predecessor?)
    children1_expected = [{2, [{3, []}]}]
    children2 = children(2, l, immediate_predecessor?)
    children2_expected = [{3, []}]
    children3 = children(3, l, immediate_predecessor?)
    children3_expected = []
    children1001 = children(1001, l, immediate_predecessor?)
    children1001_expected = [{1002, [{1003, []}]}]
    children1002 = children(1002, l, immediate_predecessor?)
    children1002_expected = [{1003, []}]
    children1003 = children(1003, l, immediate_predecessor?)
    children1003_expected = []
    assert(children1 == children1_expected)
    assert(children2 == children2_expected)
    assert(children3 == children3_expected)
    assert(children1001 == children1001_expected)
    assert(children1002 == children1002_expected)
    assert(children1003 == children1003_expected)
  end
end

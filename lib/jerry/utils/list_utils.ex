defmodule Jerry.Utils.ListUtils do

  def nest_children([], _pred), do: []
  def nest_children(entries = [x|xs], pred) do
    {children, unrelated} = split_children(x, xs, pred)
    [children | nest_children(unrelated, pred)]
  end

  # Given a parent, a list of entries and a predicate which evaluates two entries to true iff the
  # the second argument is the immediate successor of the first argument. Returns a tuple
  # {{successors, rest}. rest contains all entries which are not a successor (immediate or indirect)
  # of the given parent. `successors` contains a list which is structured as follows:
  # Each entry in the list is a tuple {item, successor_def} of type f, where item is an entry,
  # successor_def is a list of tuples, each tuple of type f.
  # The following precondition must be fulfilled: each child must occur after its parent in `entries`.
  # TODO use type annotations, otherwise this function is difficult to grok.
  def split_children(parent, entries, pred) do
    # Each entry must be unique, otherwise the MapSet will not work as supposed to.
    unique_entries = Enum.with_index(entries)
    modified_pred = fn {entry1, _idx1}, {entry2, _idx2} ->
      pred.(entry1, entry2)
    end
    {{{^parent, -1}, children}, used} = successors({parent, -1}, unique_entries, modified_pred, MapSet.new)
    rest = unique_entries
           |> Enum.filter(&(!MapSet.member?(used, &1)))
           |> Enum.map(fn {entry, idx} -> entry end)
    {{parent, Enum.map(children, &without_indices/1)}, rest}
  end

  def without_indices({{x, idx}, descendants}) when is_integer(idx) do
    {x, Enum.map(descendants, &without_indices/1)}
  end

  def successors(parent, [], pred, m = %MapSet{}), do: {{parent, []}, m}
  def successors(parent, entries = [x|xs], pred, m = %MapSet{}) do
    immediate_succs = Enum.filter(entries, fn entry ->
      pred.(parent, entry)
    end)
    descendants = Enum.map(immediate_succs, fn child ->
      successors(child, xs, pred, MapSet.put(m, child))
    end)
    map = Enum.reduce(descendants, m, fn ({_, map}, acc) ->
      MapSet.union(map, acc)
    end)
    clean_descendants = Enum.map(descendants, fn {{x, y}, _map} -> {x, y} end)
    {{parent, clean_descendants}, map}
  end

end

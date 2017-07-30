defmodule Jerry.Utils.ListUtils do
  def children(parent, entries, pred) do
    {children, []} = Enum.reduce(entries, {[], entries}, fn
      child_candidate, {children_so_far, [ _ | yet_to_process]} ->
        case pred.(parent, child_candidate) do
          true ->
            grandchildren = children(child_candidate, yet_to_process, pred)
            new_child = {child_candidate, grandchildren}
            {[new_child | children_so_far], yet_to_process}
          false -> {children_so_far, yet_to_process}
        end
    end)
    children
  end
end

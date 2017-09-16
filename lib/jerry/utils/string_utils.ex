defmodule Jerry.Utils.StringUtils do
  @moduledoc """
  Functions related to string processing.
  """

  @doc """
  Removes the given suffix from the string `s`.
  """
  def remove_suffix(s, suffix), do: String.replace_suffix(s, suffix, "")
end

defmodule Jerry.Utils.StringUtils do
  def remove_suffix(s, suffix), do: String.replace_suffix(s, suffix, "")
end

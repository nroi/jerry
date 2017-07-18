defmodule Jerry.Utils.Macros do

  defmacro source(regex) do
    case regex do
      {:sigil_r, [_], [{:<<>>, [_], [s]}, []]} ->
        s
    end
  end

  defmacro compile(string) do
    quote do
      Regex.compile!(unquote(string))
    end
  end

end

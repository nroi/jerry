defmodule Jerry.Utils.Macros do

  defmacro source(regex) do
    quote do
      Regex.source(unquote(regex))
    end
  end

  defmacro compile(string) do
    quote do
      Regex.compile!(unquote(string))
    end
  end

end

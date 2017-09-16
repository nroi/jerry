Jerry
[![Build Status](https://secure.travis-ci.org/nroi/jerry.png?branch=master "Build Status")](http://travis-ci.org/nroi/jerry)
[![Coverage Status](https://coveralls.io/repos/github/nroi/jerry/badge.svg?branch=master)](https://coveralls.io/github/nroi/jerry?branch=master)
[![Hex pm](https://img.shields.io/hexpm/v/jerry.svg?style=flat)](https://hex.pm/packages/jerry)
============

Jerry is a TOML parser.

Jerry is still a work in progress, the following features are missing:
* Datetimes are not supported, the parser will just return a tuple `{:toml_datetime, datetime}` where
datetime is the verbatim string as it occurs in the TOML file.
* Tables inside arrays of tables are not supported.
* For invalid TOML, the parser may either raise an exception with a cryptic error message, or falsely recognize the TOML as valid.

With these limitations in mind, Jerry should deliver correct results if the
input is valid according to the TOML 0.4 spec and does not contain any tables inside arrays of tables.
If you do find a valid TOML string that is incorrectly parsed by Jerry, please open an issue.

## Usage:

Add jerry as a dependency in your `mix.exs` file:
```Elixir
defp deps do
  [
    {:jerry, "~> 0.1"}
  ]
end
```

Run `mix deps.get` to fetch the dependencies.

`Jerry.decode!` is the only function you want to use, it converts a string into an Elixir map:
```Elixir
~s([table]\nmultiline = """\na string\nspanning over\nmultiple lines.""") |> Jerry.decode!
%{"table" => %{"multiline" => "a string\nspanning over\nmultiple lines."}}
```

## Contributing:
If you find a valid TOML string which is parsed incorrectly by Jerry, please
open an issue, unless the TOML string contains tables inside arrays of tables.
Alternatively, may also create a pull request with a failing test case. Just fork the repository, put the TOML string inside a new file in the `test/valid` directory, and create a new test case in `test/jerry_valid_test.exs`.

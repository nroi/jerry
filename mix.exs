defmodule Jerry.Mixfile do
  use Mix.Project

  def project do
    [app: :jerry,
     version: "0.1.4",
     elixir: "~> 1.5",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     description: description(),
     package: package(),
     deps: deps(),
     name: "Jerry",
     source_url: "https://github.com/nroi/jerry",
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.detail": :test, "coveralls.post": :test, "coveralls.html": :test]
    ]
  end

  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:excoveralls, "~> 0.7", only: :test}
    ]
  end

  defp description() do
    "A TOML parser"
  end

  defp package() do
    [
      maintainers: ["Fabian Muscariello"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/nroi/jerry"}
    ]
  end
end

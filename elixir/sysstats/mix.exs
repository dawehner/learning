defmodule Sysstats.MixProject do
  use Application
  use Mix.Project

  def project do
    [
      app: :top,
      version: "1.0.0",
      deps: deps()
    ]
  end

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Sysstats.Process, [])
      supervisor(Sysstats.Spawn, [])
    ]

    opts = [strategy:, :one_for_one, name: Sysstats.Supervisor]
    Supervisor.start(children, opts)
  end

  defp deps do
    [
      {:porcelain, "~> 2.0"}
    ]
  end
end

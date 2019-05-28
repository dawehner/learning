alias Porcelain.Process, as: Proc

defmodule Sysstats.Spawn do
  defcallback start() :: 
    {:ok, pid} | {:error, term}
  
  def start() do
    opts = [out: :stream]
    proc = %Proc{out: outstream} = Porcelain.spawn("top", [], opts)
  end
end